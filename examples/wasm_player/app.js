const state = {
    wasmModule: null,
    wasmMemory: null,
    audioInitialized: false,
    initializing: false,
    contextStored: false,
    contextPtr: 0,
};

const handleStore = {
    next: 1,
    values: new Map(),
    reverse: new WeakMap(),
    primitive: new Map(),
};

const textDecoder = new TextDecoder("utf-8");
let cachedUint8 = null;
let cachedUint32 = null;

function resetCachedViews() {
    cachedUint8 = null;
    cachedUint32 = null;
}

function getUint8Memory() {
    if (!state.wasmMemory) return null;
    if (!cachedUint8 || cachedUint8.buffer !== state.wasmMemory.buffer) {
        cachedUint8 = new Uint8Array(state.wasmMemory.buffer);
    }
    return cachedUint8;
}

function getUint32Memory() {
    if (!state.wasmMemory) return null;
    if (!cachedUint32 || cachedUint32.buffer !== state.wasmMemory.buffer) {
        cachedUint32 = new Uint32Array(state.wasmMemory.buffer);
    }
    return cachedUint32;
}

function UTF8ToString(ptr) {
    if (!ptr) return "";
    const memory = getUint8Memory();
    if (!memory || ptr >= memory.length) return "";
    let end = ptr;
    while (end < memory.length && memory[end] !== 0) {
        end += 1;
    }
    return textDecoder.decode(memory.subarray(ptr, end));
}

function readHandles(ptr, len) {
    if (!ptr || len <= 0) return [];
    const memory = getUint32Memory();
    if (!memory) return [];
    const start = ptr >>> 2;
    const handles = [];
    for (let i = 0; i < len; i += 1) {
        handles.push(memory[start + i]);
    }
    return handles;
}

function getJSValueHandle(obj) {
    if (obj === null || obj === undefined) return 0;

    if (typeof obj === "object" || typeof obj === "function") {
        const existing = handleStore.reverse.get(obj);
        if (existing) {
            return existing;
        }
        const handle = handleStore.next++;
        handleStore.values.set(handle, obj);
        handleStore.reverse.set(obj, handle);
        return handle;
    }

    const key = `${typeof obj}:${String(obj)}`;
    if (handleStore.primitive.has(key)) {
        return handleStore.primitive.get(key);
    }
    const handle = handleStore.next++;
    handleStore.values.set(handle, obj);
    handleStore.primitive.set(key, handle);
    return handle;
}

function getJSValue(handle) {
    if (handle === 0) return null;
    return handleStore.values.get(handle) ?? null;
}

function releaseJSValue(handle) {
    if (handle === 0) return;
    const value = handleStore.values.get(handle);
    if (!value) return;
    handleStore.values.delete(handle);
    if (typeof value === "object" || typeof value === "function") {
        handleStore.reverse.delete(value);
    } else {
        const key = `${typeof value}:${String(value)}`;
        handleStore.primitive.delete(key);
    }
}

function buildArgs(ptr, len) {
    const handles = readHandles(ptr, len);
    return handles.map((handle) => getJSValue(handle));
}

const jsInterop = createJsInterop();

function createJsInterop() {
    return {
        js_global_get(propPtr) {
            try {
                const prop = UTF8ToString(propPtr);
                return getJSValueHandle(globalThis[prop]);
            } catch (error) {
                console.error("js_global_get error:", error);
                return 0;
            }
        },
        js_value_new(classHandle, argsPtr, argsLen) {
            try {
                const Class = getJSValue(classHandle);
                if (typeof Class !== "function") {
                    return 0;
                }
                const args = buildArgs(argsPtr, argsLen);
                return getJSValueHandle(new Class(...args));
            } catch (error) {
                console.error("js_value_new error:", error);
                return 0;
            }
        },
        js_value_get(objHandle, propPtr) {
            try {
                const obj = getJSValue(objHandle);
                if (obj == null) return 0;
                const prop = UTF8ToString(propPtr);
                return getJSValueHandle(obj[prop]);
            } catch (error) {
                console.error("js_value_get error:", error);
                return 0;
            }
        },
        js_value_set(objHandle, propPtr, valueHandle) {
            try {
                const obj = getJSValue(objHandle);
                if (obj == null) return;
                const prop = UTF8ToString(propPtr);
                obj[prop] = getJSValue(valueHandle);
            } catch (error) {
                console.error("js_value_set error:", error);
            }
        },
        js_value_call(objHandle, methodPtr, argsPtr, argsLen) {
            try {
                const obj = getJSValue(objHandle);
                if (obj == null) return 0;
                const method = UTF8ToString(methodPtr);
                const fn = obj[method];
                if (typeof fn !== "function") {
                    return 0;
                }
                const args = buildArgs(argsPtr, argsLen);
                return getJSValueHandle(fn.apply(obj, args));
            } catch (error) {
                console.error("js_value_call error:", error);
                return 0;
            }
        },
        js_value_call_func(funcHandle, thisHandle, argsPtr, argsLen) {
            try {
                const func = getJSValue(funcHandle);
                if (typeof func !== "function") return 0;
                const thisObj = getJSValue(thisHandle);
                const args = buildArgs(argsPtr, argsLen);
                return getJSValueHandle(func.apply(thisObj, args));
            } catch (error) {
                console.error("js_value_call_func error:", error);
                return 0;
            }
        },
        js_value_truthy(handle) {
            return !!getJSValue(handle);
        },
        js_value_is_null(handle) {
            const value = getJSValue(handle);
            return value === null || value === undefined;
        },
        js_value_release(handle) {
            releaseJSValue(handle);
        },
        js_string_new(strPtr) {
            try {
                return getJSValueHandle(UTF8ToString(strPtr));
            } catch (error) {
                console.error("js_string_new error:", error);
                return 0;
            }
        },
        js_number_new(value) {
            return getJSValueHandle(value);
        },
        js_float32_array_new(bufferPtr, len) {
            try {
                if (!state.wasmMemory || !bufferPtr) return 0;
                const data = new Float32Array(state.wasmMemory.buffer, bufferPtr, len);
                return getJSValueHandle(new Float32Array(data));
            } catch (error) {
                console.error("js_float32_array_new error:", error);
                return 0;
            }
        },
        js_blob_new(partsPtr, partsLen, mimeTypePtr) {
            try {
                const mimeType = UTF8ToString(mimeTypePtr) || "text/plain";
                const parts = readHandles(partsPtr, partsLen).map((handle) => getJSValue(handle));
                return getJSValueHandle(new Blob(parts, { type: mimeType }));
            } catch (error) {
                console.error("js_blob_new error:", error);
                return 0;
            }
        },
        js_url_create_object_url(blobHandle) {
            try {
                const blob = getJSValue(blobHandle);
                if (!blob) return 0;
                return getJSValueHandle(URL.createObjectURL(blob));
            } catch (error) {
                console.error("js_url_create_object_url error:", error);
                return 0;
            }
        },
        js_add_event_listener(targetHandle, eventPtr, callbackHandle) {
            try {
                const target = getJSValue(targetHandle);
                const callback = getJSValue(callbackHandle);
                if (!target || typeof target.addEventListener !== "function" || typeof callback !== "function") {
                    return;
                }
                const event = UTF8ToString(eventPtr);
                target.addEventListener(event, callback);
            } catch (error) {
                console.error("js_add_event_listener error:", error);
            }
        },
        js_remove_event_listener(targetHandle, eventPtr, callbackHandle) {
            try {
                const target = getJSValue(targetHandle);
                const callback = getJSValue(callbackHandle);
                if (!target || typeof target.removeEventListener !== "function" || typeof callback !== "function") {
                    return;
                }
                const event = UTF8ToString(eventPtr);
                target.removeEventListener(event, callback);
            } catch (error) {
                console.error("js_remove_event_listener error:", error);
            }
        },
        js_promise_then(promiseHandle, onFulfilledHandle) {
            try {
                const promise = getJSValue(promiseHandle);
                const onFulfilled = getJSValue(onFulfilledHandle);
                if (!promise || typeof promise.then !== "function" || typeof onFulfilled !== "function") {
                    return;
                }
                promise.then(onFulfilled).catch((error) => {
                    console.error("Promise rejected:", error);
                });
            } catch (error) {
                console.error("js_promise_then error:", error);
            }
        },
        js_create_callback(ctxPtr, funcNamePtr) {
            try {
                const funcName = UTF8ToString(funcNamePtr);
                const callback = function callback() {
                    const fn = state.wasmModule?.exports?.[funcName];
                    if (typeof fn === "function") {
                        fn(ctxPtr);
                    }
                };
                return getJSValueHandle(callback);
            } catch (error) {
                console.error("js_create_callback error:", error);
                return 0;
            }
        },
        js_create_script_processor_callback(ctxPtr, funcNamePtr) {
            try {
                const funcName = UTF8ToString(funcNamePtr);
                const callback = function callback(event) {
                    const fn = state.wasmModule?.exports?.[funcName];
                    if (typeof fn !== "function") return;
                    const outputBufferHandle = getJSValueHandle(event?.outputBuffer ?? null);
                    try {
                        fn(ctxPtr, outputBufferHandle);
                    } finally {
                        releaseJSValue(outputBufferHandle);
                    }
                };
                return getJSValueHandle(callback);
            } catch (error) {
                console.error("js_create_script_processor_callback error:", error);
                return 0;
            }
        },
    };
}

async function loadWASM() {
    try {
        const response = await fetch("zig-out/bin/player.wasm");
        if (!response.ok) {
            throw new Error(`Failed to fetch WASM (${response.status})`);
        }
        const wasmBytes = await response.arrayBuffer();

        const wasi = createWasiImports();
        const instantiatePromise = WebAssembly.instantiate(wasmBytes, {
            env: jsInterop,
            wasi_snapshot_preview1: wasi,
        });
        const timeoutPromise = new Promise((_, reject) => {
            setTimeout(() => reject(new Error("WASM instantiation timeout after 5 seconds")), 5000);
        });

        const wasm = await Promise.race([instantiatePromise, timeoutPromise]);
        state.wasmModule = wasm.instance;
        state.wasmMemory = wasm.instance.exports.memory;
        window.wasmModule = wasm.instance;
        window.wasmMemory = state.wasmMemory;
        resetCachedViews();
        updateStatus("WASM module loaded successfully");
        return true;
    } catch (error) {
        showError(`Failed to load WASM module: ${error.message}`);
        console.error(error);
        return false;
    }
}

function createWasiImports() {
    const writeResult = (ptr, value) => {
        const memory = getUint32Memory();
        if (memory && ptr) {
            memory[ptr >>> 2] = value;
        }
    };

    return {
        fd_write(fd, iovs, iovsLen, nwritten) {
            writeResult(nwritten, 0);
            return 0;
        },
        fd_read(fd, iovs, iovsLen, nread) {
            writeResult(nread, 0);
            return 0;
        },
        fd_pread(fd, iovs, iovsLen, offset, nread) {
            writeResult(nread, 0);
            return 0;
        },
        fd_pwrite(fd, iovs, iovsLen, offset, nwritten) {
            writeResult(nwritten, 0);
            return 0;
        },
        fd_seek(fd, offset, whence, newOffset) {
            writeResult(newOffset, 0);
            return 0;
        },
        fd_close() {
            return 0;
        },
        fd_fdstat_get() {
            return 0;
        },
        fd_filestat_get() {
            return 0;
        },
        environ_sizes_get(countPtr, bufSizePtr) {
            writeResult(countPtr, 0);
            writeResult(bufSizePtr, 0);
            return 0;
        },
        environ_get() {
            return 0;
        },
        proc_exit(code) {
            throw new Error(`WASI proc_exit called with code ${code}`);
        },
        random_get(buf, bufLen) {
            if (!state.wasmMemory) return 0;
            const bytes = new Uint8Array(bufLen);
            crypto.getRandomValues(bytes);
            new Uint8Array(state.wasmMemory.buffer).set(bytes, buf);
            return 0;
        },
        clock_time_get(clockId, precision, timePtr) {
            if (!state.wasmMemory) return 0;
            const view = new DataView(state.wasmMemory.buffer);
            const now = BigInt(Date.now()) * 1000000n;
            view.setBigUint64(timePtr, now, true);
            return 0;
        },
        path_open() {
            return 8;
        },
        fd_prestat_get() {
            return 8;
        },
        fd_prestat_dir_name() {
            return 8;
        },
        fd_filestat_set_size() {
            return 8;
        },
        fd_filestat_set_times() {
            return 8;
        },
        fd_readdir() {
            return 8;
        },
        path_filestat_get() {
            return 8;
        },
        path_filestat_set_times() {
            return 8;
        },
        path_rename() {
            return 8;
        },
        path_remove_directory() {
            return 8;
        },
        path_unlink_file() {
            return 8;
        },
        fd_allocate() {
            return 8;
        },
        fd_sync() {
            return 0;
        },
        fd_datasync() {
            return 0;
        },
        fd_advise() {
            return 0;
        },
        poll_oneoff() {
            return 8;
        },
    };
}

function updateStatus(message) {
    const statusEl = document.getElementById("status");
    if (statusEl) {
        statusEl.textContent = message;
    }
}

function showError(message) {
    const container = document.getElementById("error-container");
    if (!container) return;
    container.innerHTML = "";
    const errorDiv = document.createElement("div");
    errorDiv.className = "error";
    errorDiv.textContent = message;
    container.appendChild(errorDiv);
}

function clearError() {
    const container = document.getElementById("error-container");
    if (container) {
        container.innerHTML = "";
    }
}

async function loadAudioFile(file) {
    if (!state.wasmModule) {
        showError("WASM module not loaded");
        return false;
    }

    try {
        const arrayBuffer = await file.arrayBuffer();
        const uint8Array = new Uint8Array(arrayBuffer);

        const ptr = state.wasmModule.exports.malloc(uint8Array.length);
        if (ptr === 0) {
            showError("Failed to allocate memory");
            return false;
        }

        new Uint8Array(state.wasmMemory.buffer).set(uint8Array, ptr);

        const result = state.wasmModule.exports.zoto_load_audio(ptr, uint8Array.length);
        if (result !== 0) {
            state.wasmModule.exports.free(ptr);
            showError(`Failed to load audio: ${result}`);
            return false;
        }

        state.wasmModule.exports.free(ptr);

        updateStatus(`Audio loaded: ${file.name} (${(arrayBuffer.byteLength / 1024).toFixed(2)} KB)`);
        document.getElementById("file-name").textContent = file.name;
        document.getElementById("play-btn").disabled = false;
        document.getElementById("stop-btn").disabled = false;
        clearError();
        return true;
    } catch (error) {
        showError(`Error loading audio file: ${error.message}`);
        console.error(error);
        return false;
    }
}

function playAudio() {
    if (!state.wasmModule) return;
    try {
        const result = state.wasmModule.exports.zoto_play();
        if (result !== 0) {
            showError(`Failed to play audio: ${result}`);
            return;
        }
        updateStatus("Playing...");
        document.getElementById("play-btn").disabled = true;
        document.getElementById("pause-btn").disabled = false;
        clearError();
        checkPlayingStatus();
    } catch (error) {
        showError(`Error playing audio: ${error.message}`);
    }
}

function pauseAudio() {
    if (!state.wasmModule) return;
    try {
        const result = state.wasmModule.exports.zoto_pause();
        if (result !== 0) {
            showError(`Failed to pause audio: ${result}`);
            return;
        }
        updateStatus("Paused");
        document.getElementById("play-btn").disabled = false;
        document.getElementById("pause-btn").disabled = true;
    } catch (error) {
        showError(`Error pausing audio: ${error.message}`);
    }
}

function stopAudio() {
    if (!state.wasmModule) return;
    try {
        state.wasmModule.exports.zoto_stop();
        updateStatus("Stopped");
        document.getElementById("play-btn").disabled = false;
        document.getElementById("pause-btn").disabled = true;
    } catch (error) {
        showError(`Error stopping audio: ${error.message}`);
    }
}

function checkPlayingStatus() {
    if (!state.wasmModule) return;
    try {
        const playing = state.wasmModule.exports.zoto_is_playing();
        if (playing === 0) {
            updateStatus("Finished");
            document.getElementById("play-btn").disabled = false;
            document.getElementById("pause-btn").disabled = true;
        } else {
            setTimeout(checkPlayingStatus, 100);
        }
    } catch {
        // Ignore errors during polling
    }
}

function updateVolume(value) {
    if (!state.wasmModule) return;
    try {
        state.wasmModule.exports.zoto_set_volume(value / 100);
        document.getElementById("volume-value").textContent = value;
    } catch (error) {
        showError(`Error setting volume: ${error.message}`);
    }
}

const log = (...args) => console.log("[zoto]", ...args);
const yieldToBrowser = () => new Promise((resolve) => setTimeout(resolve, 0));

async function performAudioInitialization() {
    log("performAudioInitialization: begin");
    await yieldToBrowser();
    log("performAudioInitialization: after first yield");

    if (!state.contextStored) {
        log("performAudioInitialization: calling zoto_init");
        const initResult = state.wasmModule.exports.zoto_init(44100, 2, 2);
        if (initResult !== 0) {
            throw new Error(`zoto_init failed (${initResult})`);
        }
        state.contextStored = true;
    }

    await yieldToBrowser();
    log("performAudioInitialization: after storing context");
    log("performAudioInitialization: calling zoto_create_context");
    const ctxPtr = state.wasmModule.exports.zoto_create_context();
    if (ctxPtr === 0) {
        throw new Error("Failed to create context");
    }
    state.contextPtr = ctxPtr;

    await yieldToBrowser();
    log("performAudioInitialization: before setup");
    log("performAudioInitialization: invoking zoto_setup_after_user_interaction");
    state.wasmModule.exports.zoto_setup_after_user_interaction(ctxPtr);
    log("performAudioInitialization: setup complete");
    state.audioInitialized = true;
    updateStatus("Ready - Select an audio file to play");
}

const enableButton = document.getElementById("enable-audio");

function setEnableButtonState(text, disabled) {
    if (!enableButton) return;
    enableButton.textContent = text;
    enableButton.disabled = disabled;
}

function initAudioOnInteraction() {
    if (state.audioInitialized || state.initializing) return;
    if (!state.wasmModule) {
        log("initAudioOnInteraction: wasm module not ready");
        updateStatus("WASM still loading, please try again…");
        return;
    }
    state.initializing = true;
    setEnableButtonState("Initializing...", true);
    updateStatus("Initializing audio context...");
    log("initAudioOnInteraction: scheduling initialization");

    setTimeout(() => {
        log("initAudioOnInteraction: timeout callback fired");
        performAudioInitialization().catch((error) => {
            showError(`Failed to initialize audio: ${error.message}`);
            console.error(error);
            state.audioInitialized = false;
            setEnableButtonState("Enable Audio", false);
        }).finally(() => {
            state.initializing = false;
            if (state.audioInitialized) {
                setEnableButtonState("Audio Ready", true);
            } else if (!state.wasmModule) {
                setEnableButtonState("Enable Audio", false);
            }
        });
    }, 0);
}

document.getElementById("file-input").addEventListener("change", async (event) => {
    const [file] = event.target.files;
    if (file) {
        await loadAudioFile(file);
    }
});

document.getElementById("play-btn").addEventListener("click", playAudio);
document.getElementById("pause-btn").addEventListener("click", pauseAudio);
document.getElementById("stop-btn").addEventListener("click", stopAudio);

document.getElementById("enable-audio").addEventListener("click", initAudioOnInteraction);
document.getElementById("volume-slider").addEventListener("input", (event) => {
    updateVolume(Number(event.target.value));
});

window.addEventListener("load", async () => {
    updateStatus("Loading WASM module...");
    const ok = await loadWASM();
    if (ok) {
        setEnableButtonState("Enable Audio", false);
    } else {
        setEnableButtonState("Reload and try again", true);
    }
});

window.addEventListener("beforeunload", () => {
    if (state.wasmModule) {
        state.wasmModule.exports.zoto_cleanup();
    }
});
