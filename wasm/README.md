Emscripten crossfiles are provided for Windows / non-Windows

For IntelliJ, `emscripten-cmake.bat` is provided as a forwarder to `emcmake cmake`

To make an IntelliJ toolchain:
- run `pnpm run build` in parent before importing in IntelliJ to prepopulate cross data
- set CMake to `emscripten-cmake`
- set C compiler to installed `emcc`
- set C++ compiler to installed `em++`
