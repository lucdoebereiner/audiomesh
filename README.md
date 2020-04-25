# audiomesh
A rust workspace for sound synthesis signal networks

There are currently three packages in this workspace:

* `processgraph` - is the fundamental library that defines graphs of stateful processes.
* `graphugen` - c++ template code and ffi for creating SuperCollider-Ugens using `processgraph`.
* `graphwasm` - template code, script and bridge code for generating wasm webaudio worklets.
* `audiomesh` - a binary using jack audio for standalone executables using processgraph

## Requirements

* rust (with target wasm32-unknown-unknown)
* jack audio including dev lib
* SuperCollider (+ source) for `graphugen`
* python for `http.server`
* wasm-opt
