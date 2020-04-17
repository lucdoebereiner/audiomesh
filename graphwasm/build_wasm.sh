#!/bin/sh -e

echo "compile wasm"
cargo build --target wasm32-unknown-unknown --release

# echo "gc wasm"
# RUST_BACKTRACE=1 wasm-gc target/wasm32-unknown-unknown/release/wasm_audioworklet_synth.wasm -o wasm/wasm_audioworklet_synth.wasm

echo "copy"
cp ../target/wasm32-unknown-unknown/release/graphwasm.wasm ./wasm/

echo "optimizing"
cd ./wasm/
wasm-opt -O3 -o graphwasm_opt.wasm graphwasm.wasm
rm graphwasm.wasm
mv graphwasm_opt.wasm graphwasm.wasm
cd ..

echo "finish!"
