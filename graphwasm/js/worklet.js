
class MyProcessor extends AudioWorkletProcessor {

  constructor() {
    super()
    this.port.onmessage = e => {
      if (e.data.type === 'loadWasm') {
        WebAssembly.instantiate(e.data.data).then(w => {
            this._wasm = w.instance;
            this._size = 128;
            this._inPtr = this._wasm.exports.alloc(this._size);
            this._outPtr_l = this._wasm.exports.alloc(this._size);
	    this._outPtr_r = this._wasm.exports.alloc(this._size);
            this._inBuf = new Float32Array(
		this._wasm.exports.memory.buffer,
		this._inPtr,
		this._size
            );
            this._outBuf_l = new Float32Array(
		this._wasm.exports.memory.buffer,
		this._outPtr_l,
		this._size
            );
	    this._outBuf_r = new Float32Array(
		this._wasm.exports.memory.buffer,
		this._outPtr_r,
		this._size
            );
        });
      } else if (e.data.type === 'start') {
//        this._wasm.exports.trigger()
      }
    };
  }

  process(inputs, outputs, parameters) {
      if (!this._wasm) {
	  return true;
      }

      let output = outputs[0];
      
      // TODO set input
      this._wasm.exports.process(this._outPtr_l, this._outPtr_r, this._size);
      output[0].set(this._outBuf_l);
      output[1].set(this._outBuf_r);

      return true;
  }
}

registerProcessor('my-processor', MyProcessor);
