const ctx = new AudioContext();
if (ctx.audioWorklet === undefined) {
    alert("AudioWorklet isn't supported... It cannot work.");
} else {
  ctx.audioWorklet.addModule('js/worklet.js?t=' + new Date().getTime()).then(() => {
      const n = new AudioWorkletNode(ctx, 'my-processor', { numberOfInputs: 1,
							    numberOfOutputs: 1,
							    outputChannelCount: [2], } );
      n.connect(ctx.destination);

      fetch('wasm/graphwasm.wasm?t=' + new Date().getTime())
	  .then(r => r.arrayBuffer())
	  .then(r => n.port.postMessage({ type: 'loadWasm', data: r }));

      // const button = document.getElementById('start');
      // button.onclick = function() {
      //     n.port.postMessage({ type: 'trigger' });
      // };
      
  });
}


document.querySelector('#start').addEventListener('click', function() {
    ctx.resume().then(() => {
	console.log('Playback resumed successfully');
    });
});

