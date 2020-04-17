RustSynth : MultiOutUGen {
	*ar { arg n;
		//		arg;
		^this.multiNewList(['audio', n]);
	}

	init {arg ... theInputs;
		inputs = theInputs;
		^this.initOutputs(inputs[0], 'audio');
	}	

}