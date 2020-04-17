#include "SC_PlugIn.h"
#include <stdio.h>
#include "./bindings.h"

static InterfaceTable *ft;

struct RustSynth : public Unit {
  struct UGenState* state;
};

extern "C" {
  void load(InterfaceTable *inTable);

  void RustSynth_Ctor(RustSynth* unit);
  void RustSynth_next_a(RustSynth* unit, int inNumSamples);
  void RustSynth_Dtor(RustSynth* unit);
  
};


void RustSynth_Ctor(RustSynth* unit) {
  SETCALC(RustSynth_next_a);

  unit->state = new_state();

  set_graph(unit->state);
  
  //  unit->phases_prev = (float*)RTAlloc(unit->mWorld, n * sizeof(float));
  
  RustSynth_next_a(unit, 1);
}


void RustSynth_next_a(RustSynth* unit, int inNumSamples) {
  
  UGenState* state = unit->state;
  //float** inputs = unit->mInBuf;

  process(state, unit->mOutBuf, inNumSamples);

}

void RustSynth_Dtor(RustSynth* unit)
{

  state_free(unit->state);
  //RTFree(unit->mWorld, unit->phases_prev);
  
}


PluginLoad(RustSynth)
{
  ft = inTable;

  DefineDtorUnit(RustSynth);
  //  DefineSimpleUnit(RustSynth);
}


