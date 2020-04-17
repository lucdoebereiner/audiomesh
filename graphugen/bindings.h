#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <new>

struct UGenState;

extern "C" {

UGenState *new_state();

void process(UGenState *state, float** sc_out, int sc_nsamples);

void set_graph(UGenState *state);

void state_free(UGenState *state);

} // extern "C"
