//use std::f32;
use petgraph::stable_graph::NodeIndex;
use processgraph::*;
use std::os::raw::{c_float, c_int};
use std::slice;

pub struct UGenState {
    output: Vec<f64>,
    output_indices: Vec<NodeIndex>,
    flow: Vec<NodeIndex>,
    graph: UGenGraph,
}

#[no_mangle]
pub extern "C" fn new_state() -> *mut UGenState {
    Box::into_raw(Box::new(UGenState {
        output: Vec::new(),
        output_indices: Vec::new(),
        flow: Vec::new(),
        graph: new_graph(),
    }))
}

#[no_mangle]
pub extern "C" fn state_free(state: *mut UGenState) {
    if state.is_null() {
        return;
    }
    // here it goes out of scope and thus gets dropped by rust
    unsafe { Box::from_raw(state) };
}

#[no_mangle]
pub extern "C" fn set_graph(state: *mut UGenState) {
    unsafe {
        let idx1 = (*state).graph.add_node(noise(100));
        let idx2 = (*state).graph.add_node(lpf(300., 3.));
        (*state).graph.add_edge(idx1, idx2, (0, 1.0));
        (*state).output_indices = vec![idx1, idx2];
        (*state).output = vec![0.0; 2];
        (*state).flow = establish_flow(&(*state).graph, &(*state).output_indices);
    }
}

#[no_mangle]
pub extern "C" fn process(state: &mut UGenState, sc_out: *mut *mut c_float, sc_nsamples: c_int) {
    let n_out = (*state).output_indices.len();
    unsafe {
        let out_buffer: &mut [*mut f32] = slice::from_raw_parts_mut(sc_out, n_out as usize);
        let mut channels: Vec<&mut [f32]> = Vec::new();
        for c in 0..n_out {
            channels.push(slice::from_raw_parts_mut(
                out_buffer[c],
                sc_nsamples as usize,
            ));
        }

        for i in 0..sc_nsamples {
            process_graph(
                &mut (*state).graph,
                &(*state).flow,
                &(*state).output_indices,
                &mut (*state).output,
            );
            // insert in output
            for channel in 0..n_out {
                channels[channel][i as usize] = (*state).output[channel] as f32;
            }
        }
    }
}
