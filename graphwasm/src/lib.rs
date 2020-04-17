#[macro_use]
extern crate lazy_static;

use petgraph::stable_graph::NodeIndex;
use processgraph::*;
use std::sync::Mutex;

#[no_mangle]
pub extern "C" fn alloc(size: usize) -> *mut f32 {
    let vec: Vec<f32> = vec![0.123; size];
    Box::into_raw(vec.into_boxed_slice()) as *mut f32
    // let mut buf = Vec::<f32>::with_capacity(size);
    // let ptr = buf.as_mut_ptr();
    // std::mem::forget(buf);
    // ptr as *mut f32
}

pub struct UGenState {
    output: Vec<f64>,
    output_indices: Vec<NodeIndex>,
    flow: Vec<NodeIndex>,
    graph: UGenGraph,
}

fn process_state(state: &mut UGenState) {
    process_graph(
        &mut state.graph,
        &state.flow,
        &state.output_indices,
        &mut state.output,
    );
}

fn make_state() -> UGenState {
    let mut g = new_graph();
    let idx1 = g.add_node(noise(100));
    let idx2 = g.add_node(lpf(300., 3.));
    let out_idx = vec![idx1, idx2];
    let flow = establish_flow(&g, &out_idx);
    g.add_edge(idx1, idx2, 0);
    UGenState {
        output: vec![0.0; 2],
        output_indices: out_idx,
        flow: flow,
        graph: g,
    }
}

lazy_static! {
    static ref STATE: Mutex<UGenState> = Mutex::new(make_state());
}

// fixed to 2 channels
#[no_mangle]
pub extern "C" fn process(out_ptr_l: *mut f32, out_ptr_r: *mut f32, size: usize) {
    let mut state = STATE.lock().unwrap();
    let out_buf_l: &mut [f32] = unsafe { std::slice::from_raw_parts_mut(out_ptr_l, size) };
    let out_buf_r: &mut [f32] = unsafe { std::slice::from_raw_parts_mut(out_ptr_r, size) };
    for i in 0..128 {
        process_state(&mut state);
        out_buf_l[i] = state.output[0] as f32;
        out_buf_r[i] = state.output[1] as f32;
    }
}
