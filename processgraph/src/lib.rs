mod compenv;
mod filters;
pub mod integrator;
pub mod lag;
pub mod processspec;
pub mod process;
pub mod tapdelay;
use crate::processspec::*;
use crate::lag::{lag_freq_deserialize, lag_freq_serialize, Lag};
use crate::process::*;
use crate::tapdelay::TapDelay;
//use petgraph::stable_graph::StableGraph;
//use petgraph::graph::Graph;
use petgraph::stable_graph::*;
use petgraph::visit::IntoNodeReferences;
use petgraph::Directed;
use petgraph::Direction::*;
//use rand::rngs::SmallRng;
use rand::Rng;
use std::f64;
use std::fmt;
pub mod numerical;
use petgraph::visit::Bfs;
use rand::seq::SliceRandom;
use rand::thread_rng;
use serde::{Deserialize, Serialize};
use serde_json::Result;

static mut DEBUG: bool = false;

// TODO
// move debug flag to main

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum ClipType {
    None,
    SoftClip,
    Wrap,
}

#[derive(Serialize, Deserialize)]
pub struct UGen {
    #[serde(skip)]
    feedback_delay: bool,
    process: Process,
    #[serde(skip)]
    value: Option<f64>,
    #[serde(skip)]
    pub last_value: f64,
    sum_inputs: bool,
    clip: ClipType,
    pub output_sends: Vec<(usize, lag::Lag)>,
    // #[serde(skip_deserializing)]
    // process_type: Option<ProcessType>,
    pub output_amp: lag::Lag,
}

impl UGen {
    pub fn new(process: Process) -> Self {
        // let pt = process.spec().process_type;
        UGen {
            feedback_delay: false,
            process,
            value: None,
            sum_inputs: true,
            last_value: 0.0,
            clip: ClipType::None,
            output_sends: vec![],
            // process_type: Some(pt),
            output_amp: lag::lag(0.0),
        }
    }

    pub fn init_after_deserialization(&mut self) {
        // self.process_type = Some(self.process.spec().process_type);
        self.output_amp.set_duration(2.0);
    }

    pub fn clip(self, clip_type: ClipType) -> Self {
        UGen {
            clip: clip_type,
            ..self
        }
    }

    pub fn sum_inputs(self) -> Self {
        UGen {
            sum_inputs: true,
            ..self
        }
    }

    pub fn set_output(&mut self, idx: usize, amp: f64) {
        match self.output_sends.iter().position(|(i, _)| *i == idx) {
            Some(pos) => self.output_sends[pos].1.set_target(amp),
            None => self.output_sends.push((idx, lag::lag(amp))),
        }
    }

    fn process(&mut self, input: &[f64]) -> f64 {
        match self.value {
            Some(v) => v,
            None => {
                let v = self.process.process(input);
                let output = match self.clip {
                    ClipType::None => v,
                    ClipType::SoftClip => v.tanh(),
                    ClipType::Wrap => numerical::wrap(v, -1., 1.),
                };
                if unsafe { DEBUG } {
                    println!("processing: [{:?}] with result {}", self, output)
                }
                self.last_value = output;
                self.value = Some(output);
                self.process.clear_inputs();
                output
            }
        }
    }

    fn reset(&mut self) {
        self.value = None
    }

    fn set_input_with_connection(&mut self, connection: (u32, f64)) {
        let (idx, value) = connection;
        if unsafe { DEBUG } {
            println!("setting input of [{:?}] to {}", self, value)
        }
        self.process.set_input(idx, value, self.sum_inputs);
        if unsafe { DEBUG } {
            println!("having set [{:?}]", self)
        }
    }
}

impl fmt::Debug for UGen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "UGenData: name: {}, value: {:?}, process: {:?}, clip: {:?}, sum_inputs: {:?}",
            //self.feedback_delay,
            self.process.name(),
            self.value,
            self.process,
            self.clip,
            self.sum_inputs
        )
    }
}

// index and weight
//pub type Connection = (u32, lag::Lag);

#[derive(Serialize, Deserialize, Debug)]
pub struct Connection {
    input_idx: u32,
    weight: Lag,
    delay: TapDelay,
    #[serde(skip)]
    pub input: f64,
    #[serde(serialize_with = "lag_freq_serialize")]
    #[serde(deserialize_with = "lag_freq_deserialize")]
    output: Lag,
    #[serde(skip_serializing)]
    #[serde(default = "default_false")]
    processed: bool,
}

fn connection_default_lag() -> lag::Lag {
    let mut clag = lag::lag(0.0);
    clag.set_factor(0.0);
    clag
}

fn default_false() -> bool {
    false
}

impl Connection {
    pub fn new(idx: u32, w: f64) -> Connection {
        let mut out = lag::lag(0.0);
        out.set_factor(0.0);
        Connection {
            input_idx: idx,
            weight: lag::lag(w),
            delay: TapDelay::new(7.0),
            input: 0.0,
            output: out,
            processed: false,
        }
    }

    pub fn current(&self) -> f64 {
        self.output.current
    }

    pub fn set_delay(&mut self, del: f64) {
        self.delay.set_delay(del)
    }

    pub fn set_lag(&mut self, dur: f64) {
        self.output.set_duration(dur);
    }

    pub fn set_frequency(&mut self, freq: f64) {
        self.output.set_frequency(freq);
    }

    pub fn set_weight(&mut self, weight: f64) {
//        println!("set target to {}", weight);
        self.weight.set_target(weight);
    }

    pub fn tick(&mut self) -> f64 {
        if !self.processed {
            self.processed = true;
            self.output
                .set_target(self.delay.process(self.input) * self.weight.tick());
            let out = self.output.tick();
            out
        } else {
            self.current()
        }
    }

    pub fn reset_after_process(&mut self) {
        self.input = 0.0;
        self.processed = false;
    }
}

pub type UGenGraphStructure = StableGraph<UGen, Connection, Directed, DefaultIx>;

#[derive(Debug)]
pub struct UGenGraph {
    pub graph: UGenGraphStructure, // TODO: goal, make no longer pub, all edge/node manipulations via impl
    pub edge_fac: lag::Lag,
    current_listening_nodes: Vec<NodeIndex>, // TODO check if this is kept up to date when output_amp of ugen is set
                                             // TODO move flow in here
}

#[derive(Serialize, Deserialize, Debug)]
pub struct OutputSpec {
    pub node: usize,
    pub output: usize,
    pub amp: f64,
}

fn round(x: f64, quant: f64) -> f64 {
    if quant == 0.0 {
        x
    } else {
        (x / quant + 0.5).floor() * quant
    }
}

fn trunc(x: f64, quant: f64) -> f64 {
    if quant == 0.0 {
        x
    } else {
        (x / quant).floor() * quant
    }
}

fn fold(input: f64, lo: f64, hi: f64) -> f64 {
    let x = input - lo;
    let mut input_res = input;

    if input_res >= hi {
        input_res = hi + hi - input_res;
        if input_res >= lo {
            return input_res;
        }
    } else if input_res < lo {
        input_res = lo + lo - input_res;
        if input_res < hi {
            return input_res;
        }
    } else {
        return input_res;
    }
    if hi == lo {
        return lo;
    }
    // ok do the divide
    let range = hi - lo;
    let range2 = range + range;
    let mut c = x - range2 * (x / range2).floor();
    if c >= range {
        c = range2 - c;
    }
    return c + lo;
}

fn fold2(a: f64, b: f64) -> f64 {
    fold(a, -b, b)
}

fn steto_indices(index: f64, length: usize) -> (usize, usize, f64) {
    let index_scaled = index * length as f64;
    let lower = round(index_scaled, 2.) as usize;
    let upper = trunc(index_scaled, 2.) as usize + 1;
    let bin_index = fold2(index_scaled * 2. - 1., 1.);
    (lower, upper, bin_index)
}

fn calc_steto_amp(steto: f64, i: usize, n_output_ugens: usize) -> f64 {
    let (lower, upper, bin_index) = steto_indices(steto, n_output_ugens);
    let index_scaled = (bin_index + 1.0) / 2.0;
    let result = if i == lower {
        1. - index_scaled
    } else if i == upper {
        index_scaled
    } else {
        0.
    };
    result
}

impl UGenGraph {
    pub fn new() -> UGenGraph {
        let mut ef = lag::lag(1.0);
        ef.set_duration(2.0);
        UGenGraph {
            graph: StableGraph::with_capacity(100, 100),
            edge_fac: ef,
            current_listening_nodes: vec![],
        }
    }

    pub fn set_edges_weight(&mut self, weight: f64, step: usize, skip: usize) {
        self.graph
            .edge_weights_mut()
            .skip(skip)
            .step_by(step)
            .for_each(|e| e.set_weight(weight));
    }

    pub fn set_edges_delay(&mut self, delay: f64, step: usize, skip: usize) {
        self.graph
            .edge_weights_mut()
            .skip(skip)
            .step_by(step)
            .for_each(|e| e.set_delay(delay));
    }

    pub fn set_edges_lp_freq(&mut self, freq: f64, step: usize, skip: usize) {
        self.graph
            .edge_weights_mut()
            .skip(skip)
            .step_by(step)
            .for_each(|e| e.set_frequency(freq));
    }

    pub fn from_json_string(json: String, flow: &mut Vec<NodeIndex>) -> Result<UGenGraph> {
        let struc: UGenGraphStructure = serde_json::from_str(&json)?;
        let mut g = UGenGraph::new();
        g.graph = struc;
        g.init_after_deserialization();
        g.update_connections_and_flow(flow, false);
        Ok(g)
    }

    pub fn offset_sound_ins(&mut self, offset: usize) {
        for ugen in self.graph.node_weights_mut() {
            match &ugen.process {
                Process::SoundIn {
                    input,
                    index,
                    factor,
                } => {
                    ugen.process = Process::SoundIn {
                        input: *input,
                        index: index + offset,
                        factor: *factor,
                    }
                }
                _ => (),
            }
        }
    }

    pub fn reset_outputs(&mut self) {
        for ugen in self.graph.node_weights_mut() {
            ugen.output_sends = vec![];
        }
    }

    pub fn set_steto_output_channel(&mut self, output_idx: usize, steto: f64) {
        let n_output_ugens = self
            .graph
            .node_weights_mut()
            .filter(|u| u.process.is_output())
            .count();
        if n_output_ugens < 2 {
            self.graph
                .node_weights_mut()
                .filter(|u| u.process.is_output())
                .for_each(|u| u.set_output(output_idx, 1.0));
        } else {
            self.graph
                .node_weights_mut()
                .filter(|u| u.process.is_output())
                .enumerate()
                .for_each(|(i, u)| {
                    u.set_output(output_idx, calc_steto_amp(steto, i, n_output_ugens))
                });
        }
    }

    pub fn set_edge_fac(&mut self, f: f64) {
        self.edge_fac.set_target(f);
    }

    pub fn connect(&mut self, from: NodeIndex, to: NodeIndex, weight: Connection) -> EdgeIndex {
        self.graph.update_edge(from, to, weight)
    }

    pub fn add(&mut self, ugen: UGen) -> NodeIndex {
        self.graph.add_node(ugen)
    }

    pub fn init_after_deserialization(&mut self) {
        self.edge_fac.set_duration(2.0);
        // self.graph
        //     .node_weights_mut()
        //     .for_each(|u| u.process_type = Some(u.process.spec().process_type));
    }

    pub fn set_parameter(&mut self, node_idx: NodeIndex, idx: u32, input_value: f64) {
        if let Some(node) = self.graph.node_weight_mut(node_idx) {
            node.process.set_input(idx, input_value, false)
        }
    }
    pub fn set_output(&mut self, spec: OutputSpec) {
        match self.graph.node_weight_mut(NodeIndex::new(spec.node)) {
            None => (),
            Some(node) => node.set_output(spec.output, spec.amp),
        }
        //self.graph[NodeIndex::new(spec.node)].set_output(spec.output, spec.amp)
    }

    pub fn clear_output_sends(&mut self) {
        for ugen in self.graph.node_weights_mut() {
            ugen.output_sends = vec![];
        }
    }

    // returns true if it had to update
    pub fn update_listening_nodes(&mut self) -> bool {
        let mut nodes_to_listen_to = vec![];
        for (idx, ugen) in self.graph.node_references() {
            if ugen
                .output_sends
                .iter()
                .any(|(_, w)| w.target > 0.0 || w.current > 0.0)
            {
                nodes_to_listen_to.push(idx);
            }
        }
        if (self.current_listening_nodes.len() == nodes_to_listen_to.len())
            && nodes_to_listen_to
                .iter()
                .all(|n| self.current_listening_nodes.contains(n))
        {
            false
        } else {
            self.current_listening_nodes = nodes_to_listen_to;
            true
        }
    }

    pub fn rnd_connections(&mut self, nodes: &[NodeIndex]) {
        let mut rng = thread_rng();
        let mut shuffled = nodes.to_vec();
        shuffled.shuffle(&mut rng);
        shuffled.iter().for_each(|&idx| {
            self.rnd_connect_if_necessary(idx);
        })
    }

    pub fn get_spec(&self, node: NodeIndex) -> Option<&ProcessSpec> {
        self.graph.node_weight(node).map(|u| u.process.spec())
    }

    fn has_two_inputs(&self, node: NodeIndex) -> bool {
        self.get_spec(node)
            .map(|s| {
                s.process_type == ProcessType::TwoInputs
                    || s.process_type == ProcessType::SidechainEnv
            })
            .unwrap_or(false)
    }

    fn connect_if_possible(&mut self, from: NodeIndex, to: NodeIndex) {
        match self.graph.find_edge(from, to) {
            None => {
                let mut rng = thread_rng();
                match self.graph[to].process.spec().process_type {
                    ProcessType::NoInputGenerator => (),
                    _ => {
                        let mut idx: u32 = 0;
                        let lacking = self.lacking_input_edges(to);
                        if lacking.is_empty() {
                            if self.has_two_inputs(to) {
                                // TODO: eventually get exact number of inputs
                                idx = rng.gen_range(0, 2);
                            }
                        } else {
                            idx = *(lacking.choose(&mut rng).unwrap());
                        }
                        self.connect(from, to, Connection::new(idx, 0.0));
                    }
                }
            }
            _ => (),
        }
    }

    // make matrix connection
    pub fn connect_all(&mut self) {
        let indices: Vec<NodeIndex> = self.graph.node_indices().collect();
        for from in &indices {
            for to in &indices {
                self.connect_if_possible(*from, *to);
            }
        }
    }

    pub fn rnd_connect_if_necessary(&mut self, node: NodeIndex) {
        if !self.does_idx_have_sufficient_inputs(node) {
            self.rnd_connect_input(node);
        };
        if !self.does_idx_have_sufficient_outputs(node) {
            self.rnd_connect_output(node);
        }
    }

    fn rnd_connect_output(&mut self, new_node: NodeIndex) {
        let mut rng = thread_rng();
        let is_noinput =
            self.graph[new_node].process.spec().process_type == ProcessType::NoInputGenerator;
        // output
        if let Some(target) = self.choose_with_input(Some(new_node), is_noinput) {
            //            let w = rng.gen_range(0.7, 1.0);
            let mut idx: u32 = 0;
            if self.has_two_inputs(target) {
                if is_noinput {
                    idx = 1;
                } else {
                    idx = rng.gen_range(0, 2);
                }
            };
            self.connect(new_node, target, Connection::new(idx, 1.0));
        }
    }

    fn rnd_connect_input(&mut self, new_node: NodeIndex) {
        let mut rng = thread_rng();
        let mut shuffled: Vec<NodeIndex> = self.graph.node_indices().collect();
        shuffled = shuffled
            .into_iter()
            .filter(|idx| *idx != new_node)
            .collect();
        shuffled.shuffle(&mut rng);

        self.lacking_input_edges(new_node).iter().for_each(|i| {
            if shuffled.len() > (*i as usize) {
                self.connect(shuffled[(*i as usize)], new_node, Connection::new(*i, 1.0));
            };
        })
    }

    pub fn rnd_circle(&mut self, nodes: &[NodeIndex], n_connections: u32) -> Vec<EdgeIndex> {
        let mut rng = thread_rng();
        let mut shuffled = nodes.to_vec();
        shuffled.shuffle(&mut rng);
        let mut edges = Vec::new();
        for _i in 0..n_connections {
            let filtered_shuffled: Vec<&NodeIndex> = shuffled
                .iter()
                .filter(|&idx| self.does_idx_have_input(*idx))
                .collect();
            let length = filtered_shuffled.len();
            for (i, &idx) in filtered_shuffled.iter().enumerate() {
                //                let w1 = rng.gen_range(0.7, 1.0);
                edges.push(self.connect(
                    *idx,
                    *filtered_shuffled[(i + 1) % length],
                    Connection::new(0, 1.0),
                ));
            }

            //            let w2 = rng.gen_range(0.7, 1.0);
            let generators: Vec<&NodeIndex> = shuffled
                .iter()
                .filter(|&idx| !self.does_idx_have_input(*idx))
                .collect();
            for &idx in generators.iter() {
                edges.push(self.connect(
                    *idx,
                    **filtered_shuffled.choose(&mut rng).unwrap(),
                    Connection::new(0, 1.0),
                ))
            }
        }
        edges
    }

    fn does_idx_have_input(&self, node: NodeIndex) -> bool {
        if let Some(u) = self.graph.node_weight(node) {
            !(u.process.spec().process_type == ProcessType::NoInputGenerator)
        } else {
            false
        }
    }

    fn lacking_input_edges(&self, node: NodeIndex) -> Vec<u32> {
        let mut lacking_indices: Vec<u32> = vec![];
        if let Some(u) = self.graph.node_weight(node) {
            let incoming_edges: Vec<u32> = self
                .graph
                .edges_directed(node, Incoming)
                .map(|e| e.weight().input_idx)
                .collect();
            match u.process.spec().process_type {
                ProcessType::NoInputGenerator => (),
                ProcessType::TwoInputs
                | ProcessType::MultipleInputs
                | ProcessType::SidechainEnv => {
                    if !incoming_edges.contains(&0) {
                        lacking_indices.push(0);
                    }
                    if !incoming_edges.contains(&1) {
                        lacking_indices.push(1);
                    }
                }
                ProcessType::TransparentProcessor | ProcessType::OpaqueProcessor => {
                    if !incoming_edges.contains(&0) {
                        lacking_indices.push(0);
                    }
                }
            }
            lacking_indices
        } else {
            lacking_indices
        }
    }

    fn does_idx_have_sufficient_inputs(&self, node: NodeIndex) -> bool {
        self.lacking_input_edges(node).is_empty()
    }

    fn does_idx_have_sufficient_outputs(&self, node: NodeIndex) -> bool {
        let n = self.graph.neighbors_directed(node, Outgoing).count();
        n > 0
    }

    // guarantees that choosen idx is a process with input
    fn choose_with_input(
        &self,
        exclude: Option<NodeIndex>,
        exclude_transparent: bool,
    ) -> Option<NodeIndex> {
        let mut rng = thread_rng();
        let with_input: Vec<NodeIndex> = self
            .graph
            .node_references()
            .filter_map(|(idx, u)| {
                if (exclude_transparent
                    && !(u.process.spec().process_type == ProcessType::TransparentProcessor))
                    || !exclude_transparent
                {
                    Some((idx, u))
                } else {
                    None
                }
            })
            .filter_map(|(idx, u)| {
                if !(u.process.spec().process_type == ProcessType::NoInputGenerator) {
                    match exclude {
                        Some(ex) => {
                            if ex != idx {
                                Some(idx)
                            } else {
                                None
                            }
                        }
                        None => Some(idx),
                    }
                } else {
                    None
                }
            })
            .collect();
        with_input.choose(&mut rng).map(|r| *r)
    }

    fn collect_components(&self) -> Vec<Vec<NodeIndex>> {
        let mut sets = Vec::new();

        for node in self.graph.node_indices() {
            let mut bfs = Bfs::new(&self.graph, node);

            let mut all_neighbors = Vec::new();
            while let Some(nx) = bfs.next(&self.graph) {
                all_neighbors.push(nx)
            }

            let intersects_with = sets.iter().position(|set: &Vec<NodeIndex>| {
                all_neighbors.iter().any(|neighbor| set.contains(neighbor))
            });

            match intersects_with {
                Some(idx) => {
                    sets[idx].append(&mut all_neighbors);
                }
                None => sets.push(all_neighbors),
            }

            for s in sets.iter_mut() {
                s.sort();
                s.dedup();
            }
        }

        sets
    }

    fn nodes_with_neighbors(&self) -> Vec<(NodeIndex, Vec<NodeIndex>)> {
        let mut result = Vec::new();
        for node in self.graph.node_indices() {
            let neighbors = self.graph.neighbors_undirected(node).collect();
            result.push((node, neighbors));
        }
        result.sort_by(
            |(_, a): &(NodeIndex, Vec<NodeIndex>), (_, b): &(NodeIndex, Vec<NodeIndex>)| {
                a.len().partial_cmp(&b.len()).unwrap()
            },
        );
        result
    }

    pub fn connect_least_connected(&mut self) {
        let nodes = self.nodes_with_neighbors();
        if let Some((first, _)) = nodes.first() {
            if let Some((future_neighbor, _)) = nodes
                .iter()
                .skip(1)
                .filter(|(_, neighbors)| !(neighbors.contains(first)))
                .next()
            {
                //let w = thread_rng().gen_range(0.7, 1.0);
                self.connect(*first, *future_neighbor, Connection::new(0, 1.0));
            }
        }
    }

    pub fn disconnect_most_connected(&mut self) {
        let nodes = self.nodes_with_neighbors();
        if let Some((last, _)) = nodes.last() {
            if let Some((neighbor, _)) = nodes
                .iter()
                .rev()
                .skip(1)
                .filter(|(_, neighbors)| neighbors.contains(last))
                .next()
            {
                if let Some((e, _)) = self.graph.find_edge_undirected(*last, *neighbor) {
                    self.graph.remove_edge(e);
                }
            }
        }
    }

    pub fn ensure_connectivity(&mut self) {
        // first do an rnd connect to ensure everyone has sufficient inputs/outputs
        let indices: Vec<NodeIndex> = self.graph.node_indices().collect();
        indices
            .iter()
            .for_each(|idx| self.rnd_connect_if_necessary(*idx));
        let components = self.collect_components();
        let mut rng = thread_rng();
        if let Some((first, rest)) = components.split_first() {
            for disconnected_component in rest.iter() {
                let first_nodes: Vec<&NodeIndex> = first
                    .iter()
                    .filter(|&u| {
                        if let Some(ugen) = self.graph.node_weight(*u) {
                            ugen.process.spec().process_type != ProcessType::NoInputGenerator
                        } else {
                            false
                        }
                    })
                    .collect();

                let rest_nodes: Vec<&NodeIndex> = disconnected_component
                    .iter()
                    .filter(|&u| {
                        if let Some(ugen) = self.graph.node_weight(*u) {
                            ugen.process.spec().process_type != ProcessType::NoInputGenerator
                        } else {
                            false
                        }
                    })
                    .collect();

                match (first_nodes.choose(&mut rng), rest_nodes.choose(&mut rng)) {
                    (Some(f), Some(r)) => {
                        self.connect(**f, **r, Connection::new(0, 1.0));
                        ()
                    }
                    _ => (),
                }

                match (first_nodes.choose(&mut rng), rest_nodes.choose(&mut rng)) {
                    (Some(f), Some(r)) => {
                        self.connect(**r, **f, Connection::new(0, 1.0));
                        ()
                    }
                    _ => (),
                }
            }
        }
    }

    pub fn establish_flow(&mut self) -> Vec<NodeIndex> {
        let _ = self.update_listening_nodes();
        let mut to_visit = self.current_listening_nodes.to_owned();
        let mut visited: Vec<NodeIndex> = to_visit.to_owned();

        while !to_visit.is_empty() {
            match to_visit.pop() {
                Some(current_node) => self
                    .graph
                    .neighbors_directed(current_node, Incoming)
                    .for_each(|neighbor| {
                        if !visited.contains(&neighbor) {
                            visited.push(neighbor);
                            match self.graph.node_weight(neighbor) {
                                Some(fb) => {
                                    if !fb.feedback_delay {
                                        to_visit.insert(0, neighbor);
                                    }
                                }
                                None => (),
                            }
                        }
                    }),
                None => break,
            }
        }
        visited.reverse();
        visited
    }

    pub fn update_connections_and_flow(&mut self, flow: &mut Vec<NodeIndex>, matrix_mode: bool) {
        if matrix_mode {
            self.connect_all();
        } else {
            self.ensure_connectivity();
        }
        *flow = self.establish_flow();
    }

    /// Calls a ugen and sends result to connected ugens
    pub fn call_and_output(&mut self, idx: NodeIndex, input: &[f64], edge_fac: f64) {
        match self.graph.node_weight_mut(idx) {
            Some(ugen) => {
                let output = ugen.process(input);
                let mut neighbors = self.graph.neighbors_directed(idx, Outgoing).detach();
                while let Some(neighbor_idx) = neighbors.next_node(&self.graph) {
                    let edge = self
                        .graph
                        .find_edge(idx, neighbor_idx)
                        .and_then(|e| self.graph.edge_weight_mut(e))
                        //                        .as_deref_mut()
                        .map(|e| {
                            //                            let this_connection = &*e;
                            e.input = e.input + output;
                            (e.input_idx, e.tick() * edge_fac)
                            //                            (*idx, w.current)
                        }); // deref to stop borrowing
                    match edge {
                        Some(connection) => {
                            self.graph[neighbor_idx].set_input_with_connection(connection)
                        }
                        None => (),
                    }
                }
            }
            None => (),
        }
    }

    pub fn number_of_inputs(&mut self) -> usize {
        self.graph
            .node_weights_mut()
            .filter_map(|u| match u.process {
                Process::SoundIn { index, .. } => Some(index),
                _ => None,
            })
            .max()
            .unwrap_or(0)
            + 1
    }

    pub fn number_of_outputs(&mut self) -> usize {
        let mut max_out: usize = 0;
        for ugen in self.graph.node_weights_mut() {
            for (out_i, amp_out) in ugen.output_sends.iter_mut() {
                if ((amp_out.target > 0.0) || (amp_out.current > 0.0)) && (*out_i > max_out) {
                    max_out = *out_i;
                }
            }
        }
        max_out + 1
    }

    pub fn process(
        &mut self,
        flow: &Vec<NodeIndex>,
        input_buffer: &[f64],
        output_buffer: &mut [f64],
    ) {
        flow.iter()
            .for_each(|&idx| self.call_and_output(idx, input_buffer, self.edge_fac.current));

        output_buffer.iter_mut().for_each(|x| *x = 0.0);

        for ugen in self.graph.node_weights_mut() {
            let current_value = ugen.value.unwrap_or(0.0);
            for (out_i, amp_out) in ugen.output_sends.iter_mut() {
                output_buffer[*out_i % output_buffer.len()] +=
                    current_value * ugen.output_amp.tick() * amp_out.tick();
            }
        }
        for &i in flow {
            match self.graph.node_weight_mut(i) {
                Some(n) => {
                    n.reset();
                }
                None => (),
            }
        }
        for e in self.graph.edge_weights_mut() {
            e.reset_after_process()
        }
        self.edge_fac.tick();
    }

    pub fn render(
        &mut self,
        n: usize,
        output_channels: usize,
        output_specs: Vec<OutputSpec>,
        inputs: &[Vec<f64>],
        input_channels: usize,
    ) -> Vec<Vec<f64>> {
        for spec in output_specs.into_iter() {
            self.set_output(spec);
        }
        let flow = self.establish_flow();
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let mut output_buffer = vec![0.0; output_channels];
            let default_input = vec![0.0; input_channels];
            // let default_input_ptr: &[f64] = default_input.as_slice();
            let this_input = inputs.get(i).unwrap_or(&default_input);
            // println!("in {:?}", this_input);
            self.process(&flow, this_input, &mut output_buffer);
            out.push(output_buffer)
        }
        out
    }
}

// UGENS
// pub fn add() -> UGen {
//     UGen::new(Process::Add { inputs: Vec::new() })
// }

// pub fn mul() -> UGen {
//     UGen::new(Process::Mul { inputs: Vec::new() })
// }

// pub fn square() -> UGen {
//     UGen::new(Process::Square { input: 0.0 })
// }

// pub fn sound_in(index: usize) -> UGen {
//     UGen::new(Process::SoundIn {
//         input: 0.0,
//         index,
//         factor: lag::lag(1.0),
//     })
// }

// pub fn sqrt() -> UGen {
//     UGen::new(Process::Sqrt { input: 0.0 })
// }

// fn lpf2000() -> Process {
//     filter(filters::FilterType::BLPF, 2000.0, 6.0)
// }

// pub fn rms() -> UGen {
//     UGen::new(rms_proc())
// }

// pub fn spike(threshold: f64, t_const: f64, r: f64, t_rest: usize) -> UGen {
//     UGen::new(Process::Spike {
//         input: 0.0,
//         v: 0.0,
//         last_v: 0.0,
//         threshold: lag::lag(threshold),
//         t_const: lag::lag(t_const),
//         r: lag::lag(r),
//         t_rest,
//         t_this_rest: t_rest,
//         t_rest_counter: 0,
//     })
// }

// pub fn mem(init: f64) -> UGen {
//     UGen::new(Process::Mem {
//         input: init,
//         last_value: 0.0,
//     })
// }

// pub fn constant(v: f64) -> UGen {
//     UGen::new(Process::Constant { value: v })
// }

// pub fn sin() -> UGen {
//     UGen::new(Process::Sin {
//         input: 0.0,
//         mul: lag::lag(1.0),
//     })
// }

// pub fn sin_osc(freq: f64, freq_mul: f64) -> UGen {
//     UGen::new(Process::SinOsc {
//         input: 0.0,
//         freq: lag::lag(freq),
//         freq_mul: lag::lag(freq_mul),
//         phase: 0.0,
//     })
// }

// fn filter(filter_type: filters::FilterType, freq: f64, q: f64) -> Process {
//     unsafe {
//         Process::Filter {
//             input: 0.0,
//             filter: filters::Biquad::new(filter_type, freq, q, SR),
//         }
//     }
// }

// pub fn lpf(freq: f64, q: f64) -> UGen {
//     UGen::new(filter(filters::FilterType::BLPF, freq, q))
// }

// pub fn resonator(freq: f64, decay: f64) -> UGen {
//     UGen::new(Process::Resonator {
//         input: 0.0,
//         freq_mod: 0.0,
//         freq_center: lag::lag(freq),
//         freq_factor: lag::lag(100.0),
//         resonator: filters::ComplexRes::new(freq, decay, get_sr()),
//     })
// }

// pub fn ring() -> UGen {
//     UGen::new(Process::Ring {
//         inputs: Vec::new(),
//         input_counter: 0,
//     })
// }

// pub fn hpf(freq: f64, q: f64) -> UGen {
//     UGen::new(filter(filters::FilterType::BHPF, freq, q))
// }

// pub fn bpf(freq: f64, q: f64) -> UGen {
//     UGen::new(filter(filters::FilterType::BBPF, freq, q))
// }

// pub fn noise(seed: u64) -> UGen {
//     UGen::new(Process::Noise {
//         rng: SeedableRng::seed_from_u64(seed),
//     })
// }

// pub fn ducking() -> UGen {
//     UGen::new(Process::Ducking {
//         input: 0.0,
//         input_level: vec![],
//         output_factor: ducking_lag(),
//     })
// }

// pub fn env_follow() -> UGen {
//     UGen::new(Process::EnvFollow {
//         input: 0.0,
//         input_level: vec![],
//     })
// }

// pub fn pll() -> UGen {
//     UGen::new(Process::PLL {
//         input: 0.0,
//         factor: lag::lag(0.25),
//         phase: 0.0,
//         error: pll_error_lag(),
//     })
// }

pub fn sinosc(freq: f64) -> UGen {
    UGen::new(Process::SinOsc {
        input: 0.0,
        freq: lag::lag(freq),
        freq_mul: lag::lag(1.0),
        phase: 0.0,
    })
}

pub fn soundinput(index: usize) -> UGen {
    UGen::new(Process::SoundIn {
        input: 0.0,
        index: index,
        factor: lag::lag(1.0),
    })
}

pub fn vdp() -> UGen {
    UGen::new(vanderpol(0.5, 0., 0.05))
}

pub fn perceptron() -> UGen {
    UGen::new(Process::Perceptron { input: 0.0, bias: lag::lag(0.0) })
}


// pub fn wrap(lo: f64, hi: f64) -> UGen {
//     UGen::new(Process::Wrap { input: 0.0, lo, hi })
// }

// pub fn softclip() -> UGen {
//     UGen::new(Process::Softclip { input: 0.0 })
// }

// pub fn compressor(threshold: f64, ratio: f64, makeup: f64) -> UGen {
//     UGen::new(Process::Compressor {
//         input: 0.0,
//         input_level: vec![],
//         threshold: lag::lag(threshold),
//         ratio: lag::lag(ratio),
//         makeup: lag::lag(makeup),
//     })
// }

// pub fn bitneg() -> UGen {
//     UGen::new(Process::BitNeg { input: 0.0 })
// }

// pub fn gauss() -> UGen {
//     UGen::new(Process::Gauss { input: 0.0 })
// }

// pub fn lpf1(freq: f64) -> UGen {
//     UGen::new(Process::LPF1 {
//         input: 0.0,
//         freq: lag::lag(freq),
//         p: lpf1_calc_p(freq),
//         last_out: 0.0,
//     })
// }

// pub fn delay(n: usize) -> UGen {
//     UGen::new(Process::Delay {
//         input: vec![0.0; n],
//         rec_idx: 0,
//     })
// }
// pub fn curvelin(in_min: f64, in_max: f64, out_min: f64, out_max: f64) -> UGen {
//     UGen::new(Process::CurveLin {
//         input: 0.0,
//         in_min,
//         in_max,
//         out_min,
//         out_max,
//         curve: -4.,
//     })
// }

// fn fb_delay() -> UGen {
//     UGen {
//         feedback_delay: true,
//         process: Process::Mem {
//             input: 0.0,
//             last_value: 0.0,
//         },
//         value: None,
//         last_value: 0.0,
//         clip: ClipType::None,
//         sum_inputs: true,
//         output_sends: vec![(0, lag::lag(1.0))],
//     }
// }

// pub fn band_pass2(g: &mut UGenGraph, f1: f64, f2: f64, q: f64) -> (NodeIndex, NodeIndex) {
//     let low1 = g.graph.add_node(lpf(f2, q));
//     let low2 = g.graph.add_node(lpf(f2, q));
//     let high1 = g.graph.add_node(hpf(f1, q));
//     let high2 = g.graph.add_node(hpf(f1, q));
//     g.connect(low1, low2, (0, lag::lag(1.0)));
//     g.connect(low2, high1, (0, lag::lag(1.0)));
//     g.connect(high1, high2, (0, lag::lag(1.0)));
//     (low1, high2)
// }

// fn geo_series(n: u32, start: f64, fac: f64) -> Vec<f64> {
//     let mut output = vec![start];
//     let mut last = start;
//     for _i in 0..n {
//         last = last * fac;
//         output.push(last);
//     }
//     output
// }

// fn geom_from_to(n: u32, from: f64, to: f64) -> Vec<f64> {
//     let fac = (to / from).powf(1. / ((n - 1) as f64));
//     geo_series(n, from, fac)
// }

// pub fn filter_bank(
//     g: &mut UGenGraph,
//     n: u32,
//     f1: f64,
//     f2: f64,
//     q: f64,
// ) -> (NodeIndex, Vec<NodeIndex>) {
//     let filters: Vec<(NodeIndex, NodeIndex)> = geom_from_to(n, f1, f2)
//         .windows(2)
//         .map(|pair| band_pass2(g, pair[0], pair[1], q))
//         .collect();
//     let input_sum = g.graph.add_node(add());
//     let outputs = filters
//         .iter()
//         .map(|(input, output)| {
//             g.connect(input_sum, *input, (0, lag::lag(1.0)));
//             *output
//         })
//         .collect();
//     (input_sum, outputs)
// }
