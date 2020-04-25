use jack;
use petgraph::dot::{Config, Dot};
//use petgraph::stable_graph::next_node;
use processgraph::numerical;
use processgraph::*;
use std::{f32, thread, time};

fn main() {
    let mut g = new_graph();

    let mem1 = g.add_node(mem(0.5).clip(ClipType::Wrap));
    //    let mem1 = g.add_node(mem(0.5));
    //    let mem2 = g.add_node(mem(-0.1).softclip());
    let del1 = g.add_node(delay(29101).clip(ClipType::Wrap));
    let del2 = g.add_node(delay(300).clip(ClipType::SoftClip));
    let del3 = g.add_node(delay(82122).clip(ClipType::Wrap));
    let sin = g.add_node(sin());
    let sum = g.add_node(add().clip(ClipType::Wrap));
    let sum2 = g.add_node(add().clip(ClipType::SoftClip));
    let mul = g.add_node(mul().clip(ClipType::Wrap));
    let wrap1 = g.add_node(wrap(-0.5, 0.5));
    //    let wrap2 = g.add_node(wrap(-0.5, 0.5));
    let filter = g.add_node(lpf(2.0, 6.0));
    let filter2 = g.add_node(hpf(1000.0, 3.0));
    let gauss = g.add_node(gauss());
    let curve = g.add_node(curvelin(-1.0, 1.0, 1.0, -1.0));
    //  let osc = g.add_node(sin_osc(300.0));
    // let linexp = g.add_node(linexp(-1., 1., 1., -1.));

    //    let nodes = vec![mem1, sin, del1, filter, sum, mul, sum2, del2];
    let nodes = vec![
        mem1, del1, del3, filter2, filter, sum, curve, gauss, sum2, del2,
    ];
    rnd_connections(&mut g, &nodes, 1);
    // g.add_edge(mem1, mul, 0);
    // g.add_edge(sum, mem1, 0);
    // g.add_edge(mul, wrap1, 0);
    // g.add_edge(wrap1, sum, 0);
    let output_indices = vec![sum, sum2];

    //    let noise_idx = g.add_node(noise(92));
    //  let (fbank_input, output_indices) = filter_bank(&mut g, 100, 20., 10000., 3.);
    //g.add_edge(noise_idx, fbank_input, 0);
    //let out_idx = g.add_node(lpf(1300., 3.));

    //    let mut g: UGenGraph = StableGraph::with_capacity(1000, 1000);

    //  let noise_idx = g.add_node(UGen::new(noise(92)));
    //    let out_idx = g.add_node(UGen::new(filter(filters::FilterType::BLPF, 1300., 3.)));

    // let sinosc1_idx = g.add_node(UGen::new(sinosc(100.)));
    // let sinosc2_idx = g.add_node(UGen::new(sinosc(100.)));
    // let mul1_idx = g.add_node(UGen::new(map_process(|x| (x * 100.))));
    // let add1_idx = g.add_node(UGen::new(map_process(|x| (x + 10.))));
    // let mul2_idx = g.add_node(UGen::new(map_process(|x| (x * 120.))));
    //let add2_idx = g.add_node(UGen::new(map_process(|x| (x + 20.))));
    // let sum_idx = g.add_node(UGen::new(add()));
    // let out_idx = g.add_node(UGen::new(map_process(|x| (x * 0.5))));

    // g.add_edge(sinosc1_idx, mul1_idx, 0);
    // g.add_edge(mul1_idx, add1_idx, 0);
    // g.add_edge(add1_idx, sinosc2_idx, 0);

    // g.add_edge(sinosc2_idx, mul2_idx, 0);
    // g.add_edge(mul2_idx, add2_idx, 0);
    // g.add_edge(add2_idx, sinosc1_idx, 0);

    // g.add_edge(sinosc1_idx, sum_idx, 0);
    // g.add_edge(sinosc2_idx, sum_idx, 0);
    // g.add_edge(sum_idx, out_idx, 0);

    // let const_idx = g.add_node(UGen::new(constant(1.0)));
    // let map1_idx = g.add_node(UGen::new(map_process(|x| (x + 3.))));
    // let map2_idx = g.add_node(UGen::new(map_process(|x| (x * 2.))));
    // let map3_idx = g.add_node(UGen::new(map_process(|x| (x - 1.))));
    // let out_idx = g.add_node(UGen::new(add()));
    // g.add_edge(const_idx, map1_idx, 0);
    // g.add_edge(const_idx, map2_idx, 0);
    // g.add_edge(const_idx, map3_idx, 0);
    // g.add_edge(map1_idx, out_idx, 0);
    // g.add_edge(map2_idx, out_idx, 0);
    // g.add_edge(map3_idx, out_idx, 0);

    //    insert_fb_nodes(&mut g, Vec::new(), vec![out_idx]);

    // let output_indices = vec![out_idx, noise_idx];
    let flow = establish_flow(&g, &output_indices);
    let n_outs = output_indices.len();
    let mut frame_buffer = vec![0.0; n_outs];

    println!("flow: {:?}", flow);
    //    println!("{:?}", Dot::with_config(&g, &[Config::EdgeNoLabel]));
    // for _i in 0..(64 * 40) {
    //     process_graph(&mut g, &flow, &output_indices, &mut frame_buffer);
    //     println!("result: {:?}", frame_buffer);
    //    }

    let (client, _status) = jack::Client::new("gengraph", jack::ClientOptions::empty()).unwrap();

    let mut out_ports = Vec::new();

    for i in 1..(n_outs + 1) {
        let name = String::from(format!("out_{}", i));
        out_ports.push(
            client
                .register_port(&name, jack::AudioOut::default())
                .unwrap(),
        );
    }

    let process = jack::ClosureProcessHandler::new(
        move |_: &jack::Client, ps: &jack::ProcessScope| -> jack::Control {
            // Get output buffer
            let mut outs: Vec<&mut [f32]> =
                out_ports.iter_mut().map(|p| p.as_mut_slice(ps)).collect();

            // Write output
            for i in 0..ps.n_frames() {
                process_graph(&mut g, &flow, &output_indices, &mut frame_buffer);
                if i == 0 {
                    println!("{}", frame_buffer[0]);
                }

                for (k, o) in outs.iter_mut().enumerate() {
                    o[i as usize] = frame_buffer[k] as f32;
                }
            }

            // Continue as normal
            jack::Control::Continue
        },
    );

    // 4. activate the client
    let _active_client = client.activate_async((), process).unwrap();

    loop {
        thread::sleep(time::Duration::from_millis(500));
    }
}
