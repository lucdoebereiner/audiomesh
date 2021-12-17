use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ProcessSpec {
    pub name: String,
    pub process_type: ProcessType,
    pub inputs: Vec<InputSpec>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct InputSpec {
    pub index: usize,
    pub name: String,
    pub input_type: InputType,
    pub controllable: bool,
}

fn input_spec(index: usize, name: &str, input_type: InputType) -> InputSpec {
    InputSpec {
        index,
        name: name.to_string(),
        input_type,
        controllable: true,
    }
}

fn input_spec_init(index: usize, name: &str, input_type: InputType) -> InputSpec {
    let mut spec = input_spec(index, name, input_type);
    spec.controllable = false;
    spec
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum InputScaling {
    Log,
    Exp,
    Cubic,
    Lin,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct InputRange {
    pub min: f32,
    pub max: f32,
    pub default: f32,
    pub scaling: InputScaling,
}

fn range(min: f32, max: f32, default: f32, scaling: InputScaling) -> InputRange {
    InputRange {
        min,
        max,
        default,
        scaling,
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum InputType {
    Any(f32),
    Audio,
    Frequency(InputRange),
    Q(f32),
    Phase(f32),
    Index(u32),
    Factor(InputRange),
    Threshold(InputRange),
    Amplitude(f32),
    Seconds(f32),
    Offset(InputRange),
    Samples(u32),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum ProcessType {
    NoInputGenerator,
    TransparentProcessor,
    OpaqueProcessor,
    SidechainEnv,
    TwoInputs,
    MultipleInputs,
}

// pub fn spec(&self) -> ProcessSpec {
//     match self {
//         Process::Spike { .. } => procspec(
//             "spike",
//             ProcessType::OpaqueProcessor,
//             vec![
//                 InputType::Any,
//                 InputType::Threshold,
//                 InputType::Parameter,
//                 InputType::Parameter,
//             ],
//         ),
//         Process::Duffing { .. } => procspec(
//             "duffing",
//             ProcessType::OpaqueProcessor,
//             vec![
//                 InputType::Audio,
//                 InputType::Factor,
//                 InputType::Factor,
//                 InputType::Amplitude,
//             ],
//         ),

//         Process::Env { .. } => procspec(
//             "env",
//             ProcessType::TransparentProcessor,
//             vec![InputType::Audio],
//         ),
//         Process::PLL { .. } => procspec(
//             "pll",
//             ProcessType::OpaqueProcessor,
//             vec![InputType::Any, InputType::Factor],
//         ),

//         Process::Add { .. } => {
//             procspec("add", ProcessType::MultipleInputs, vec![InputType::Any])
//         }
//         Process::Constant { .. } => procspec("constant", ProcessType::NoInputGenerator, vec![]),
//         //        Process::Map { .. } => procspec("map", vec![InputType::Any]),
//         Process::Filter { .. } => procspec(
//             "filter",
//             ProcessType::TransparentProcessor,
//             vec![InputType::Audio, InputType::Frequency, InputType::Q],
//         ),
//         Process::Resonator { .. } => procspec(
//             "resonator",
//             ProcessType::MultipleInputs,
//             vec![
//                 InputType::Audio,
//                 InputType::Any,
//                 InputType::Frequency,
//                 InputType::Factor,
//                 InputType::Seconds,
//             ],
//         ),

//         Process::Compressor { .. } => procspec(
//             "compressor",
//             ProcessType::TransparentProcessor,
//             vec![
//                 InputType::Audio,
//                 InputType::Threshold,
//                 InputType::Parameter,
//                 InputType::Amplitude,
//             ],
//         ),
//         Process::Ducking { .. } => procspec(
//             "ducking",
//             ProcessType::SidechainEnv,
//             vec![InputType::Any; 2],
//         ),
//         Process::EnvFollow { .. } => procspec(
//             "envfollow",
//             ProcessType::SidechainEnv,
//             vec![InputType::Any; 2],
//         ),

//         Process::GateDecision { .. } => procspec(
//             "gatedecision",
//             ProcessType::SidechainEnv,
//             vec![
//                 InputType::Any,
//                 InputType::Any,
//                 InputType::Seconds,
//                 InputType::Seconds,
//             ],
//         ),

//         Process::Wrap { .. } => procspec(
//             "wrap",
//             ProcessType::OpaqueProcessor,
//             vec![InputType::Any; 2],
//         ),
//         Process::Softclip { .. } => procspec(
//             "softclip",
//             ProcessType::TransparentProcessor,
//             vec![InputType::Any],
//         ),
//         Process::Perceptron { .. } => procspec(
//             "perceptron",
//             ProcessType::TransparentProcessor,
//             vec![InputType::Any, InputType::Factor],
//         ),
//         Process::Square { .. } => procspec(
//             "square",
//             ProcessType::TransparentProcessor,
//             vec![InputType::Any],
//         ),
//         Process::Sqrt { .. } => procspec(
//             "sqrt",
//             ProcessType::TransparentProcessor,
//             vec![InputType::Any],
//         ),
//         Process::VarDelay { .. } => procspec(
//             "vardelay",
//             ProcessType::TransparentProcessor,
//             vec![InputType::Any, InputType::Seconds],
//         ),
//         Process::BitNeg { .. } => {
//             procspec("bitneg", ProcessType::OpaqueProcessor, vec![InputType::Any])
//         }
//         Process::BitOr { .. } => {
//             procspec("bitor", ProcessType::TwoInputs, vec![InputType::Any; 2])
//         }
//         Process::BitXOr { .. } => {
//             procspec("bitxor", ProcessType::TwoInputs, vec![InputType::Any; 2])
//         }
//         Process::KanekoChain { .. } => procspec(
//             "kanekochain",
//             ProcessType::TwoInputs,
//             vec![
//                 InputType::Any,
//                 InputType::Any,
//                 InputType::Factor,
//                 InputType::Factor,
//             ],
//         ),

//         Process::BitAnd { .. } => {
//             procspec("bitand", ProcessType::TwoInputs, vec![InputType::Any; 2])
//         }
//         Process::GateIfGreater { .. } => procspec(
//             "gateifgreater",
//             ProcessType::TwoInputs,
//             vec![InputType::Any; 2],
//         ),
//         Process::CurveLin { .. } => procspec(
//             "curvelin",
//             ProcessType::TransparentProcessor,
//             vec![InputType::Any; 6],
//         ),
//         Process::Gauss { .. } => procspec(
//             "gauss",
//             ProcessType::TransparentProcessor,
//             vec![InputType::Audio],
//         ),
//         Process::RMS { .. } => {
//             procspec("rms", ProcessType::OpaqueProcessor, vec![InputType::Audio])
//         }
//         Process::LPF1 { .. } => procspec(
//             "lpf1",
//             ProcessType::TransparentProcessor,
//             vec![InputType::Audio, InputType::Frequency],
//         ),
//         Process::LinCon { .. } => procspec(
//             "lincon",
//             ProcessType::OpaqueProcessor,
//             vec![InputType::Any, InputType::Factor, InputType::Factor],
//         ),
//     }
// }

fn spec(name: &str, process_type: ProcessType, inputs: Vec<InputSpec>) -> (String, ProcessSpec) {
    (
        name.to_string(),
        ProcessSpec {
            name: name.to_string(),
            process_type,
            inputs,
        },
    )
}

pub type SpecTable = HashMap<String, ProcessSpec>;

lazy_static! {
    pub static ref SPECS: SpecTable = HashMap::from([
        spec(
            "Sin",
            ProcessType::OpaqueProcessor,
            vec![input_spec(
                1,
                "mul",
                InputType::Factor(range(0.0, 100.0, 1.0, InputScaling::Lin)),
            )]
        ),
        spec(
            "SinOsc",
            ProcessType::OpaqueProcessor,
            vec![
                input_spec(
                    1,
                    "freq",
                    InputType::Frequency(InputRange {
                        min: 0.1,
                        max: 10000.0,
                        scaling: InputScaling::Exp,
                        default: 100.0,
                    })
                ),
                input_spec(
                    2,
                    "freq_mul",
                    InputType::Factor(InputRange {
                        min: 0.1,
                        max: 5000.0,
                        scaling: InputScaling::Exp,
                        default: 1.0,
                    })
                )
            ]
        ),
        spec("Mul", ProcessType::MultipleInputs, vec![]),
        spec(
            "Fold",
            ProcessType::OpaqueProcessor,
            vec![
                input_spec(
                    1,
                    "threshold",
                    InputType::Threshold(InputRange {
                        min: 0.0,
                        max: 1.0,
                        default: 1.0,
                        scaling: InputScaling::Lin,
                    })
                ),
                input_spec(
                    2,
                    "mul",
                    InputType::Factor(InputRange {
                        min: 1.0,
                        max: 20.0,
                        default: 1.0,
                        scaling: InputScaling::Exp,
                    })
                ),
                input_spec(
                    3,
                    "add",
                    InputType::Offset(InputRange {
                        min: 0.0,
                        max: 1.0,
                        default: 0.0,
                        scaling: InputScaling::Lin,
                    })
                ),
            ],
        ),
        spec(
            "Kaneko",
            ProcessType::TwoInputs,
            vec![
                input_spec(
                    2,
                    "e",
                    InputType::Factor(InputRange {
                        min: 0.0,
                        max: 1.0,
                        default: 0.5,
                        scaling: InputScaling::Lin,
                    }),
                ),
                input_spec(
                    3,
                    "a",
                    InputType::Factor(InputRange {
                        min: 1.0,
                        max: 2.0,
                        default: 1.5,
                        scaling: InputScaling::Lin,
                    }),
                ),
            ],
        ),
        spec("Ring", ProcessType::MultipleInputs, vec![]),
        spec(
            "VanDerPol",
            ProcessType::OpaqueProcessor,
            vec![
                input_spec(
                    1,
                    "e",
                    InputType::Factor(InputRange {
                        min: 0.02,
                        max: 10.0,
                        default: 3.5,
                        scaling: InputScaling::Exp,
                    }),
                ),
                input_spec(
                    2,
                    "frac",
                    InputType::Frequency(InputRange {
                        min: 0.01,
                        max: 4000.0,
                        default: 100.0,
                        scaling: InputScaling::Exp,
                    }),
                ),
                input_spec(
                    3,
                    "a",
                    InputType::Factor(InputRange {
                        min: 0.0001,
                        max: 30.0,
                        default: 0.5,
                        scaling: InputScaling::Exp,
                    }),
                ),
            ],
        ),
        spec(
            "SoundIn",
            ProcessType::NoInputGenerator,
            vec![
                input_spec_init(0, "index", InputType::Index(0)),
                input_spec(1, "factor", InputType::Amplitude(0.0))
            ]
        ),
        spec(
            "Mem",
            ProcessType::TransparentProcessor,
            vec![input_spec(0, "last_value", InputType::Any(0.0))]
        ),
        spec(
            "NoseHoover",
            ProcessType::OpaqueProcessor,
            vec![
                input_spec(
                    1,
                    "a",
                    InputType::Factor(range(0.1, 20.0, 3.5, InputScaling::Exp))
                ),
                input_spec(
                    2,
                    "frac",
                    InputType::Frequency(range(0.1, 8000.0, 100.0, InputScaling::Exp))
                ),
                input_spec(
                    3,
                    "coupling",
                    InputType::Factor(range(0.0, 2.0, 0.0, InputScaling::Lin))
                )
            ]
        ),
        spec(
            "FitzHughNagumo",
            ProcessType::OpaqueProcessor,
            vec![
                input_spec(
                    1,
                    "a",
                    InputType::Factor(range(0.01, 2.0, 1.0, InputScaling::Exp))
                ),
                input_spec(
                    2,
                    "b",
                    InputType::Factor(range(0.01, 2.0, 1.0, InputScaling::Exp))
                ),
                input_spec(
                    3,
                    "c",
                    InputType::Factor(range(0.01, 2.0, 1.0, InputScaling::Exp))
                ),
                input_spec(
                    4,
                    "frac",
                    InputType::Frequency(range(0.1, 8000.0, 100.0, InputScaling::Exp))
                ),
                input_spec(
                    5,
                    "coupling",
                    InputType::Factor(range(-2.0, 2.0, 0.0, InputScaling::Cubic))
                )
            ],
        ),
        spec(
            "Chua",
            ProcessType::OpaqueProcessor,
            vec![
                input_spec(
                    1,
                    "a",
                    InputType::Factor(range(-20.0, 30.0, 15.74, InputScaling::Lin))
                ),
                input_spec(
                    2,
                    "b",
                    InputType::Factor(range(-12.0, 40.0, 28.49, InputScaling::Lin))
                ),
                input_spec(
                    3,
                    "c",
                    InputType::Factor(range(-2.0, 2.0, 0.029, InputScaling::Lin))
                ),
                input_spec(
                    4,
                    "frac",
                    InputType::Frequency(range(0.01, 3000.0, 100.0, InputScaling::Exp))
                ),
                input_spec(
                    5,
                    "coupling",
                    InputType::Factor(range(-30.0, 30.0, 0.0, InputScaling::Cubic))
                ),
                input_spec(
                    6,
                    "bp",
                    InputType::Threshold(range(0.1, 2.5, 1.0, InputScaling::Exp))
                ),
                input_spec(
                    7,
                    "m0",
                    InputType::Factor(range(-2.5, -0.1, -1.143, InputScaling::Lin))
                ),
                input_spec(
                    8,
                    "m1",
                    InputType::Factor(range(-2.5, -0.1, -0.714, InputScaling::Lin))
                ),
            ]
        ),
        spec(
            "Perceptron",
            ProcessType::OpaqueProcessor,
            vec![input_spec(
                1,
                "bias",
                InputType::Offset(range(-2.0, 2.0, 0.0, InputScaling::Cubic))
            )]
        ),
        spec(
            "Delay",
            ProcessType::TransparentProcessor,
            vec![input_spec_init(1, "input", InputType::Samples(1))]
        ),
        spec(
            "LinCon",
            ProcessType::OpaqueProcessor,
            vec![
                input_spec(
                    1,
                    "lincon_a",
                    InputType::Factor(range(0.001, 10.0, 1.0, InputScaling::Exp))
                ),
                input_spec(
                    2,
                    "lincon_b",
                    InputType::Factor(range(0.001, 20.0, 1.0, InputScaling::Exp))
                )
            ]
        )
    ]);
}

// TODO delay has init only input!!

pub fn specs_vec() -> Vec<ProcessSpec> {
    SPECS.values().cloned().collect()
}
