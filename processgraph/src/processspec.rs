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
    pub name: String,
    pub input_type: InputType,
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
    pub input_scaling: InputScaling,
}

// fn procspec(name: &'static str, process_type: ProcessType, inputs: Vec<InputSpec>) -> ProcessSpec {
//     ProcessSpec {
//         name: name.to_string(),
//         process_type,
//         inputs,
//     }
// }


#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum InputType {
    Any,
    Audio,
    Frequency(f32),
    Q,
    Phase,
    Index(u32),
    Factor(InputRange),
    Threshold(InputRange),
    Parameter,
    Amplitude(f32),
    Seconds,
    Offset(InputRange),
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
    //         Process::SoundIn { .. } => procspec(
    //             "soundin",
    //             ProcessType::NoInputGenerator,
    //             vec![InputType::Index, InputType::Factor],
    //         ),
    //         Process::VanDerPol { .. } => procspec(
    //             "vanderpool",
    //             ProcessType::OpaqueProcessor,
    //             vec![
    //                 InputType::Audio,
    //                 InputType::Factor,
    //                 InputType::Factor,
    //                 InputType::Amplitude,
    //             ],
    //         ),
    //         Process::NoseHoover { .. } => procspec(
    //             "nosehover",
    //             ProcessType::OpaqueProcessor,
    //             vec![
    //                 InputType::Audio,
    //                 InputType::Factor,
    //                 InputType::Frequency,
    //                 InputType::Factor,
    //             ],
    //         ),
    //         Process::FitzHughNagumo { .. } => procspec(
    //             "fitzhughnagumo",
    //             ProcessType::OpaqueProcessor,
    //             vec![
    //                 InputType::Audio,
    //                 InputType::Factor,
    //                 InputType::Factor,
    //                 InputType::Factor,
    //                 InputType::Frequency,
    //                 InputType::Factor,
    //             ],
    //         ),

    //         Process::Chua { .. } => procspec(
    //             "chua",
    //             ProcessType::OpaqueProcessor,
    //             vec![InputType::Audio, InputType::Factor, InputType::Factor],
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

    //         Process::Fold { .. } => procspec(
    //             "fold",
    //             ProcessType::OpaqueProcessor,
    //             vec![
    //                 InputType::Audio,
    //                 InputType::Threshold,
    //                 InputType::Amplitude,
    //                 InputType::Offset,
    //             ],
    //         ),

    //         Process::Sin { .. } => {
    //             procspec("sin", ProcessType::OpaqueProcessor, vec![InputType::Phase])
    //         }
    //         Process::Env { .. } => procspec(
    //             "env",
    //             ProcessType::TransparentProcessor,
    //             vec![InputType::Audio],
    //         ),
    //         Process::SinOsc { .. } => procspec(
    //             "sinosc",
    //             ProcessType::OpaqueProcessor,
    //             vec![InputType::Frequency, InputType::Factor],
    //         ),
    //         Process::PLL { .. } => procspec(
    //             "pll",
    //             ProcessType::OpaqueProcessor,
    //             vec![InputType::Any, InputType::Factor],
    //         ),

    //         Process::Mul { .. } => {
    //             procspec("mul", ProcessType::MultipleInputs, vec![InputType::Any])
    //         }
    //         Process::Ring { .. } => {
    //             procspec("ring", ProcessType::MultipleInputs, vec![InputType::Audio])
    //         }
    //         Process::Add { .. } => {
    //             procspec("add", ProcessType::MultipleInputs, vec![InputType::Any])
    //         }
    //         Process::Mem { .. } => procspec(
    //             "mem",
    //             ProcessType::TransparentProcessor,
    //             vec![InputType::Any],
    //         ),
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
    //         Process::Delay { .. } => procspec(
    //             "delay",
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
    //         Process::Kaneko { .. } => procspec(
    //             "kaneko",
    //             ProcessType::TwoInputs,
    //             vec![
    //                 InputType::Any,
    //                 InputType::Any,
    //                 InputType::Factor,
    //                 InputType::Factor,
    //             ],
    //         ),
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

pub type SpecTable = HashMap<String, ProcessSpec>;

lazy_static! {
    pub static ref SPECS: SpecTable = HashMap::from([
        ("SoundIn".to_string(), ProcessSpec {
            name: "SoundIn".to_string(),
            process_type: ProcessType::NoInputGenerator,
            inputs: vec![
                InputSpec {
                    name: "Index".to_string(),
                    input_type: InputType::Index(0),
                },
                InputSpec {
                    name: "Amp".to_string(),
                    input_type: InputType::Amplitude(0.0),
                }
            ]   
        })
    ]);
}

pub fn specs_vec() -> Vec<ProcessSpec> {
    SPECS.values().cloned().collect()
}
