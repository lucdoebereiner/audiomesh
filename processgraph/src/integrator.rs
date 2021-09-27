pub fn runge_kutta<A>(f: &dyn Fn(&[f64], &A) -> Vec<f64>, state: &[f64], additional_vars: &A, h: f64) -> Vec<f64> {
    let k1 = f(state, additional_vars);
    let k2_state : Vec<f64> = state
        .iter()
        .enumerate()
        .map(|(index, var)| {
            var + ((h / 2.0) * k1[index])
        })
        .collect();
    let k2 = f(&k2_state, additional_vars);
    let k3_state : Vec<f64> = state
        .iter()
        .enumerate()
        .map(|(index, var)| {
            var + ((h / 2.0) * k2[index])
        })
        .collect();
    let k3 = f(&k3_state, additional_vars);
    let k4_state : Vec<f64> = state
        .iter()
        .enumerate()
        .map(|(index, var)| {
            var + (h * k3[index])
        })
        .collect();
    let k4 = f(&k4_state, additional_vars);
    let result : Vec<f64> = state
        .iter()
        .enumerate()
        .map(|(index, var)| {
            var + ((k1[index] + (2.0 * k2[index]) + (2.0 * k3[index]) + k4[index]) * (h/6.0))
        })
        .collect();
    result
    //k1

    
}
