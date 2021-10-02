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


pub fn runge_kutta_6<A>(f: &dyn Fn(&[f64], &A) -> Vec<f64>, state: &[f64], additional_vars: &A, h: f64) -> Vec<f64> {
    let k1 = f(state, additional_vars);
    let k2_state : Vec<f64> = state
        .iter()
        .enumerate()
        .map(|(index, var)| {
            var + ((h / 3.0) * k1[index])
        })
        .collect();
    let k2 = f(&k2_state, additional_vars);
    let k3_state : Vec<f64> = state
        .iter()
        .enumerate()
        .map(|(index, var)| {
            var + ((h * (2.0/3.0)) * k2[index])
        })
        .collect();
    let k3 = f(&k3_state, additional_vars);
    let k4_state : Vec<f64> = state
        .iter()
        .enumerate()
        .map(|(index, var)| {
            var + (h * (1.0/12.0) * k1[index]) + (h * (1.0/3.0) * k2[index]) - (h * (1.0/12.0) * k3[index])
        })
        .collect();
    let k4 = f(&k4_state, additional_vars);
    let k5_state : Vec<f64> = state
        .iter()
        .enumerate()
        .map(|(index, var)| {
            var - (h * (1.0/16.0) * k1[index]) + (h * (9.0/8.0) * k2[index]) - (h * (3.0/16.0) * k3[index]) - (h * (3.0/8.0) * k4[index])
        })
        .collect();
    let k5 = f(&k5_state, additional_vars);
    let k6_state : Vec<f64> = state
        .iter()
        .enumerate()
        .map(|(index, var)| {
            var + (h * (9.0/8.0) * k2[index]) - (h * (3.0/8.0) * k3[index]) - (h * (3.0/4.0) * k4[index]) + (h * (1.0/2.0) * k5[index])
        })
        .collect();
    let k6 = f(&k6_state, additional_vars);
    let k7_state : Vec<f64> = state
        .iter()
        .enumerate()
        .map(|(index, var)| {
            var + (h * (9.0/44.0) * k1[index]) - (h * (9.0/11.0) * k2[index]) + (h * (63.0/44.0) * k3[index]) + (h * (18.0/11.0) * k4[index]) - (h * (16.0/11.0) * k6[index])
        })
        .collect();
    let k7 = f(&k7_state, additional_vars);


    let result : Vec<f64> = state
        .iter()
        .enumerate()
        .map(|(index, var)| {
            var + (((11.0/120.0) * k1[index]) + ((27.0/40.0) * k3[index]) + ((27.0/40.0) * k4[index]) - ((4.0/15.0) * k5[index]) - ((4.0/15.0) * k6[index]) + ((11.0/120.0) * k7[index])) * h
        })
        .collect();
    result


    
}
