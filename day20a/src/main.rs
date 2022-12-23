fn unmix(vals: &[i32]) -> Vec<(usize, i32)> {
    let mut unmixed = vals.iter().copied().enumerate().collect::<Vec<_>>();

    for val in vals.iter().copied().enumerate() {
        // Linear scan will suffice
        let idx = unmixed
            .iter()
            .enumerate()
            .find(|(_, &v)| v == val)
            .unwrap()
            .0;

        let val = unmixed.remove(idx);
        let new_idx = (idx as i64 + val.1 as i64).rem_euclid(unmixed.len() as i64) as usize;
        unmixed.insert(new_idx, val);
    }

    unmixed
}

fn main() {
    let vals = std::io::stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| s.parse().unwrap())
        .collect::<Vec<i32>>();

    let unmixed = unmix(&vals);

    let i0 = unmixed
        .iter()
        .enumerate()
        .find(|(_, &v)| v.1 == 0)
        .unwrap()
        .0;
    let v1 = unmixed[(i0 + 1000) % unmixed.len()];
    let v2 = unmixed[(i0 + 2000) % unmixed.len()];
    let v3 = unmixed[(i0 + 3000) % unmixed.len()];
    println!("{} + {} + {} = {}", v1.1, v2.1, v3.1, v1.1 + v2.1 + v3.1);
}
