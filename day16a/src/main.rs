use std::collections::{HashMap, VecDeque};

use nom::{
    branch::alt, bytes::complete::tag, character::complete::*, combinator::*,
    multi::separated_list0, sequence::tuple, IResult,
};

fn parse_valve_id(input: &str) -> IResult<&str, String> {
    alpha1(input).map(|(i, s)| (i, s.to_owned()))
}

fn parse_u32(input: &str) -> IResult<&str, u32> {
    map_res(
        recognize(tuple((opt(tag("+")), digit1))),
        &str::parse::<u32>,
    )(input)
}

type ValveId = String;

#[derive(Debug, Clone)]
struct Valve {
    id: ValveId,
    flow_rate: u32,
    goes_to: Vec<ValveId>,
}

fn parse_valve(input: &str) -> IResult<&str, Valve> {
    let (input, _) = tag("Valve ")(input)?;
    let (input, id) = parse_valve_id(input)?;
    let (input, _) = tag(" has flow rate=")(input)?;
    let (input, flow_rate) = parse_u32(input)?;
    let (input, _) = tuple((
        tag("; "),
        alt((tag("tunnel leads to valve"), tag("tunnels lead to valves"))),
        tag(" "),
    ))(input)?;
    let (input, goes_to) = separated_list0(tag(", "), parse_valve_id)(input)?;

    Ok((
        input,
        Valve {
            id,
            flow_rate,
            goes_to,
        },
    ))
}

fn bfs(start: &ValveId, end: &ValveId, valves: &HashMap<ValveId, Valve>) -> u32 {
    let mut queue = VecDeque::new();
    queue.push_back(start.clone());
    let mut explored = HashMap::new();
    explored.insert(start.clone(), None);

    while let Some(it) = queue.pop_front() {
        let item = &valves[&it];

        if item.id == *end {
            let mut path = Vec::new();
            let mut curr = end;
            while let Some(node) = &explored[curr] {
                path.push(node);
                curr = node;
            }

            path.reverse();

            return path.len() as u32;
        }

        for adj in item.goes_to.iter() {
            if explored.contains_key(adj) {
                continue;
            }

            queue.push_back(adj.clone());
            explored.insert(adj.clone(), Some(item.id.clone()));
        }
    }

    panic!("No path found!");
}

fn find_paths(
    time: u32,
    pressurre: u32,
    curr: &ValveId,
    predecessors: &[String],
    valid_paths: &mut Vec<(Vec<ValveId>, u32)>,
    distances: &HashMap<(ValveId, ValveId), u32>,
    valves: &HashMap<ValveId, Valve>,
) {
    const T_MAX: u32 = 30;

    if time > T_MAX - 1 {
        return;
    }

    let mut pred = Vec::from(predecessors);
    pred.push(curr.clone());

    let v_curr = &valves[curr];

    let steam = pressurre + (T_MAX - time) * v_curr.flow_rate;

    valid_paths.push((pred.clone(), steam));

    for ((_, id), dist) in distances.iter().filter(|(a, _)| a.0 == *curr) {
        if pred.contains(id) {
            continue;
        }

        let t = time + dist + if v_curr.flow_rate > 0 { 1 } else { 0 };

        find_paths(t, steam, id, &pred, valid_paths, distances, valves)
    }
}

fn find_best_path(
    start: &ValveId,
    distances: &HashMap<(ValveId, ValveId), u32>,
    valves: &HashMap<ValveId, Valve>,
) {
    let mut valid_paths = Vec::new();

    find_paths(1, 0, start, &[], &mut valid_paths, distances, valves);

    valid_paths.sort_by_key(|(_, pp)| *pp);
    let last = valid_paths.last();
    println!("{last:?}");
}

fn main() {
    let valves = std::io::stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| parse_valve(&s).unwrap().1)
        .map(|i| (i.id.clone(), i))
        .collect::<HashMap<_, _>>();

    let distances = valves
        .values()
        .filter(|v| v.flow_rate > 0 || v.id == "AA")
        .collect::<Vec<_>>();

    let mut dist = HashMap::new();

    for i in 0..distances.len() {
        for j in 0..distances.len() {
            if i == j {
                continue;
            }

            let start = &distances[i].id;
            let end = &distances[j].id;

            dist.insert((start.clone(), end.clone()), bfs(start, end, &valves));
        }
    }

    find_best_path(&"AA".to_owned(), &dist, &valves);
}
