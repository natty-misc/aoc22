use nom::{bytes::complete::tag, character::complete::*, combinator::*, sequence::tuple, IResult};
use rayon::prelude::{IntoParallelRefIterator, ParallelIterator};
use smallvec::SmallVec;

#[derive(Debug)]
struct Blueprint {
    id: u32,
    ore_cost: u32,
    clay_cost: u32,
    obsidian_cost_ore: u32,
    obsidian_cost_clay: u32,
    geode_cost_ore: u32,
    geode_cost_obsidian: u32,
}

#[derive(Debug, Clone, Copy)]
struct Resources {
    ore: u32,
    clay: u32,
    obsidian: u32,
    geodes: u32,
    ore_robots: u32,
    clay_robots: u32,
    obsidian_robots: u32,
    geode_robots: u32,
}

impl Default for Resources {
    fn default() -> Self {
        Resources {
            ore: 0,
            ore_robots: 1,
            clay: 0,
            clay_robots: 0,
            obsidian: 0,
            obsidian_robots: 0,
            geodes: 0,
            geode_robots: 0,
        }
    }
}

fn parse_u32(input: &str) -> IResult<&str, u32> {
    map_res(
        recognize(tuple((opt(tag("+")), digit1))),
        &str::parse::<u32>,
    )(input)
}

fn parse_line(input: &str) -> IResult<&str, Blueprint> {
    let (input, _) = tag("Blueprint ")(input)?;
    let (input, id) = parse_u32(input)?;
    let (input, _) = tag(": Each ore robot costs ")(input)?;
    let (input, ore_cost) = parse_u32(input)?;
    let (input, _) = tag(" ore. Each clay robot costs ")(input)?;
    let (input, clay_cost) = parse_u32(input)?;
    let (input, _) = tag(" ore. Each obsidian robot costs ")(input)?;
    let (input, obsidian_cost_ore) = parse_u32(input)?;
    let (input, _) = tag(" ore and ")(input)?;
    let (input, obsidian_cost_clay) = parse_u32(input)?;
    let (input, _) = tag(" clay. Each geode robot costs ")(input)?;
    let (input, geode_cost_ore) = parse_u32(input)?;
    let (input, _) = tag(" ore and ")(input)?;
    let (input, geode_cost_obsidian) = parse_u32(input)?;
    let (input, _) = tag(" obsidian.")(input)?;

    Ok((
        input,
        Blueprint {
            id,
            ore_cost,
            clay_cost,
            obsidian_cost_ore,
            obsidian_cost_clay,
            geode_cost_ore,
            geode_cost_obsidian,
        },
    ))
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
enum Edge {
    Wait,
    MakeOre,
    MakeClay,
    MakeObsidian,
    MakeGeode,
}

fn get_edges(blueprint: &Blueprint, res: &Resources) -> SmallVec<[Edge; 8]> {
    let mut edges = SmallVec::new();

    // Important heuristic: Generate edges that contribute to geodes first!
    // Major speedup

    if res.ore >= blueprint.geode_cost_ore && res.obsidian >= blueprint.geode_cost_obsidian {
        edges.push(Edge::MakeGeode);
    }

    if res.ore >= blueprint.obsidian_cost_ore && res.clay >= blueprint.obsidian_cost_clay {
        edges.push(Edge::MakeObsidian);
    }

    if res.ore >= blueprint.clay_cost {
        edges.push(Edge::MakeClay);
    }

    if res.ore >= blueprint.ore_cost {
        edges.push(Edge::MakeOre);
    }

    // Not sure if the bound can be reduced further
    if res.ore < blueprint.ore_cost
        || res.ore < blueprint.clay_cost
        || res.ore < blueprint.obsidian_cost_ore
        || res.clay < blueprint.obsidian_cost_clay
        || res.ore < blueprint.geode_cost_ore
        || res.obsidian < blueprint.geode_cost_obsidian
    {
        edges.push(Edge::Wait);
    }

    edges
}

fn should_prune(remaining: u32, blueprint: &Blueprint, curr: &Resources, max: u32) -> bool {
    // Check if enough geodes can be generated in the remaining time
    // This bound assumes a robot would be created every remaining iteration
    let bnd_ore_robot = remaining.saturating_sub(2);
    let bnd_ore = curr.ore
        + curr.ore_robots * remaining.saturating_sub(1)
        + (bnd_ore_robot * remaining.saturating_sub(1) + 1) / 2;

    let bnd_clay_robot = remaining
        .min(bnd_ore / blueprint.clay_cost)
        .min(remaining.saturating_sub(3));
    let bnd_clay = curr.clay
        + curr.clay_robots * remaining.saturating_sub(2)
        + (bnd_clay_robot * remaining.saturating_sub(2) + 1) / 2;

    let bnd_obs_robot = (bnd_clay / blueprint.obsidian_cost_clay)
        .min(bnd_ore / blueprint.obsidian_cost_ore)
        .min(remaining.saturating_sub(2));
    let bnd_obs = curr.obsidian
        + curr.obsidian_robots * remaining.saturating_sub(1)
        + (bnd_obs_robot * remaining.saturating_sub(1) + 1) / 2;

    let bnd_geode_robot = (bnd_obs / blueprint.geode_cost_obsidian)
        .min(bnd_ore / blueprint.geode_cost_ore)
        .min(remaining.saturating_sub(1));
    let bnd_geode =
        curr.geodes + curr.geode_robots * remaining + (bnd_geode_robot * remaining + 1) / 2;

    bnd_geode <= max
}

fn find_paths(
    time: u32,
    blueprint: &Blueprint,
    curr: &Resources,
    predecessors: &SmallVec<[Edge; 36]>,
    max_yield: &mut u32,
) {
    const T_MAX: u32 = 24;

    let rem_t = T_MAX + 1 - time;

    if should_prune(rem_t, blueprint, curr, *max_yield) {
        return;
    }

    let edges = get_edges(blueprint, curr);

    let mut res_new = *curr;
    res_new.ore += res_new.ore_robots;
    res_new.clay += res_new.clay_robots;
    res_new.obsidian += res_new.obsidian_robots;
    res_new.geodes += res_new.geode_robots;

    if curr.geodes > *max_yield {
        *max_yield = curr.geodes;
    }

    if rem_t == 0 {
        return;
    }

    for edge in edges {
        let mut res_edge = res_new;

        match edge {
            Edge::MakeOre => {
                res_edge.ore -= blueprint.ore_cost;
                res_edge.ore_robots += 1;
            }
            Edge::MakeClay => {
                res_edge.ore -= blueprint.clay_cost;
                res_edge.clay_robots += 1;
            }
            Edge::MakeObsidian => {
                res_edge.ore -= blueprint.obsidian_cost_ore;
                res_edge.clay -= blueprint.obsidian_cost_clay;
                res_edge.obsidian_robots += 1;
            }
            Edge::MakeGeode => {
                res_edge.ore -= blueprint.geode_cost_ore;
                res_edge.obsidian -= blueprint.geode_cost_obsidian;
                res_edge.geode_robots += 1;
            }
            Edge::Wait => {}
        }

        let mut chain = predecessors.clone();
        chain.push(edge);

        find_paths(time + 1, blueprint, &res_edge, &chain, max_yield)
    }
}

fn main() {
    let blueprints = std::io::stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| parse_line(&s).unwrap().1)
        .collect::<Vec<_>>();

    let sol: u32 = blueprints
        .par_iter()
        .map(|bp| {
            let mut max = 0;
            find_paths(1, bp, &Resources::default(), &SmallVec::new(), &mut max);

            println!("{bp:?} -- {max}");

            bp.id * max
        })
        .sum();

    println!("{sol}");
}
