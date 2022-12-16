use std::collections::HashSet;

use nom::{bytes::streaming::tag, character::complete::*, combinator::*, sequence::tuple, IResult};

type XY = (i32, i32);

type Beacon = XY;
type Sensor = XY;

fn manhattan(xy1: XY, xy2: XY) -> i32 {
    (xy1.0 - xy2.0).abs() + (xy1.1 - xy2.1).abs()
}

fn parse_i32(input: &str) -> IResult<&str, i32> {
    map_res(
        recognize(tuple((opt(one_of("+-")), digit1))),
        &str::parse::<i32>,
    )(input)
}

fn parse_sensor(input: &str) -> IResult<&str, (Sensor, Beacon)> {
    let (input, _) = tag("Sensor at ")(input)?;
    let (input, _) = tag("x=")(input)?;
    let (input, sensor_x) = parse_i32(input)?;
    let (input, _) = tag(", y=")(input)?;
    let (input, senson_y) = parse_i32(input)?;
    let (input, _) = tag(": closest beacon is at ")(input)?;
    let (input, _) = tag("x=")(input)?;
    let (input, beacon_x) = parse_i32(input)?;
    let (input, _) = tag(", y=")(input)?;
    let (input, beacon_y) = parse_i32(input)?;

    let sensor = (sensor_x, senson_y);
    let beacon = (beacon_x, beacon_y);
    Ok((input, (sensor, beacon)))
}

fn main() {
    let lines = std::io::stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| parse_sensor(&s).unwrap().1)
        .collect::<Vec<_>>();

    let sensors = lines
        .iter()
        .copied()
        .map(|(s, b)| (s, manhattan(s, b)))
        .collect::<Vec<_>>();
    let beacons = lines
        .iter()
        .copied()
        .map(|(_, b)| b)
        .collect::<HashSet<_>>();

    const YY: i32 = 2000000;

    let mut ranges = Vec::new();

    for ((x, y), radius) in sensors {
        let dy = (y - YY).abs();
        if dy > radius {
            continue;
        }

        let thc = radius - dy;
        let rng = (x - thc)..=(x + thc);
        ranges.push(rng);
    }

    let min = ranges.iter().map(|r| *r.start()).min().unwrap();
    let max = ranges.iter().map(|r| *r.end()).max().unwrap();

    let mut cnt = 0;

    for x in min..=max {
        if beacons.contains(&(x, YY)) {
            continue;
        }

        if ranges.iter().any(|r| r.contains(&x)) {
            cnt += 1;
        }
    }

    println!("{cnt:?}");
}
