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

#[derive(Debug, Clone, Copy)]
struct Line {
    xy1: XY,
    xy2: XY,
}

fn intersection(l1: &Line, l2: &Line) -> Option<XY> {
    let ax1 = l1.xy1.0 as f64;
    let ay1 = l1.xy1.1 as f64;
    let ax2 = l1.xy2.0 as f64;
    let ay2 = l1.xy2.1 as f64;

    let bx1 = l2.xy1.0 as f64;
    let by1 = l2.xy1.1 as f64;
    let bx2 = l2.xy2.0 as f64;
    let by2 = l2.xy2.1 as f64;

    let axy = ax1 * ay2 - ay1 * ax2;
    let ax = ax1 - ax2;
    let ay = ay1 - ay2;
    let bxy = bx1 * by2 - by1 * bx2;
    let bx = bx1 - bx2;
    let by = by1 - by2;

    let px_denom = ax * by - ay * by;
    let py_denom = ax * by - ay * bx;

    const EPSILON: f64 = 0.001;

    if px_denom.abs() < EPSILON || py_denom.abs() < EPSILON {
        return None;
    }

    let px = (axy * bx - ax * bxy) / px_denom;
    let py = (axy * by - ay * bxy) / py_denom;

    if px < ax1.min(ax2) - EPSILON
        || px > ax1.max(ax2) + EPSILON
        || py < ay1.min(ay2) - EPSILON
        || py > ay1.max(ay2) + EPSILON
    {
        return None;
    }

    Some((px.round() as i32, py as i32))
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

    let mut edges = vec![
        Line {
            xy1: (-1, -1),
            xy2: (4000000, -1),
        },
        Line {
            xy1: (4000000, -1),
            xy2: (4000000, 4000000),
        },
        Line {
            xy1: (4000000, 4000000),
            xy2: (-1, 4000000),
        },
        Line {
            xy1: (-1, 4000000),
            xy2: (-1, -1),
        },
    ];

    for ((x, y), radius) in sensors {
        edges.push(Line {
            xy1: (x - radius, y),
            xy2: (x, y - radius),
        });

        edges.push(Line {
            xy1: (x, y - radius),
            xy2: (x + radius, y),
        });

        edges.push(Line {
            xy1: (x + radius, y),
            xy2: (x, y + radius),
        });

        edges.push(Line {
            xy1: (x, y + radius),
            xy2: (x - radius, y),
        });
    }

    let mut intersections = HashSet::new();

    for i in 0..edges.len() {
        for j in i + 1..edges.len() {
            let l1 = edges[i];
            let l2 = edges[j];

            let ins = intersection(&l1, &l2);

            if let Some(intersection) = ins {
                intersections.insert(intersection);
            }
        }
    }

    // This makes several wrong assumptions, like the fact the point is probably NOT in the corner of the map
    // But it works, and really fast
    for (ix, iy) in intersections.iter().copied() {
        if intersections.contains(&(ix + 1, iy + 1))
            && intersections.contains(&(ix, iy + 2))
            && intersections.contains(&(ix - 1, iy + 1))
        {
            let spot = (ix, iy + 1);
            println!("Pos: {spot:?}");
            let freq: i64 = 4000000i64 * (ix as i64) + (iy as i64 + 1i64);
            println!("Freq: {freq:?}")
        }
    }
}
