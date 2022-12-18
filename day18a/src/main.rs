use std::{collections::HashSet, ops::Index};

use nom::{bytes::complete::tag, character::complete::*, combinator::*, sequence::tuple, IResult};

use ndarray::Array3;

#[repr(u8)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum Tile {
    #[default]
    Air,
    Solid,
}

#[derive(Clone, Debug)]
struct VoxelMap(pub Array3<Tile>);

impl Index<(i32, i32, i32)> for VoxelMap {
    type Output = Tile;

    fn index(&self, (x, y, z): (i32, i32, i32)) -> &Self::Output {
        if x < 0 || y < 0 || z < 0 {
            return &Tile::Air;
        }

        self.0
            .get((x as usize, y as usize, z as usize))
            .unwrap_or(&Tile::Air)
    }
}

fn parse_i32(input: &str) -> IResult<&str, i32> {
    map_res(
        recognize(tuple((opt(tag("+-")), digit1))),
        &str::parse::<i32>,
    )(input)
}

fn parse_line(input: &str) -> IResult<&str, (i32, i32, i32)> {
    let (input, x) = parse_i32(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, y) = parse_i32(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, z) = parse_i32(input)?;

    Ok((input, (x, y, z)))
}

fn main() {
    let vox = std::io::stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| parse_line(&s).unwrap().1)
        .collect::<HashSet<_>>();

    let max_x = vox.iter().map(|v| v.0).max().unwrap() + 1;
    let max_y = vox.iter().map(|v| v.1).max().unwrap() + 1;
    let max_z = vox.iter().map(|v| v.2).max().unwrap() + 1;

    let voxels = VoxelMap(Array3::from_shape_fn(
        (max_x as usize, max_y as usize, max_z as usize),
        |(x, y, z)| {
            if vox.contains(&(
                i32::try_from(x).unwrap(),
                i32::try_from(y).unwrap(),
                i32::try_from(z).unwrap(),
            )) {
                Tile::Solid
            } else {
                Tile::Air
            }
        },
    ));

    let sum: usize = voxels
        .0
        .indexed_iter()
        .map(|((x, y, z), t)| {
            let x: i32 = x.try_into().unwrap();
            let y: i32 = y.try_into().unwrap();
            let z: i32 = z.try_into().unwrap();

            let mut c = 0;

            if let Tile::Air = t {
                return 0;
            }

            if let Tile::Air = voxels[(x - 1, y, z)] {
                c += 1;
            }

            if let Tile::Air = voxels[(x + 1, y, z)] {
                c += 1;
            }

            if let Tile::Air = voxels[(x, y - 1, z)] {
                c += 1;
            }

            if let Tile::Air = voxels[(x, y + 1, z)] {
                c += 1;
            }

            if let Tile::Air = voxels[(x, y, z - 1)] {
                c += 1;
            }

            if let Tile::Air = voxels[(x, y, z + 1)] {
                c += 1;
            }

            c
        })
        .sum();

    println!("Sur-faces: {sum}");
}
