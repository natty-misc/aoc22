use std::{
    collections::HashSet,
    ops::{Index, IndexMut},
};

use nom::{bytes::complete::tag, character::complete::*, combinator::*, sequence::tuple, IResult};

use ndarray::Array3;

#[repr(u8)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum Tile {
    #[default]
    Air,
    Solid,
    Outside,
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

impl IndexMut<(i32, i32, i32)> for VoxelMap {
    fn index_mut(&mut self, (x, y, z): (i32, i32, i32)) -> &mut Self::Output {
        self.0
            .get_mut((x as usize, y as usize, z as usize))
            .unwrap()
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
        .map(|(x, y, z)| (x + 1, y + 1, z + 1))
        .collect::<HashSet<_>>();

    let max_x = vox.iter().map(|v| v.0).max().unwrap() + 2;
    let max_y = vox.iter().map(|v| v.1).max().unwrap() + 2;
    let max_z = vox.iter().map(|v| v.2).max().unwrap() + 2;

    let mut voxels = VoxelMap(Array3::from_shape_fn(
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

    // Poor girl's floodfill
    voxels[(0, 0, 0)] = Tile::Outside;
    for _ in 0..1000 {
        let mut new = VoxelMap(voxels.0.clone());

        voxels.0.indexed_iter().for_each(|((x, y, z), t)| {
            let x: i32 = x.try_into().unwrap();
            let y: i32 = y.try_into().unwrap();
            let z: i32 = z.try_into().unwrap();
            let xyz = (x, y, z);

            if let Tile::Solid = t {
                new[xyz] = Tile::Solid;
                return;
            }

            if let Tile::Outside = voxels[(x - 1, y, z)] {
                new[xyz] = Tile::Outside;
                return;
            }

            if let Tile::Outside = voxels[(x + 1, y, z)] {
                new[xyz] = Tile::Outside;
                return;
            }

            if let Tile::Outside = voxels[(x, y - 1, z)] {
                new[xyz] = Tile::Outside;
                return;
            }

            if let Tile::Outside = voxels[(x, y + 1, z)] {
                new[xyz] = Tile::Outside;
                return;
            }

            if let Tile::Outside = voxels[(x, y, z - 1)] {
                new[xyz] = Tile::Outside;
                return;
            }

            if let Tile::Outside = voxels[(x, y, z + 1)] {
                new[xyz] = Tile::Outside;
                return;
            }

            new[xyz] = *t;
        });

        voxels.0 = new.0;
    }

    let sum: usize = voxels
        .0
        .indexed_iter()
        .map(|((x, y, z), t)| {
            let x: i32 = x.try_into().unwrap();
            let y: i32 = y.try_into().unwrap();
            let z: i32 = z.try_into().unwrap();

            match t {
                Tile::Solid => {
                    let mut c = 0;
                    if let Tile::Outside = voxels[(x - 1, y, z)] {
                        c += 1;
                    }

                    if let Tile::Outside = voxels[(x + 1, y, z)] {
                        c += 1;
                    }

                    if let Tile::Outside = voxels[(x, y - 1, z)] {
                        c += 1;
                    }

                    if let Tile::Outside = voxels[(x, y + 1, z)] {
                        c += 1;
                    }

                    if let Tile::Outside = voxels[(x, y, z - 1)] {
                        c += 1;
                    }

                    if let Tile::Outside = voxels[(x, y, z + 1)] {
                        c += 1;
                    }

                    c
                }
                Tile::Outside | Tile::Air => 0,
            }
        })
        .sum();

    println!("Sur-faces: {sum}");
}
