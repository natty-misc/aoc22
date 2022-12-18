use std::fmt::Display;

use ndarray::{array, s, Array2};

#[repr(u8)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
enum Tile {
    #[default]
    Air,
    Solid,
}

impl Tile {
    fn is_solid(&self) -> bool {
        match *self {
            Tile::Air => false,
            Tile::Solid => true,
        }
    }
}

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Air => write!(f, "."),
            Self::Solid => write!(f, "#"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Move {
    Left,
    Right,
}

fn main() {
    #[rustfmt::skip]
    let tetrominos = &[
        array![
            [Tile::Solid, Tile::Solid, Tile::Solid, Tile::Solid]
        ],
        array![
            [Tile::Air, Tile::Solid, Tile::Air],
            [Tile::Solid, Tile::Solid, Tile::Solid],
            [Tile::Air, Tile::Solid, Tile::Air]
        ],
        array![
            [Tile::Air, Tile::Air, Tile::Solid],
            [Tile::Air, Tile::Air, Tile::Solid],
            [Tile::Solid, Tile::Solid, Tile::Solid]
        ],
        array![
            [Tile::Solid], 
            [Tile::Solid],
            [Tile::Solid],
            [Tile::Solid]
        ],
        array![
            [Tile::Solid, Tile::Solid],
            [Tile::Solid, Tile::Solid]
        ],
    ];

    let input = std::io::stdin().lines().next().unwrap().unwrap();
    let mut moves = input
        .chars()
        .map(|c| match c {
            '>' => Move::Right,
            '<' => Move::Left,
            x => panic!("Invalid move: {x}"),
        })
        .cycle();

    let tetrominos = tetrominos.iter().cycle();

    const WIDTH: usize = 7;
    const HEIGHT: usize = 12000;
    let mut chamber = Array2::<Tile>::default((HEIGHT, WIDTH));

    let mut cur_height = 0;

    for (i, tt) in tetrominos.enumerate() {
        let tile_dim = tt.dim();

        let mut x = 2;
        let init_y = HEIGHT - cur_height - tile_dim.0 - 3;
        let mut y = init_y;

        loop {
            let curr_move = moves.next().unwrap();

            match curr_move {
                Move::Left if x > 0 => {
                    let slc = chamber.slice(s![y..y + tile_dim.0, x - 1..x - 1 + tile_dim.1]);

                    if !slc
                        .iter()
                        .zip(tt.iter())
                        .any(|(a, b)| a.is_solid() && b.is_solid())
                    {
                        x -= 1
                    }
                }
                Move::Right if x + tile_dim.1 < WIDTH => {
                    let slc = chamber.slice(s![y..y + tile_dim.0, x + 1..x + 1 + tile_dim.1]);

                    if !slc
                        .iter()
                        .zip(tt.iter())
                        .any(|(a, b)| a.is_solid() && b.is_solid())
                    {
                        x += 1
                    }
                }
                _ => {}
            }

            if y + tile_dim.0 >= HEIGHT {
                break;
            }

            let slc = chamber.slice(s![y + 1..y + 1 + tile_dim.0, x..x + tile_dim.1]);

            if slc
                .iter()
                .zip(tt.iter())
                .any(|(a, b)| a.is_solid() && b.is_solid())
            {
                break;
            }

            y += 1;
        }

        let mut slc = chamber.slice_mut(s![y..y + tile_dim.0, x..x + tile_dim.1]);
        for (dst, src) in slc.iter_mut().zip(tt.iter()).filter(|(_, s)| s.is_solid()) {
            *dst = *src;
        }

        if y < init_y + tile_dim.0 + 3 {
            let dy = (init_y + tile_dim.0 + 3) - y;
            cur_height += dy;
        }

        if i == 2022 {
            break;
        }
    }

    println!("{cur_height:?}");
}
