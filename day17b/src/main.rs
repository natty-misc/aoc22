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

    let moves_lst = input
        .chars()
        .map(|c| match c {
            '>' => Move::Right,
            '<' => Move::Left,
            x => panic!("Invalid move: {x}"),
        })
        .collect::<Vec<_>>();

    let mut moves = moves_lst.iter().enumerate().cycle().peekable();

    let mut tetromino_it = tetrominos.iter().cycle();

    const WIDTH: usize = 7;
    const HEIGHT: usize = 10_000_000;
    let mut chamber = Array2::<Tile>::default((HEIGHT, WIDTH));

    let mut cur_height = 0;
    let mut cur_height_prev = 0;

    let mut imp = 0;

    let cycle_start_idx;
    let mut patterns = Vec::<(Vec<_>, usize)>::new();
    let mut curr_pattern = Vec::new();

    loop {
        let tt = tetromino_it.next().unwrap();
        let (im, _) = moves.peek().unwrap();

        if *im < imp {
            let cycle_start = patterns
                .iter()
                .enumerate()
                .find(|(_, (v, _))| *v == curr_pattern);

            if let Some((csi, _)) = cycle_start {
                cycle_start_idx = csi;
                break;
            }

            println!("{tt}");
            println!("{:?}", &curr_pattern[0..5]);

            patterns.push((curr_pattern, cur_height - cur_height_prev));
            cur_height_prev = cur_height;
            curr_pattern = Vec::new();
        }

        curr_pattern.push((*im, cur_height - cur_height_prev));

        imp = *im;

        let tile_dim = tt.dim();

        let mut x = 2;
        let init_y = HEIGHT - cur_height - tile_dim.0 - 3;
        let mut y = init_y;

        loop {
            let (_, curr_move) = moves.next().unwrap();

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
    }

    println!("Cycle from {} on", cycle_start_idx);

    let lead: usize = patterns[..cycle_start_idx].iter().map(|(_, s)| *s).sum();
    let lead_len: usize = patterns[..cycle_start_idx]
        .iter()
        .map(|(v, _)| v.len())
        .sum();

    println!("Lead: {lead}, length: {lead_len}");

    let pp = 1000000000000usize;

    let cycle: usize = patterns[cycle_start_idx..].iter().map(|(_, s)| *s).sum();
    let cycle_len: usize = patterns[cycle_start_idx..]
        .iter()
        .map(|(v, _)| v.len())
        .sum();

    println!("Cycle: {cycle}, length: {cycle_len}");

    let rem = pp - lead_len;

    println!("Remaining: {rem}");

    let rmm = rem / cycle_len;
    let sol = lead + rmm * cycle;

    let srm = rem - rmm * cycle_len;
    println!("Still remaining: {srm}");

    let fsol: usize = sol + {
        let mut ff = 0;
        let mut ffr = 0;
        let mut ij = 0;

        'bb: for (v, u) in patterns[cycle_start_idx..].iter() {
            for (_, pu) in v {
                ffr = ff + pu;
                if ij == srm {
                    break 'bb;
                }

                ij += 1;
            }

            ff += *u;
        }

        ffr
    };

    println!("Solution: {fsol}");
}
