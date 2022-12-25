use std::{fmt::Display, ops::Index, str::FromStr};

use nom::{
    branch::*, bytes::complete::tag, character::complete::*, combinator::*, multi::many1,
    sequence::*, IResult,
};

fn parse_num<T: FromStr>(input: &str) -> IResult<&str, T> {
    map_res(recognize(tuple((opt(tag("+-")), digit1))), &str::parse::<T>)(input)
}

#[derive(Debug, Clone, Copy)]
enum Command {
    Move(u32),
    Left,
    Right,
}

fn parse_move(input: &str) -> IResult<&str, Command> {
    let (input, val) = parse_num(input)?;
    Ok((input, Command::Move(val)))
}

fn parse_rot(input: &str) -> IResult<&str, Command> {
    let (input, dir) = alt((tag("L"), tag("R")))(input)?;

    Ok((
        input,
        match dir {
            "L" => Command::Left,
            "R" => Command::Right,
            _ => unreachable!(),
        },
    ))
}

fn parse_commands(input: &str) -> IResult<&str, Vec<Command>> {
    many1(alt((parse_move, parse_rot)))(input)
}

#[derive(Debug)]
enum Tile {
    Void,
    Empty,
    Wall,
}

fn map_tile(c: char) -> Tile {
    match c {
        ' ' => Tile::Void,
        '.' => Tile::Empty,
        '#' => Tile::Wall,
        _ => unreachable!(),
    }
}

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Tile::Void => write!(f, " "),
            Tile::Empty => write!(f, "."),
            Tile::Wall => write!(f, "#"),
        }
    }
}

struct GameMap {
    tiles: Vec<Tile>,
    width: usize,
}

impl Index<(usize, usize)> for GameMap {
    type Output = Tile;

    fn index(&self, (x, y): (usize, usize)) -> &Self::Output {
        &self.tiles[self.width * y + x]
    }
}

impl GameMap {
    fn new(tiles: Vec<Tile>, width: usize) -> Self {
        Self { tiles, width }
    }

    fn width(&self) -> usize {
        self.width
    }

    fn height(&self) -> usize {
        self.tiles.len() / self.width
    }

    fn next_step(&self, ori: &Orientation, x: usize, y: usize) -> (Orientation, (usize, usize)) {
        const SIZE: usize = 50;

        let macro_x = x / SIZE;
        let macro_y = y / SIZE;

        let sub_x = x % SIZE;
        let sub_y = y % SIZE;

        const MXS: usize = SIZE - 1;

        // Naive lazy solution :P
        match (macro_x, macro_y) {
            (1, 0) => {
                if sub_y == 0 && matches!(ori, Orientation::Up) {
                    return (Orientation::Right, (0, 3 * SIZE + sub_x));
                }

                if sub_x == 0 && matches!(ori, Orientation::Left) {
                    return (Orientation::Right, (0, 2 * SIZE + (MXS - sub_y)));
                }
            }
            (2, 0) => {
                if sub_y == 0 && matches!(ori, Orientation::Up) {
                    return (Orientation::Up, (sub_x, 3 * SIZE + MXS));
                }

                if sub_x == MXS && matches!(ori, Orientation::Right) {
                    return (Orientation::Left, (SIZE + MXS, 2 * SIZE + (MXS - sub_y)));
                }

                if sub_y == MXS && matches!(ori, Orientation::Down) {
                    return (Orientation::Left, (SIZE + MXS, SIZE + sub_x));
                }
            }
            (1, 1) => {
                if sub_x == 0 && matches!(ori, Orientation::Left) {
                    return (Orientation::Down, (sub_y, 2 * SIZE));
                }

                if sub_x == MXS && matches!(ori, Orientation::Right) {
                    return (Orientation::Up, (2 * SIZE + sub_y, MXS));
                }
            }
            (0, 2) => {
                if sub_x == 0 && matches!(ori, Orientation::Left) {
                    return (Orientation::Right, (SIZE, MXS - sub_y));
                }

                if sub_y == 0 && matches!(ori, Orientation::Up) {
                    return (Orientation::Right, (SIZE, SIZE + sub_x));
                }
            }
            (1, 2) => {
                if sub_x == MXS && matches!(ori, Orientation::Right) {
                    return (Orientation::Left, (2 * SIZE + MXS, MXS - sub_y));
                }

                if sub_y == MXS && matches!(ori, Orientation::Down) {
                    return (Orientation::Left, (MXS, 3 * SIZE + sub_x));
                }
            }
            (0, 3) => {
                if sub_x == MXS && matches!(ori, Orientation::Right) {
                    return (Orientation::Up, (SIZE + sub_y, 2 * SIZE + MXS));
                }

                if sub_y == MXS && matches!(ori, Orientation::Down) {
                    return (Orientation::Down, (2 * SIZE + sub_x, 0));
                }

                if sub_x == 0 && matches!(ori, Orientation::Left) {
                    return (Orientation::Down, (SIZE + sub_y, 0));
                }
            }
            (_, _) => {}
        };

        let mut xx = x as i64;
        let mut yy = y as i64;

        match ori {
            Orientation::Up => yy -= 1,
            Orientation::Left => xx -= 1,
            Orientation::Down => yy += 1,
            Orientation::Right => xx += 1,
        }
        (
            *ori,
            (
                xx.rem_euclid(self.width() as i64) as usize,
                yy.rem_euclid(self.height() as i64) as usize,
            ),
        )
    }

    fn try_move(&self, ori: &mut Orientation, x: &mut usize, y: &mut usize, dist: u32) {
        let mut nrx = *x;
        let mut nry = *y;
        let mut oo = *ori;

        let mut i = 0;
        while i < dist {
            let (ori_n, (xx_n, yy_n)) = self.next_step(&oo, nrx, nry);

            oo = ori_n;
            nrx = xx_n;
            nry = yy_n;

            if matches!(self[(nrx, nry)], Tile::Wall) {
                break;
            }

            if matches!(self[(nrx, nry)], Tile::Empty) {
                *x = nrx;
                *y = nry;
                *ori = oo;
                i += 1;
            }
        }
    }
}

impl Display for GameMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.tiles.len() {
            write!(f, "{}", self.tiles[i])?;

            if i % self.width == self.width - 1 {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

#[repr(u32)]
#[derive(Debug, Clone, Copy)]
enum Orientation {
    Right = 0,
    Down = 1,
    Left = 2,
    Up = 3,
}

impl Orientation {
    fn rotate_left(&self) -> Self {
        match *self {
            Orientation::Up => Orientation::Left,
            Orientation::Left => Orientation::Down,
            Orientation::Down => Orientation::Right,
            Orientation::Right => Orientation::Up,
        }
    }

    fn rotate_right(&self) -> Self {
        match *self {
            Orientation::Left => Orientation::Up,
            Orientation::Up => Orientation::Right,
            Orientation::Right => Orientation::Down,
            Orientation::Down => Orientation::Left,
        }
    }
}

fn main() {
    let mut s_map = Vec::new();

    for line in std::io::stdin().lines() {
        let ln = line.unwrap();

        if ln.is_empty() {
            break;
        }

        s_map.push(ln);
    }

    let w = s_map.iter().map(String::len).max().unwrap();
    let mut map = Vec::new();
    for line in s_map {
        let row = line
            .chars()
            .chain(std::iter::repeat(' '))
            .take(w)
            .map(map_tile);

        map.extend(row);
    }

    let mmap = GameMap::new(map, w);

    let mut cmd_buf = String::new();
    std::io::stdin().read_line(&mut cmd_buf).unwrap();
    let commands = parse_commands(&cmd_buf).unwrap().1;

    let mut x = mmap
        .tiles
        .iter()
        .enumerate()
        .find(|(_, t)| matches!(**t, Tile::Empty))
        .unwrap()
        .0;

    let mut y = 0;

    let mut ori = Orientation::Right;

    for cmd in commands {
        match cmd {
            Command::Left => {
                ori = ori.rotate_left();
            }
            Command::Right => {
                ori = ori.rotate_right();
            }
            Command::Move(dist) => {
                mmap.try_move(&mut ori, &mut x, &mut y, dist);
            }
        }
    }

    x += 1;
    y += 1;

    println!(
        "1000 * {x} + 4 * {y} + {} = {}",
        ori as u32,
        1000 * y + 4 * x + ori as u32 as usize
    );
}
