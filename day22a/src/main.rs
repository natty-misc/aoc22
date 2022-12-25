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

struct GameMap(Vec<Tile>, usize);

impl Index<(usize, usize)> for GameMap {
    type Output = Tile;

    fn index(&self, (x, y): (usize, usize)) -> &Self::Output {
        &self.0[self.1 * y + x]
    }
}

impl GameMap {
    fn width(&self) -> usize {
        self.1
    }

    fn height(&self) -> usize {
        self.0.len() / self.1
    }

    fn try_move(&self, ori: &Orientation, x: &mut usize, y: &mut usize, dist: u32) {
        let mut xx = *x as i64;
        let mut yy = *y as i64;

        let mut i = 0;
        while i < dist {
            match ori {
                Orientation::Up => yy -= 1,
                Orientation::Left => xx -= 1,
                Orientation::Down => yy += 1,
                Orientation::Right => xx += 1,
            };

            let nrx = xx.rem_euclid(self.width() as i64) as usize;
            let nry = yy.rem_euclid(self.height() as i64) as usize;

            if matches!(self[(nrx, nry)], Tile::Wall) {
                break;
            }

            if matches!(self[(nrx, nry)], Tile::Empty) {
                *x = nrx;
                *y = nry;
                i += 1;
            }
        }
    }
}

impl Display for GameMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.0.len() {
            write!(f, "{}", self.0[i])?;

            if i % self.1 == self.1 - 1 {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

#[repr(u32)]
#[derive(Debug)]
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

    let mmap = GameMap(map, w);

    let mut cmd_buf = String::new();
    std::io::stdin().read_line(&mut cmd_buf).unwrap();
    let commands = parse_commands(&cmd_buf).unwrap().1;

    let mut x = mmap
        .0
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
                mmap.try_move(&ori, &mut x, &mut y, dist);
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
