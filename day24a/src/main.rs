use std::cmp::Reverse;

use priority_queue::PriorityQueue;

type XY = (i32, i32);

trait Coord2D {
    fn x(&self) -> i32;
    fn y(&self) -> i32;

    fn manhattan(&self, other: &impl Coord2D) -> i32 {
        (self.x() - other.x()).abs() + (self.y() - other.y()).abs()
    }
}

impl Coord2D for XY {
    fn x(&self) -> i32 {
        self.0
    }

    fn y(&self) -> i32 {
        self.1
    }
}

#[derive(Debug, Clone, Copy)]
enum BlizzardDir {
    None,
    North,
    South,
    West,
    East,
}

#[derive(Debug, Clone)]
struct GameMap {
    blizz: Vec<BlizzardDir>,
    w: usize,
    h: usize,
}

impl GameMap {
    fn tile_obstructed(&self, (x, y): XY, t: i32) -> bool {
        if x < 0 || y < 0 || x >= self.w as i32 || y >= self.h as i32 {
            return !(x == 0 && y == -1 || x == self.w as i32 - 1 && y == self.h as i32);
        }

        let y_north = (y as i64 + t as i64).rem_euclid(self.h as i64) as usize;
        let blizz_north = self.blizz[y_north * self.w + x as usize];

        let y_south = (y as i64 - t as i64).rem_euclid(self.h as i64) as usize;
        let blizz_south = self.blizz[y_south * self.w + x as usize];

        let x_east = (x as i64 - t as i64).rem_euclid(self.w as i64) as usize;
        let blizz_east = self.blizz[y as usize * self.w + x_east];

        let x_west = (x as i64 + t as i64).rem_euclid(self.w as i64) as usize;
        let blizz_west = self.blizz[y as usize * self.w + x_west];

        matches!(blizz_north, BlizzardDir::North)
            || matches!(blizz_south, BlizzardDir::South)
            || matches!(blizz_west, BlizzardDir::West)
            || matches!(blizz_east, BlizzardDir::East)
    }
}

#[derive(Debug, Clone, Copy)]
enum Edge {
    North,
    South,
    West,
    East,
    Wait,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
struct Node {
    val: i32,
    pos: XY,
    time: i32,
}

fn a_star(start: XY, goal: XY, map: &GameMap) -> Option<i32> {
    let mut open = PriorityQueue::new();

    let start_node = Node {
        val: start.manhattan(&goal),
        time: 0,
        pos: start,
    };

    open.push(start_node, Reverse(start_node.val));

    while let Some((node, _)) = open.pop() {
        open.remove(&node);

        for act in [Edge::North, Edge::South, Edge::West, Edge::East, Edge::Wait] {
            let npos = match act {
                Edge::North => (node.pos.x(), node.pos.y() - 1),
                Edge::South => (node.pos.x(), node.pos.y() + 1),
                Edge::West => (node.pos.x() - 1, node.pos.y()),
                Edge::East => (node.pos.x() + 1, node.pos.y()),
                Edge::Wait => (node.pos.x(), node.pos.y()),
            };

            let ntime = node.time + 1;

            let new_node = Node {
                val: ntime + goal.manhattan(&npos),
                time: ntime,
                pos: npos,
            };

            if map.tile_obstructed(npos, ntime) {
                continue;
            }

            open.push(new_node, Reverse(new_node.val));

            if new_node.pos == goal {
                return Some(new_node.time);
            }
        }
    }

    None
}

fn main() {
    let raw_map = std::io::stdin()
        .lines()
        .map(|l| l.unwrap())
        .collect::<Vec<_>>();

    let h = raw_map.len() as usize - 2;
    let w = raw_map[0].len() - 2;

    let blizz = raw_map
        .into_iter()
        .skip(1)
        .take(h)
        .flat_map(|l| l.chars().skip(1).take(w).collect::<Vec<_>>())
        .map(|c| match c {
            '^' => BlizzardDir::North,
            'v' => BlizzardDir::South,
            '<' => BlizzardDir::West,
            '>' => BlizzardDir::East,
            _ => BlizzardDir::None,
        })
        .collect();

    let map = GameMap { blizz, w, h };

    let sol = a_star((0, -1), (map.w as i32 - 1, map.h as i32), &map);

    println!("{sol:?}");
}
