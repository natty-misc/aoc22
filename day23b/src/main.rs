use std::collections::HashMap;

use smallvec::{smallvec, SmallVec};

type XY = (i32, i32);

#[derive(Debug, Clone)]
struct GameMap(HashMap<XY, Elf>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SearchDir {
    North,
    South,
    West,
    East,
}

#[derive(Debug, Clone)]
struct Elf {
    search_queue: SmallVec<[SearchDir; 8]>,
}

fn main() {
    let map = std::io::stdin()
        .lines()
        .map(|l| l.unwrap())
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .filter_map(|(x, c)| {
                    if matches!(c, '#') {
                        Some((
                            (x as i32, y as i32),
                            Elf {
                                search_queue: smallvec![
                                    SearchDir::North,
                                    SearchDir::South,
                                    SearchDir::West,
                                    SearchDir::East
                                ],
                            },
                        ))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<HashMap<_, _>>();

    let mut map = GameMap(map);

    for i in 1.. {
        let old = map.clone();

        let mut proposed = HashMap::new();

        for (&xy @ (x, y), v) in map.0.iter() {
            let empty_around = {
                let mut found = false;

                'e: for xx in x - 1..=x + 1 {
                    for yy in y - 1..=y + 1 {
                        if x == xx && y == yy {
                            continue;
                        }

                        if map.0.contains_key(&(xx, yy)) {
                            found = true;
                            break 'e;
                        }
                    }
                }

                !found
            };

            if empty_around {
                continue;
            }

            for (d_idx, dir) in v.search_queue.iter().enumerate() {
                match dir {
                    SearchDir::North => {
                        if let (None, None, None) = (
                            map.0.get(&(x - 1, y - 1)),
                            map.0.get(&(x, y - 1)),
                            map.0.get(&(x + 1, y - 1)),
                        ) {
                            proposed
                                .entry((x, y - 1))
                                .or_insert(Vec::new())
                                .push((xy, d_idx));
                            break;
                        }
                    }
                    SearchDir::South => {
                        if let (None, None, None) = (
                            map.0.get(&(x - 1, y + 1)),
                            map.0.get(&(x, y + 1)),
                            map.0.get(&(x + 1, y + 1)),
                        ) {
                            proposed
                                .entry((x, y + 1))
                                .or_insert(Vec::new())
                                .push((xy, d_idx));
                            break;
                        }
                    }
                    SearchDir::West => {
                        if let (None, None, None) = (
                            map.0.get(&(x - 1, y - 1)),
                            map.0.get(&(x - 1, y)),
                            map.0.get(&(x - 1, y + 1)),
                        ) {
                            proposed
                                .entry((x - 1, y))
                                .or_insert(Vec::new())
                                .push((xy, d_idx));
                            break;
                        }
                    }
                    SearchDir::East => {
                        if let (None, None, None) = (
                            map.0.get(&(x + 1, y - 1)),
                            map.0.get(&(x + 1, y)),
                            map.0.get(&(x + 1, y + 1)),
                        ) {
                            proposed
                                .entry((x + 1, y))
                                .or_insert(Vec::new())
                                .push((xy, d_idx));
                            break;
                        }
                    }
                }
            }
        }

        for (pos_to, froms) in proposed {
            if froms.len() > 1 {
                continue;
            }

            let (pos_from, _d_idx) = froms[0];

            let elf = map.0.remove(&pos_from).unwrap();
            map.0.insert(pos_to, elf);
        }

        map.0.values_mut().for_each(|elf| {
            let rem = elf.search_queue.remove(0);
            elf.search_queue.push(rem);
        });

        if map.0.keys().zip(old.0.keys()).all(|(a, b)| a == b) {
            println!("Round: {i}");
            break;
        }
    }
}
