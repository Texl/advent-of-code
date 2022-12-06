use std::fs::File;
use std::io::prelude::*;
use itertools::Itertools;

fn open_file() -> Result<Vec<String>, std::io::Error> {
    let mut file: File = File::open("data/Day01.txt")?;
    let mut contents: String = String::new();
    file.read_to_string(&mut contents)?;
    let vec =
        contents
            .split("\r\n\r\n")
            .map(str::to_string)
            .collect();
    Ok(vec)
}

pub fn part1() -> () {
    let chunks = open_file().unwrap();

    let sum: i64 =
        chunks
            .iter()
            .map(|chunk|
                chunk
                    .split("\r\n")
                    .map(|x| x.parse::<i64>().unwrap())
                    .sum())
            .max()
            .unwrap();

    println!("Day 01, Part 1");
    println!("{sum}")
}

pub fn part2() -> () {
    let chunks = open_file().unwrap();

    let sum: i64 =
        chunks
            .iter()
            .map(|chunk|
                chunk
                    .split("\r\n")
                    .map(|x| x.parse::<i64>().unwrap())
                    .sum::<i64>())
            .sorted()
            .rev()
            .take(3)
            .sum();

    println!("Day 01, Part 1");
    println!("{sum}")
}

