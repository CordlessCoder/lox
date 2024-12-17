#![allow(dead_code)]
use std::{
    io::{stdin, stdout, IsTerminal, Write},
    path::Path,
};

mod interpreter;
mod lex;
mod parse;
mod tree;
use interpreter::Lox;

fn repl() {
    let mut output = stdout();
    let input = stdin();
    let mut buf = String::new();
    let mut lox = Lox::default();
    let is_term = input.is_terminal();

    loop {
        if is_term {
            write!(&mut output, "> ").unwrap();
            output.flush().unwrap();
        }

        buf.clear();
        input.read_line(&mut buf).unwrap();
        if buf.is_empty() {
            break;
        }
        lox.run(&buf).unwrap();
    }
}

fn run_file(path: impl AsRef<Path>) {
    let data = std::fs::read_to_string(path).expect("Failed to read input file");
    let mut lox = Lox::default();
    lox.run(&data).unwrap();
}

fn main() {
    let mut args = std::env::args().skip(1);
    match args.len() {
        0 => repl(),
        1 => run_file(args.next().unwrap()),
        _ => {
            eprintln!("Usage: rlox [script]");
            std::process::exit(64)
        }
    }
}
