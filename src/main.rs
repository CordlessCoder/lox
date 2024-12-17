#![allow(dead_code)]
use std::path::Path;

mod interpreter;
mod lex;
mod parse;
mod tree;
mod types;
use interpreter::Lox;
use owo_colors::OwoColorize;

fn repl() {
    use rustyline::{config::Configurer, error::ReadlineError, DefaultEditor};
    let mut rl = DefaultEditor::new().unwrap();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    rl.set_tab_stop(4);
    rl.set_edit_mode(rustyline::EditMode::Vi);
    rl.set_completion_type(rustyline::CompletionType::List);
    let mut lox = Lox::default();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).unwrap();
                if let Err(errors) = lox.run(&line) {
                    for error in &errors {
                        eprintln!("{}\n", error.bright_red());
                    }
                }
            }
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
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
