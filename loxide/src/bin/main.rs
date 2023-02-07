use std::{
    env,
    error::Error,
    io::{self, Read, Write},
    fs::File,
    path::Path,
    process,
};

use loxide::{ErrorBundle, lex, parse_expression,};

fn run(source: &str) -> Result<(), ErrorBundle> {
    let ex = parse_expression(lex(source))?;
    println!("expression: {:?}", ex);
    println!("expression (pretty): {}", ex);
    Ok(())
}

fn run_file<P: AsRef<Path>>(path: P) -> Result<(), Box<dyn Error>> {
    let mut source = String::new();
    File::open(path)?.read_to_string(&mut source)?;
    Ok(run(&source)?)
}

fn run_prompt() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut any_err = false;
    loop {
        write!(&mut stdout, "> ")?;
        stdout.flush()?;
        let mut line = String::new();
        match stdin.read_line(&mut line)? {
            0 => break,
            _ => if let Err(es) = run(line.trim_end()) {
                any_err = true;
                eprint!("{}", es);
            },
        };
    }
    if any_err {
        Err("at least one line resulted in an error".into())
    } else {
        Ok(())
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let result = match &args[..] {
        [_, path] => run_file(path),
        [_] => run_prompt(),
        [prog, ..] => {
            eprintln!("Usage: {} [source-file]", prog);
            process::exit(2)
        },
        [] => {
            eprintln!("Usage: loxide [source-file]");
            process::exit(2)
        }
    };

    match result {
        Ok(_) => { },
        Err(e) => {
            eprintln!("Error: {}", e);
            process::exit(1)
        }
    };
}
