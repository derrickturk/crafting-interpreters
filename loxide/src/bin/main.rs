use std::{
    env,
    error::Error,
    io::{self, Read, Write},
    fs::File,
    path::Path,
    process,
};

use loxide::{
    lex
};

fn run(source: &str) -> Result<(), Box<dyn Error>> {
    for tok in lex(source) {
        println!("token: {:?}", tok);
    }
    Ok(())
}

fn run_file<P: AsRef<Path>>(path: P) -> Result<(), Box<dyn Error>> {
    let mut source = String::new();
    File::open(path)?.read_to_string(&mut source)?;
    run(&source)
}

fn run_prompt() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    // TODO: swallow errors, but remember if any happened (for exit status)
    loop {
        write!(&mut stdout, "> ")?;
        stdout.flush()?;
        let mut line = String::new();
        match stdin.read_line(&mut line)? {
            0 => break,
            _ => run(&line)?,
        };
    }
    Ok(())
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
