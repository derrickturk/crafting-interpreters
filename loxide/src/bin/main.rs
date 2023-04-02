use std::{
    env,
    error::Error,
    io::{self, Read, Write},
    fs::File,
    path::Path,
    process,
    rc::Rc,
};

use loxide::{Env, ErrorBundle, Resolver, run, lex, parse};

fn run_file<P: AsRef<Path>>(path: P) -> Result<(), Box<dyn Error>> {
    let mut source = String::new();
    File::open(path)?.read_to_string(&mut source)?;

    let prog = parse(lex(&source))?;

    let mut resolver = Resolver::new();
    let prog = resolver.resolve(prog)?;

    let global = resolver.initialize_env();
    Ok(run(&global, &prog)?)
}

#[inline]
fn run_incremental(resolver: &mut Resolver, env: &Rc<Env>, src: &str
  ) -> Result<(), ErrorBundle> {
    let prog = parse(lex(src))?;
    let prog = resolver.resolve(prog)?;
    resolver.update_env(env);
    Ok(run(env, &prog)?)
}

fn run_prompt() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut any_err = false;
    let mut resolver = Resolver::new();
    let global = resolver.initialize_env();
    loop {
        write!(&mut stdout, "> ")?;
        stdout.flush()?;
        let mut line = String::new();
        match stdin.read_line(&mut line)? {
            0 => break,
            _ => if let Err(es) = run_incremental(
              &mut resolver, &global, line.trim_end()) {
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
