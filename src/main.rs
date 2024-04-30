use monkey_lang::REPL;
use std::thread;

fn main() {
    let stack_size = 16 * 1024 * 1024; // 16MB
    let builder = thread::Builder::new().stack_size(stack_size);

    let handle = builder
        .spawn(|| {
            REPL::run();
        })
        .expect("failed to create a thread for the REPL");

    handle.join().expect("REPL thread has panicked");
}
