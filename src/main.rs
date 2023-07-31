
mod chunk;
mod utils;
mod value;
mod vm;
mod lexer;

fn main() {
    // let file = File::open("test/sample.lox").unwrap();
    // let buffer = BufReader::new(file);

    // let lines = buffer
    //     .lines()
    //     .filter_map(|line| line.ok())
    //     .enumerate()
    //     .collect::<Vec<_>>();

    // for ((line_no, col), line) in lines
    //     .iter()
    //     .flat_map(|(line_no, line)| line.char_indices().map(|(col, c)| ((*line_no, col), c)))
    // {
    //     println!("[{:5}:{:3}] {line}", line_no + 1, col + 1);
    // }
}
