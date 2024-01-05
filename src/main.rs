fn main() {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).unwrap();
    println!("{:#?}", lispy::eval(&lispy::parse(input.trim()).unwrap()));
}
