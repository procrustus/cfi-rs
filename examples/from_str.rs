// This example makes use of [`std::str::FromStr`] to enable &str.parse() into
// CFI object.

use cfi::CFI;

fn main() {
    let res: CFI = "ESVUFR".parse().unwrap();
    println!("Parsed CFI: {}", res.to_string()); // "ESVUFR"
    println!("  Category: {}", res.category()); // 'E'
    println!("  Group: {}", res.group()); // 'S'
    println!("  Attributes: {:?}", res.attributes()); // ['V', 'U', 'F', 'R']
}
