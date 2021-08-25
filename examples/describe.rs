// This example requires describe feature to be enabled for cfi.
// 
// Add this to Cargo.toml
// [dependencies]
// cfi = { version = "0.1", feature = ["describe"] }

use cfi::{parse, describe};

fn main() {
    let cfi_string = "ESVUFR";

    match parse(cfi_string) {
        Ok(res) => {
            println!("Parsed CFI: {}", res.to_string()); // "ESVUFR"
            // describe::category().0 == category name
            println!("  Category:\n    [{}] {}.", 
                res.category(), describe::category(&res).0);
            // describe::group().0 == group name
            println!("  Group:\n    [{}] {}", 
                res.group(), describe::group(&res).0);
            println!("  Attributes:");
            for attr in describe::attributes(&res) {
                // describe::attributes().0 == attribute letter
                // describe::attributes().1 == attribute description
                println!("    [{}] {}.", attr.0, attr.1);
            }
        },
        Err(err) => panic!("Unable to parse CFI `{}`: {}.", cfi_string, err),
    }
}