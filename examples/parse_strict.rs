use cfi::parse_strict;

fn main () {
    let cfi_string = "ESVUFX";

    // This example triggers the Err(err) match.
    match parse_strict(cfi_string) {
        Ok(res) => {
            println!("Parsed CFI: {}", res.to_string());
            println!("  Category: {}", res.category());
            println!("  Group: {}", res.group());
            println!("  Attributes: {:?}", res.attributes());
        },
        Err(err) => println!("Unable to parse CFI `{}`: {}.", cfi_string, err),
    }
}
