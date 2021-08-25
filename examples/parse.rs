use cfi::parse;

fn main () {
    let cfi_string = "ESVUFX";

    // This example triggers the Ok(res) match.
    match parse(cfi_string) {
        Ok(res) => {
            println!("Parsed CFI: {}", res.to_string()); // "ESVUFR"
            println!("  Category: {}", res.category()); // 'E'
            println!("  Group: {}", res.group()); // 'S'
            println!("  Attributes: {:?}", 
                res.attributes()); // ['V', 'U', 'F', 'X']
        },
        Err(err) => panic!("Unable to parse CFI `{}`: {}.", cfi_string, err),
    }
}
