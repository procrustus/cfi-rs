cfi
===
A memory efficient `CFI` type for working with validated Classification of 
Financial Instruments (CFIs) as defined in [ISO 10962](https://www.iso.org/standard/81140.html).

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
cfi = { version = "0.1", git = "https://github.com/procrustus/cfi-rs" }
```


## Example

```rust
use cfi;

let cfi_string = "ESVUFR";

match cfi::parse(cfi_string) {
    Ok(code) => {
        println!("Parsed CFI: {}", code.to_string()); // "ESVUFR"
        println!("  Category: {}", code.category()); // 'E'
        println!("  Group: {}", code.group()); // 'S'
        println!("  Attributes: {:?}", code.attributes()); // ['V', 'U', 'F', 'R']
    }
    Err(err) => panic!("Unable to parse CFI `{}`: {}.", cfi_string, err),
}
```

## Related crates

This is heavily inspired by the Financial Identifier series created by 
[Gregor Purdy](https://github.com/gnp):

* [CUSIP](https://crates.io/crates/cusip)
* [ISIN](https://crates.io/crates/isin)

By me:

* CFI

## License

Licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.


## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
