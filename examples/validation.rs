use cfi::{validate, validate_strict};

fn main() {
    let first = "esvufr";
    let second = "ESXXXX";
    let third = "ESVUFR";

    // Fails due to lower case letters.
    assert!(!validate(first));
    // Succeeds since 'X' – "Not applicable/undefined" is allowed by default.
    assert!(validate(second));
    // Succeeds since it all attributes are correct and known for "ES".
    assert!(validate(third));
    
    // Fails due to lower case.
    assert!(!validate_strict(first));
    // Fails since "ES" does not have 'X' defined in specification.
    assert!(!validate_strict(second));
    // Succeeds since it all attributes are correct and known for "ES".
    assert!(validate_strict(third));
}