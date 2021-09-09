#![warn(missing_docs)]

//! # CFI
//! 
//! Provides an [`CFI`] type for working with validated Classification of 
//! Financial Instruments (CFIs) as defined in [ISO 10962:2021 Securities and 
//! related financial instruments — Classification of financial instruments 
//! (CFI) code][1] ("The Standard").
//! 
//! [The Association of National Numbering Agencies (ANNA)][2] has a page 
//! describing [ISO 10962][3].
//! 
//! [Six Group][4] maintains a an [updated list][5] describing all possible 
//! combinations of valid CFI codes.
//! 
//! ## Structure
//! 
//! A CFI code is comprised of 6 ASCII uppercase alphabetic letters with the 
//! following parts, in order:
//! 
//! 1. First letter represent _Category_, out of 14 valid alternatives.
//! 2. Second letter represent _Group_, which is is dependant on Category.
//! 3. Four letters each describing attribute 1-4. These are dependant on both 
//!    Category and Group. For all Category/Group variations there is a total 
//!    of 1229 attributes defined.
//!
//! For example: "___ESVUFR___" refer to a "_Common/ordinary share_" of the 
//! "_Equities_" category with "_Voting (one vote per share)_", "_Free 
//! (unrestricted)_", "_Fully paid_" and "_Registered_" as attributes.
//! 
//! The first two letters together are referred to as the _Asset Class (AC)_
//! within this crate. There are currently 78 distinct asset classes.
//! 
//! Regarding Attributes, according to ANNAs page describing [ISO 10962][3]:
//! > 'X' – "Not applicable/undefined": the character 'X' can be used for any 
//! > respective _Attribute_ if the information is unknown, not available or 
//! > applicable at the time of assignment, regardless of whether it is stated 
//! > as an available character for the Attribute and should be updated to 
//! > reflect the respective Attribute as soon as it is known or available.
//!
//! ## Crate layout
//! 
//! This crate provides a memory efficient struct [`CFI`], 
//! 
//! Use [`parse()`], [`parse_strict()`] to create [`CFI`] objects from `&str`, 
//! which also validates the input data before creation.
//!
//! [`validate()`], [`validate_strict()`] provides &str validation if there is 
//! no need to create a [`CFI`] object. 
//!
//! The _strict() variations requires attributes to be assigned, not 'X'.
//!
//! Lastly it provides human readable description of CFI codes if the feature
//! [`describe`] is enabled.
//! 
//! 
//! Since all valid variations is statically defined this crates makes heavy
//! use of const allocations for its various validations. Rough estimation is
//! that 1.5-3kB. Note that with feature [`describe`] is enabled const 
//! allocations jump by an additional 85-100kB.
//! 
//! ### Feature `describe`
//! 
//! __NOTE: [`describe`] is currently only proof-of-concept and subject to 
//! change at a later date.__
//!
//! (Currently) Provided methods:
//!
//! * [`describe::category()`] -> (Name, Description)
//! * [`describe::group()`] -> (Name, Description)
//! * [`describe::attributes()`] -> [(Letter, Description, Header); 4]
//! * [`describe::print_all()`]
//! 
//! The string data used to describe the various parts of a CFI code has been 
//! deduplicated. There is a total of 2642 individual allocations within the 
//! current set of CFI codes that has been reduced down to 618 const &str in 
//! crate.
//! 
//! ## Usage (Basic)
//! 
//! Add this to your `Cargo.toml`:
//! 
//! ```toml
//! [dependencies]
//! cfi = { version = "0.1", git = "https://github.com/procrustus/cfi-rs" }
//! ```
//! 
//! ### Example
//! 
//! ```rust
//! use cfi::parse;
//!
//! let cfi_string = "ESVUFR";
//! match parse(cfi_string) {
//!     Ok(res) => {
//!         println!("Parsed CFI: {}", res.to_string()); // "ESVUFR"
//!         println!("  Category: {}", res.category()); // 'E'
//!         println!("  Group: {}", res.group()); // 'S'
//!         println!("  Attributes: {:?}", 
//!             res.attributes()); // ['V', 'U', 'F', 'R']
//!     },
//!     Err(err) => panic!("Unable to parse CFI {}: {}", cfi_string, err),
//! }
//! ```
//! 
//! ## Usage (with `describe` feature)
//! 
//! Add this to your `Cargo.toml`:
//! 
//! ```toml
//! [dependencies]
//! cfi = { 
//!   version = "0.1", 
//!   git = "https://github.com/procrustus/cfi-rs", 
//!   feature = ["describe"] 
//! }
//! ```
//! 
//! ### Example (with `describe` feature)
//! 
//! ```rust
//! use cfi::{parse, describe};
//!
//! let cfi_string = "ESVUFR";
//! match parse(cfi_string) {
//!    Ok(res) => {
//!        println!("Parsed CFI: {}", res.to_string()); // "ESVUFR"
//!        println!("  Category:\n    [{}] {}.", 
//!            res.category(), describe::category(&res).0);
//!        println!("  Group:\n    [{}] {}", 
//!            res.group(), describe::group(&res).0);
//!        println!("  Attributes:");
//!        for attr in describe::attributes(&res) {
//!            println!("    [{}] {}.", attr.0, attr.1);
//!        }
//!    },
//!     Err(err) => panic!("Unable to parse CFI {}: {}", cfi_string, err),
//! }
//! ```
//! 
//! ## Related crates
//! 
//! This is heavily inspired by the Financial Identifier series created by 
//! [Gregor Purdy](https://github.com/gnp):
//! 
//! * [CUSIP](https://crates.io/crates/cusip)
//! * [ISIN](https://crates.io/crates/isin)
//! 
//! By me:
//! 
//! * CFI
//! 
//! [1]: https://www.iso.org/standard/81140.html
//! [2]: https://www.anna-web.org/
//! [3]: https://www.anna-web.org/standards/cfi-iso-10962/
//! [4]: https://www.six-group.com/en/home.html
//! [5]: https://www.six-group.com/en/products-services/financial-information/data-standards.html
//! 

use std::fmt::Formatter;
use std::fmt::{Debug, Display};
use std::str::{FromStr, from_utf8_unchecked};

pub(crate) mod internals;
pub mod error;

pub use error::CFIError;

/// A representation of a CFI code.
#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Hash, Debug)]
#[repr(transparent)]
pub struct CFI([u8; 6]);

impl CFI {
    /// Creates a new CFI, ensuring __only__ that input is valid Ascii. Use 
    /// [`parse()`] as CFI initializer to ensure CFI validity.
    pub fn from_utf8(v: [u8; 6]) -> Self {
        assert!(v.iter().all(|x| x & 128 == 0));
        CFI(v)
    }
    /// Creates a new CFI without any checks. 
    /// Use [`parse()`] as initializer to ensure CFI validity.
    /// 
    /// # Safety
    /// 
    /// This method is unsafe as it can cause panics if input data is not valid
    /// UTF-8.
    pub unsafe fn from_utf8_unchecked(v: [u8; 6]) -> Self {
        CFI(v)
    }
    /// Return the CFI as a borrowed string.
    /// 
    /// # Safety
    ///
    /// This method can cause panic __if__ [`CFI`] was created using
    /// [`CFI::from_utf8_unchecked`] passing non-UTF8 `u8`s.
    pub fn as_str(&self) -> &str {
        // SAFETY: This is safe because we know it is ASCII uppercase letters.
        unsafe { from_utf8_unchecked(&self.0[..]) }
    }
    /// Return the _Asset Class_ (two letters) for the CFI.
    pub fn asset_class(&self) -> [char; 2] {
        [self.category(), self.group()]
    }
    /// Return the _Attributes_ (four letters) of the CFI.
    pub fn attributes(&self) -> [char; 4] {
        let mut res: [char; 4] = ['\0'; 4];
        for (i, c) in self.0[2..6].iter().enumerate() {
            res[i] = *c as char
        }
        res
    }
    /// Return the _Category_ code for the CFI.
    pub fn category(&self) -> char {
        self.0[0] as char
    }
    /// Return the _Group_ code for the CFI.
    pub fn group(&self) -> char {
        self.0[1] as char
    }
}

impl Display for CFI {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for CFI {
    type Err = CFIError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse(s)
    }
}

/// Parse `value` to a valid [`CFI`] or [`CFIError`], allowing wildcard 'X' as 
/// attribute.
///
/// Case sensitive, no whitespace is allowed.
/// Allows attributes to be unassigned ('X'), however _if_ assigned they must
/// conform to the specification.
///
/// # Examples
/// ```
/// use cfi;
///
/// let first = cfi::parse("ESXXXX").unwrap();
/// let second = cfi::parse("ESVUFR").unwrap();
///
/// assert_eq!(first, cfi::CFI::from_utf8([b'E', b'S', b'X', b'X', b'X', b'X']));
/// assert_eq!(second, cfi::CFI::from_utf8([b'E', b'S', b'V', b'U', b'F', b'R']));
/// ```
pub fn parse(value: &str) -> Result<CFI, CFIError> {
    internals::parse(value, false)
}

/// Parse `value` to a valid [`CFI`] or [`CFIError`], allowing only defined 
/// attributes.
///
/// Case sensitive, no whitespace is allowed.
/// Attributes can not be unassigned ('X') unless specification say so.
///
/// # Examples
/// ```
/// use cfi;
///
/// let first = cfi::parse_strict("ESXXXX").unwrap_err();
/// let second = cfi::parse_strict("ESVUFR").unwrap();
///
/// assert_eq!(first, cfi::CFIError::InvalidAttribute { was: (1, b'X') });
/// assert_eq!(second, cfi::CFI::from_utf8([b'E', b'S', b'V', b'U', b'F', b'R']));
/// ```
pub fn parse_strict(value: &str) -> Result<CFI, CFIError> {
    internals::parse(value, true)
}

/// Test if `value` is in valid CFI format, w/o producing a [`CFI`], 
/// allowing wildcard 'X' as attribute.
///
/// Case sensitive, no whitespace is allowed.
///
/// # Examples
/// ```
/// use cfi::validate;
///
/// assert_eq!(false, validate("esvufr"));
/// assert_eq!(true, validate("ESXXXX"));
/// assert_eq!(true, validate("ESVUFR"));
/// ```
pub fn validate(value: &str) -> bool {
    internals::validate(value, false)
}

/// Test if `value` is in valid CFI format, w/o producing a [`CFI`], 
/// allowing only defined attributes.
///
/// Case sensitive, no whitespace is allowed.
///
/// # Examples
/// ```
/// use cfi::validate_strict;
///
/// assert_eq!(false, validate_strict("esvufr"));
/// assert_eq!(false, validate_strict("ESXXXX"));
/// assert_eq!(true, validate_strict("ESVUFR"));
/// ```
pub fn validate_strict(value: &str) -> bool {
    internals::validate(value, true)
}

#[cfg(any(feature = "describe", doc))]
pub mod describe {
    //! # Features methods used to derive string descriptions for CFI codes.
    //! 
    //! NOTE: This module is proof-of-concept for now, subject to change at a
    //! later date.

    use crate::CFI;
    use crate::internals::{
        Resolve, 
        str_at, strs::invalid,
        attr_idx, attr_str, attr_header_str,
        attrs_for_ac, refs_for_ac,
    };

    /// Translates [`CFI`] category into (_Name_, _Description_) &str.
    pub fn category(cfi: &CFI) -> (&str, &str) {
        if let Some(refs) = refs_for_ac(cfi.ac()) {
            return (str_at(refs.0.0), str_at(refs.0.1));
        }
        (invalid(), invalid())
    }

    /// Translates [`CFI`] group into (_Name_, _Description_) &str.
    pub fn group(cfi: &CFI) -> (&str, &str) {
        if let Some(refs) = refs_for_ac(cfi.ac()) {
            return (str_at(refs.1.0), str_at(refs.1.1));
        }
        (invalid(), invalid())
    }


    /// Translates [`CFI`] attributes into (_Letter_, _Description_, _Header_) 
    /// for each of the four attributes.
    pub fn attributes(cfi: &CFI) -> [(char, &str, &str); 4] {
        let mut res = [('X', invalid(), invalid()); 4];

        let ac = cfi.ac();
        if let Some(cfi_ref) = refs_for_ac(ac) {
            let attrs = attrs_for_ac(ac);

            for (i, resi) in res.iter_mut().enumerate() {
                let ix = attr_idx(i, cfi.attr(i), attrs);
                *resi = (
                    cfi.attr(i) as char,
                    attr_str(i, ix, cfi_ref),
                    attr_header_str(i, cfi_ref),
                );
            }
        }

        res
    }

    /// Proof-of-concept for describing all the various parts of a [`CFI`].
    pub fn print_all(cfi: &CFI) {
        let ac = cfi.ac();
        if let Some(refs) = refs_for_ac(ac) {
            let attrs = attrs_for_ac(ac);
            println!("Category: {}\n\t{}", str_at(refs.0.0), str_at(refs.0.1));
            println!("Group: {}\n\t{}", str_at(refs.1.0), str_at(refs.1.1));

            println!("Attributes:");
            for i in 0..4 {
                let attr = attr_str(i, attr_idx(i, cfi.attr(i), attrs), refs);
                println!("\t{}", attr_header_str(i, refs));
                println!("\t\t[{}]: {}", cfi.attr(i) as char, attr);
            }
        }
    }


}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_strict() {
        let cfi_lower = parse_strict("esvufr");
        let cfi_upper = parse_strict("ESVUFR");
        let esvufr = CFI([b'E', b'S', b'V', b'U', b'F', b'R']);

        assert_eq!(cfi_lower.unwrap_err(), CFIError::InvalidChar { was: b'e' });
        assert_eq!(cfi_upper.unwrap(), esvufr);

        assert!(!validate_strict("esvufr"));
        assert!(!validate("esvufr"));
        assert!(validate_strict("ESVUFR"));
        assert!(validate("ESVUFR"));
    }

    #[test]
    fn valid_with_attr_x() {
        let cfi_a1 = parse("ESXUFR");
        let cfi_a2 = parse("ESVXFR");
        let cfi_a3 = parse("ESVUXR");
        let cfi_a4 = parse("ESVUFX");

        assert_eq!(cfi_a1.unwrap(), CFI([b'E', b'S', b'X', b'U', b'F', b'R']));
        assert_eq!(cfi_a2.unwrap(), CFI([b'E', b'S', b'V', b'X', b'F', b'R']));
        assert_eq!(cfi_a3.unwrap(), CFI([b'E', b'S', b'V', b'U', b'X', b'R']));
        assert_eq!(cfi_a4.unwrap(), CFI([b'E', b'S', b'V', b'U', b'F', b'X']));

        assert!(!validate_strict("ESXUFR"));
        assert!(validate("ESXUFR"));
        assert!(!validate_strict("ESVXFR"));
        assert!(validate("ESVXFR"));
        assert!(!validate_strict("ESVUXR"));
        assert!(validate("ESVUXR"));
        assert!(!validate_strict("ESVUFX"));
        assert!(validate("ESVUFX"));
    }

    #[test]
    fn invalid_with_non_ascii_str() {
        let cfi_lower = parse_strict("ésvufr");
        let cfi_upper = parse_strict("ÉSVUFR");

        // NOTE: "É".as_bytes() = [195u8, 135u8]

        assert_eq!(cfi_lower.unwrap_err(), CFIError::InvalidChar { 
            was: 195u8 });
        assert_eq!(cfi_upper.unwrap_err(), CFIError::InvalidChar { 
            was: 195u8 });
        
        assert!(!validate_strict("ésvufr"));
        assert!(!validate("ésvufr"));
        assert!(!validate_strict("ÉSVUFR"));
        assert!(!validate("ÉSVUFR"));
        }

    #[test]
    fn invalid_length() {
        let cfi_short = parse_strict("ESVUF");
        let cfi_long = parse_strict("ESVUFRX");

        assert_eq!(cfi_short.unwrap_err(), CFIError::InvalidLength { was: 5 });
        assert_eq!(cfi_long.unwrap_err(), CFIError::InvalidLength { was: 7 });

        assert!(!validate_strict("ESVUF"));
        assert!(!validate("ESVUF"));
        assert!(!validate_strict("ESVUFRX"));
        assert!(!validate("ESVUFRX"));
    }

    #[test]
    fn invalid_attributes() {
        let cfi_a1 = parse_strict("ESZUFR");
        let cfi_a2 = parse_strict("ESVZFR");
        let cfi_a3 = parse_strict("ESVUZR");
        let cfi_a4 = parse_strict("ESVUFZ");

        assert_eq!(cfi_a1.unwrap_err(), CFIError::InvalidAttribute { 
            was: (1, b'Z') });
        assert_eq!(cfi_a2.unwrap_err(), CFIError::InvalidAttribute { 
            was: (2, b'Z') });
        assert_eq!(cfi_a3.unwrap_err(), CFIError::InvalidAttribute { 
            was: (3, b'Z') });
        assert_eq!(cfi_a4.unwrap_err(), CFIError::InvalidAttribute { 
            was: (4, b'Z') });

        assert!(!validate_strict("ESZUFR"));
        assert!(!validate("ESZUFR"));
        assert!(!validate_strict("ESVZFR"));
        assert!(!validate("ESVZFR"));
        assert!(!validate_strict("ESVUZR"));
        assert!(!validate("ESVUZR"));
        assert!(!validate_strict("ESVUFZ"));
        assert!(!validate("ESVUFZ"));
    }

    // NOTE: CFIError::InvalidAttributeIndex should not be accessible by users,
    // thus not tested.
}
