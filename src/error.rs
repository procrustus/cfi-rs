#![warn(missing_docs)]
//! Contains CFIError, an error type for CFI parsing and building.

use std::error::Error;
use std::fmt::Formatter;
use std::fmt::{Debug, Display};

/// Contains all the ways parsing or building could fail.
#[non_exhaustive]
#[derive(Clone, PartialEq, Eq)]
pub enum CFIError {
    /// The input character was not an uppercase ascii letter.
    InvalidChar {
        /// The _Char_ we found
        was: u8
    },
    /// The input length is not exactly 6 bytes.
    InvalidLength {
        /// The length we found
        was: usize,
    },
    /// The input _Category_ is not a valid Category.
    InvalidCategory {
        /// The _Category_ we found
        was: u8,
    },
    /// The input _Asset Class_ is not a valid Asset Class.
    InvalidAssetClass {
        /// The _Asset Class_ we found
        was: [u8; 2],
    },
    /// The input _Attribute_ is not a valid Attribute.
    InvalidAttribute {
        /// The _Attribute_ we found, (number, value)
        was: (usize, u8),
    },
    /// The input _AttributeIndex_ is not a valid Index.
    InvalidAttributeIndex {
        /// The _AttributeIndex_ we found
        was: usize,
    },
}

impl Debug for CFIError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CFIError::InvalidChar { was } => {
                write!(f, "InvalidChar {{ was: {:?} }}", was)
            },
            CFIError::InvalidLength { was } => {
                write!(f, "InvalidLength {{ was: {:?} }}", was)
            },
            CFIError::InvalidCategory { was } => {
                write!(f, "InvalidCategory {{ was: {:?} }}", was)
            },
            CFIError::InvalidAssetClass { was } => {
                write!(f, "InvalidGroup {{ was: {:?} }}", was)
            },
            CFIError::InvalidAttribute { was } => {
                write!(f, "InvalidAttribute {{ was: {:?} }}", was)
            },
            CFIError::InvalidAttributeIndex { was } => {
                write!(f, "InvalidAttributeIndex {{ was: {:?} }}", was)
            },
        }
    }
}

// Display for CFIError converts all u8 into chars to make it more humanly 
// readable.

impl Display for CFIError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CFIError::InvalidChar { was } => {
                write!(f, "non ascii letter `{}`", *was as char)
            },
            CFIError::InvalidLength { was } => {
                write!(f, "invalid length {} bytes when expecting 12", was)
            },
            CFIError::InvalidCategory { was } => {
                write!(f, "unknown category `{}`", *was as char)
            },
            CFIError::InvalidAssetClass { was } => {
                write!(f, "unknown asset class `{:?}`", 
                    [was[0] as char, was[1] as char])
            },
            CFIError::InvalidAttribute { was } => {
                write!(f, "unknown attribute `{}` at {}", was.1 as char, was.0)
            },
            CFIError::InvalidAttributeIndex { was } => {
                write!(f, "attribute index {} outside of range [1-4]", was)
            },
        }
    }
}

impl Error for CFIError {}