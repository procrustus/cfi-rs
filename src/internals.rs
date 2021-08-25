//! # Internals
//! 
//! Internals contain lookup tables, methods and such for validating and 
//! describing CFI codes.
//! 
//! ## CFI codes
//! 
//! A CFI code consists of 6 ASCII letters where the first letter signify
//! category and the second letter signify group related to the category.
//! 
//! The first two chars together can be referred to as a _Asset Class (Ac)_.
//! 
//! The other four letters _each_ represent an attribute which is dependant 
//! on the CFI asset class.
//! 
//! To make the logic of the data structures used more visible, type aliases
//! are used to represent the various components.

use std::{convert::TryInto};

use crate::{CFI, CFIError};

/// Internal trait for accessing the various components of the CFI code.
pub(crate) trait Resolve {
    fn cat(&self) -> u8;
    fn grp(&self) -> u8;
    fn ac(&self) -> &[u8; 2];
    fn attr(&self, i: usize) -> u8;
}

impl Resolve for CFI {
    /// Returns _raw_ CFI Category.
    fn cat(&self) -> u8 {
        self.0[0]
    }
    /// Returns _raw_ CFI Group.
    fn grp(&self) -> u8 {
        self.0[1]
    }
    /// Returns _raw_ CFI Asset Class.
    fn ac(&self) -> &[u8; 2] {
        self.0[0..2]
            .try_into()
            .expect("asset class could not be resolved.")
    }
    /// Returns the `i`-th _raw_ CFI Attribute.
    /// 
    /// `i` must be in the interval [0..=3].
    fn attr(&self, i: usize) -> u8 {
        if i < 4 {
            return self.0[i+2];
        }
        0
    }
}

/// [`Ac`] refers to the two first chars of a CFI. Basically = Asset Class.
pub(crate) type Ac<'a> = &'a [u8; 2];

/// [`Attrs`] refers to an _arbitrary length_ `&[u8]` representing all valid 
/// (ASCII) letters each of the four attributes can yield a for a specific CFI.
pub(crate) type Attrs = &'static [&'static [u8]; 4]; // GrpAttrs

/// [`Ix`] refers to an index. Used both to access string data and "translate"
/// [`Attrs`] index to [`AttrDescRefs`] index.
pub(crate) type Ix = usize;

// ------------------------------------------------------------------------- //
// Ixs, Nd, AttrHdrDescRefs, AttrDescRefs & AcRefs are all type-aliases only //
// to be used when _describe_ feature-gate is enabled.                       //
// ------------------------------------------------------------------------- //

#[cfg(any(feature = "describe", doc))]
/// [`Ixs`] refers to an _arbitrary length_ `&[usize]` representing the indices 
/// for all valid _Attributes_ defined in the [`Ac`]'s [`Attrs`]. 
pub(crate) type Ixs = &'static [Ix];

#[cfg(any(feature = "describe", doc))]
/// [`Nd`] refers to a pair of string indices representing a _(Name, Description)_
/// combination. This is used for both Category and Group data.
pub(crate) type Nd = (Ix, Ix);

#[cfg(any(feature = "describe", doc))]
/// [`AttrHdrDescRefs`] refers to the description string indices of the 
/// _header_ for each of the four attribute groupings associated to the CFI's 
/// [`Ac`].
pub(crate) type AttrHdrDescRefs = &'static [Ix; 4];

#[cfg(any(feature = "describe", doc))]
/// [`AttrDescRefs`] refers to description string indices for each of the 
/// attribute ASCII letters defined in the CFI's [`Attrs`].
pub(crate) type AttrDescRefs = &'static [Ixs; 4];

#[cfg(any(feature = "describe", doc))]
/// [`AcRefs`] is an amalgation of _Category_ [`Nd`], _Group_ [`Nd`], 
/// [`AttrHdrDescRefs`] and [`AttrDescRefs`] for each valid [`Ac`].
pub(crate) type AcRefs = &'static (Nd, Nd, AttrHdrDescRefs, AttrDescRefs);


/// Parse a string to a valid CFI or an error message. Case sensitive. No 
/// whitespace is allowed.
pub(crate) fn parse(value: &str, strict: bool) -> Result<CFI, CFIError> {
    let v = value.as_bytes();
    
    validate_letters(v)?;

    // validate_len might yield incorrect result if validate_letters succeeded.
    validate_len(v)?;

    validate_cat(v[0])?;

    let ac = v[0..2].try_into().expect("asset class init failed");
    validate_ac(ac)?;

    let attrs = attrs_for_ac(ac);
    for (i, &c) in v[2..6].iter().enumerate() {
        validate_attr(i, c, attrs, strict)?;
    }

    Ok(CFI(v.try_into().expect("asset class init failed")))
}

/// Test whether or not the passed string is in valid CFI format. Case 
/// sensitive. No whitespace is allowed.
pub(crate) fn validate(value: &str, strict: bool) -> bool {
    let v = value.as_bytes();

    if validate_letters(v).is_err() || 
        validate_len(v).is_err() || 
        validate_cat(v[0]).is_err() 
    {
        return false;
    }

    let ac = v[0..2].try_into().expect("asset class init failed");

    if validate_ac(ac).is_err() {
        return false;
    }

    let attrs = attrs_for_ac(ac);

    for (i, c) in v[2..6].iter().enumerate() {
        if validate_attr(i,*c,attrs, strict).is_err() {
            return false;
        }
    }

    true
}

/// Validates input chars all are uppercase ASCII letters.
pub(crate) fn validate_letters(v: &[u8]) -> Result<(), CFIError>  {
    for c in v {
        // (65..=91) => 'A'..='Z'.
        if !(65..=91).contains(c) {
            return Err(CFIError::InvalidChar { was: *c });
        }
    }

    Ok(())
}

/// Validates string length requirement.
pub(crate) fn validate_len(s: &[u8]) -> Result<(), CFIError>  {
    // If valid Ascii letters, each char should not use more than one byte.
    if s.len() == 6 {
        return Ok(());
    }
    Err(CFIError::InvalidLength { was: s.len() })
}

/// Validates category.
pub(crate) fn validate_cat(c: u8) -> Result<(), CFIError> {
    if cats::CATEGORIES.contains(&c) {
        return Ok(());
    }
    Err(CFIError::InvalidCategory { was: c })
}

/// Validates asset class belonging.
pub(crate) fn validate_ac(ac: Ac) -> Result<(), CFIError> {
    let acs = acs_for_cat(ac[0]);
    if acs.contains(&ac) {
        return Ok(());
    }
    Err(CFIError::InvalidAssetClass { was: *ac })
}

/// Validates that `c` is part of `attrs`.
pub(crate) fn validate_attr(i: Ix, c: u8, attrs: Attrs, strict: bool) 
    -> Result<(), CFIError> 
{
    if !(0..4).contains(&i) {
        return Err(CFIError::InvalidAttributeIndex { was: i });
    }

    let m_strict  = || { attrs[i].contains(&c) };
    let m_loose = || { c == b'X' || attrs[i].contains(&c) };

    if strict {
        if m_strict() { return Ok(());}
    } else if m_loose() { return Ok(());}

    Err(CFIError::InvalidAttribute { was: (i + 1, c) })
}

/// Resolves valid asset classes for category `cat`.
pub(crate) fn acs_for_cat(cat: u8) -> &'static [Ac<'static>] {
    match cat {
        cats::E => { acs::E_AC },
        cats::C => { acs::C_AC },
        cats::D => { acs::D_AC },
        cats::R => { acs::R_AC },
        cats::O => { acs::O_AC },
        cats::F => { acs::F_AC },
        cats::S => { acs::S_AC },
        cats::H => { acs::H_AC },
        cats::I => { acs::I_AC },
        cats::J => { acs::J_AC },
        cats::K => { acs::K_AC },
        cats::L => { acs::L_AC },
        cats::T => { acs::T_AC },
        cats::M => { acs::M_AC },
        _ => { &[] },
    }
}

/// Resolves group of valid attributes for asset class `ac`.
pub(crate) fn attrs_for_ac(ac: Ac) -> Attrs {
    match ac {
        acs::ES => { attrs::ES_ATTRS },
        acs::EP => { attrs::EP_ATTRS },
        acs::EC => { attrs::EC_ATTRS },
        acs::EF => { attrs::EF_ATTRS },
        acs::EL => { attrs::EL_ATTRS },
        acs::ED => { attrs::ED_ATTRS },
        acs::EY => { attrs::EY_ATTRS },
        acs::EM => { attrs::EM_ATTRS },
        acs::CI => { attrs::CI_ATTRS },
        acs::CH => { attrs::CH_ATTRS },
        acs::CB => { attrs::CB_ATTRS },
        acs::CE => { attrs::CE_ATTRS },
        acs::CS => { attrs::CS_ATTRS },
        acs::CF => { attrs::CF_ATTRS },
        acs::CP => { attrs::CP_ATTRS },
        acs::CM => { attrs::CM_ATTRS },
        acs::DB => { attrs::DB_ATTRS },
        acs::DC => { attrs::DC_ATTRS },
        acs::DW => { attrs::DW_ATTRS },
        acs::DT => { attrs::DT_ATTRS },
        acs::DY => { attrs::DY_ATTRS },
        acs::DS => { attrs::DS_ATTRS },
        acs::DE => { attrs::DE_ATTRS },
        acs::DG => { attrs::DG_ATTRS },
        acs::DA => { attrs::DA_ATTRS },
        acs::DN => { attrs::DN_ATTRS },
        acs::DD => { attrs::DD_ATTRS },
        acs::DM => { attrs::DM_ATTRS },
        acs::RA => { attrs::RA_ATTRS },
        acs::RS => { attrs::RS_ATTRS },
        acs::RP => { attrs::RP_ATTRS },
        acs::RW => { attrs::RW_ATTRS },
        acs::RF => { attrs::RF_ATTRS },
        acs::RD => { attrs::RD_ATTRS },
        acs::RM => { attrs::RM_ATTRS },
        acs::OC => { attrs::OC_ATTRS },
        acs::OP => { attrs::OP_ATTRS },
        acs::OM => { attrs::OM_ATTRS },
        acs::FF => { attrs::FF_ATTRS },
        acs::FC => { attrs::FC_ATTRS },
        acs::SR => { attrs::SR_ATTRS },
        acs::ST => { attrs::ST_ATTRS },
        acs::SE => { attrs::SE_ATTRS },
        acs::SC => { attrs::SC_ATTRS },
        acs::SF => { attrs::SF_ATTRS },
        acs::SM => { attrs::SM_ATTRS },
        acs::HR => { attrs::HR_ATTRS },
        acs::HT => { attrs::HT_ATTRS },
        acs::HE => { attrs::HE_ATTRS },
        acs::HC => { attrs::HC_ATTRS },
        acs::HF => { attrs::HF_ATTRS },
        acs::HM => { attrs::HM_ATTRS },
        acs::IF => { attrs::IF_ATTRS },
        acs::IT => { attrs::IT_ATTRS },
        acs::JE => { attrs::JE_ATTRS },
        acs::JF => { attrs::JF_ATTRS },
        acs::JC => { attrs::JC_ATTRS },
        acs::JR => { attrs::JR_ATTRS },
        acs::JT => { attrs::JT_ATTRS },
        acs::KR => { attrs::KR_ATTRS },
        acs::KT => { attrs::KT_ATTRS },
        acs::KE => { attrs::KE_ATTRS },
        acs::KC => { attrs::KC_ATTRS },
        acs::KF => { attrs::KF_ATTRS },
        acs::KY => { attrs::KY_ATTRS },
        acs::KM => { attrs::KM_ATTRS },
        acs::LL => { attrs::LL_ATTRS },
        acs::LR => { attrs::LR_ATTRS },
        acs::LS => { attrs::LS_ATTRS },
        acs::TC => { attrs::TC_ATTRS },
        acs::TT => { attrs::TT_ATTRS },
        acs::TR => { attrs::TR_ATTRS },
        acs::TI => { attrs::TI_ATTRS },
        acs::TB => { attrs::TB_ATTRS },
        acs::TD => { attrs::TD_ATTRS },
        acs::TM => { attrs::TM_ATTRS },
        acs::MC => { attrs::MC_ATTRS },
        acs::MM => { attrs::MM_ATTRS },
        _ => { &[&[], &[], &[], &[]]}
    }
}

#[cfg(any(feature = "describe", doc))]
/// Resolves index for the letter `c` under the `i`th attribute in `attrs`.
pub(crate) fn attr_idx(i: Ix, c: u8, attrs: Attrs) -> Option<Ix> {
    if i > 3 { return None; }

    attrs[i].iter().position(|x| *x == c)
}

#[cfg(any(feature = "describe", doc))]
/// Resolves description &str for `ix` as the `i`th attribute.
pub(crate) fn attr_str(i: Ix, ix: Option<Ix>, refs: AcRefs) -> &'static str {
    if i > 3 { return strs::invalid(); }

    if let Some(ix) = ix {
        return str_at(refs.3[i][ix])
    }
    strs::na()
}

#[cfg(any(feature = "describe", doc))]
/// Resolves the header &str for the `i`th attribute.
pub(crate) fn attr_header_str(i: Ix, refs: AcRefs) -> &'static str {
    assert!(i < 4, "`i` can not be greater than 4!");
    str_at(refs.2[i])
}

#[cfg(any(feature = "describe", doc))]
/// Convenience method to access the string resources.
pub(crate) fn str_at(ix: Ix) -> &'static str{
    if ix >= strs::DATA.len() {
        return strs::invalid();
    }
    strs::DATA[ix]
}

#[cfg(any(feature = "describe", doc))]
/// Resolves struct of indices for string reference data assigned to `ac`. 
pub(crate) fn refs_for_ac(ac: Ac) -> Option<AcRefs> {
    match ac {
        acs::ES => { Some(refs::ES_REF) },
        acs::EP => { Some(refs::EP_REF) },
        acs::EC => { Some(refs::EC_REF) },
        acs::EF => { Some(refs::EF_REF) },
        acs::EL => { Some(refs::EL_REF) },
        acs::ED => { Some(refs::ED_REF) },
        acs::EY => { Some(refs::EY_REF) },
        acs::EM => { Some(refs::EM_REF) },
        acs::CI => { Some(refs::CI_REF) },
        acs::CH => { Some(refs::CH_REF) },
        acs::CB => { Some(refs::CB_REF) },
        acs::CE => { Some(refs::CE_REF) },
        acs::CS => { Some(refs::CS_REF) },
        acs::CF => { Some(refs::CF_REF) },
        acs::CP => { Some(refs::CP_REF) },
        acs::CM => { Some(refs::CM_REF) },
        acs::DB => { Some(refs::DB_REF) },
        acs::DC => { Some(refs::DC_REF) },
        acs::DW => { Some(refs::DW_REF) },
        acs::DT => { Some(refs::DT_REF) },
        acs::DY => { Some(refs::DY_REF) },
        acs::DS => { Some(refs::DS_REF) },
        acs::DE => { Some(refs::DE_REF) },
        acs::DG => { Some(refs::DG_REF) },
        acs::DA => { Some(refs::DA_REF) },
        acs::DN => { Some(refs::DN_REF) },
        acs::DD => { Some(refs::DD_REF) },
        acs::DM => { Some(refs::DM_REF) },
        acs::RA => { Some(refs::RA_REF) },
        acs::RS => { Some(refs::RS_REF) },
        acs::RP => { Some(refs::RP_REF) },
        acs::RW => { Some(refs::RW_REF) },
        acs::RF => { Some(refs::RF_REF) },
        acs::RD => { Some(refs::RD_REF) },
        acs::RM => { Some(refs::RM_REF) },
        acs::OC => { Some(refs::OC_REF) },
        acs::OP => { Some(refs::OP_REF) },
        acs::OM => { Some(refs::OM_REF) },
        acs::FF => { Some(refs::FF_REF) },
        acs::FC => { Some(refs::FC_REF) },
        acs::SR => { Some(refs::SR_REF) },
        acs::ST => { Some(refs::ST_REF) },
        acs::SE => { Some(refs::SE_REF) },
        acs::SC => { Some(refs::SC_REF) },
        acs::SF => { Some(refs::SF_REF) },
        acs::SM => { Some(refs::SM_REF) },
        acs::HR => { Some(refs::HR_REF) },
        acs::HT => { Some(refs::HT_REF) },
        acs::HE => { Some(refs::HE_REF) },
        acs::HC => { Some(refs::HC_REF) },
        acs::HF => { Some(refs::HF_REF) },
        acs::HM => { Some(refs::HM_REF) },
        acs::IF => { Some(refs::IF_REF) },
        acs::IT => { Some(refs::IT_REF) },
        acs::JE => { Some(refs::JE_REF) },
        acs::JF => { Some(refs::JF_REF) },
        acs::JC => { Some(refs::JC_REF) },
        acs::JR => { Some(refs::JR_REF) },
        acs::JT => { Some(refs::JT_REF) },
        acs::KR => { Some(refs::KR_REF) },
        acs::KT => { Some(refs::KT_REF) },
        acs::KE => { Some(refs::KE_REF) },
        acs::KC => { Some(refs::KC_REF) },
        acs::KF => { Some(refs::KF_REF) },
        acs::KY => { Some(refs::KY_REF) },
        acs::KM => { Some(refs::KM_REF) },
        acs::LL => { Some(refs::LL_REF) },
        acs::LR => { Some(refs::LR_REF) },
        acs::LS => { Some(refs::LS_REF) },
        acs::TC => { Some(refs::TC_REF) },
        acs::TT => { Some(refs::TT_REF) },
        acs::TR => { Some(refs::TR_REF) },
        acs::TI => { Some(refs::TI_REF) },
        acs::TB => { Some(refs::TB_REF) },
        acs::TD => { Some(refs::TD_REF) },
        acs::TM => { Some(refs::TM_REF) },
        acs::MC => { Some(refs::MC_REF) },
        acs::MM => { Some(refs::MM_REF) },
        _ => { None }
    }
}

pub mod cats {
    //! Defines and scopes the categories available in [`crate::CFI`]'s.

    /// All available categories.
    pub const CATEGORIES: &[u8] = &[E, C, D, R, O, F, S, H, I, J, K, L, T, M];

    /// Category for _Equities_.
    pub const E: u8 = b'E';
    /// Category for _CIVs (Collective Investment Vehicles)_.
    pub const C: u8 = b'C';
    /// Category for _Debt instruments_.
    pub const D: u8 = b'D';
    /// Category for _Entitlement (rights)_.
    pub const R: u8 = b'R';
    /// Category for _Listed options_.
    pub const O: u8 = b'O';
    /// Category for _Futures_.
    pub const F: u8 = b'F';
    /// Category for _Swaps_.
    pub const S: u8 = b'S';
    /// Category for _Non-listed and complex listed options_.
    pub const H: u8 = b'H';
    /// Category for _Spot_.
    pub const I: u8 = b'I';
    /// Category for _Forwards_.
    pub const J: u8 = b'J';
    /// Category for _Strategies_.
    pub const K: u8 = b'K';
    /// Category for _Financing_.
    pub const L: u8 = b'L';
    /// Category for _Referential instruments_.
    pub const T: u8 = b'T';
    /// Category for _Others (miscellaneous)_.
    pub const M: u8 = b'M';
}

pub mod acs {
    //! Defines and scopes the asset classes available in [`crate::CFI`]'s.

    use super::Ac;

    /// All groups available for _Equities_.
    pub const E_AC: &[Ac] = &[ES, EP, EC, EF, EL, ED, EY, EM];
    /// Category for _CIVs (Collective Investment Vehicles)_.
    pub const C_AC: &[Ac] = &[CI, CH, CB, CE, CS, CF, CP, CM];
    /// All groups available for _Debt instruments_.
    pub const D_AC: &[Ac] = &[
        DB, DC, DW, DT, DY, DS, DE, DG, DA, DN, DD, DM
    ];
    /// All groups available for _Entitlement (rights)_.
    pub const R_AC: &[Ac] = &[RA, RS, RP, RW, RF, RD, RM];
    /// All groups available for _Listed options_.
    pub const O_AC: &[Ac] = &[OC, OP, OM];
    /// All groups available for _Futures_.
    pub const F_AC: &[Ac] = &[FF, FC];
    /// All groups available for _Swaps_.
    pub const S_AC: &[Ac] = &[SR, ST, SE, SC, SF, SM];
    /// All groups available for _Non-listed and complex listed options_.
    pub const H_AC: &[Ac] = &[HR, HT, HE, HC, HF, HM];
    /// All groups available for _Spot_.
    pub const I_AC: &[Ac] = &[IF, IT];
    /// All groups available for _Forwards_.
    pub const J_AC: &[Ac] = &[JE, JF, JC, JR, JT];
    /// All groups available for _Strategies_.
    pub const K_AC: &[Ac] = &[KR, KT, KE, KC, KF, KY, KM];
    /// All groups available for _Financing_.
    pub const L_AC: &[Ac] = &[LL, LR, LS];
    /// All groups available for _Referential instruments_.
    pub const T_AC: &[Ac] = &[TC, TT, TR, TI, TB, TD, TM];
    /// All groups available for _Others (miscellaneous)_.
    pub const M_AC: &[Ac] = &[MC, MM];

    /// Group _Equities_ :: _Common/ordinary shares_.
    pub const ES: Ac = &[b'E', b'S'];
    /// Group _Equities_ :: _Preferred/preference shares_.
    pub const EP: Ac = &[b'E', b'P'];
    /// Group _Equities_ :: _Common/ordinary convertible shares_.
    pub const EC: Ac = &[b'E', b'C'];
    /// Group _Equities_ :: _Preferred/preference convertible shares_.
    pub const EF: Ac = &[b'E', b'F'];
    /// Group _Equities_ :: _Limited partnership units_.
    pub const EL: Ac = &[b'E', b'L'];
    /// Group _Equities_ :: _Depositary receipts on equities_.
    pub const ED: Ac = &[b'E', b'D'];
    /// Group _Equities_ :: _Structured instruments (participation)_.
    pub const EY: Ac = &[b'E', b'Y'];
    /// Group _Equities_ :: _Others (miscellaneous)_.
    pub const EM: Ac = &[b'E', b'M'];
    /// Group _CIVs_ :: _Standard (vanilla) investment funds/mutual funds_.
    pub const CI: Ac = &[b'C', b'I'];
    /// Group _CIVs_ :: _Hedge funds_.
    pub const CH: Ac = &[b'C', b'H'];
    /// Group _CIVs_ :: _Real estate investment trust (REITs)_.
    pub const CB: Ac = &[b'C', b'B'];
    /// Group _CIVs_ :: _Exchange traded funds (ETFs)_.
    pub const CE: Ac = &[b'C', b'E'];
    /// Group _CIVs_ :: _Pension funds_.
    pub const CS: Ac = &[b'C', b'S'];
    /// Group _CIVs_ :: _Funds of funds_.
    pub const CF: Ac = &[b'C', b'F'];
    /// Group _CIVs_ :: _Private equity funds_.
    pub const CP: Ac = &[b'C', b'P'];
    /// Group _CIVs_ :: _Others (miscellaneous)_.
    pub const CM: Ac = &[b'C', b'M'];
    /// Group _Debt instruments_ :: _Bonds_.
    pub const DB: Ac = &[b'D', b'B'];
    /// Group _Debt instruments_ :: _Convertible bonds_.
    pub const DC: Ac = &[b'D', b'C'];
    /// Group _Debt instruments_ :: _Bonds with warrants attached_.
    pub const DW: Ac = &[b'D', b'W'];
    /// Group _Debt instruments_ :: _Medium-term notes_.
    pub const DT: Ac = &[b'D', b'T'];
    /// Group _Debt instruments_ :: _Money market instruments_.
    pub const DY: Ac = &[b'D', b'Y'];
    /// Group _Debt instruments_ :: _Structured instruments (capital protection)_.
    pub const DS: Ac = &[b'D', b'S'];
    /// Group _Debt instruments_ :: _Structured instruments (without capital protection)_.
    pub const DE: Ac = &[b'D', b'E'];
    /// Group _Debt instruments_ :: _Mortgage-backed securities_.
    pub const DG: Ac = &[b'D', b'G'];
    /// Group _Debt instruments_ :: _Asset-backed securities_.
    pub const DA: Ac = &[b'D', b'A'];
    /// Group _Debt instruments_ :: _Municipal bonds_.
    pub const DN: Ac = &[b'D', b'N'];
    /// Group _Debt instruments_ :: _Depositary receipts on debt instruments_.
    pub const DD: Ac = &[b'D', b'D'];
    /// Group _Debt instruments_ :: _Others (miscellaneous)_.
    pub const DM: Ac = &[b'D', b'M'];
    /// Group _Entitlement (rights)_ :: _Allotment (bonus) rights_.
    pub const RA: Ac = &[b'R', b'A'];
    /// Group _Entitlement (rights)_ :: _Subscription rights_.
    pub const RS: Ac = &[b'R', b'S'];
    /// Group _Entitlement (rights)_ :: _Purchase rights_.
    pub const RP: Ac = &[b'R', b'P'];
    /// Group _Entitlement (rights)_ :: _Warrants_.
    pub const RW: Ac = &[b'R', b'W'];
    /// Group _Entitlement (rights)_ :: _Mini-future certificates, constant leverage certificates_.
    pub const RF: Ac = &[b'R', b'F'];
    /// Group _Entitlement (rights)_ :: _Depositary receipts on entitlements_.
    pub const RD: Ac = &[b'R', b'D'];
    /// Group _Entitlement (rights)_ :: _Others (miscellaneous)_.
    pub const RM: Ac = &[b'R', b'M'];
    /// Group _Listed options_ :: _Call options_.
    pub const OC: Ac = &[b'O', b'C'];
    /// Group _Listed options_ :: _Put options_.
    pub const OP: Ac = &[b'O', b'P'];
    /// Group _Listed options_ :: _Others (miscellaneous)_.
    pub const OM: Ac = &[b'O', b'M'];
    /// Group _Futures_ :: _Financial futures_.
    pub const FF: Ac = &[b'F', b'F'];
    /// Group _Futures_ :: _Commodities futures_.
    pub const FC: Ac = &[b'F', b'C'];
    /// Group _Swaps_ :: _Rates_.
    pub const SR: Ac = &[b'S', b'R'];
    /// Group _Swaps_ :: _Commodities_.
    pub const ST: Ac = &[b'S', b'T'];
    /// Group _Swaps_ :: _Equity_.
    pub const SE: Ac = &[b'S', b'E'];
    /// Group _Swaps_ :: _Credit_.
    pub const SC: Ac = &[b'S', b'C'];
    /// Group _Swaps_ :: _Foreign exchange_.
    pub const SF: Ac = &[b'S', b'F'];
    /// Group _Swaps_ :: _Others (miscellaneous)_.
    pub const SM: Ac = &[b'S', b'M'];
    /// Group _Non-listed and complex listed options_ :: _Rates_.
    pub const HR: Ac = &[b'H', b'R'];
    /// Group _Non-listed and complex listed options_ :: _Commodities_.
    pub const HT: Ac = &[b'H', b'T'];
    /// Group _Non-listed and complex listed options_ :: _Equity_.
    pub const HE: Ac = &[b'H', b'E'];
    /// Group _Non-listed and complex listed options_ :: _Credit_.
    pub const HC: Ac = &[b'H', b'C'];
    /// Group _Non-listed and complex listed options_ :: _Foreign exchange_.
    pub const HF: Ac = &[b'H', b'F'];
    /// Group _Non-listed and complex listed options_ :: _Others (miscellaneous)_.
    pub const HM: Ac = &[b'H', b'M'];
    /// Group _Spot_ :: _Foreign exchange_.
    pub const IF: Ac = &[b'I', b'F'];
    /// Group _Spot_ :: _Commodities_.
    pub const IT: Ac = &[b'I', b'T'];
    /// Group _Forwards_ :: _Equity_.
    pub const JE: Ac = &[b'J', b'E'];
    /// Group _Forwards_ :: _Foreign exchange_.
    pub const JF: Ac = &[b'J', b'F'];
    /// Group _Forwards_ :: _Credit_.
    pub const JC: Ac = &[b'J', b'C'];
    /// Group _Forwards_ :: _Rates_.
    pub const JR: Ac = &[b'J', b'R'];
    /// Group _Forwards_ :: _Commodities_.
    pub const JT: Ac = &[b'J', b'T'];
    /// Group _Strategies_ :: _Rates_.
    pub const KR: Ac = &[b'K', b'R'];
    /// Group _Strategies_ :: _Commodities_.
    pub const KT: Ac = &[b'K', b'T'];
    /// Group _Strategies_ :: _Equity_.
    pub const KE: Ac = &[b'K', b'E'];
    /// Group _Strategies_ :: _Credit_.
    pub const KC: Ac = &[b'K', b'C'];
    /// Group _Strategies_ :: _Foreign exchange_.
    pub const KF: Ac = &[b'K', b'F'];
    /// Group _Strategies_ :: _Mixed assets_.
    pub const KY: Ac = &[b'K', b'Y'];
    /// Group _Strategies_ :: _Others (miscellaneous)_.
    pub const KM: Ac = &[b'K', b'M'];
    /// Group _Financing_ :: _Loan-lease_.
    pub const LL: Ac = &[b'L', b'L'];
    /// Group _Financing_ :: _Repurchase agreements_.
    pub const LR: Ac = &[b'L', b'R'];
    /// Group _Financing_ :: _Securities lending_.
    pub const LS: Ac = &[b'L', b'S'];
    /// Group _Referential instruments_ :: _Currencies_.
    pub const TC: Ac = &[b'T', b'C'];
    /// Group _Referential instruments_ :: _Commodities_.
    pub const TT: Ac = &[b'T', b'T'];
    /// Group _Referential instruments_ :: _Interest rates_.
    pub const TR: Ac = &[b'T', b'R'];
    /// Group _Referential instruments_ :: _Indices_.
    pub const TI: Ac = &[b'T', b'I'];
    /// Group _Referential instruments_ :: _Baskets_.
    pub const TB: Ac = &[b'T', b'B'];
    /// Group _Referential instruments_ :: _Stock dividends_.
    pub const TD: Ac = &[b'T', b'D'];
    /// Group _Referential instruments_ :: _Others (miscellaneous)_.
    pub const TM: Ac = &[b'T', b'M'];
    /// Group _Others (miscellaneous)_ :: _Combined instruments_.
    pub const MC: Ac = &[b'M', b'C'];
    /// Group _Others (miscellaneous)_ :: _Other assets (miscellaneous)_.
    pub const MM: Ac = &[b'M', b'M'];
}

pub mod attrs {
    //! Defines and scopes the attributes available in [`crate::CFI`]'s.

    use super::Attrs;

    /// _A_ as u8.
    pub const A: u8 = b'A';
    /// _B_ as u8.
    pub const B: u8 = b'B';
    /// _C_ as u8.
    pub const C: u8 = b'C';
    /// _D_ as u8.
    pub const D: u8 = b'D';
    /// _E_ as u8.
    pub const E: u8 = b'E';
    /// _F_ as u8.
    pub const F: u8 = b'F';
    /// _G_ as u8.
    pub const G: u8 = b'G';
    /// _H_ as u8.
    pub const H: u8 = b'H';
    /// _I_ as u8.
    pub const I: u8 = b'I';
    /// _J_ as u8.
    pub const J: u8 = b'J';
    /// _K_ as u8.
    pub const K: u8 = b'K';
    /// _L_ as u8.
    pub const L: u8 = b'L';
    /// _M_ as u8.
    pub const M: u8 = b'M';
    /// _N_ as u8.
    pub const N: u8 = b'N';
    /// _O_ as u8.
    pub const O: u8 = b'O';
    /// _P_ as u8.
    pub const P: u8 = b'P';
    /// _Q_ as u8.
    pub const Q: u8 = b'Q';
    /// _R_ as u8.
    pub const R: u8 = b'R';
    /// _S_ as u8.
    pub const S: u8 = b'S';
    /// _T_ as u8.
    pub const T: u8 = b'T';
    /// _U_ as u8.
    pub const U: u8 = b'U';
    /// _V_ as u8.
    pub const V: u8 = b'V';
    /// _W_ as u8.
    pub const W: u8 = b'W';
    /// _X_ as u8.
    pub const X: u8 = b'X';
    /// _Y_ as u8.
    pub const Y: u8 = b'Y';
    /// _Z_ as u8.
    pub const Z: u8 = b'Z';
    
    /// Attributes available for  _ES_.
    pub const ES_ATTRS: Attrs = &[
        &[V, N, R, E], 
        &[T, U], 
        &[O, P, F], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _EP_.
    pub const EP_ATTRS: Attrs = &[
        &[V, N, R, E], 
        &[R, E, T, G, A, C, N], 
        &[F, C, P, Q, A, N, U], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _EC_.
    pub const EC_ATTRS: Attrs = &[
        &[V, N, R, E], 
        &[T, U], 
        &[O, P, F], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _EF_.
    pub const EF_ATTRS: Attrs = &[
        &[V, N, R, E], 
        &[R, E, T, G, A, C, N], 
        &[F, C, P, Q, A, N, U], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _EL_.
    pub const EL_ATTRS: Attrs = &[
        &[V, N, R, E], 
        &[T, U], 
        &[O, P, F], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _ED_.
    pub const ED_ATTRS: Attrs = &[
        &[S, P, C, F, L, M], 
        &[R, N, B, D], 
        // cfitest_20210507_current.xls contains an error in the (E-D) column.
        // &[F, C, P, Q, A, N, U, N],
        // 8th char is supposed to be 'D' according to rdfs.label.
        &[F, C, P, Q, A, N, U, D], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _EY_.
    pub const EY_ATTRS: Attrs = &[
        &[A, B, C, D, E, M], 
        &[D, Y, M], 
        &[F, V, E, M], 
        &[B, S, D, G, T, C, I, N, M]
    ];
    /// Attributes available for  _EM_.
    pub const EM_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _CI_.
    pub const CI_ATTRS: Attrs = &[
        &[C, O, M], 
        &[I, G, J], 
        &[R, B, E, V, L, C, D, F, K, M], 
        &[S, Q, U, Y]
    ];
    /// Attributes available for  _CH_.
    pub const CH_ATTRS: Attrs = &[
        &[D, R, S, E, A, N, L, M], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _CB_.
    pub const CB_ATTRS: Attrs = &[
        &[C, O, M], 
        &[I, G, J], 
        &[X], 
        &[S, Q, U, Y]
    ];
    /// Attributes available for  _CE_.
    pub const CE_ATTRS: Attrs = &[
        &[C, O, M], 
        &[I, G, J], 
        &[R, B, E, V, L, C, D, F, K, M], 
        &[S, U]
    ];
    /// Attributes available for  _CS_.
    pub const CS_ATTRS: Attrs = &[
        &[C, O, M], 
        &[B, G, L, M], 
        &[R, B, M], 
        &[S, U]
    ];
    /// Attributes available for  _CF_.
    pub const CF_ATTRS: Attrs = &[
        &[C, O, M], 
        &[I, G, J], 
        // cfitest_20210507_current.xls contains an error in the (C-F) column.
        // &[I, M, H, B, E, P, M]
        // 'M' as "Others (miscellaneous)" exist twice.
        &[I, H, B, E, P, M], 
        &[S, Q, U, Y]
    ];
    /// Attributes available for  _CP_.
    pub const CP_ATTRS: Attrs = &[
        &[C, O, M], 
        &[I, G, J], 
        &[R, B, E, V, L, C, D, F, K, M], 
        &[S, Q, U, Y]
    ];
    /// Attributes available for  _CM_.
    pub const CM_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[S, Q, U, Y]
    ];
    /// Attributes available for  _DB_.
    pub const DB_ATTRS: Attrs = &[
        &[F, Z, V, C, K], 
        &[T, G, S, U, P, N, O, Q, J, C], 
        &[F, G, C, D, A, B, T, L, P, Q, R, E], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _DC_.
    pub const DC_ATTRS: Attrs = &[
        &[F, Z, V, K], 
        &[T, G, S, U, P, N, O, Q, J, C], 
        &[F, G, C, D, A, B, T, L, P, Q, R, E], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _DW_.
    pub const DW_ATTRS: Attrs = &[
        &[F, Z, V, K], 
        &[T, G, S, U, P, N, O, Q, J, C], 
        &[F, G, C, D, A, B, T, L, P, Q, R, E], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _DT_.
    pub const DT_ATTRS: Attrs = &[
        &[F, Z, V, K], 
        &[T, G, S, U, P, N, O, Q, J, C], 
        &[F, G, C, D, A, B, T, L, P, Q, R, E], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _DY_.
    pub const DY_ATTRS: Attrs = &[
        &[F, Z, V, K], 
        &[T, G, S, U, P, N, O, Q, J, C], 
        &[X], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _DS_.
    pub const DS_ATTRS: Attrs = &[
        &[A, B, C, D, M], 
        &[F, D, V, Y, M], 
        &[F, V, M], 
        &[B, S, D, T, C, I, N, M]
    ];
    /// Attributes available for  _DE_.
    pub const DE_ATTRS: Attrs = &[
        &[A, B, C, D, E, M], 
        &[F, D, V, Y, M], 
        &[R, S, C, T, M], 
        &[B, S, D, T, C, I, N, M]
    ];
    /// Attributes available for  _DG_.
    pub const DG_ATTRS: Attrs = &[
        &[F, Z, V], 
        &[T, G, S, U, P, N, O, Q, J, C], 
        &[F, G, C, D, A, B, T, L, P, Q, R, E], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _DA_.
    pub const DA_ATTRS: Attrs = &[
        &[F, Z, V], 
        &[T, G, S, U, P, N, O, Q, J, C], 
        &[F, G, C, D, A, B, T, L, P, Q, R, E], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _DN_.
    pub const DN_ATTRS: Attrs = &[
        &[F, Z, V], 
        &[T, G, S, U, P, N, O, Q, J, C], 
        &[F, G, C, D, A, B, T, L, P, Q, R, E], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _DD_.
    pub const DD_ATTRS: Attrs = &[
        &[B, C, W, T, Y, G, A, N, M], 
        &[F, Z, V, C], 
        &[T, G, S, U, P, N, O, Q, J, C], 
        &[F, G, C, D, A, B, T, L, P, Q, R, E]
    ];
    /// Attributes available for  _DM_.
    pub const DM_ATTRS: Attrs = &[
        &[B, P, M], 
        &[X], 
        &[X], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _RA_.
    pub const RA_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _RS_.
    pub const RS_ATTRS: Attrs = &[
        &[S, P, C, F, B, I, M], 
        &[X], 
        &[X], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _RP_.
    pub const RP_ATTRS: Attrs = &[
        &[S, P, C, F, B, I, M], 
        &[X], 
        &[X], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _RW_.
    pub const RW_ATTRS: Attrs = &[
        &[B, S, D, T, C, I, M], 
        &[T, N, C], 
        &[C, P, B], 
        &[E, A, B, M]
    ];
    /// Attributes available for  _RF_.
    pub const RF_ATTRS: Attrs = &[
        &[B, S, D, T, C, I, M], 
        &[T, N, M], 
        &[C, P, M], 
        &[E, A, B, M]
    ];
    /// Attributes available for  _RD_.
    pub const RD_ATTRS: Attrs = &[
        &[A, S, P, W, M], 
        &[X], 
        &[X], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _RM_.
    pub const RM_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _OC_.
    pub const OC_ATTRS: Attrs = &[
        &[E, A, B], 
        &[B, S, D, T, C, I, O, F, W, N, M], 
        &[P, C, N, E], 
        &[S, N]
    ];
    /// Attributes available for  _OP_.
    pub const OP_ATTRS: Attrs = &[
        &[E, A, B], 
        &[B, S, D, T, C, I, O, F, W, N, M], 
        &[P, C, N, E], 
        &[S, N]
    ];
    /// Attributes available for  _OM_.
    pub const OM_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _FF_.
    pub const FF_ATTRS: Attrs = &[
        &[B, S, D, C, I, O, F, W, N, V, M], 
        &[P, C, N], 
        &[S, N], 
        &[X]
    ];
    /// Attributes available for  _FC_.
    pub const FC_ATTRS: Attrs = &[
        &[E, A, I, S, N, P, H, M], 
        &[P, C, N], 
        &[S, N], 
        &[X]
    ];
    /// Attributes available for  _SR_.
    pub const SR_ATTRS: Attrs = &[
        &[A, C, D, G, H, Z, M], 
        &[C, I, D, Y], 
        &[S, C], 
        &[D, N]
    ];
    /// Attributes available for  _ST_.
    pub const ST_ATTRS: Attrs = &[
        &[J, K, A, N, G, P, S, T, I, H, B, C, Q, M], 
        &[C, T], 
        &[X], 
        &[C, P, E]
    ];
    /// Attributes available for  _SE_.
    pub const SE_ATTRS: Attrs = &[
        &[S, I, B, M], 
        &[P, D, V, L, T, C, M], 
        &[X], 
        &[C, P, E]
    ];
    /// Attributes available for  _SC_.
    pub const SC_ATTRS: Attrs = &[
        &[U, V, I, B, M], 
        &[C, T, M], 
        &[C, S, L], 
        &[C, P, A]
    ];
    /// Attributes available for  _SF_.
    pub const SF_ATTRS: Attrs = &[
        &[A, C, M], 
        &[X], 
        &[X], 
        &[P, C]
    ];
    /// Attributes available for  _SM_.
    pub const SM_ATTRS: Attrs = &[
        &[P, M], 
        &[X], 
        &[X], 
        &[C, P, E]
    ];
    /// Attributes available for  _HR_.
    pub const HR_ATTRS: Attrs = &[
        &[A, C, D, E, G, H, O, R, F, M], 
        &[A, B, C, D, E, F, G, H, I], 
        &[V, A, D, B, G, L, P, C, F, M], 
        &[C, P, E]
    ];
    /// Attributes available for  _HT_.
    pub const HT_ATTRS: Attrs = &[
        &[J, K, A, N, G, P, S, T, I, H, B, C, O, R, F, W, M], 
        &[A, B, C, D, E, F, G, H, I], 
        &[V, A, D, B, G, L, P, M], 
        &[C, P, E]
    ];
    /// Attributes available for  _HE_.
    pub const HE_ATTRS: Attrs = &[
        &[S, I, B, O, R, F, M], 
        &[A, B, C, D, E, F, G, H, I], 
        &[V, A, D, B, G, L, P, M], 
        &[C, P, E]
    ];
    /// Attributes available for  _HC_.
    pub const HC_ATTRS: Attrs = &[
        &[U, V, I, W, M], 
        &[A, B, C, D, E, F, G, H, I], 
        &[V, A, D, B, G, L, P, M], 
        &[C, P, E]
    ];
    /// Attributes available for  _HF_.
    pub const HF_ATTRS: Attrs = &[
        &[R, F, T, V, B, C, D, E, Q, U, W, Y, M], 
        &[J, K, L], 
        &[V, A, D, B, G, L, P, M], 
        &[C, P, E]
    ];
    /// Attributes available for  _HM_.
    pub const HM_ATTRS: Attrs = &[
        &[P, M], 
        &[A, B, C, D, E, F, G, H, I, J, K, L], 
        &[V, A, D, B, G, L, P, M], 
        &[C, P, E, N, A]
    ];
    /// Attributes available for  _IF_.
    pub const IF_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[P]
    ];
    /// Attributes available for  _IT_.
    pub const IT_ATTRS: Attrs = &[
        &[A, J, K, N, P, S, T, M], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _JE_.
    pub const JE_ATTRS: Attrs = &[
        &[S, I, B, O, F], 
        &[X], 
        &[C, S, F], 
        &[C, P]
    ];
    /// Attributes available for  _JF_.
    pub const JF_ATTRS: Attrs = &[
        &[T, U, V, R, S, W, O, J, K, F, L, N], 
        &[X], 
        &[C, S, F, R], 
        &[C, P]
    ];
    /// Attributes available for  _JC_.
    pub const JC_ATTRS: Attrs = &[
        &[A, I, B, C, D, G, O], 
        &[X], 
        &[C, S, F], 
        &[C, P]
    ];
    /// Attributes available for  _JR_.
    pub const JR_ATTRS: Attrs = &[
        &[I, O, M], 
        &[X], 
        &[C, S, F], 
        &[C, P]
    ];
    /// Attributes available for  _JT_.
    pub const JT_ATTRS: Attrs = &[
        &[A, B, C, G, I, H, J, K, N, P, S, T, M], 
        &[X], 
        &[C, S, F], 
        &[C, P]
    ];
    /// Attributes available for  _KR_.
    pub const KR_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _KT_.
    pub const KT_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _KE_.
    pub const KE_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _KC_.
    pub const KC_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _KF_.
    pub const KF_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _KY_.
    pub const KY_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _KM_.
    pub const KM_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _LL_.
    pub const LL_ATTRS: Attrs = &[
        &[A, B, J, K, N, P, S, T, M], 
        &[X], 
        &[X], 
        &[C, P]
    ];
    /// Attributes available for  _LR_.
    pub const LR_ATTRS: Attrs = &[
        &[G, S, C], 
        &[F, N, O, T], 
        &[X], 
        &[D, H, T]
    ];
    /// Attributes available for  _LS_.
    pub const LS_ATTRS: Attrs = &[
        &[C, G, P, T, E, L, D, W, K, M], 
        &[N, O, T], 
        &[X], 
        &[D, F, H, T]
    ];
    /// Attributes available for  _TC_.
    pub const TC_ATTRS: Attrs = &[
        &[N, L, C, M], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _TT_.
    pub const TT_ATTRS: Attrs = &[
        &[E, A, I, S, N, P, H, M], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _TR_.
    pub const TR_ATTRS: Attrs = &[
        &[N, V, F, R, M], 
        &[D, W, N, Q, S, A, M], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _TI_.
    pub const TI_ATTRS: Attrs = &[
        &[E, D, F, R, T, C, M], 
        &[P, C, E, F, M], 
        &[P, N, G, M], 
        &[X]
    ];
    /// Attributes available for  _TB_.
    pub const TB_ATTRS: Attrs = &[
        &[E, D, F, I, T, C, M], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _TD_.
    pub const TD_ATTRS: Attrs = &[
        &[S, P, C, F, L, K, M], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _TM_.
    pub const TM_ATTRS: Attrs = &[
        &[X], 
        &[X], 
        &[X], 
        &[X]
    ];
    /// Attributes available for  _MC_.
    pub const MC_ATTRS: Attrs = &[
        &[S, B, H, A, W, U, M], 
        &[T, U], 
        &[X], 
        &[B, R, N, M]
    ];
    /// Attributes available for  _MM_.
    pub const MM_ATTRS: Attrs = &[
        &[R, I, E, T, N, P, S, M], 
        &[X], 
        &[X], 
        &[X]
    ];
}

#[cfg(any(feature = "describe", doc))]
pub mod refs {
    //! Defines and scopes the the reference string data for [`crate::CFI`]'s.

    use super::AcRefs;

    // Basic layout of a AcRefs:
    // &(
    //     (Cat.Name, Cat.Desc), (Grp.Name, Grp.Desc),  &[Attr.Header; 4], &[
    //              &[Attr.Desc],
    //              &[Attr.Desc],
    //              &[Attr.Desc],
    //              &[Attr.Desc],
    // ]);
    //
    // Each AttributeX.Desc slice is the same size and order as the group ->
    // attribute scope defined under consts::attribute.
    //     pub const ES_ATTRS: Attrs = &[
    //         &[V, N, R, E], 
    //         &[T, U], 
    //         &[O, P, F], 
    //         &[B, R, N, M]
    //     ];
    //     pub const ES_REF: AcRefs = &(
    //         (223, 253), (144, 320), &[602, 418, 428, 277], &[
    //             &[601, 391, 486, 216]), // &[V, N, R, E], 
    //             &[487, 293]),           // &[T, U], 
    //             &[383, 424, 298]),      // &[O, P, F], 
    //             &[ 83, 477,  85, 412])  // &[B, R, N, M]
    //     ]);
    
    /// Reference data for _ES_.
    pub const ES_REF: AcRefs = &(
        (223, 253), (144, 320), &[602, 418, 428, 277], &[
            &[601, 391, 486, 216], 
            &[487, 293], 
            &[383, 424, 298], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _EP_.
    pub const EP_REF: AcRefs = &(
        (223, 253), (444, 427), &[602, 469, 321, 277], &[
            &[601, 391, 486, 216], 
            &[465, 245, 468, 241, 467, 466, 432], 
            &[268, 167, 423, 169,  18, 393,  61], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _EC_.
    pub const EC_REF: AcRefs = &(
        (223, 253), (143, 505), &[602, 418, 428, 277], &[
            &[601, 391, 486, 216], 
            &[487, 293], 
            &[383, 424, 298], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _EF_.
    pub const EF_REF: AcRefs = &(
        (223, 253), (443, 445), &[602, 469, 321, 277], &[
            &[601, 391, 486, 216], 
            &[465, 245, 468, 241, 467, 466, 432], 
            &[268, 167, 423, 169,  18, 393,  61], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _EL_.
    pub const EL_REF: AcRefs = &(
        (223, 253), (353,   8), &[602, 418, 428, 277], &[
            &[601, 391, 486, 216], 
            &[487, 293], 
            &[383, 424, 298], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _ED_.
    pub const ED_REF: AcRefs = &(
        (223, 253), (192, 195), &[340, 470, 321, 276], &[
            &[144, 444, 143, 443, 353, 412], 
            &[464, 430, 154, 157], 
            &[267, 166, 422, 168,  17, 392,  60, 209], 
            &[ 82, 476,  84, 412], 
    ]);
    /// Reference data for _EY_.
    pub const EY_REF: AcRefs = &(
        (223, 253), (538, 549), &[562, 204, 480, 573], &[
            &[556, 414,  99, 413, 561, 412], 
            &[208, 384, 412], 
            &[128, 439, 213, 412], 
            &[ 80, 223, 175, 197, 139, 171, 335, 345, 412], 
    ]);
    /// Reference data for _EM_.
    pub const EM_REF: AcRefs = &(
        (223, 253), (412, 225), &[394, 394, 394, 277], &[
            &[394], 
            &[394], 
            &[394], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _CI_.
    pub const CI_REF: AcRefs = &(
        (108, 495), (528,  41), &[133, 205,  57, 497], &[
            &[131, 400, 412], 
            &[322,  16, 369], 
            &[461, 176, 223, 156, 366, 139, 196, 474, 164, 412], 
            &[506, 507, 585, 586], 
    ]);
    /// Reference data for _CH_.
    pub const CH_REF: AcRefs = &(
        (108, 495), (317, 571), &[346, 394, 394, 394], &[
            &[202, 479, 496, 239,  50, 376,  55, 412], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _CB_.
    pub const CB_REF: AcRefs = &(
        (108, 495), (463,   0), &[133, 205, 394, 497], &[
            &[131, 400, 412], 
            &[322,  16, 369], 
            &[394], 
            &[506, 507, 585, 586], 
    ]);
    /// Reference data for _CE_.
    pub const CE_REF: AcRefs = &(
        (108, 495), (240,  36), &[133, 205,  57, 497], &[
            &[131, 400, 412], 
            &[322,  16, 369], 
            &[461, 176, 223, 156, 366, 139, 196, 474, 164, 412], 
            &[504, 584], 
    ]);
    /// Reference data for _CS_.
    pub const CS_REF: AcRefs = &(
        (108, 495), (429,   9), &[132, 536, 562, 497], &[
            &[130, 398, 412], 
            &[ 62, 314, 352, 412], 
            &[180, 181, 412], 
            &[504, 584], 
    ]);
    /// Reference data for _CF_.
    pub const CF_REF: AcRefs = &(
        (108, 495), (300,   7), &[133, 205, 566, 497], &[
            &[131, 400, 412], 
            &[322,  16, 369], 
            // cfitest_20210507_current.xls contains an error in the (C-F) column.
            // &[528, 412, 317, 457, 210, 449, 412], 
            // 'M' as "Others (miscellaneous)" exist twice.
            &[528, 317, 457, 210, 449, 412], 
            &[506, 507, 585, 586], 
    ]);
    /// Reference data for _CP_.
    pub const CP_REF: AcRefs = &(
        (108, 495), (449,  10), &[133, 205,  57, 497], &[
            &[131, 400, 412], 
            &[322,  16, 369], 
            &[461, 176, 223, 156, 366, 139, 196, 474, 164, 412], 
            &[506, 507, 585, 586], 
    ]);
    /// Reference data for _CM_.
    pub const CM_REF: AcRefs = &(
        (108, 495), (412, 109), &[394, 394, 394, 497], &[
            &[394], 
            &[394], 
            &[394], 
            &[506, 507, 585, 586], 
    ]);
    /// Reference data for _DB_.
    pub const DB_REF: AcRefs = &(
        (175, 251), ( 97 ,  49), &[568, 315, 472, 277], &[
            &[266, 609, 593, 127, 426], 
            &[311, 347, 493, 587, 380, 499, 500, 348, 349, 541], 
            &[261, 262, 264, 263,  31,  32,  34,  33, 431, 433, 434, 244], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _DC_.
    pub const DC_REF: AcRefs = &(
        (175, 251), (155,   2), &[567, 315, 472, 277], &[
            &[265, 608, 591, 425], 
            &[311, 347, 493, 587, 380, 499, 500, 348, 349, 541], 
            &[261, 262, 264, 263,  31,  32,  34,  33, 431, 433, 434, 244], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _DW_.
    pub const DW_REF: AcRefs = &(
        (175, 251), ( 98 ,   3), &[567, 315, 472, 277], &[
            &[265, 608, 591, 425], 
            &[311, 347, 493, 587, 380, 499, 500, 348, 349, 541], 
            &[261, 262, 264, 263,  31,  32,  34,  33, 431, 433, 434, 244], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _DT_.
    pub const DT_REF: AcRefs = &(
        (175, 251), (361, 381), &[567, 315, 472, 277], &[
            &[265, 608, 591, 425], 
            &[311, 347, 493, 587, 380, 499, 500, 348, 349, 541], 
            &[261, 262, 264, 263,  31,  32,  34,  33, 431, 433, 434, 244], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _DY_.
    pub const DY_REF: AcRefs = &(
        (175, 251), (371, 250), &[567, 315, 394, 277], &[
            &[265, 608, 591, 425], 
            &[311, 347, 493, 587, 380, 499, 500, 348, 349, 541], 
            &[394], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _DS_.
    pub const DS_REF: AcRefs = &(
        (175, 251), (537, 115), &[562, 204, 480, 575], &[
            &[117, 118,  66, 116, 412], 
            &[260, 207, 595, 384, 412], 
            &[259, 594, 412], 
            &[ 79, 223, 175, 139, 171, 335, 345, 412], 
    ]);
    /// Reference data for _DE_.
    pub const DE_REF: AcRefs = &(
        (175, 251), (539,  13), &[562, 204, 480, 575], &[
            &[203,  68, 491,  70, 243, 412], 
            &[260, 207, 595, 384, 412], 
            &[484, 481, 482, 483, 412], 
            &[ 79, 223, 175, 139, 171, 335, 345, 412], 
    ]);
    /// Reference data for _DG_.
    pub const DG_REF: AcRefs = &(
        (175, 251), (373, 374), &[567, 316, 471, 276], &[
            &[265, 608, 591], 
            &[311, 347, 493, 587, 380, 499, 500, 348, 349, 541], 
            &[261, 262, 264, 263,  31,  32,  34,  33, 431, 433, 434, 244], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _DA_.
    pub const DA_REF: AcRefs = &(
        (175, 251), ( 54 , 177), &[567, 315, 472, 277], &[
            &[265, 608, 591], 
            &[311, 347, 493, 587, 380, 499, 500, 348, 349, 541], 
            &[261, 262, 264, 263,  31,  32,  34,  33, 431, 433, 434, 244], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _DN_.
    pub const DN_REF: AcRefs = &(
        (175, 251), (377,  96), &[567, 315, 472, 277], &[
            &[265, 608, 591], 
            &[531, 347, 493, 587, 380, 499, 500, 348, 349, 541], 
            &[261, 262, 264, 263,  31,  32,  34,  33, 431, 433, 434, 244], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _DD_.
    pub const DD_REF: AcRefs = &(
        (175, 251), (190, 193), &[339, 570, 315, 472], &[
            &[ 97, 155,  98, 361, 371, 373,  54, 377, 412], 
            &[265, 608, 591, 126], 
            &[311, 347, 493, 587, 380, 499, 500, 348, 349, 541], 
            &[261, 262, 264, 263,  31,  32,  34,  33, 431, 433, 434, 244], 
    ]);
    /// Reference data for _DM_.
    pub const DM_REF: AcRefs = &(
        (175, 251), (412, 178), &[562, 394, 394, 277], &[
            &[ 63, 452, 412], 
            &[394], 
            &[394], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _RA_.
    pub const RA_REF: AcRefs = &(
        (217, 252), ( 21 , 450), &[394, 394, 394, 277], &[
            &[394], 
            &[394], 
            &[394], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _RS_.
    pub const RS_REF: AcRefs = &(
        (217, 252), (540, 451), &[ 56, 394, 394, 277], &[
            &[144, 444, 143, 443,  97, 136, 412], 
            &[394], 
            &[394], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _RP_.
    pub const RP_REF: AcRefs = &(
        (217, 252), (453,  48), &[ 56, 394, 394, 277], &[
            &[144, 444, 143, 443,  97, 136, 412], 
            &[394], 
            &[394], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _RW_.
    pub const RW_REF: AcRefs = &(
        (217, 252), (604, 255), &[580, 563, 113, 242], &[
            &[ 81, 224, 179, 140, 172, 336, 412], 
            &[558, 378, 160], 
            &[110, 454, 111], 
            &[232,  24,  89, 412], 
    ]);
    /// Reference data for _RF_.
    pub const RF_REF: AcRefs = &(
        (217, 252), (364, 365), &[580,  67, 358, 242], &[
            &[ 81, 224, 179, 140, 172, 336, 412], 
            &[ 71,  69, 412], 
            &[357, 508, 412], 
            &[232,  24,  89, 412], 
    ]);
    /// Reference data for _RD_.
    pub const RD_REF: AcRefs = &(
        (217, 252), (191, 194), &[339, 394, 394, 277], &[
            &[ 21, 540, 453, 604, 412], 
            &[394], 
            &[394], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _RM_.
    pub const RM_REF: AcRefs = &(
        (217, 252), (412, 218), &[394, 394, 394, 394], &[
            &[394], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _OC_.
    pub const OC_REF: AcRefs = &(
        (354, 551), (112, 149), &[242, 577, 187, 530], &[
            &[229,  22,  86], 
            &[ 79, 533, 175, 139, 170, 334, 402, 302, 542, 344, 412], 
            &[438, 124, 388, 212], 
            &[529, 390], 
    ]);
    /// Reference data for _OP_.
    pub const OP_REF: AcRefs = &(
        (354, 551), (455, 150), &[242, 578, 187, 530], &[
            &[229,  22,  86], 
            &[ 79, 533, 175, 139, 170, 334, 402, 302, 542, 344, 412], 
            &[438, 124, 388, 212], 
            &[529, 390], 
    ]);
    /// Reference data for _OM_.
    pub const OM_REF: AcRefs = &(
        (354, 551), (412, 403), &[394, 394, 394, 394], &[
            &[394], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _FF_.
    pub const FF_REF: AcRefs = &(
        (302, 152), (249, 304), &[576, 186, 530, 394], &[
            &[ 79, 533, 175, 170, 334, 402, 302, 542, 344, 532, 412], 
            &[435, 121, 386], 
            &[529, 390], 
            &[394], 
    ]);
    /// Reference data for _FC_.
    pub const FC_REF: AcRefs = &(
        (302, 152), (141, 303), &[572, 186, 530, 394], &[
            &[246,  20, 337, 501, 221, 441, 309, 412], 
            &[435, 121, 386], 
            &[529, 390], 
            &[394], 
    ]);
    /// Reference data for _SR_.
    pub const SR_REF: AcRefs = &(
        (542,  14), (459,  11), &[572, 395, 512, 185], &[
            &[ 72, 271, 270, 338, 417, 607, 412], 
            &[147,  15,  35, 173], 
            &[509, 165], 
            &[182, 387], 
    ]);
    /// Reference data for _ST_.
    pub const ST_REF: AcRefs = &(
        (542,  14), (139,   4), &[572, 490, 394, 183], &[
            &[215, 363,  20, 221, 296, 440, 248, 421, 331, 329,  78,  77, 375, 412], 
            &[148, 555], 
            &[394], 
            &[121, 435, 213], 
    ]);
    /// Reference data for _SE_.
    pub const SE_REF: AcRefs = &(
        (542,  14), (226,  39), &[572, 488, 394, 183], &[
            &[515, 324,  74, 412], 
            &[446, 206, 596, 597, 554, 107, 412], 
            &[394], 
            &[121, 435, 213], 
    ]);
    /// Reference data for _SC_.
    pub const SC_REF: AcRefs = &(
        (542,  14), (161,   5), &[572, 488, 583, 183], &[
            &[511, 328, 326,  74, 412], 
            &[162, 554, 412], 
            &[159, 516, 356], 
            &[121, 435,  59], 
    ]);
    /// Reference data for _SF_.
    pub const SF_REF: AcRefs = &(
        (542,  14), (274,   6), &[572, 394, 394, 183], &[
            &[525, 288, 412], 
            &[394], 
            &[394], 
            &[436, 122], 
    ]);
    /// Reference data for _SM_.
    pub const SM_REF: AcRefs = &(
        (542,  14), (412, 544), &[572, 394, 394, 183], &[
            &[138, 412], 
            &[394], 
            &[394], 
            &[121, 435, 213], 
    ]);
    /// Reference data for _HR_.
    pub const HR_REF: AcRefs = &(
        (389, 552), (459,  44), &[ 72, 401, 588, 183], &[
            &[610, 611, 612, 343, 613, 614, 402, 615, 302, 412], 
            &[234,  26,  91, 238,  30,  95, 236,  28,  93], 
            &[590,  52, 199,  65, 201, 360, 410, 114, 273, 412], 
            &[121, 435, 213], 
    ]);
    /// Reference data for _HT_.
    pub const HT_REF: AcRefs = &(
        (389, 552), (139,  45), &[572, 401, 588, 183], &[
            &[214, 362,  19, 219, 295, 440, 247, 420, 332, 329,  78,  77, 402, 289, 302, 542, 412], 
            &[234,  26,  91, 238,  30,  95, 236,  28,  93], 
            &[589,  51, 198,  64, 200, 359, 409, 412], 
            &[121, 435, 213], 
    ]);
    /// Reference data for _HE_.
    pub const HE_REF: AcRefs = &(
        (389, 552), (226,  46), &[572, 401, 588, 183], &[
            &[513, 325,  76, 402, 289, 302, 412], 
            &[234,  26,  91, 238,  30,  95, 236,  28,  93], 
            &[589,  51, 198,  64, 200, 359, 409, 412], 
            &[121, 435, 213], 
    ]);
    /// Reference data for _HC_.
    pub const HC_REF: AcRefs = &(
        (389, 552), (161,  42), &[572, 401, 588, 183], &[
            &[103, 106, 105, 543, 412], 
            &[234,  26,  91, 238,  30,  95, 236,  28,  93], 
            &[589,  51, 198,  64, 200, 359, 409, 412], 
            &[121, 435, 213], 
    ]);
    /// Reference data for _HF_.
    pub const HF_REF: AcRefs = &(
        (389, 552), (274,  43), &[572, 401, 588, 183], &[
            &[292, 307, 524, 600, 290, 305, 521, 598, 291, 306, 522, 599, 412], 
            &[231, 616,  88], 
            &[589,  51, 198,  64, 200, 359, 409, 412], 
            &[121, 435, 213], 
    ]);
    /// Reference data for _HM_.
    pub const HM_REF: AcRefs = &(
        (389, 552), (412, 403), &[572, 401, 588, 183], &[
            &[137,  412], 
            &[233,  25,  90, 237,  29,  94, 235,  27,  92, 230,  23,  87], 
            &[589,  51, 198,  64, 200, 359, 409, 412], 
            &[121, 435, 211, 386,  58], 
    ]);
    /// Reference data for _IF_.
    pub const IF_REF: AcRefs = &(
        (518, 151), (274, 519), &[394, 394, 394, 183], &[
            &[394], 
            &[394], 
            &[394], 
            &[435], 
    ]);
    /// Reference data for _IT_.
    pub const IT_REF: AcRefs = &(
        (518, 151), (139, 520), &[579, 394, 394, 394], &[
            &[ 19, 214, 362, 220, 440, 247, 420, 412], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _JE_.
    pub const JE_REF: AcRefs = &(
        (289, 153), (226, 282), &[581, 394, 489, 183], &[
            &[514, 323,  75, 402, 302], 
            &[394], 
            &[107, 527, 284], 
            &[123, 437], 
    ]);
    /// Reference data for _JF_.
    pub const JF_REF: AcRefs = &(
        (289, 153), (274, 281), &[572, 394, 488, 183], &[
            &[523, 521, 522, 287, 285, 286, 406, 404, 405, 307, 305, 306], 
            &[394], 
            &[107, 526, 283, 492], 
            &[123, 437], 
    ]);
    /// Reference data for _JC_.
    pub const JC_REF: AcRefs = &(
        (289, 153), (161, 279), &[572, 394, 489, 183], &[
            &[510, 323,  73, 102, 104, 101, 402], 
            &[394], 
            &[107, 527, 284], 
            &[123, 437], 
    ]);
    /// Reference data for _JR_.
    pub const JR_REF: AcRefs = &(
        (289, 153), (459, 280), &[572, 394, 489, 183], &[
            &[343, 402, 412], 
            &[394], 
            &[107, 527, 284], 
            &[123, 437], 
    ]);
    /// Reference data for _JT_.
    pub const JT_REF: AcRefs = &(
        (289, 153), (139, 278), &[572, 394, 489, 183], &[
            &[ 19,  78,  77, 295, 332, 329, 214, 362, 219, 440, 247, 420, 412], 
            &[394], 
            &[107, 527, 284], 
            &[123, 437], 
    ]);
    /// Reference data for _KR_.
    pub const KR_REF: AcRefs = &(
        (534, 553), (459, 342), &[394, 394, 394, 394], &[
            &[394], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _KT_.
    pub const KT_REF: AcRefs = &(
        (534, 553), (139, 142), &[394, 394, 394, 394], &[
            &[394], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _KE_.
    pub const KE_REF: AcRefs = &(
        (534, 553), (226, 227), &[394, 394, 394, 394], &[
            &[394], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _KC_.
    pub const KC_REF: AcRefs = &(
        (534, 553), (161, 163), &[394, 394, 394, 394], &[
            &[394], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _KF_.
    pub const KF_REF: AcRefs = &(
        (534, 553), (274, 275), &[394, 394, 394, 394], &[
            &[394], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _KY_.
    pub const KY_REF: AcRefs = &(
        (534, 553), (368, 367), &[394, 394, 394, 394], &[
            &[394], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _KM_.
    pub const KM_REF: AcRefs = &(
        (534, 553), (412, 535), &[394, 394, 394, 394], &[
            &[394], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _LL_.
    pub const LL_REF: AcRefs = &(
        (256, 257), (355, 396), &[572, 394, 394, 183], &[
            &[ 19,  79, 214, 362, 219, 440, 247, 420, 412], 
            &[394], 
            &[394], 
            &[123, 437], 
    ]);
    /// Reference data for _LR_.
    pub const LR_REF: AcRefs = &(
        (256, 257), (485,  12), &[582, 548, 394, 184], &[
            &[308, 517, 125], 
            &[272, 416, 399, 546], 
            &[394], 
            &[189, 319, 560], 
    ]);
    /// Reference data for _LS_.
    pub const LS_REF: AcRefs = &(
        (256, 257), (494,  38), &[574, 547, 394, 183], &[
            &[125, 310, 158, 155, 226, 351, 129, 604, 371, 412], 
            &[415, 397, 545], 
            &[394], 
            &[188, 294, 318, 559], 
    ]);
    /// Reference data for _TC_.
    pub const TC_REF: AcRefs = &(
        (473, 333), (170,  37), &[562, 394, 394, 394], &[
            &[379, 350, 100, 412], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _TT_.
    pub const TT_REF: AcRefs = &(
        (473, 333), (139,   1), &[564, 394, 394, 394], &[
            &[246,  20, 337, 501, 221, 441, 309, 412], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _TR_.
    pub const TR_REF: AcRefs = &(
        (473, 333), (344, 458), &[569, 297, 394, 394], &[
            &[385, 592, 258, 460, 412], 
            &[174, 605, 372, 456, 498,  47, 412], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _TI_.
    pub const TI_REF: AcRefs = &(
        (473, 333), (334,  40), &[ 53, 606, 327, 394], &[
            &[223, 175, 108, 461, 139, 170, 412], 
            &[448, 119, 222, 370, 411], 
            &[447, 382, 312, 412], 
            &[394], 
    ]);
    /// Reference data for _TB_.
    pub const TB_REF: AcRefs = &(
        (473, 333), ( 79 , 313), &[146, 394, 394, 394], &[
            &[223, 175, 108, 334, 139, 170, 412], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _TD_.
    pub const TD_REF: AcRefs = &(
        (473, 333), (532, 478), &[565, 394, 394, 394], &[
            &[144, 444, 143, 443, 353, 108, 412], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _TM_.
    pub const TM_REF: AcRefs = &(
        (473, 333), (412, 475), &[394, 394, 394, 394], &[
            &[394], 
            &[394], 
            &[394], 
            &[394], 
    ]);
    /// Reference data for _MC_.
    pub const MC_REF: AcRefs = &(
        (412, 254), (136, 419), &[145, 418, 394, 277], &[
            &[135, 134, 502, 503, 603, 299, 412], 
            &[487, 293], 
            &[394], 
            &[ 83, 477,  85, 412], 
    ]);
    /// Reference data for _MM_.
    pub const MM_REF: AcRefs = &(
        (412, 254), (408, 550), &[301, 394, 394, 394], &[
            &[462, 341, 228, 557, 120, 442, 407, 412], 
            &[394], 
            &[394], 
            &[394], 
    ]);
}

#[cfg(any(feature = "describe", doc))]
pub mod strs {
    //! Deduplicated string resources used to describe [`crate::CFI`]'s.

    /// Returns the "Invalid" &str.
    pub fn invalid() -> &'static str {
        DATA[DATA.len()-1]
    }
    /// Returns the "Not applicable/undefined" &str.
    pub fn na() -> &'static str {
        DATA[394]
    }
    /// Deduplicated &str DATA for CFI.
    pub const DATA: &[&str] = &[
        // NOTE: DO NOT ALTER THE ORDER OF THIS LIST!
        "A REIT is a real estate company that offers shares/units to the public and invests in real estate directly, either through properties or mortgages.", 
        "A basic good used in commerce that is interchangeable with other commodities of the same type. Commodities are most often used as inputs in the production of other goods or services.", 
        "A bond that can be converted into other securities.", 
        "A bond that is issued together with one or more warrant(s) attached as part of the offer, the warrant(s) granting the holder the right to purchase a designated security, often the common stock of the issuer of the debt, at a specified price.", 
        "A commodity swap is a derivative contract where the value of the contract is derived from an underlying commodity or commodity index. Commodity derivatives can be physically settled or cash settled. Primary underliers include metals, agricultural goods and energy.", 
        "A credit swap references a value or event related to a debt product or debt issuer.", 
        "A foreign exchange swap is a foreign exchange agreement between two parties to exchange a given amount of one currency for another currency for spot delivery or for forward delivery at an agreed rate after a specified period of time.", 
        "A fund of funds is a CIV that invests directly in other investment funds rather than investing in stocks, bonds or other securities.", 
        "A limited partnership is a form of partnership similar to a general partnership, except that in addition to one or more general partners (GPs), there are one or more limited partners (LPs).\nLike shareholders in a corporation, the LPs have limited liability, i.e. they are only liable on debts incurred by the firm to the extent of their registered investment and they have no management authority. The GPs pay the LPs the equivalent of a dividend on their investment, the nature and extent of which is usually defined in the partnership agreement.", 
        "A pension fund is run by a financial intermediary for the company and its employees. The pension fund is a common asset pool meant to generate stable growth over the long term.", 
        "A private equity fund is normally structured as a limited partnership or a limited liability company (investors are limited partners) managed by a GP.", 
        "A rates swap is a contract in which two counterparties each agree to pay the other cash flows on defined dates during an agreed period, based on a specified notional amount and a floating interest, floating inflation or fixed interest rate.", 
        "A simultaneous sale and repurchase agreement entered into by two parties where one party agrees to sell securities or cash to the other party in exchange for collateral. The repurchase terms are generally for the repurchase of the same or equivalent securities at a specified price on an agreed future date. During the term of the agreement, the ownership rights of the securities are transferred.", 
        "A structured instrument without capital protection is a short-term note linked to an underlying stock. The security offers a steady stream of income due to the payment of a coupon rate. The redemption at the end of the term is determined on the basis of the performance and final fixing of the underlying asset: a redemption at the nominal value is guaranteed as long as the underlying asset has not touched its barrier during relevant barrier monitoring. If the underlying asset has touched its barrier but is again above the strike price at final fixing, the nominal price is also repaid. Nevertheless, if the underlying asset has touched its barrier during barrier monitoring and closes below the strike price at final fixing, the underlying asset is delivered or cash compensation paid, provided that no credit event by the reference entity has occurred. Depending on the characteristics of the product, either a coupon or a discount to the underlying asset can apply. A coupon is paid out regardless of the performance of the underlying asset, provided that no credit event by the reference entity has occurred.", 
        "A swap is an agreement or contract where two counterparties agree to exchange periodic streams of cash flows with each other. Swaps can be executed with a variety of asset classes, as listed below.", 
        "Accreting (the notional amount increases through the life of the contract)", 
        "Accumulation funds (the fund normally reinvests its investment profits)", 
        "Adjustable/variable rate income", 
        "Adjustable/variable rate income (the dividend rate is set periodically, usually based on a certain yield)", 
        "Agriculture", 
        "Agriculture (commodities which include forestry, fishing, livestock, grain, dairy, corn, cocoa, soybeans, sugar, coffee)", 
        "Allotment (bonus) rights", 
        "American", 
        "American (an option which allows its holder to exercise the right to buy/sell the specified call/put asset at a specified price at any time during the term of the option, up to and including the expiration date)", 
        "American (warrant that can be exercised at any time between the purchase date and the expiration date)", 
        "American-Call", 
        "American-Call [an option on a contract which allows its holder (buyer) to exercise the right to buy specified assets (interest rates product) at a fixed price at any time during the term of the call option, up to and including the expiration date of the call]", 
        "American-Chooser", 
        "American-Chooser [an option on a contract which allows its holder (buyer) to exercise the right to buy (call) or sell (put) specified assets (interest rates product) at a fixed price at any time during the term of the contract, up to and including the expiration date of the call or put; the buyer does not have to decide whether the contract will be a put or a call until an agreed future date, prior to expiration]", 
        "American-Put", 
        "American-Put [an option on a contract which allows its holder (buyer) to exercise the right to sell specified assets (interest rates product) at a fixed price at any time during the term of the put option, up to and including the expiration date of the put]", 
        "Amortization plan (reduction of principal by regular payments)", 
        "Amortization plan with call feature (the redemption of principal may occur as the result of the outstanding portion of the bond being called)", 
        "Amortization plan with put and call", 
        "Amortization plan with put feature", 
        "Amortizing (the notional amount decreases through the life of the contract)", 
        "An ETF is an investment fund traded on stock exchanges, much like stocks. An ETF holds assets such as stocks, commodities or bonds, and trades close to its net asset value over the course of the trading day. Most ETFs track an index, such as a stock, bond or commodity. index", 
        "An accepted form of money, including coins and paper notes, which is generally issued by a government and circulated within an economy.", 
        "An agreement entered into by two parties where one party (lender) agrees to lend cash or securities to the other party (borrower) in exchange for collateral plus a lending fee. The lender retains the rights (i.e. title, dividend/interest payments and corporate actions that may arise during the term of the loan) to the securities on loan to the borrower.", 
        "An equity swap is a derivative contract where payments are linked to the change in value of an underlying equity (e.g. shares, basket of equities or index). Equity swaps can be physically or cash settled.", 
        "An index provides a representation of the value of its constituents. Indices often serve as barometers for a given market or industry and benchmarks against which financial or economic performance is measured.", 
        "An investment vehicle that is made up of a pool of funds collected from many investors for the purpose of investing in securities such as stocks, bonds, money market instruments and similar assets.", 
        "An option to buy or sell a credit product which is a contract in which one party (protection seller) agrees to provide payment to the other party (protection buyer) should a credit event occur against the underlying, which could be a specified debt (the reference obligation), a specific debt issuer (reference entity), a basket of reference entities and/or reference obligations, or a credit index (reference index).", 
        "An option to buy or sell a foreign exchange agreement between two parties to exchange a given amount of one currency for another currency for spot delivery or for forward delivery at an agreed rate after a specified period of time.", 
        "An option where the holder of the option has the right but not the obligation to enter into the underlying contract, or pay or receive payment related to the underlying rate on a specified future date based on a specified future rate and term.", 
        "An option where the option buyer has the right to buy or sell specified commodities assets at a fixed price or formula, on or before a specified date.", 
        "An option where the underlying asset is an equity-linked instrument (i.e. shares, depository receipts, ETFs, indices, baskets).", 
        "Annually", 
        "Anti-takeover device that gives a prospective acquiree's shareholders the right to buy shares of the firm or shares of anyone who acquires the firm at a deep discount to their fair market value.", 
        "Any interest-bearing or discounted security that normally obliges the issuer to pay the bondholder a contracted sum of money and to repay the principal amount of the debt.", 
        "Arbitrage (in economics and finance, arbitrage is the practice of taking advantage of a price difference between two or more markets, striking a combination of matching deals that capitalize upon the imbalance, the profit being the difference between the market prices)", 
        "Asian", 
        "Asian (an option where either the strike price or the settlement price is the average level of an underlying instrument over a predetermined period; the averaging can be either a geometric or arithmetic average)", 
        "Asset classes", 
        "Asset-backed securities", 
        "Asset-based lending (strategy based on providing loans against assets to companies, including the ones viewed as not being creditworthy by commercial banks; the amount of the loan is secured by claims against the borrower’s assets and as such it is directly determined by the assets' value)", 
        "Assets (indicates the type of assets that the rights holder is entitled to acquire)", 
        "Assets (indicates the underlying assets in which the fund invests)", 
        "Auction", 
        "Auction (an independently administered synthetic auction process on a set of defined deliverable obligations that sets a reference final price that can be used to facilitate cash settlement of all covered transactions following a credit event)", 
        "Auction rate income", 
        "Auction rate income (dividend is adjusted through an auction, such as the Dutch auction)", 
        "Balanced/conservative", 
        "Bank loan (an amount of money loaned at interest by a bank to a borrower, usually on collateral security, for a certain period of time)", 
        "Barrier", 
        "Barrier (an option whose final exercise depends upon the path taken by the price of an underlying instrument; for a “knock-out” barrier option, the option is cancelled if the underlying price crosses a predetermined barrier level; for a “knock-in” barrier option, the option becomes available-for-exercise if the underlying price crosses a predetermined barrier level)", 
        "Barrier capital protection certificate [minimum redemption at expiry equivalent to the capital protection; capital protection is defined as a percentage of the nominal price (e.g. 100 %); capital protection refers to the nominal amount only, and not to the purchase price; the value of the product may fall below its capital protection value during its lifetime; participation is in the underlying price increase above the strike up to the barrier; possibility of rebate payment once barrier is breached; limited profit potential]", 
        "Barrier dependency type (indicates whether the instrument barrier depends on the underlying level or on the instrument trading price level)", 
        "Barrier discount certificate [the maximum redemption amount (Cap) is paid out if the barrier is never breached; barrier discount certificates enable investors to acquire the underlying asset(s) at a lower price; due to the barrier, the probability of maximum redemption is higher; the discount, however, is smaller than for a discount certificate; if the barrier is breached the product changes into a discount certificate; it has reduced risk compared to a direct investment into the underlying asset; limited profit potential (Cap); with higher risk levels multiple underlying assets (worst-of) allow for higher discounts or a lower barrier]", 
        "Barrier instrument based (the instrument immediately expires if the barrier instrument trading price level is breached during product lifetime)", 
        "Barrier reverse convertible [should the barrier never be breached, the nominal price plus coupon is paid at redemption; due to the barrier, the probability of maximum redemption is higher; the coupon, however, is smaller than for a reverse convertible; if the barrier is breached the product changes into a reverse convertible; the coupon is paid regardless of the underlying development; it has reduced risk compared to a direct investment into the underlying asset(s); with higher risk levels, multiple underlying assets (worst-of) allow for higher coupons or lower barriers; limited profit potential (Cap)]", 
        "Barrier underlying based (the instrument immediately expires if the barrier underlying level is breached during product lifetime)", 
        "Basis swap (float-float) [a rate swap where the cash flows that are exchanged between each party are based on different floating interest rates or prices (i.e. one party pays an agreed floating rate multiplied by a notional amount, in exchange for receipt of periodic payments based on another agreed floating rate multiplied by the same notional amount, from the other party)], except those swaps covered by the definitions below for attributes G or H", 
        "Basket", 
        "Basket (a bespoke, synthetic portfolio of underlying assets whose components have been agreed to for a specific OTC derivative by the parties to the transaction)", 
        "Basket (a defined basket of instruments)", 
        "Basket (an option on a contract that may be exercised based on the weighted average performance of several underlying equities instruments)", 
        "Basket – multi-commodity (a custom basket containing constituents from two or more of the underlying assets identified for this attribute)", 
        "Basket – single-commodity (a custom basket containing constituents from one of the underlying assets identified for this attribute)", 
        "Baskets", 
        "Baskets (group of securities that have been put together for a specific investment purpose)", 
        "Baskets (the warrant holder is entitled to acquire a package or group of assets)", 
        "Bearer", 
        "Bearer (the owner is not registered in the books of the issuer or of the registrar)", 
        "Bearer/registered", 
        "Bearer/registered (securities are issued in both bearer and registered form but with the same identification number)", 
        "Bermudan", 
        "Bermudan (an option which allows its holder to exercise the right to buy/sell the specified call/put asset at a specified price on a number of specific dates within the exercise period of the option)", 
        "Bermudan (an option which allows its holder to exercise the right to buy/sell the specified call/put currency at a specified price on a number of specific dates within the exercise period of the option)", 
        "Bermudan (warrant that can only be exercised on predetermined dates, usually every month)", 
        "Bermudan-Call", 
        "Bermudan-Call [an option on a contract which allows its holder (buyer) to exercise the right to buy specified assets (interest rates product) at a fixed price on a number of specific dates within the exercise period of the call]", 
        "Bermudan-Chooser", 
        "Bermudan-Chooser [an option on a contract which allows its holder (buyer) to exercise the right to buy (call) or sell (put) specified assets (interest rates product) at a fixed price on a number of specific dates within the exercise period of the contract; the buyer does not have to decide whether the contract will be a put or a call until an agreed future date, prior to expiration]", 
        "Bermudan-Put", 
        "Bermudan-Put [an option on a contract which allows its holder (buyer) to exercise the right to sell specified assets (interest rates product) at a fixed price on a number of specific dates within the exercise period of the put]", 
        "Bond issued by a state, provincial, city or local government excluding municipal money market securities, which shall be classified as debt, money market instruments (see money market instruments).", 
        "Bonds", 
        "Bonds with warrants attached", 
        "Bonus certificate [participation in development of the underlying asset(s); minimum redemption is equal to the nominal value provided the barrier has not been breached; if the barrier is breached the product changes into a tracker certificate; with greater risk multiple underlying asset(s) (worst-of) allow for a higher bonus level or lower barrier; reduced risk compared to a direct investment into the underlying asset(s)]", 
        "Bullion coins (coins struck from precious metals and kept as a store of value or an investment, rather than used in day-to-day commerce)", 
        "CDS on a basket (where the underlying risk is multiple reference entities or multiple reference obligations)", 
        "CDS on a single name", 
        "CDS on a single name (a CDS where the underlying risk is a single reference entity or single reference obligation)", 
        "CDS on an index", 
        "CDS on an index [family of standardized credit derivative indices, where the underlying reference entities are a defined basket of credit from a particular geographic region (e.g. Asia, North America, Europe), and/or credit rating level (e.g. emerging markets, high yield, investment grade); credit default indices trade in standard maturities, and the reference entities are typically the most liquid; the reference portfolio is reassessed periodically to maintain this]", 
        "CDS on an index tranche (a synthetic CDO based on a CDS index where each tranche references a different segment of the loss distribution of the underlying CDS index; each tranche has a different priority of claims on the principal and interest flows from the collateral pool, and is traditionally portioned into rising levels of seniority)", 
        "CFD", 
        "CIVs", 
        "CIVs which do not fit into any of the above Groups.", 
        "Call (in most cases, the warrant entitles the holder to acquire specific underlying assets during a specified period at a specified price)", 
        "Call and put (warrants with neither call nor put feature or warrants with call and put feature)", 
        "Call options", 
        "Call/put (indicates whether the warrant entitles the holder to acquire assets at specified terms or to acquire cash in exchange for specific underlying assets)", 
        "Cap (an option in which the payment is triggered when the value of the underlier exceeds a specified level)", 
        "Capital protected structured instruments offer investors exposure to chosen underlying assets using various approaches and offering a large variety of asymmetric pay-off profiles. There are one or more reference entities underlying the product. Redemption is made at least in the amount of the conditional capital protection at maturity, provided that no credit event by the reference entity has occurred. Conditional capital protection only applies to the nominal amount and not to the purchase price. The general functioning of a capital guaranteed structured instrument is as follows: the notional amount is split into a zero bond, that will deliver the capital guarantee at maturity, and the difference between the zero bond’s value (= present value of the guarantee level at maturity) and the notional amount is used for structuring the performance component with options which deliver the agreed pay-off profile of the structured instrument.", 
        "Capital protection certificate with coupons [minimum redemption at expiry equivalent to the capital protection; capital protection is defined as a percentage of the nominal price (e.g. 100 %); capital protection refers to the nominal amount only, and not to the purchase price; the value of the product may fall below its capital protection value during its lifetime; the coupon amount is dependent on the development of the underlying asset; periodic coupon payment is expected]", 
        "Capital protection certificate with participation [minimum redemption at expiry equivalent to the capital protection; capital protection is defined as a percentage of the nominal amount (e.g. 100 %); capital protection refers to the nominal amount only, and not to the purchase price; the value of the product may fall below its capital protection value during its lifetime; participation is in the underlying price increase above the strike]", 
        "Capital protection convertible certificate [minimum redemption at expiry equivalent to the capital protection; capital protection is defined as a percentage of the nominal price (e.g. 100 %); capital protection refers to the nominal price only, and not to the purchase price; the value of the product may fall below its capital protection value during its lifetime; participation is in the underlying price increase above the conversion price; coupon payment is possible]", 
        "Capitalization weighted (index whose components are weighted according to the total market value of their outstanding shares)", 
        "Carbon credit (certificate or permit representing the right to emit carbon dioxide)", 
        "Cash", 
        "Cash (on settlement date of the trade, if the net settlement amount is positive, then the currency buyer will pay that amount in the settlement currency to the currency seller; if that amount is negative, the seller will make that payment to the buyer)", 
        "Cash (the contract will settle as cash on the performance of the contract at maturity)", 
        "Cash (the discharge of an obligation by payment or receipt of a net cash amount instead of payment or delivery by both parties)", 
        "Cash collateral", 
        "Cash payment", 
        "Cash payment (this attribute applies only for sukuk certificates; a sukuk takes place when a set of investors pool their wealth to invest in accordance with sharia principles to earn profits which are then distributed pro rata)", 
        "Cash repayment", 
        "Certificate of deposit", 
        "Closed (pension funds supporting only pension plans that are limited to certain employees)", 
        "Closed-end [units are sold on either an organized exchange or in the over-the-counter (OTC) market and are usually not redeemed]", 
        "Closed/open", 
        "Closed/open-end (indicates whether units are traded or whether funds continually stand ready to sell new units and redeem the outstanding units on demand)", 
        "Combination of bonds (with different characteristics)", 
        "Combination of shares (with different characteristics)", 
        "Combined instruments", 
        "Commercial property (or property derivative)", 
        "Commercial property (or property derivative) [a derivative where the underlying is commercial property; property derivatives are mostly in the form of swaps where one party pays the return on the index if positive versus the other party paying LIBOR (London interbank offered rate)]", 
        "Commodities", 
        "Commodities (the warrant holder is entitled to acquire a specific commodity)", 
        "Commodities futures", 
        "Commodity derivative strategies are the simultaneous trading of two or more commodity contracts where the value of the contract is derived from an underlying commodity or commodity index. Commodity derivatives can be physically or cash settled. Primary underliers include metals, agricultural goods and energy.", 
        "Common/ordinary convertible shares", 
        "Common/ordinary shares", 
        "Components", 
        "Composition", 
        "Constant (the notional amount is constant through the life of the contract)", 
        "Contract for difference (CFD) (a cash-settled total return swap or forward where the parties agree to exchange on the maturity of the contract the difference between the opening price and closing price of the underlying)", 
        "Contracts between a buyer and a seller giving the buyer (holder) the right, but not the obligation, to buy the assets specified at a fixed price or formula, on or before a specified date. The seller of the call option assumes the obligation of delivering the assets specified should the buyer exercise his or her option.", 
        "Contracts between a buyer and a seller giving the buyer (holder) the right, but not the obligation, to sell the assets specified at a fixed price or formula, on or before a specified date. The seller of the put option assumes the obligation of buying the assets specified should the buyer exercise his or her option.", 
        "Contracts conducted on the spot market which are bought and sold for cash with immediate delivery based on market convention for the asset.", 
        "Contracts, listed on an exchange or regulated market, which obligate the buyer to receive and the seller to deliver in the future the assets specified at an agreed price. This includes forwards on regulated markets.", 
        "Contracts, which are not exchange traded or listed, entered between two parties to buy or sell the underlying asset at a specified future date at the price specified at the outset of the contract.", 
        "Convertible", 
        "Convertible bonds", 
        "Convertible securities", 
        "Convertible/redeemable", 
        "Corporate bonds", 
        "Corporate entity [the underlying exposure is a corporate (a private sector entity)]", 
        "Covered warrants (issued by a third party which is not the issuer of the underlying securities to which the warrant refers; warrant issuer holds as many securities as would be required if all the warrants are exercised)", 
        "Credit", 
        "Credit default [a credit default swap (CDS) is a contract in which one party (protection seller) agrees to provide payment to the other party (protection buyer) should a credit event occur against the underlying, which could be a specified debt (the reference obligation), a specific debt issuer (reference entity), a basket of reference entities and/or reference obligations or a credit index (reference index)]", 
        "Credit derivative strategies are the simultaneous trading of two or more credit default contracts in which one party (protection seller) agrees to provide payment to the other party (protection buyer) should a credit event occur against the underlying, which could be a specified debt (the reference obligation), a specific debt issuer (reference entity), a basket of reference entities and/or reference obligations, or a credit index (reference index).", 
        "Credits [contractual agreement in which a borrower receives something of value (good, service or money) now and agrees to repay the lender at some date in the future, generally with interest; CIVs normally invest in credits originated by third parties; credits are not freely transferable like debt securities]", 
        "Cross-currency (multi-currency)", 
        "Cumulative, fixed rate income", 
        "Cumulative, fixed rate income (the shareholder periodically receives a stated amount; dividends not paid in any year accumulate and shall be paid at a later date before dividends can be paid on the common/ordinary shares)", 
        "Cumulative, participating income", 
        "Cumulative, participating income (shareholders are entitled to dividends in excess of the stipulated preferential rate under specified conditions; dividends not paid in any year accumulate and shall be paid at a later date before dividends can be paid on the common/ordinary shares)", 
        "Currencies", 
        "Currencies (specified exchange rate)", 
        "Currencies (the warrant holder is entitled to acquire a specified amount in a certain currency at a specified exchange rate)", 
        "Custom (customized notional step schedule)", 
        "Daily", 
        "Debt instruments", 
        "Debt instruments (fund invests in debt instrument regardless of maturity)", 
        "Debt instruments backed by receivables other than those arising out of real estate, loans or mortgages.", 
        "Debt instruments that do not fit into any of the above Groups.", 
        "Debt instruments/interest rates (the warrant holder is entitled to acquire debt instruments)", 
        "Defined benefit", 
        "Defined contribution", 
        "Deliverable (the settlement, i.e. payment, currency amounts are paid in the respective reference currency for each leg of the swap for which the payments are being made)", 
        "Delivery", 
        "Delivery (delivery method for the collateral at the outset of the agreement)", 
        "Delivery (indicates whether the payment currency for each leg of the swap is the same as the reference currency for that leg)", 
        "Delivery (indicates whether the settlement of the future is made in cash or whether the underlying instruments are delivered)", 
        "Delivery (indicates whether the settlement of the option, when exercised, is made in cash or whether the underlying instruments are delivered)", 
        "Delivery versus payment", 
        "Delivery versus payment (the borrower delivers the collateral to the lender against payment of funds within a securities settlement system)", 
        "Depositary receipts on debt instruments", 
        "Depositary receipts on entitlements", 
        "Depositary receipts on equities", 
        "Depository receipts are securities that facilitate the ownership of instruments traded in other jurisdictions. Depository receipts are widely used in order to allow the trading of debt instruments in jurisdictions other than the one where the original debt instruments were issued.", 
        "Depository receipts are securities that facilitate the ownership of instruments traded in other jurisdictions. Depository receipts are widely used in order to allow the trading of entitlements in jurisdictions other than the one where the original entitlements were issued.", 
        "Depository receipts are securities that facilitate the ownership of securities traded in other jurisdictions. Depository receipts are widely used in order to allow the trading of shares in jurisdictions other than the one where the original shares were issued.", 
        "Derivatives", 
        "Derivatives (options, futures, swaps, spot, forwards, strategies, financing)", 
        "Digital (Binary)", 
        "Digital (Binary) (an option that has a pre-determined payout if the option is in-the-money and the payoff condition is satisfied; also referred to as a “binary option” or an “all-or-nothing option”)", 
        "Digital barrier", 
        "Digital barrier (a digital option embedded with a barrier option; there are different variations of this type of option; as an example, a down-and-out digital call option will pay a fixed payoff, or the underlying, at any time before maturity that the underlying price is equal to or greater than the barrier level; it will pay zero if the underlying price is less than the barrier level)", 
        "Directional [the two biggest constituents of directional are macro and commodity trading advisor (CTA)/managed futures; macro describes directional strategies that are based upon the direction of market prices of currencies, commodities, equities, fixed income and includes futures and cash markets; CTA/managed futures describe strategies that are based upon futures contracts across all asset classes only]", 
        "Discount certificate [should the underlying asset close below the strike on expiry, the underlying asset(s) and/or a cash amount is redeemed; discount certificates enable investors to acquire the underlying asset at a lower price; it corresponds to a buy-write-strategy; it has reduced risk compared to a direct investment into the underlying asset; with higher risk levels multiple underlying assets (worst-of) allow for higher discounts; limited profit opportunity (Cap)]", 
        "Distribution (indicates the cash distribution provided by the structured instrument)", 
        "Distribution policy (indicates the fund's normal distribution policy)", 
        "Dividend (a fixed-term contract between two parties where one party will make an interest rate payment for each interval and the other party will pay the total dividends received as pay-out by a selected underlying asset)", 
        "Dividend payments", 
        "Dividend payments (depending on strategy of the structured instrument)", 
        "Dividends", 
        "ETFs", 
        "Elect at exercise", 
        "Elect at exercise (the method of delivery of the underlying instrument when the option is exercised shall be determined at the time of exercise)", 
        "Elect at settlement (determined at the time of settlement)", 
        "Energy", 
        "Energy [an energy-related product, or a derivative of an energy-related product, including electricity, renewable energy, or any power/energy delivered through a utility network of provider; diesel fuel, fuel oil, gas oil, gasoline, heating oil, jet fuel, kerosene, natural gas, oil (Brent, Tapis, Dubai, WTI)]", 
        "Enhanced voting (the shareholder is entitled to more than one vote per share)", 
        "Entitlement (rights)", 
        "Entitlements (rights) that do not fit into any of the above Groups described.", 
        "Environmental", 
        "Environmental (e.g. carbon credits and similar products)", 
        "Environmental (includes carbon-related, emission reduction, weather)", 
        "Equal weighted (an equal-weighted index uses a type of weighting that gives the same weight or importance to each stock in a portfolio or index fund)", 
        "Equities", 
        "Equities (the warrant holder is entitled to acquire equity)", 
        "Equities that do not fit into any of the other Equity Groups", 
        "Equity", 
        "Equity derivative strategies are the simultaneous trading of two or more equity contracts where payments are linked to the change in value of an underlying equity (e.g. shares, basket of equities or index). The equity return payer pays to the equity return receiver any increase in the value of the underlying plus any dividends received. The equity return receiver pays the equity return payer any decrease in the value of the underlying plus funding cost.", 
        "Escrow receipts (bank guarantee that an option writer has the underlying security on deposit and that the underlying security is readily available for delivery if the option is exercised)", 
        "European", 
        "European (an option which allows its holder to exercise the right to buy/sell the specified call/put asset at a specified price only on the expiration date)", 
        "European (an option which allows its holder to exercise the right to buy/sell the specified call/put currency at a specified price only on the expiration date", 
        "European (warrant that can only be exercised for a short, specified period of time just prior to its expiration, usually a single day)", 
        "European-Call", 
        "European-Call [an option on a contract which allows its holder (buyer) to exercise the right to buy specified assets (interest rates product) at a fixed price only on the expiration date of the call]", 
        "European-Chooser", 
        "European-Chooser [an option on a contract which allows its holder (buyer) to exercise the right to buy (call) or sell (put) specified assets (interest rates product) at a fixed price, only on the contract's expiration date; the buyer does not have to decide whether the contract will be a put or a call until an agreed future date, prior to expiration]", 
        "European-Put", 
        "European-Put [an option on a contract which allows its holder (buyer) to exercise the right to sell specified assets (interest rates product) at a fixed price only on the expiration date of the put]", 
        "Event-driven (combination of investment strategies focusing on securities that are expected to experience a change in valuation due to corporate transactions or events such as bankruptcies)", 
        "Exchange traded funds (ETFs)", 
        "Exchangeable (the shares may be exchanged for securities of another issuer)", 
        "Exercise option style", 
        "Express certificate [should the underlying trade above the strike on the observation date, an early redemption consisting of nominal price plus an additional coupon amount is paid; it offers the possibility of an early redemption combined with an attractive yield opportunity; it has reduced risk compared to a direct investment into the underlying asset(s); with higher risk levels, multiple underlying assets (worst-of) allow for higher coupons or lower barriers; limited profit opportunity (Cap)]", 
        "Extendible", 
        "Extendible (the redemption date can be extended at the issuer or holder option)", 
        "Extraction resources (metals, precious metals, coal, oil, gas)", 
        "Fertilizer", 
        "Fertilizer [ammonia, diammonium phosphate (DAP), potash, sulphur, urea, urea and ammonium nitrate (UAN)]", 
        "Financial futures", 
        "Financial instruments designated at issuance as such with a short-term life, for instance treasury bills and commercial paper including municipal money market instruments.", 
        "Financial instruments evidencing monies owed by the issuer to the holder on terms as specified.", 
        "Financial instruments providing the holder with the privilege to subscribe to or receive specific assets on terms specified.", 
        "Financial instruments representing an ownership interest in an entity or pool of assets.", 
        "Financial instruments that do not fit the above categories as defined.", 
        "Financial instruments which permit the holder to purchase a specified amount of a financial instrument, commodity, currency or other during a specified period at a specified price.", 
        "Financing", 
        "Financing is a collateralized loan agreement entered into between two parties where one party, the lender, lends (temporarily) the underlying asset which is secured with cash or other acceptable collateral (securities or other assets) provided by the borrower. Depending on the exact type of financing transaction, a simultaneous agreement to reverse the agreement may be entered into at the same time with an agreed-upon future date for the reverse transaction to take place.", 
        "Fixed (an interest rate that remains fixed for the entire term)", 
        "Fixed cash repayment (only protected capital level)", 
        "Fixed interest payments", 
        "Fixed maturity (the principal amount is repaid in full at maturity)", 
        "Fixed maturity with call feature (the issue may be called for redemption prior to the fixed maturity date)", 
        "Fixed maturity with put and call", 
        "Fixed maturity with put feature (the holder may request the reimbursement of his or her bonds prior to the maturity date)", 
        "Fixed rate", 
        "Fixed rate (all interest payments are known at issuance and remain constant for the life of the issue)", 
        "Fixed rate income", 
        "Fixed rate income (the shareholder periodically receives a stated income)", 
        "Fixed rate income (the shareholder periodically receives a stated income)", 
        "Fixed-fixed (a rate swap in which both parties pay a fixed interest rate that they could not otherwise obtain outside of a swap arrangement; for example, if each counterparty uses a different native currency, but wants to borrow money in the other counterparty’s native currency; fixed-fixed swaps generally take the form of either a zero coupon swap or a cross-currency swap), except those swaps covered by the definitions below for attributes G or Z", 
        "Fixed-floating [a rate swap in which one party (the fixed rate payer) agrees to make fixed payments (the fixed leg) on set dates for an agreed period to another party (the floating rate payer), based on a fixed interest rate multiplied by a notional amount, in exchange for receipt of periodic payments (the floating leg), from the floating rate payer, based on a floating interest rate index multiplied by the same notional amount (in most cases) upon which the fixed rate payments are based], except those swaps covered by the definitions below for attributes G, H or Z", 
        "Flexible (the agreement has flexible term duration of up to 120 days)", 
        "Floor (an option in which the payment is triggered when the value of the underlier falls below a specified level)", 
        "Foreign exchange", 
        "Foreign exchange derivative strategies are the simultaneous trading of two or more foreign exchange agreements between two parties to exchange a given amount of one currency for another currency for spot delivery or for forward delivery at an agreed rate after a specified period of time.", 
        "Form", 
        "Form (negotiability, transmission)", 
        "Forward contracts to buy or sell the commodity asset.", 
        "Forward contracts to buy or sell the credit product.", 
        "Forward contracts to buy or sell the financial rates product.", 
        "Forward contracts to buy or sell the foreign currency.", 
        "Forward contracts to buy or sell the underlying equity stock, equity index or basket of equity stock.", 
        "Forward price of underlying instrument", 
        "Forward price of underlying instrument (the agreed-upon price for the time of delivery)", 
        "Forward – Currency Index", 
        "Forward – Custom Basket of Currencies", 
        "Forward – Single Currency Pair", 
        "Forward-forward swap (a transaction that involves both an exchange of two currencies on a specified future date at a fixed rate that is agreed upon at the inception of the contract covering the exchange; and a reverse exchange of the same two currencies at a further future date, at a fixed rate that is agreed upon at the inception of the contract covering the exchange, e.g. a swap between the 3-month forward and 6-month forward dates)", 
        "Forwards", 
        "Forwards – Currency Index", 
        "Forwards – Custom Basket of Currencies", 
        "Forwards – Single Currency Pair", 
        "Free (unrestricted)", 
        "Free of payment (the borrower delivers the collateral with no corresponding payment of funds)", 
        "Freight", 
        "Freight (the specified commodity is a freight index route)", 
        "Frequency of calculation", 
        "Fully paid", 
        "Fund unit and other components", 
        "Funds of funds", 
        "Further grouping", 
        "Futures", 
        "Futures contracts based on bulk goods.", 
        "Futures contracts based on underlying assets excluding commodities.", 
        "Futures – Currency Index", 
        "Futures – Custom Basket of Currencies", 
        "Futures – Single Currency Pair", 
        "General collateral [the repurchase agreement is secured by a range of acceptable high-quality and liquid assets, or a general collateral basket of acceptable general collateral securities as prescribed by a repo trading system or the central counterparty (CCP); the securities may be substituted with other acceptable securities, as determined by the seller]", 
        "Generated resources (includes electricity, renewable energy, or any power/energy delivered through a utility network or provider)", 
        "Government bonds", 
        "Government guarantee [the debt instrument is guaranteed by a federal, state, (semi)-government, sovereigns, agencies]", 
        "Gross total return (a gross total return is the rate of return on an investment portfolio, where the return measure takes into account all gross cash distributions made to the components of an index)", 
        "Group of securities that have been put together for a specific investment purpose.", 
        "Growth", 
        "Guarantee or ranking (indicates, in the case of the issuer's inability to settle, whether the debt issue is additionally secured\nGuideline: the values N (senior), O (senior subordinated), Q (junior) and J (junior subordinated) may only be used for unsecured securities. P (negative pledge) may only be used for unsecured securities that are neither senior nor junior. U (unsecured) may be used only if one of these codes does not apply to the relevant security.)", 
        "Guarantee or ranking \nGuideline: the values N (senior), O (senior subordinated), Q (junior) and J (junior subordinated) may only be used for unsecured securities. P (negative pledge) may only be used for unsecured securities that are neither senior nor junior. U (unsecured) may be used only if one of these codes does not apply to the relevant security.", 
        "Hedge funds", 
        "Hold-in-custody", 
        "Hold-in-custody (the borrower holds the collateral in a segregated customer account, in custody, for the lender)", 
        "Holders are typically entitled to vote and receive dividends. In the event of liquidation, holders of shares usually rank behind the entity's creditors and holders of preferred/preference shares.", 
        "Income (indicates the kind of dividend income the shareholders are entitled to)", 
        "Income funds (the fund regularly distributes its investment profits)", 
        "Index", 
        "Index (a synthetic portfolio of underlying assets whose components have been set by a third-party administrator)", 
        "Index (an option on a contract which gives the holder the right to buy, and to sell, specified equity indices)", 
        "Index [family of standardized credit derivative indices, where the underlying reference entities are a defined basket of credit from a particular geographic region (e.g. Asia, North America, Europe), and/or credit rating level (e.g. emerging markets, high yield, investment grade); credit default indices trade in standard maturities, and the reference entities are typically the most liquid; the reference portfolio is reassessed periodically to maintain this]", 
        "Index return types", 
        "Index tranche [a synthetic collateralized debt obligation (CDO) based on a credit index where each tranche references a different segment of the loss distribution of the underlying index; each tranche has a different priority of claims on the principal and interest flows from the collateral pool, and is traditionally portioned into rising levels of seniority]", 
        "Index – multi-commodity (an index containing constituents from two or more of the underlying assets identified for this attribute)", 
        "Index – multi-commodity (an index containing constituents from two or more of the underlying assets identified for this attribute)", 
        "Index – single-commodity (an index containing constituents from one of the underlying assets identified for this attribute)", 
        "Index – single-commodity (an option where the underlying reference entity is a commodity index)", 
        "Indicators that are used as a reference for other financial instruments.", 
        "Indices", 
        "Indices (the performance of an index)", 
        "Indices (the warrant holder is entitled to acquire a specified amount based on the performance of an index)", 
        "Industrial products (construction, manufacturing)", 
        "Inflation swap (a rate swap in which one party pays an amount calculated using an inflation rate index, and the other party pays an amount calculated using another inflation rate index, or a fixed or floating interest rate)", 
        "Instrument dependency", 
        "Instrument dependency (represents the ownership of an instrument provided in this table)", 
        "Insurance policies", 
        "Interest rate derivative strategies are the simultaneous trading of two or more rate contracts in which two counterparties agree to exchange interest rate cash flows on defined dates during an agreed period, based on a specified notional amount, from a fixed rate to a floating rate, floating to fixed, or floating to floating.", 
        "Interest rate index", 
        "Interest rates", 
        "Interest rates (specified amount based on the future level of interest rates)", 
        "Investment strategy  (the investment process describes core hedge fund strategy characteristics)", 
        "Joint guarantee [the debt instrument is guaranteed by an entity (e.g. corporation) other than the issuer; not a federal or state government]", 
        "Junior (applies to junior debts that are placed before junior subordinated in the ranking in the event of liquidation)", 
        "Junior subordinated (applies to junior subordinated debts in the ranking in the event of liquidation)", 
        "Legacy currency (national currency that ceased to be a legal tender)", 
        "Letter of credit", 
        "Life style (strategy changes depending on age group of members)", 
        "Limited partnership units", 
        "Listed options", 
        "Loan-lease", 
        "Local (a municipality or local government authority)", 
        "Long (in most cases, the instrument entitles the holder to acquire specific underlying assets during a specified period at a specified price)", 
        "Long/short (indicates whether the instrument entitles the holder to acquire assets at specified terms or to acquire cash in exchange for specific underlying assets)", 
        "Lookback", 
        "Lookback (an option that minimizes the uncertainties related to the timing of market entry; there are two types of lookback options: fixed and floating; the fixed option strike is determined at purchase, and the floating option strike is determined at maturity)", 
        "Medium-term notes", 
        "Metals", 
        "Metals (a precious or industrial metal, such as aluminium, copper, gold, lead, nickel, platinum, silver, tin, zinc)", 
        "Mini-future certificates, constant leverage certificates", 
        "Mini-futures combine the structure of open-end certificates with leverage option. Mini-futures have no fixed term. The leverage is therefore available without a term restriction. The price of a mini-future always corresponds to its intrinsic value, i.e. the capital outlay, plus the bid-ask spread. The financing costs associated with building up the leverage effect are offset against the capital outlay on a daily basis, thereby eliminating the need for a premium. Investors have to pay only financing costs they actually utilize. In contrast to options, factors like volatility have no influence at all on the price of mini-futures.", 
        "Mixed (fund invests in different assets)", 
        "Mixed asset derivative strategies are the simultaneous trading of two or more contracts of different asset types between two parties.", 
        "Mixed assets", 
        "Mixed funds (investment profits are partly distributed, partly reinvested)", 
        "Modified market capitalization weighted (a modified market cap weighted index is a hybrid between equal weighting and capitalization weighting; it is similar to a general market cap with one main difference: the largest stocks are capped to a percentage of the weight of the total stock index and the excess weight will be redistributed equally among the stocks under that cap)", 
        "Money market instruments", 
        "Monthly", 
        "Mortgage-backed securities", 
        "Mortgage-backed securities are debt obligations that represent claims to the cash flows from pools of mortgage loans, most commonly on residential property. Mortgage loans are purchased from banks, mortgage companies and other originators, and then assembled into pools by a governmental, quasi-governmental or private entity. The entity then issues securities that represent claims on the principal and interest payments made by borrowers on the loans in the pool, a process known as securitization.", 
        "Multi-commodity (each leg of the swap references a different respective commodity than the other leg)", 
        "Multi-strategy (multi-strategy as a separate set of investment strategies is broad and by it the manager is expected to maintain approximately 25 % of portfolio exposure in two or more strategies that are distinct from one another)", 
        "Municipal bonds", 
        "Naked warrants (issued by a third party which is not the issuer of the underlying securities to which the warrant refers; warrant issuer does not hold as many securities as would be required if all the warrants are exercised)", 
        "National currency (the currency or legal tender issued by a nation's central bank or monetary authority; the national currency of a nation is usually the predominant currency used for most financial transactions in that country according to ISO 4217)", 
        "Negative pledge (the borrower agrees not to pledge any assets if such pledging would result in less security for the agreement's bondholders)", 
        "Negotiable debt instruments offered under a program agreement through one or more dealers upon request of the issuer. The program defines the terms and conditions of the notes.", 
        "Net total return (a net total return is the rate of return on an investment portfolio, where the return measure takes into account all net cash distributions made to the components of an index. Net cash distributions are cash distributions after any withholding tax)", 
        "Nil paid", 
        "No payments", 
        "Nominal (an interest rate before taking inflation into account)", 
        "Non-deliverable", 
        "Non-deliverable (the settlement, i.e. payment, currency amounts are paid in a single currency that either reflects the currency of either leg of the swap or in a currency other than the respective reference currency for each leg of the swap for which the payments are being made)", 
        "Non-deliverable [synthetic options on foreign exchange forwards that are based on non-convertible or thinly traded currencies]", 
        "Non-listed and complex listed options", 
        "Non-standardized (options traded on option exchanges which have non-standard delivery or expiry terms)", 
        "Non-voting (the shareholder has no voting right)", 
        "Normal rate income", 
        "Normal rate income (shareholders are entitled to the same dividends as common/ordinary shareholders, but have other privileges, for example as regards distribution of assets upon dissolution)", 
        "Not applicable/undefined", 
        "Notional (indicates the face amount of a swap upon which the payment streams for that swap are based)", 
        "One party lends a commodity to a counterparty, in return for a fixed price or a premium (aka lease rate). At the end of the contract, the lender expects to receive the commodity back, and their return is linked to fluctuations in the market price of the commodity, or the premium, respectively.", 
        "Open", 
        "Open (pension funds supporting at least one pension plan with no restriction on membership)", 
        "Open (the agreement has no fixed termination date)", 
        "Open-end (funds permanently sell new units to the public and redeem outstanding units on demand, resulting in an increase or decrease of outstanding capital)", 
        "Option style and type", 
        "Options", 
        "Options that do not fit into any of the above Groups.", 
        "Options – Currency Index", 
        "Options – Custom Basket of Currencies", 
        "Options – Single Currency Pair", 
        "Other OTC derivative products", 
        "Other assets (miscellaneous)", 
        "Other path dependent", 
        "Other path dependent (an option on a contract whose payoff is directly related to the price pattern the underlying asset follows during the life of the contract)", 
        "Other weighting (miscellaneous)", 
        "Others (miscellaneous)", 
        "Outperformance bonus certificate [participation in development of the underlying asset(s); disproportionate participation (outperformance) in positive performance above the strike; minimum redemption is equal to the nominal value provided the barrier has not been breached; if the barrier is breached the product changes into an outperformance certificate; with greater risk multiple underlying asset(s) (worst-of) allow for a higher bonus level or lower barrier; reduced risk compared to a direct investment into the underlying asset(s)]", 
        "Outperformance certificate [participation in development of the underlying asset(s); disproportionate participation (outperformance) in positive performance above the strike; reflects underlying price moves 1:1 (adjusted by conversion ratio and any related fees); risk is comparable to direct investment in the underlying asset(s)]", 
        "Overnight", 
        "Overnight (the term of the agreement is one day)", 
        "Overnight index swap (OIS) [a rate swap in which one party (the fixed rate payer) makes periodic payments to another party (the floating rate payer) based on a fixed interest rate (other than zero) or floating interest rate multiplied by a notional amount in exchange for receipt of periodic payments based on an overnight interest rate index multiplied by the same notional amount upon which the fixed rate payments are based]", 
        "Ownership/transfer/sales restrictions (the ownership or transfer of the security is subject to special conditions including country-specific restrictions)", 
        "Packages of different financial instruments issued and/or traded as one single unit (also referred to as staple security). They can be separated during their life cycle and be traded individually. In that case, CFI codes classifying each of the financial instruments involved individually shall be assigned.", 
        "Paper", 
        "Paper (containerboard, newsprint, pulp, recovered paper)", 
        "Participating income", 
        "Participating income (preferred/preference shareholders, in addition to receiving their fixed rate of prior dividend, share with the common shareholders in further dividend distributions and in capital distributions)", 
        "Partly paid", 
        "Payment in kind", 
        "Payment in kind (pays interest using other assets instead of cash)", 
        "Payment of dividends to holders normally takes preference over the payment of dividends to other classes of shares. In the event of liquidation, preferred/preference shares normally rank above ordinary shares but behind creditors of the company.", 
        "Payment status", 
        "Pension funds", 
        "Perpetual", 
        "Perpetual (the debt instrument has no fixed maturity date and is only due for redemption in the case of the issuer's liquidation)", 
        "Perpetual (the share has no fixed maturity date)", 
        "Perpetual with call feature (the issue may be called for redemption at some time in the future)", 
        "Perpetual with put feature (the issue may be puttable for redemption at some time in the future)", 
        "Physical", 
        "Physical (delivery of traded currencies on settlement date)", 
        "Physical (the contract will settle with the delivery of shares on the performance of the contract at maturity)", 
        "Physical [the meeting of a settlement obligation under a derivative contract through the receipt or delivery of the actual underlying instrument(s) instead of through cash settlement]", 
        "Physical repayment", 
        "Polypropylene products", 
        "Polypropylene products (includes plastics)", 
        "Precious metal receipts", 
        "Preferred/preference convertible shares", 
        "Preferred/preference shares", 
        "Preferred/preference shares which, at the discretion of the holder, are convertible into other securities, usually common/ordinary shares, at a designated rate. The conversion privilege may be perpetual or limited to a specified period.", 
        "Price (price return equity swap; similar to a total return swap, except that dividends are not passed through to the buyer)", 
        "Price return (the price return is the rate of return on an investment portfolio, where the return measure takes into account only the capital appreciation of the portfolio, while the income generated by the assets in the portfolio, in the form of interest and dividends, is ignored)", 
        "Price weighted (market index where each constituent makes up a fraction of the index that is proportional to its price; for a stock market index, this implies that stocks are included in proportions based on their quoted prices)", 
        "Private equity funds", 
        "Privileges allotted to existing security holders, entitling them to receive new securities free of charge.", 
        "Privileges allotted to existing security holders, entitling them to subscribe to new securities at a price normally lower than the prevailing market price.", 
        "Promissory note (written promise by one party to pay another party a definite sum of money either on demand or at a specified future date)", 
        "Purchase rights", 
        "Put (the warrant entitles the holder to acquire cash in exchange for specific underlying assets)", 
        "Put options", 
        "Quarterly", 
        "REITs", 
        "Rate at which interest is paid by a borrower for the use of money that they borrow from a lender.", 
        "Rates", 
        "Real (an interest rate that has been adjusted to remove the effects of inflation)", 
        "Real estate", 
        "Real estate deeds (represent ownership of property)", 
        "Real estate investment trust (REITs)", 
        "Redeemable", 
        "Redeemable (the shares may be redeemed at the option of the issuer and/or of the shareholder)", 
        "Redeemable/exchangeable (the shares may be redeemed at the option of the issuer and/or of the shareholder and may be exchanged for securities of another issuer)", 
        "Redeemable/exchangeable/extendible (the issuer and/or holders of redeemable shares with a fixed maturity date have the option to extend the maturity date and the shares may be exchanged for securities of another issuer)", 
        "Redeemable/extendible (the issuer and/or holders of redeemable shares with a fixed maturity date have the option to extend the maturity date)", 
        "Redemption (indicates the retirement provisions made for the shares)", 
        "Redemption/conversion of the underlying assets (Guideline: for common/ordinary shares and limited partnership units, only the values N (perpetual) and X (not applicable/undefined) may be used. All values apply for other underlying instruments.)", 
        "Redemption/reimbursement", 
        "Redemption/reimbursement (indicates the retirement provisions made for the debt issue)", 
        "Referential instruments", 
        "Referential instruments excluding commodities", 
        "Referential instruments that do not fit into any of the Groups described in above groups.", 
        "Registered", 
        "Registered (securities are recorded in the name of the owner on the books of the issuer or the issuer's registrar and can only be transferred to another owner when endorsed by the registered owner)", 
        "Relate to the dividend payments of a specific underlying equity stock.", 
        "Relative value (strategies focusing on the spread relationships across various financial assets or commodities; they often utilize leverage and avoid market risk, although spread risk may often be large)", 
        "Repayment (indicates the repayment form provided by the structured instrument)", 
        "Repayment in assets", 
        "Repayment in assets and cash", 
        "Repayment in assets or cash", 
        "Repayment in cash (depending on the underlying, if the barrier is not breached)", 
        "Repurchase agreements", 
        "Restricted voting (the shareholder may be entitled to less than one vote per share)", 
        "Restrictions", 
        "Return or payout trigger", 
        "Return or payout trigger (method used to determine contract value if different from standard forward price of underlying instrument)", 
        "Return or payout trigger (method used to determine contract value)", 
        "Reverse convertible [should the underlying asset close below the strike on expiry, the underlying asset(s) and/or a cash amount is redeemed; should the underlying asset close above the strike at expiry, the nominal amount plus the coupon is paid at redemption; the coupon is paid regardless of the underlying development; it has reduced risk compared to a direct investment into the underlying asset; with higher risk levels, multiple underlying assets (worst-of) allow for higher coupons; limited profit potential (Cap)]", 
        "Rolling spot (an indefinitely renewed position in which no currency is actually delivered until a party closes out its position)", 
        "Secured (debt issue against which specific assets are pledged to secure the obligation, e.g. mortgage or receivables)", 
        "Securities lending", 
        "Securities representing a portion of assets pooled by investors run by a management company whose share capital remains separate from such assets and includes issues of shares or units in the form of, for example, a unit trust, mutual fund, OICVM, OPCVM, SICAV or SICAF.", 
        "Security selection (strategies typically equity-based and including long/short equity; the manager attempts to make money from superior stock selection by building some combination of long and short positions in such a way to mitigate systematic market risks)", 
        "Security type and investor restrictions", 
        "Semi-annually", 
        "Senior (applies to senior debts that are placed before senior subordinated, junior and junior subordinated in the ranking in the event of liquidation)", 
        "Senior subordinated (applies to senior subordinated debts that are placed before junior and junior subordinated in the ranking in the event of liquidation)", 
        "Services (transportation, communication, trade)", 
        "Share and bond", 
        "Share and warrant", 
        "Shares", 
        "Shares (common/ordinary) which, at the discretion of the holder, are convertible into other securities, at a designated rate. The conversion privilege may be perpetual or limited to a specific period.", 
        "Shares (retail and/or qualified/institutional/professional investors)", 
        "Shares for QI (qualified/institutional/professional investors only)", 
        "Short (the instrument entitles the holder to acquire cash in exchange for specific underlying assets)", 
        "Single currency", 
        "Single name", 
        "Single name (the underlying risk is a single reference entity or reference obligation)", 
        "Single or multi-currency (indicates whether the swap is single or multi-currency)", 
        "Single stock (an option on a contract which gives the holder the right to buy, and to sell, single-named equity)", 
        "Single stock (single equity stock)", 
        "Single stock (single name security)", 
        "Sovereign entity [the underlying exposure is a sovereign, e.g. country; thus, investor’s risk is that a country may not (be able to) pay its debt obligations; supranationals would be included here]", 
        "Specific security collateral (the repurchase agreement is secured by a specific agreed-upon security, which may not be substituted)", 
        "Spot", 
        "Spot contracts to deliver the agreed foreign currency at settlement.", 
        "Spot contracts to physically deliver the commodity asset at settlement.", 
        "Spot – Currency Index", 
        "Spot – Custom Basket of Currencies", 
        "Spot – Single Currency Pair", 
        "Spot – Single Currency Pair (an option on a foreign exchange transaction in which two parties agree to buy one currency against selling another currency at an agreed price for settlement on the spot date)", 
        "Spot-forward swap (a transaction that involves both an exchange of two currencies on the spot settlement date at a fixed rate that is agreed upon at the inception of the contract covering the exchange; and a reverse exchange of the same two currencies at a later date and at a fixed rate that is agreed upon at the inception of the contract covering the exchange)", 
        "Spread-bet", 
        "Spread-bet [the payout is determined by the movement in the reference price of the underlying instrument to its price at expiry (or the price when the holder wishes to close out) multiplied by an agreed amount per point movement]", 
        "Standard (vanilla) investment funds/mutual funds", 
        "Standardized (the underlying instruments, exercise price, expiration date and contract size of the options are standardized; these options are traded on special option exchanges)", 
        "Standardized/non-standardized (indicates whether the terms of the contract are standardized or not)", 
        "State guarantee (any level below sovereign or national government)", 
        "Stock dividends", 
        "Stock-equities", 
        "Strategies", 
        "Strategies that do not fit into any of the above Groups.", 
        "Strategy/style", 
        "Structured instruments (capital protection)", 
        "Structured instruments (participation)", 
        "Structured instruments (without capital protection)", 
        "Subscription rights", 
        "Supranational (organization defined as being beyond the scope or borders of any one nation such as two or more central banks or two or more central governments. Examples of supranational include the United Nations, the European Union, the European Investment Bank and the World Bank)", 
        "Swaps", 
        "Swaps (a swap other than a CDS)", 
        "Swaps that do not fit into any of the Swaps Groups", 
        "Term", 
        "Term (the set number of days specified at the outset of the agreement)", 
        "Termination", 
        "Termination (termination terms of the agreement)", 
        "The construction is generally based on a low exercise price option (LEPO) (base value less discounted future dividends) which in some cases might be comparable to a direct investment in the underlying asset(s) or a LEPO combined with other options, which together provide the desired disbursement profile.", 
        "The following are other assets that do not fit the Groupings as defined.", 
        "This Category classifies listed options, which are contracts that grant to the holder either the privilege to purchase or the privilege to sell the assets specified at a predetermined price or formula at or within a time in the future. Where a listed option cannot be classified within this Category, refer to non-listed and complex listed options.", 
        "This Category includes OTC or unlisted options and also includes any listed option which is not captured by the listed options Category. An option grants the holder either the privilege to purchase or the privilege to sell the assets specified at a predetermined price or formula at or within a time in the future.", 
        "This subclause defines a classification of derivative strategies. Strategies are the simultaneous trading of two or more derivative instruments.", 
        "Total return", 
        "Total return [the total economic return of an underlying asset is transferred from one party (total return buyer) to another (total return seller); total return seller takes on the risk of negative changes in market value of the reference asset, and pays any positive cash flow to the buyer such as coupon, capital gains or dividends of the reference asset]", 
        "Tracker certificate [participation in development of the underlying asset(s); reflects underlying price moves 1:1 (adjusted by conversion ratio and any related fees); risk is comparable to direct investment in the underlying asset(s)]", 
        "Trade finance instruments (assets based on the securitization of the movement of goods and services)", 
        "Traditional warrants (issued by the issuer of the underlying instrument)", 
        "Tri-party", 
        "Tri-party (the borrower delivers the collateral to the lender's account at the lender's clearing bank or custodian)", 
        "Twin-win-certificate [participation in development of the underlying asset(s); profits possible with rising and falling underlying asset values; falling underlying asset price converts into profit up to the barrier; minimum redemption is equal to the nominal value provided the barrier has not been breached; if the barrier is breached the product changes into a tracker certificate; with higher risk levels, multiple underlying asset(s) (worst-of) allow for a higher bonus level or lower barrier; reduced risk compared to a direct investment into the underlying asset(s)]", 
        "Type", 
        "Type (indicates whether the warrant is issued by the issuer of the underlying instrument or by a third party)", 
        "Type of commodity", 
        "Type of equity", 
        "Type of funds ( indicates the type of funds in which the fund invests)", 
        "Type of interest", 
        "Type of interest or cash payment", 
        "Type of interest rates", 
        "Type of interest/cash payment", 
        "Type of investment fund which pursues a total return and is usually open to qualified investors only.", 
        "Underlying assets", 
        "Underlying assets  (indicates the type of underlying assets in which the structured instrument participates)", 
        "Underlying assets (collateral that may be used, or substituted, to secure the lending agreement)", 
        "Underlying assets (indicates the type of underlying assets in which the structured instrument participates)", 
        "Underlying assets (indicates the type of underlying assets that the futures buyer receives, and that the seller delivers)", 
        "Underlying assets (indicates the type of underlying assets that the option holder is entitled to acquire)", 
        "Underlying assets (indicates the type of underlying assets that the option holder is entitled to sell)", 
        "Underlying assets (indicates the type of underlying assets that the spot contract buyer receives, and that the seller delivers)", 
        "Underlying assets (indicates the type of underlying assets that the warrant holder is entitled to acquire)", 
        "Underlying assets (indicates the type of underlying equity product that the forward contract buyer will buy, and that the seller will sell, at the specified future date and price)", 
        "Underlying assets (underlying collateral used to secure the repurchase agreement)", 
        "Underlying issuer type", 
        "Units", 
        "Units (retail and/or qualified/institutional/professional investors)", 
        "Units for QI (qualified/institutional/professional investors only)", 
        "Unsecured/unguaranteed (the direct obligations of the issuer rest solely on its general credit)", 
        "Valuation method or trigger", 
        "Vanilla", 
        "Vanilla (an option for which all terms are standardized)", 
        "Variable", 
        "Variable (an interest rate that fluctuates over time)", 
        "Variable (the interest rate is subject to adjustment through the life of the issue; includes graduated, i.e. step-up/step-down, floating and indexed interest rates)", 
        "Variable cash repayment (protected capital level and additional performance capital depending on the underlying)", 
        "Variable interest payments", 
        "Variance [a forward swap that uses the variance (being the volatility squared) of an underlying’s price movement over a period as the basis for the payoff calculation]", 
        "Volatility (the variability of movements in a security or underlying instrument’s price; it is a measure of the amount by which an asset’s price is expected to fluctuate over a given period of time; it is normally measured by the annual standard deviation of daily price changes)", 
        "Volatility – Currency Index", 
        "Volatility – Custom Basket of Currencies", 
        "Volatility – Single Currency Pair (please refer to 6.8.4)", 
        "Voting (each share has one vote)", 
        "Voting right (indicates the kind of voting power conferred to the shareholder)", 
        "Warrant and warrant (or multiple)", 
        "Warrants", 
        "Weekly", 
        "Weighting types (type of weighting of the financial instruments in the index)", 
        "Zero coupon [a rate swap in which the fixed rate cash flows are compounded and paid once on the expiration data, rather than periodically; the payments on the other side (which can be based on a floating interest rate or a fixed rate) follow typical swap payment schedules]", 
        "Zero rate/discounted", 
        "Zero rate/discounted [no periodical interest payments are made; the interest charge (discount) is the difference between maturity value and proceeds at time of acquisition]", 
        "Basis swap (float-float)",
        "Fixed-floating swap",
        "Fixed-fixed swap",
        "Inflation rate index",
        "OIS",
        "Forwards (derivatives involving the exchange of two rates on a defined future date, as agreed by the two parties to the transaction)",
        "American (an option which allows its holder to exercise the right to buy/sell the specified call/put currency at a specified price at any time during the term of the option, up to and including the expiration date)",
        "Invalid",
    ];    
}

#[cfg(test)]
mod tests {
    use crate::*;
    use std::collections::HashSet;

    #[test]
    fn all_valid_categories() {
        // let mut n_cats = 0u64;
        // let mut n_cats_tested = 0u64;

        // Results:
        // n_cats: 14, n_cats_tested: 58

        let valid = [
            b'E', b'C', b'D', b'R', b'O', b'F', b'S', 
            b'H', b'I', b'J', b'K', b'L', b'T', b'M',
        ];
        for c in b'A'..=b'z' {
            // n_cats_tested += 1;
            let c_res = internals::validate_cat(c);
            if valid.contains(&c) {
                // n_cats += 1;
                assert!(c_res.is_ok());
            } else {
                assert!(c_res.is_err());
            }
        }

        // std::thread::sleep(std::time::Duration::from_millis(100));
        // println!("`test_valid_categories()` n_cats: {}, n_cats_tested: {}", 
        //     n_cats, n_cats_tested);
    }

    #[test]
    fn all_valid_asset_classes() {
        // let mut n_acs = 0u64;
        // let mut n_acs_tested = 0u64;

        // Results:
        // n_acs: 78, n_acs_tested: 3364

        for c in b'A'..=b'z' {
            let valid = internals::acs_for_cat(c);
            for g in b'A'..=b'z' {
                // n_acs_tested += 1;
                let ac= &[c, g];
                if valid.contains(&ac) {
                    // n_acs += 1;
                    assert_eq!(Ok(()), internals::validate_ac(ac));
                } else {
                    assert!(internals::validate_ac(ac).is_err());
                }
            }
        }

        // std::thread::sleep(std::time::Duration::from_millis(100));
        // println!("`test_valid_asset_classes()` n_acs: {}, n_acs_tested: {}", 
        //     n_acs, n_acs_tested);
    }

    #[test]
    fn all_valid_attributes() {

        // let mut n_cats = 0u64;
        // let mut n_acs = 0u64;
        // let mut n_attrs = 0u64;
        // let mut n_attrs_tested = 0u64;

        // Results:
        // n_cats: 14, n_acs: 78, n_attrs: 1229, n_attrs_tested: 18096

        for c in internals::cats::CATEGORIES {
            // n_cats += 1;
            let acs = internals::acs_for_cat(*c);
            for ac in acs {
                // n_acs += 1;
                let valid = internals::attrs_for_ac(ac);
                for i in 0..4 {
                    for a in b'A'..=b'z' {
                        // n_attrs_tested += 1;
                        if valid[i].contains(&a) {
                            // n_attrs += 1;
                            assert_eq!(Ok(()), 
                                internals::validate_attr(i, a, valid, true));
                        } else {
                            if a == b'X' {
                                // This verifies parse_loose()/validate_loose()
                                assert_eq!(Ok(()), 
                                    internals::validate_attr(i, a, valid, false));
                            }
                            assert!(
                                internals::validate_attr(i, a, valid, true)
                                    .is_err());
                        }
                    }
                }
            }
        }

        // std::thread::sleep(std::time::Duration::from_millis(100));
        // println!("`test_valid_attributes()` n_cats: {}, n_acs: {}, n_attrs: {}, n_attrs_tested: {}",
        //     n_cats, n_acs, n_attrs, n_attrs_tested
        // );
    }

    #[test]
    fn evaluate_duplicate_attributes() {
        // This test was added to root out inconsistencies in the number of 
        // valid attributes found compared to number of valid attributes.
        for c in internals::cats::CATEGORIES {
            let acs = internals::acs_for_cat(*c);
            for ac in acs {
                let attrs = internals::attrs_for_ac(ac);
                for attri in attrs {
                    let dedup: HashSet<u8> = attri.iter().cloned().collect();
                    assert!(dedup.len() == attri.len(),
                        // format!("{:?}\n\t{:?}", [ac[0] as char, ac[1] as char], attri)
                    );
                }
            }
        }
    }

    #[cfg(feature = "describe")]
    #[test]
    fn evaluate_attr_vs_refs() {
        // This test was added to root out inconsistencies in the number of 
        // valid attributes compared to number of valid string references.

        // Results:
        // n_attrs_and_refs = 1229

        // let mut n_attrs_and_refs = 0;
        for c in internals::cats::CATEGORIES {
            let acs = internals::acs_for_cat(*c);
            for ac in acs {
                let refs = internals::refs_for_ac(ac).unwrap();
                for (i, attrsi) in internals::attrs_for_ac(ac).iter().enumerate() {
                    // n_attrs_and_refs += attrsi.len();
                    assert!(attrsi.len() == refs.3[i].len(),
                        // format!("{:?}\n\t{:?} :: {:?}", [ac[0] as char, ac[1] as char], attrsi, refs.3[i])
                    );
                }
            }
        }
        // println!("`test_attr_vs_refs()` n_attrs: {}", n_attrs_and_refs);
    }

}