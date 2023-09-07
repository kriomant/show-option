//! Extension methods and convenient macro for formatting `Option`s without
//! unnecessary code duplication and allocations.
//! 
//! # Usage
//! 
//! ```
//! // Import all
//! use show_option::prelude::*;
//! 
//! // OR, if you prefer, import explicitly only what you need:
//! use show_option::ShowOption as _; // Extension methods for `Option`.
//! use show_option::format_option; // Macro
//! 
//! // Display 'Option's
//! println!("received bytes: {}", None::<usize>.show_or("none")); // "received bytes: none"
//! # assert_eq!(format!("received bytes: {}", None::<usize>.show_or("none")), "received bytes: none");
//! println!("amount: {}", Some(20).show_prefixed_or("$", "-")); // "amount: $20"
//! # assert_eq!(format!("amount: {}", Some(20).show_prefixed_or("$", "-")), "amount: $20");
//! println!("amount: {}", format_option!(Some(20), "${}", "-")); // "amount: $20"
//! # assert_eq!(format!("amount: {}", format_option!(Some(20), "${}", "-")), "amount: $20");
//! ```
//! 
//! See full list of methods in [`ShowOption`] documentation.
//! Also see [`format_option`] macro.
//!
//! If there are many places you want to apply same formatting, consider creating function:
//! 
//! ```
//! # use std::fmt::Display;
//! # use show_option::ShowOption as _;
//! fn format_amount(amount: &Option<usize>) -> impl Display + '_ {
//!     amount.show_prefixed_or("$", "-")
//! }
//! println!("amount: {}", format_amount(&Some(20))); // prints "amount: $20"
//! # assert_eq!(format!("amount: {}", format_amount(&Some(20))), "amount: $20");
//! ```
//! 
//! Sometimes even better solution is to create new wrapper type with own `Display`
//! implementation.
//! 
//! # Motivation
//!
//! `Option` type doesn't implement `Display` intentionally, because it's formatting
//! is context-dependant. Should `None` be formatted as empty string, `"none"`,
//! or `"missing"`?
//!
//! One way to handle it is conditional formatting:
//! 
//! ```
//! let bytes = Some(20);
//! match bytes {
//!     Some(value) => println!("bytes received: {}", value),
//!     None => println!("bytes received: none"),
//! }
//! ```
//! 
//! it is very flexible, but it leads to format string duplication.
//! 
//! Another way is to convert optional value to it's representation before formatting.
//! It works in simple cases, when default value has the same type as inner value:
//! 
//! ```
//! let bytes = Some(20);
//! println!("bytes received: {}", bytes.unwrap_or(0));
//! # let _ = bytes; // ensure bytes are not consumed
//! ```
//! But when types are different, it leads to verbose code and unnecessary string allocations:
//!
//! ```
//! let bytes = Some(20);
//! println!("bytes received: {}", bytes.map_or("none".to_string(), |value| value.to_string()));
//! //                                                 ↑ allocation               ↑ allocation
//! # let _ = bytes; // ensure bytes are not consumed
//! ```
//! 
//! It is possible to avoid allocations by casting both branches to `Display` trait reference,
//! but it's very verbose:
//! 
//! ```
//! use std::fmt::Display;
//! 
//! let bytes = Some(20);
//! println!("bytes received: {}", bytes.as_ref().map_or(&"none" as &dyn Display, |value| value as &dyn Display));
//! # let _ = bytes; // ensure bytes are not consumed
//! ```
//! 
//! This crate basically wraps last solution into nice methods.

pub mod prelude {
    pub use super::ShowOption as _;
    pub use super::format_option;
}

use std::fmt::Display;

pub trait ShowOption<T> {
    /// Returns struct which emits either option value (if any)
    /// or provided default when being formatted.
    ///
    /// Both `Option`'s inner type and default must implement `Display`, but they may have different types.
    /// 
    /// Usage:
    /// ```
    /// use show_option::ShowOption as _;
    /// println!("value: {}", Some(20).show_or("none")); // prints "value: 20"
    /// println!("value: {}", None::<usize>.show_or("none")); // prints "value: none"
    /// ```
    fn show_or<'t, O>(&'t self, otherwise: O) -> ShowOr<'t, T, O>;

    /// Returns struct which emits either option value (if any) with given prefix
    /// or provided default when being formatted.
    ///
    /// `Option`'s inner type, prefix and default must implement `Display`, but they may have different types.
    /// 
    /// Usage:
    /// ```
    /// use show_option::ShowOption as _;
    /// println!("value: {}", Some(20).show_prefixed_or("set to ", "none")); // prints "value: set to 20"
    /// # assert_eq!(format!("value: {}", Some(20).show_prefixed_or("set to ", "none")), "value: set to 20");
    /// println!("value: {}", None::<usize>.show_prefixed_or("set to ", "none")); // prints "value: none"
    /// # assert_eq!(format!("value: {}", None::<usize>.show_prefixed_or("set to ", "none")), "value: none");
    /// ```
    fn show_prefixed_or<'t, P, O>(&'t self, prefix: P, otherwise: O) -> ShowPrefixedOr<'t, T, P, O>;

    /// Returns struct which emits either option value (if any) with given suffix
    /// or provided default when being formatted.
    ///
    /// `Option`'s inner type, suffix and default must implement `Display`, but they may have different types.
    /// 
    /// Usage:
    /// ```
    /// use show_option::ShowOption as _;
    /// println!("value: {}", Some(20).show_suffixed_or(" bytes", "none")); // prints "value: 20 bytes"
    /// println!("value: {}", None::<usize>.show_suffixed_or(" bytes", "none")); // prints "value: none"
    /// ```
    fn show_suffixed_or<'t, S, O>(&'t self, suffix: S, otherwise: O) -> ShowSuffixedOr<'t, T, S, O>;

    /// Returns struct which emits either option value (if any) with given suffix and suffix
    /// or provided default when being formatted.
    ///
    /// `Option`'s inner type, prefix, suffix and default must implement `Display`,
    /// but they may have different types.
    /// 
    /// Usage:
    /// ```
    /// use show_option::ShowOption as _;
    /// println!("value: {}", Some(20).show_surrounded_or("set to ", " bytes", "none")); // prints "value: set to 20 bytes"
    /// println!("value: {}", None::<usize>.show_surrounded_or("set to ", " bytes", "none")); // prints "value: none"
    /// ```
    fn show_surrounded_or<'t, P, S, O>(&'t self, prefix: P, suffix: S, otherwise: O) -> ShowSurroundedOr<'t, T, P, S, O>;

    /// Returns struct which displays either option value (if any) using given formatter function
    /// or provided default when being formatted.
    /// 
    /// It is useful when you want to apply specific formatting options (width, alignment, precision)
    /// to inner value.
    ///
    /// `format` is a function which receives reference to inner value and returns anything
    /// which implements `Display`.
    /// 
    /// # Usage
    ///
    /// ```
    /// use show_option::ShowOption as _;
    /// use lazy_format::lazy_format;
    /// println!("value: {}", Some(20).show_formatted_or(|v| lazy_format!("set to {:>5} bytes", v), "none"));
    /// // prints "value: set to    20 bytes"
    /// # assert_eq!(format!("value: {}", Some(20).show_formatted_or(|v| lazy_format!("set to {:>5} bytes", v), "none")),
    /// #            "value: set to    20 bytes");
    /// ```
    /// 
    /// Also see [`format_option`] macro which provides convenient way to format option value
    /// using format string.
    /// 
    /// # Details
    ///
    /// It is tempting to use `format_args!`, but it's not possible due to way macro is implemented
    /// (it captures all arguments and thus result can't be returned from function).
    /// However, you may use `lazy_format` crate and do:
    /// `Some(20).show_formatted_or(|v| lazy_format!("${}", v), "-")`
    /// 
    /// `Option`'s inner type and default must implement `Display`, but they may have different types.
    fn show_formatted_or<'t, F, O>(&'t self, format: F, otherwise: O) -> ShowFormattedOr<'t, T, F, O>;
}

impl<T> ShowOption<T> for Option<T> {
    fn show_or<'t, O>(&'t self, otherwise: O) -> ShowOr<'t, T, O> {
        ShowOr { option: self, otherwise }
    }

    fn show_prefixed_or<'t, P, O>(&'t self, prefix: P, otherwise: O) -> ShowPrefixedOr<'t, T, P, O> {
        ShowPrefixedOr { option: self, prefix, otherwise }
    }

    fn show_suffixed_or<'t, S, O>(&'t self, suffix: S, otherwise: O) -> ShowSuffixedOr<'t, T, S, O> {
        ShowSuffixedOr { option: self, suffix, otherwise }
    }

    fn show_surrounded_or<'t, P, S, O>(&'t self, prefix: P, suffix: S, otherwise: O) -> ShowSurroundedOr<'t, T, P, S, O> {
        ShowSurroundedOr { option: self, prefix, suffix, otherwise }
    }

    fn show_formatted_or<'t, F, O>(&'t self, format: F, otherwise: O) -> ShowFormattedOr<'t, T, F, O> {
        ShowFormattedOr { option: self, format, otherwise }
    }
}

#[doc(hidden)]
pub struct ShowOr<'t, T, O> {
    option: &'t Option<T>,
    otherwise: O
}

impl<'t, T: Display, O: Display> Display for ShowOr<'t, T, O> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.option {
            Some(value) => value.fmt(f),
            None => self.otherwise.fmt(f),
        }
    }
}

#[doc(hidden)]
pub struct ShowPrefixedOr<'t, T, P, O> {
    option: &'t Option<T>,
    prefix: P,
    otherwise: O,
}

impl<'t, T: Display, P: Display, O: Display> Display for ShowPrefixedOr<'t, T, P, O> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Some(value) = self.option else {
            return self.otherwise.fmt(f);
        };

        self.prefix.fmt(f)?;
        value.fmt(f)?;
        Ok(())
    }
}

#[doc(hidden)]
pub struct ShowSuffixedOr<'t, T, S, O> {
    option: &'t Option<T>,
    suffix: S,
    otherwise: O,
}

impl<'t, T: Display, S: Display, O: Display> Display for ShowSuffixedOr<'t, T, S, O> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Some(value) = self.option else {
            return self.otherwise.fmt(f);
        };

        value.fmt(f)?;
        self.suffix.fmt(f)?;
        Ok(())
    }
}

#[doc(hidden)]
pub struct ShowSurroundedOr<'t, T, P, S, O> {
    option: &'t Option<T>,
    prefix: P,
    suffix: S,
    otherwise: O,
}

impl<'t, T: Display, P: Display, S: Display, O: Display> Display for ShowSurroundedOr<'t, T, P, S, O> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Some(value) = self.option else {
            return self.otherwise.fmt(f);
        };

        self.prefix.fmt(f)?;
        value.fmt(f)?;
        self.suffix.fmt(f)?;
        Ok(())
    }
}

#[doc(hidden)]
pub struct ShowFormattedOr<'t, T, F, O> {
    option: &'t Option<T>,
    format: F,
    otherwise: O,
}

impl<'t, T: Display, D: 't + Display, F: Fn(&'t T) -> D, O: Display> Display for ShowFormattedOr<'t, T, F, O> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Some(value) = self.option else {
            return self.otherwise.fmt(f);
        };

        (self.format)(value).fmt(f)?;
        Ok(())
    }
}

/// Formats option using provided format or displays default.
/// 
/// # Usage:
/// 
/// ```
/// use show_option::format_option;
/// println!("value: {}", format_option!(Some(20), "set to {} bytes", "none"));
/// # assert_eq!(format!("value: {}", format_option!(Some(20), "set to {} bytes", "none")),
/// #            "value: set to 20 bytes");
/// ```
/// 
/// # Details
/// 
/// Macro is hidden `macro` feature which is enabled by default.
/// You may want to disable it to prevent additional external
/// dependency (`lazy_format`).
/// 
/// It is sugar for `ShowOption::show_formatted_or` which uses
/// `lazy_format` crate under the hood.
/// 
/// `format_option(option, "${}", "-")` is expanded into
/// `option.show_formatted_or(|v| ::lazy_format::lazy_format!("${}", v), "-")`
#[cfg(any(test, feature="macro"))]
#[macro_export]
macro_rules! format_option {
    ($option:expr, $format:literal, $default:expr) => {{
        use $crate::ShowOption as _;
        $option.show_formatted_or(|v| ::lazy_format::lazy_format!($format, v), $default)
    }};
}

#[cfg(test)]
mod tests {
    use super::ShowOption as _;

    use std::fmt::Display;
    use lazy_format::lazy_format;

    #[test]
    fn show_or() {
        assert_eq!(Some(20).show_or("none").to_string(), "20");
        assert_eq!(None::<usize>.show_or("none").to_string(), "none");
    }

    #[test]
    fn show_prefixed_or() {
        assert_eq!(Some(20).show_prefixed_or("set to ", "none").to_string(), "set to 20");
        assert_eq!(None::<usize>.show_prefixed_or("set to ", "none").to_string(), "none");
    }

    #[test]
    fn show_suffixed_or() {
        assert_eq!(Some(20).show_suffixed_or(" bytes", "none").to_string(), "20 bytes");
        assert_eq!(None::<usize>.show_suffixed_or(" bytes", "none").to_string(), "none");
    }

    #[test]
    fn show_formatted_or() {
        assert_eq!(Some(20).show_formatted_or(|v| lazy_format!("{v} bytes"), "none").to_string(), "20 bytes");
        assert_eq!(None::<usize>.show_formatted_or(|v| lazy_format!("{v} bytes"), "none").to_string(), "none");
    }

    #[test]
    fn format_option() {
        assert_eq!(format!("{}", format_option!(Some(20), "{} bytes", "none")), "20 bytes");
        assert_eq!(format!("{}", format_option!(None::<usize>, "{} bytes", "none")), "none");
    }

    #[test]
    fn reuse() {
        fn opt_bytes(v: &Option<usize>) -> impl Display + '_ {
            v.show_suffixed_or(" bytes", "none")
        }
        assert_eq!(format!("received: {}", opt_bytes(&Some(20))), "received: 20 bytes");
    }
}
