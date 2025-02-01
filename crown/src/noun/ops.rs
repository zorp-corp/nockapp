use crate::utils::Result;
use crate::CrownError;
use sword::interpreter::{interpret, Context};
use sword::noun::{Noun, D, T};
use tracing::{span, Level};

/// Slams (applies) a gate at a specific axis of the supplied kernel.
///
/// # Arguments
/// * `context` - The interpreter cotnext.
/// * `arvo` - The kernel.
/// * `axis` - The axis to slam.
/// * `ovo` - The sample noun.
///
/// # Returns
///
/// Result containing the slammed result or an error.
#[tracing::instrument(skip(context, arvo, axis, ovo))]
pub fn slam(context: &mut Context, arvo: Noun, axis: u64, ovo: Noun) -> Result<Noun> {
    let stack = &mut context.stack;
    let pul = T(stack, &[D(9), D(axis), D(0), D(2)]);
    let sam = T(stack, &[D(6), D(0), D(7)]);
    let fol = T(stack, &[D(8), pul, D(9), D(2), D(10), sam, D(0), D(2)]);
    let sub = T(stack, &[arvo, ovo]);

    let res = span!(Level::DEBUG, "interpret")
        .in_scope(|| interpret(context, sub, fol).map_err(CrownError::from));
    res
}
