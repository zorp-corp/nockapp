use crate::noun::slab::NounSlab;
use crate::utils::make_tas;
use sword::noun::{D, T};

/// Standardized wire format for kernel interaction.
pub trait Wire: Sized {
    /// Protocol version
    const VERSION: u64;

    /// Driver/Module identifier
    const SOURCE: &'static str;

    /// Convert wire format to [`NounSlab`]
    fn to_noun_slab(&self) -> NounSlab;
}

/// System wire to use when no other wire is specified
pub struct SystemWire;

impl Wire for SystemWire {
    const VERSION: u64 = 1;
    const SOURCE: &'static str = "sys";

    #[tracing::instrument(skip(self))]
    fn to_noun_slab(&self) -> NounSlab {
        let mut slab = NounSlab::new();
        let source = make_tas(&mut slab, SystemWire::SOURCE).as_noun();
        let wire = T(&mut slab, &[source, D(SystemWire::VERSION), D(0)]);
        slab.set_root(wire);
        slab
    }
}
