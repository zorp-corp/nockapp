use crate::nockapp::driver::*;
use crate::nockapp::wire::Wire;
use crate::noun::slab::NounSlab;
use crate::utils::make_tas;
use std::time::Duration;
use sword::noun::{D, T};
use tokio::time;

pub enum TimerWire {
    Tick,
}

impl Wire for TimerWire {
    const VERSION: u64 = 1;
    const SOURCE: &'static str = "timer";

    fn to_noun_slab(&self) -> NounSlab {
        let mut slab = NounSlab::new();
        let source = make_tas(&mut slab, TimerWire::SOURCE).as_noun();
        let wire = match self {
            TimerWire::Tick => T(&mut slab, &[source, D(TimerWire::VERSION), D(0)]),
        };
        slab.set_root(wire);
        slab
    }
}

pub fn make_timer_driver(interval_secs: u64, timer_slab: NounSlab) -> IODriverFn {
    make_driver(move |handle| async move {
        let mut timer_interval = time::interval(Duration::from_secs(interval_secs));

        loop {
            tokio::select! {
                _ = timer_interval.tick() => {
                    let wire = TimerWire::Tick.to_noun_slab();
                    handle.poke(wire, timer_slab.clone()).await?;
                }
            }
        }
    })
}
