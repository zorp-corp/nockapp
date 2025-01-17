use crate::nockapp::driver::*;
use crate::noun::slab::NounSlab;
use std::time::Duration;
use tokio::time;

pub fn make_timer_driver(interval_secs: u64, timer_slab: NounSlab) -> IODriverFn {
    make_driver(move |handle| async move {
        let mut timer_interval = time::interval(Duration::from_secs(interval_secs));

        loop {
            tokio::select! {
                _ = timer_interval.tick() => {
                    handle.poke(timer_slab.clone()).await?;
                }
            }
        }
    })
}
