use crate::nockapp::driver::*;
use crate::noun::slab::NounSlab;
use crate::utils::make_tas;
use std::time::Duration;
use sword::noun::{D, T};
use tokio::time;

pub fn make_timer_driver(interval_secs: u64) -> IODriverFn {
    make_driver(move |handle| async move {
        let mut timer_interval = time::interval(Duration::from_secs(interval_secs));

        loop {
            tokio::select! {
                _ = timer_interval.tick() => {
                    let mut timer_slab = NounSlab::new();
                    let timer_tas = make_tas(&mut timer_slab, "timer").as_noun();
                    let timer_noun = T(&mut timer_slab, &[timer_tas, D(0)]);
                    timer_slab.set_root(timer_noun);
                    handle.poke(timer_slab).await?;
                }
            }
        }
    })
}
