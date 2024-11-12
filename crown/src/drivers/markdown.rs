use crate::{nockapp::driver::{make_driver, IODriverFn}, AtomExt};
use sword_macros::tas;
use sword::noun::D;

use tracing::error;
use termimad::MadSkin;

pub fn markdown_driver() -> IODriverFn {
    make_driver(|handle| async move {
        let skin = MadSkin::default_dark();

        loop {
            match handle.next_effect().await {
                Ok(effect) => {
                    let Ok(effect_cell) = unsafe { effect.root() }.as_cell() else {
                        continue;
                    };
                    if unsafe { effect_cell.head().raw_equals(D(tas!(b"markdown"))) } {
                        let markdown_text = effect_cell.tail();

                        let text = if let Ok(atom) = markdown_text.as_atom() {
                            String::from_utf8_lossy(&atom.to_bytes_until_nul()?).to_string()
                        } else {
                            error!("Failed to convert markdown text to string");
                            continue;
                        };

                        println!("{}", skin.term_text(&text));
                    }
                }
                Err(e) => {
                    error!("Error in markdown driver: {:?}", e);
                    continue;
                }
            }
        }
    })
}
