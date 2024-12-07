use crate::nockapp::driver::{make_driver, IODriverFn};
use crate::AtomExt;
use sword::noun::D;
use sword_macros::tas;

use termimad::MadSkin;
use tracing::error;

pub fn markdown() -> IODriverFn {
    make_driver(|handle| async move {
        let skin = MadSkin::default_dark();

        loop {
            match handle.next_effect().await {
                Ok(effect) => {
                    let root = unsafe { effect.root() };
                    let Ok(effect_cell) = root.as_cell() else {
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
