use crate::nockapp::driver::{make_driver, IODriverFn};
use crate::NounExt;
use tracing::{error, info};
/// Creates an IO driver function for handling exit signals.
///
/// This function creates a driver that listens for exit signals and terminates
/// the process with the provided exit code when received.
///
/// # Returns
///
/// An `IODriverFn` that can be used with the NockApp to handle exit signals.
pub fn exit() -> IODriverFn {
    make_driver(|handle| async move {
        info!("exit_driver: waiting for effect");
        loop {
            tokio::select! {
                eff = handle.next_effect() => {
                    match eff {
                        Ok(eff) => {
                                let exit_code = unsafe {
                                    let noun = eff.root();
                                    if let Ok(cell) = noun.as_cell() {
                                        if cell.head().eq_bytes(b"exit") && cell.tail().is_atom() {
                                            // Exit with the code provided in the tail
                                            if let Ok(Ok(exit_code)) = cell.tail().as_atom().and_then(|atom| Ok(atom.as_u64())) {
                                                let exit_code_usize = exit_code as usize;
                                                Some(exit_code_usize)
                                            } else {
                                                // Default to error code 1 if we can't get a valid exit code
                                                // handle.exit.send(1).await.unwrap();
                                                None
                                            }
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                }
                            };
                            handle.exit.send(exit_code.unwrap_or(1)).await.unwrap();
                        }
                        Err(e) => {
                            error!("Error receiving effect: {:?}", e);
                        }
                    }
                }
            }
        }
    })
}
