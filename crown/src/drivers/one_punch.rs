use crate::nockapp::{make_driver, IODriverFn, NockAppError, NockAppHandle, Operation, PokeResult};
use crate::noun::slab::NounSlab;
use either::Either::{self, Left, Right};
use sword::noun::D;
use sword_macros::tas;
use tracing::{error, info};

pub fn one_punch_man(data: NounSlab, op: Operation) -> IODriverFn {
    make_driver(|handle| async move {
        let result = match op {
            Operation::Poke => {
                info!("poke_once_driver: poking with {:?}", data);
                Left(handle.poke(data).await?)
            }
            Operation::Peek => {
                info!("poke_once_driver: peeking with {:?}", data);
                Right(handle.peek(data).await?)
            }
        };

        tokio::select! {
            res = handle_result(result, &op) => res,
            eff = handle.next_effect() => {
                handle_effect(eff, &handle).await
            },
            _ = tokio::time::sleep(tokio::time::Duration::from_secs(600)) => {
                //TODO what is a good timeout for tests?
                info!("poke_once_driver: no effect received after 10 minutes");
                Err(NockAppError::Timeout)
            }
        }
    })
}
/// Handles the result of a poke or peek operation.
///
/// Poke:
/// - Ack: The poke operation was successful.
/// - Nack: The poke operation failed.
///
/// Peek:
/// - Some(NounSlab): The peek operation was successful and returned a NounSlab.
/// - None: The peek operation failed or returned no result.
///
/// # Arguments
///
/// * `result` - The result of the operation.
/// * `op` - The operation type (Poke or Peek).
///
/// # Returns
///
/// A Result indicating success or failure of the operation.
async fn handle_result(
    result: Either<PokeResult, Option<NounSlab>>,
    op: &Operation,
) -> Result<(), NockAppError> {
    match op {
        Operation::Poke => match result {
            Left(PokeResult::Ack) => {
                info!("Poke successful");
                Ok(())
            }
            Left(PokeResult::Nack) => {
                error!("Poke nacked");
                Err(NockAppError::PokeFailed)
            }
            Right(_) => {
                error!("Unexpected result for poke operation");
                Err(NockAppError::UnexpectedResult)
            }
        },
        Operation::Peek => match result {
            Left(_) => {
                error!("Unexpected result for peek operation");
                Err(NockAppError::UnexpectedResult)
            }
            Right(Some(peek_result)) => {
                info!("Peek result: {:?}", peek_result);
                Ok(())
            }
            Right(_) => {
                error!("Peek returned no result");
                Err(NockAppError::PeekFailed)
            }
        },
    }
}

/// Handles effects from the kernel.
///
/// # Arguments
///
/// * `eff` - The effect produced by the kernel.
/// * `_handle` - The NockAppHandle (unused in this implementation).
///
/// # Returns
///
/// A Result indicating success or failure of handling the effect.
async fn handle_effect(
    eff: Result<NounSlab, NockAppError>,
    _handle: &NockAppHandle,
) -> Result<(), NockAppError> {
    let eff = eff?;
    info!("poke_once_driver: effect received: {:?}", eff);

    let effect_cell = unsafe { eff.root() }.as_cell().unwrap();
    if unsafe { effect_cell.head().raw_equals(D(tas!(b"npc"))) } {
        let npc_effect = effect_cell.tail();
        if let Ok(npc_effect_cell) = npc_effect.as_cell() {
            match npc_effect_cell.head().as_atom().unwrap().as_u64().unwrap() {
                x if x == tas!(b"gossip") => {
                    // Ignore gossip data
                    info!("Ignoring gossip data");
                }
                x if x == tas!(b"request") => {
                    info!("Processing request effect");
                    let request_data = npc_effect_cell.tail();
                    info!("Request data: {:?}", request_data);
                    // handle.poke(create_response(request_data)).await?;
                }
                _ => info!("Received unknown npc effect"),
            }
        }
    }
    Ok(())
}
