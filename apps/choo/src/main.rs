use clap::Parser;
use futures::FutureExt;

use choo::*;
use sword::mem::{AllocationError, NewStackError};

#[tokio::main]
async fn main() -> Result<(), Error> {
    let cli = ChooCli::parse();
    let result = std::panic::AssertUnwindSafe(async {
        let mut nockapp = initialize_nockapp(cli).await?;
        nockapp.run().await?;
        Ok::<(), Error>(())
    })
    .catch_unwind()
    .await;
    if result.is_err() {
        println!("Caught panic!");
        // now we downcast the error
        // and print it out
        let e = result.unwrap_err();
        if let Some(e) = e.downcast_ref::<AllocationError>() {
            println!("Allocation error occurred: {}", e);
        } else if let Some(e) = e.downcast_ref::<NewStackError>() {
            println!("NockStack creation error occurred: {}", e);
        } else {
            println!("Unknown panic: {e:?}");
        }
    } else {
        println!("no panic!");
    }

    Ok(())
}
