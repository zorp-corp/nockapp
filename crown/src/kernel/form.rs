#![allow(dead_code)]
use blake3::{Hash, Hasher};
use byteorder::{LittleEndian, WriteBytesExt};
use std::fs::File;
use std::path::PathBuf;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use std::time::Instant;
use sword::hamt::Hamt;
use sword::interpreter::{self, interpret, Error, Mote};
use sword::jets::cold::Cold;
use sword::jets::hot::{Hot, HotEntry, URBIT_HOT_STATE};
use sword::jets::list::util::zing;
use sword::jets::nock::util::mook;
use sword::jets::warm::Warm;
use sword::mem::NockStack;
use sword::mug::met3_usize;
use sword::noun::{Atom, Cell, DirectAtom, IndirectAtom, Noun, Slots, D, T};
use sword::persist::pma_open;
use sword::trace::{path_to_cord, write_serf_trace_safe, TraceInfo};
use sword_macros::tas;
use tracing::info;

use crate::kernel::checkpoint::JamPaths;
use crate::utils::slogger::CrownSlogger;
use crate::utils::{current_da, NOCK_STACK_SIZE};
use crate::{AtomExt, CrownError, NounExt, Result, ToBytesExt};

use super::checkpoint::{Checkpoint, JammedCheckpoint};

const PEEK_AXIS: u64 = 22;
const POKE_AXIS: u64 = 23;

/// Enum representing the Sword snapshot metadata fields.
#[repr(usize)]
enum BTMetaField {
    SnapshotVersion = 0,
    Snapshot = 1,
}

/// Represents a Sword kernel, containing a Serf and snapshot location.
pub struct Kernel {
    /// The Serf managing the interface to the Sword.
    pub serf: Serf,
    /// Directory path for storing pma snapshots.
    pma_dir: PathBuf,
    /// Jam persistence buffer paths.
    pub jam_paths: JamPaths,
    /// Atomic flag for terminating the kernel.
    terminator: Arc<AtomicBool>,
}

impl Kernel {
    /// Loads a kernel with a custom hot state.
    ///
    /// # Arguments
    ///
    /// * `snap_dir` - Directory for storing snapshots.
    /// * `kernel` - Byte slice containing the kernel as a jammed noun.
    /// * `hot_state` - Custom hot state entries.
    /// * `trace` - Whether to enable tracing.
    ///
    /// # Returns
    ///
    /// A new `Kernel` instance.
    pub fn load_with_hot_state(
        pma_dir: PathBuf,
        jam_paths: JamPaths,
        kernel: &[u8],
        hot_state: &[HotEntry],
        trace: bool,
    ) -> Self {
        let mut pma_path = pma_dir.clone();
        pma_path.push(".crown");
        pma_path.push("chk");
        std::fs::create_dir_all(&pma_path).unwrap();
        pma_open(pma_path).expect("serf: pma open failed");

        std::fs::create_dir_all(jam_paths.0.parent().unwrap()).unwrap();

        let mut stack = NockStack::new(NOCK_STACK_SIZE, 0);

        let checkpoint = if jam_paths.checkpoint_exists() {
            info!("Checkpoint file(s) found, validating and loading from jam");
            jam_paths.load_checkpoint(&mut stack).ok()
        } else {
            info!("No checkpoint file found, starting from scratch");
            None
        };

        let serf = Serf::new(stack, checkpoint, kernel, hot_state, trace);
        let terminator = Arc::new(AtomicBool::new(false));
        Self {
            serf,
            pma_dir,
            jam_paths,
            terminator,
        }
    }

    /// Loads a kernel with default hot state.
    ///
    /// # Arguments
    ///
    /// * `snap_dir` - Directory for storing snapshots.
    /// * `kernel` - Byte slice containing the kernel code.
    /// * `trace` - Whether to enable tracing.
    ///
    /// # Returns
    ///
    /// A new `Kernel` instance.
    pub fn load(pma_dir: PathBuf, jam_paths: JamPaths, kernel: &[u8], trace: bool) -> Self {
        let mut pma_path = pma_dir.clone();
        pma_path.push(".crown");
        pma_path.push("chk");
        std::fs::create_dir_all(&pma_path).unwrap();
        pma_open(pma_path).expect("serf: pma open failed");

        std::fs::create_dir_all(jam_paths.0.parent().unwrap()).unwrap();

        let mut stack = NockStack::new(NOCK_STACK_SIZE, 0);

        let checkpoint = if jam_paths.checkpoint_exists() {
            info!("Checkpoint file(s) found, validating and loading from jam");
            jam_paths.load_checkpoint(&mut stack).ok()
        } else {
            info!("No checkpoint file found, starting from scratch");
            None
        };

        let serf = Serf::new(stack, checkpoint, kernel, &[], trace);
        let terminator = Arc::new(AtomicBool::new(false));
        Self {
            serf,
            pma_dir,
            jam_paths,
            terminator,
        }
    }

    /// Performs a peek operation on the Arvo state.
    ///
    /// # Arguments
    ///
    /// * `ovo` - The peek request noun.
    ///
    /// # Returns
    ///
    /// Result containing the peeked data or an error.
    pub fn peek(&mut self, ovo: Noun) -> Result<Noun> {
        if self.serf.context.trace_info.is_some() {
            let trace_name = "peek";
            let start = Instant::now();
            let slam_res = self.slam(PEEK_AXIS, ovo);
            write_serf_trace_safe(&mut self.serf.context, trace_name, start);

            slam_res
        } else {
            self.slam(PEEK_AXIS, ovo)
        }
    }

    /// Generates a goof (error) noun.
    ///
    /// # Arguments
    ///
    /// * `mote` - The error mote.
    /// * `traces` - Trace information.
    ///
    /// # Returns
    ///
    /// A noun representing the error.
    pub fn goof(&mut self, mote: Mote, traces: Noun) -> Noun {
        let trace = zing(&mut self.serf.context.stack, traces).expect("serf: goof: zing failed");
        let tone = Cell::new(&mut self.serf.context.stack, D(2), trace);
        let tang = mook(&mut self.serf.context, tone, false)
            .expect("serf: goof: +mook crashed on bail")
            .tail();
        T(&mut self.serf.context.stack, &[D(mote as u64), tang])
    }

    /// Performs a poke operation on the Arvo state.
    ///
    /// # Arguments
    ///
    /// * `job` - The poke job noun.
    ///
    /// # Returns
    ///
    /// Result containing the poke response or an error.
    pub fn do_poke(&mut self, job: Noun) -> Result<Noun> {
        match self.soft(job, Some("poke".to_string())) {
            Ok(res) => {
                let cell = res.as_cell().expect("serf: poke: +slam returned atom");
                let mut fec = cell.head();
                let eve = self.serf.event_num;

                unsafe {
                    self.serf.event_update(eve + 1, cell.tail());
                    self.serf.stack().preserve(&mut fec);
                    self.serf.preserve_event_update_leftovers();
                }
                Ok(fec)
            }
            Err(goof) => self.poke_swap(job, goof),
        }
    }

    /// Slams (applies) a gate at a specific axis of Arvo.
    ///
    /// # Arguments
    ///
    /// * `axis` - The axis to slam.
    /// * `ovo` - The sample noun.
    ///
    /// # Returns
    ///
    /// Result containing the slammed result or an error.
    pub fn slam(&mut self, axis: u64, ovo: Noun) -> Result<Noun> {
        let arvo = self.serf.arvo;
        let stack = &mut self.serf.context.stack;
        let pul = T(stack, &[D(9), D(axis), D(0), D(2)]);
        let sam = T(stack, &[D(6), D(0), D(7)]);
        let fol = T(stack, &[D(8), pul, D(9), D(2), D(10), sam, D(0), D(2)]);
        let sub = T(stack, &[arvo, ovo]);

        let res = interpret(&mut self.serf.context, sub, fol).map_err(CrownError::from);
        res
    }

    /// Performs a "soft" computation, handling errors gracefully.
    ///
    /// # Arguments
    ///
    /// * `ovo` - The input noun.
    /// * `trace_name` - Optional name for tracing.
    ///
    /// # Returns
    ///
    /// Result containing the computed noun or an error noun.
    fn soft(&mut self, ovo: Noun, trace_name: Option<String>) -> Result<Noun, Noun> {
        let slam_res = if self.serf.context.trace_info.is_some() {
            let start = Instant::now();
            let slam_res = self.slam(POKE_AXIS, ovo);
            write_serf_trace_safe(&mut self.serf.context, trace_name.as_ref().unwrap(), start);

            slam_res
        } else {
            self.slam(POKE_AXIS, ovo)
        };

        match slam_res {
            Ok(res) => Ok(res),
            Err(error) => match error {
                CrownError::InterpreterError(e) => {
                    let (mote, traces) = match e.0 {
                        Error::Deterministic(mote, traces)
                        | Error::NonDeterministic(mote, traces) => (mote, traces),
                        Error::ScryBlocked(_) | Error::ScryCrashed(_) => {
                            panic!("serf: soft: .^ invalid outside of virtual Nock")
                        }
                    };

                    Err(self.goof(mote, traces))
                }
                _ => Err(D(0)),
            },
        }
    }

    /// Plays a list of events.
    ///
    /// # Arguments
    ///
    /// * `lit` - The list of events to play.
    ///
    /// # Returns
    ///
    /// Result containing the final Arvo state or an error.
    fn play_list(&mut self, mut lit: Noun) -> Result<Noun> {
        let mut eve = self.serf.event_num;
        while let Ok(cell) = lit.as_cell() {
            let ovo = cell.head();
            lit = cell.tail();
            let trace_name = if self.serf.context.trace_info.is_some() {
                Some(format!("play [{}]", eve))
            } else {
                None
            };

            match self.soft(ovo, trace_name) {
                Ok(res) => {
                    let arvo = res.as_cell()?.tail();
                    eve += 1;

                    unsafe {
                        self.serf.event_update(eve, arvo);
                        self.serf.context.stack.preserve(&mut lit);
                        self.serf.preserve_event_update_leftovers();
                    }
                }
                Err(goof) => {
                    return Err(CrownError::KernelError(Some(goof)));
                }
            }
        }
        Ok(self.serf.arvo)
    }

    /// Handles a poke error by swapping in a new event.
    ///
    /// # Arguments
    ///
    /// * `job` - The original poke job.
    /// * `goof` - The error noun.
    ///
    /// # Returns
    ///
    /// Result containing the new event or an error.
    fn poke_swap(&mut self, job: Noun, goof: Noun) -> Result<Noun> {
        let stack = &mut self.serf.context.stack;
        self.serf.context.cache = Hamt::<Noun>::new(stack);
        let job_cell = job.as_cell().expect("serf: poke: job not a cell");
        // job data is job without event_num
        let job_data = job_cell
            .tail()
            .as_cell()
            .expect("serf: poke: data not a cell");
        //  job input is job without event_num or wire
        let job_input = job_data.tail();
        let wire = T(stack, &[D(0), D(tas!(b"arvo")), D(0)]);
        let crud = DirectAtom::new_panic(tas!(b"crud"));
        let event_num = D(self.serf.event_num + 1);

        let mut ovo = T(stack, &[event_num, wire, goof, job_input]);
        let trace_name = if self.serf.context.trace_info.is_some() {
            Some(Self::poke_trace_name(
                &mut self.serf.context.stack,
                wire,
                crud.as_atom(),
            ))
        } else {
            None
        };

        match self.soft(ovo, trace_name) {
            Ok(res) => {
                let cell = res.as_cell().expect("serf: poke: crud +slam returned atom");
                let mut fec = cell.head();
                let eve = self.serf.event_num;

                unsafe {
                    self.serf.event_update(eve + 1, cell.tail());
                    self.serf.context.stack.preserve(&mut ovo);
                    self.serf.context.stack.preserve(&mut fec);
                    self.serf.preserve_event_update_leftovers();
                }
                Ok(self.serf.poke_swap(eve, eve, ovo, fec))
            }
            Err(goof_crud) => {
                let stack = &mut self.serf.context.stack;
                let lud = T(stack, &[goof_crud, goof, D(0)]);
                Ok(self.serf.poke_bail(lud))
            }
        }
    }

    /// Generates a trace name for a poke operation.
    ///
    /// # Arguments
    ///
    /// * `stack` - The Nock stack.
    /// * `wire` - The wire noun.
    /// * `vent` - The vent atom.
    ///
    /// # Returns
    ///
    /// A string representing the trace name.
    fn poke_trace_name(stack: &mut NockStack, wire: Noun, vent: Atom) -> String {
        let wpc = path_to_cord(stack, wire);
        let wpc_len = met3_usize(wpc);
        let wpc_bytes = &wpc.as_bytes()[0..wpc_len];
        let wpc_str = match std::str::from_utf8(wpc_bytes) {
            Ok(valid) => valid,
            Err(error) => {
                let (valid, _) = wpc_bytes.split_at(error.valid_up_to());
                unsafe { std::str::from_utf8_unchecked(valid) }
            }
        };

        let vc_len = met3_usize(vent);
        let vc_bytes = &vent.as_bytes()[0..vc_len];
        let vc_str = match std::str::from_utf8(vc_bytes) {
            Ok(valid) => valid,
            Err(error) => {
                let (valid, _) = vc_bytes.split_at(error.valid_up_to());
                unsafe { std::str::from_utf8_unchecked(valid) }
            }
        };

        format!("poke [{} {}]", wpc_str, vc_str)
    }

    /// Performs a poke operation with a given cause.
    ///
    /// # Arguments
    ///
    /// * `cause` - The cause noun for the poke.
    ///
    /// # Returns
    ///
    /// Result containing the poke response or an error.
    pub fn poke(&mut self, cause: Noun) -> Result<Noun> {
        let stack = &mut self.serf.context.stack;

        let random_bytes = rand::random::<u64>();
        let bytes = random_bytes.as_bytes()?;
        let eny: Atom = Atom::from_bytes(stack, &bytes);
        let our = <sword::noun::Atom as AtomExt>::from_value(stack, 0)?; // Using 0 as default value
        let now: Atom = unsafe {
            let mut t_vec: Vec<u8> = vec![];
            t_vec.write_u128::<LittleEndian>(current_da().0)?;
            IndirectAtom::new_raw_bytes(stack, 16, t_vec.as_slice().as_ptr()).normalize_as_atom()
        };

        let event_num = D(self.serf.event_num + 1);
        let wire = T(stack, &[D(tas!(b"poke")), D(0)]);
        let poke = T(
            stack,
            &[event_num, wire, eny.as_noun(), our.as_noun(), now.as_noun(), cause],
        );

        self.do_poke(poke)
    }
}

/// Represents the Serf, which maintains context and provides an interface to
/// the Sword.
pub struct Serf {
    /// Version number of arvo
    pub version: u32,
    /// Hash of boot kernel
    pub ker_hash: Hash,
    /// The current Arvo state.
    pub arvo: Noun,
    /// The interpreter context.
    pub context: interpreter::Context,
    /// The current event number.
    pub event_num: u64,
}

impl Serf {
    /// Creates a new Serf instance.
    ///
    /// # Arguments
    ///
    /// * `stack` - The Nock stack.
    /// * `checkpoint` - Optional checkpoint to restore from.
    /// * `kernel_bytes` - Byte slice containing the kernel code.
    /// * `constant_hot_state` - Custom hot state entries.
    /// * `trace` - Bool indicating whether to enable sword tracing.
    ///
    /// # Returns
    ///
    /// A new `Serf` instance.
    fn new(
        mut stack: NockStack,
        checkpoint: Option<Checkpoint>,
        kernel_bytes: &[u8],
        constant_hot_state: &[HotEntry],
        trace: bool,
    ) -> Self {
        let hot_state = [URBIT_HOT_STATE, constant_hot_state].concat();
        let cache = Hamt::<Noun>::new(&mut stack);
        let (mut cold, event_num) = checkpoint.as_ref().map_or_else(
            || (Cold::new(&mut stack), 0),
            |snapshot| (snapshot.cold, snapshot.event_num),
        );

        let hot = Hot::init(&mut stack, &hot_state);
        let warm = Warm::init(&mut stack, &mut cold, &hot);
        let slogger = std::boxed::Box::pin(CrownSlogger {});

        let trace_info = if trace {
            let file = File::create("trace.json").expect("Cannot create trace file trace.json");
            let pid = std::process::id();
            let process_start = std::time::Instant::now();
            Some(TraceInfo {
                file,
                pid,
                process_start,
            })
        } else {
            None
        };

        let mut context = interpreter::Context {
            stack,
            slogger,
            cold,
            warm,
            hot,
            cache,
            scry_stack: D(0),
            trace_info,
        };

        let arvo = checkpoint.as_ref().map_or_else(
            || {
                let kernel_trap = Noun::cue_bytes_slice(&mut context.stack, kernel_bytes)
                    .expect("invalid kernel jam");
                let fol = T(&mut context.stack, &[D(9), D(2), D(0), D(1)]);
                let arvo = if context.trace_info.is_some() {
                    let start = Instant::now();
                    let arvo = interpret(&mut context, kernel_trap, fol).unwrap(); // TODO better error
                    write_serf_trace_safe(&mut context, "boot", start);
                    arvo
                } else {
                    interpret(&mut context, kernel_trap, fol).unwrap() // TODO better error
                };
                arvo
            },
            |snapshot| snapshot.arvo,
        );

        let version = checkpoint
            .as_ref()
            .map_or_else(|| 0, |snapshot| snapshot.version);

        let mut hasher = Hasher::new();
        hasher.update(kernel_bytes);
        let mut ker_hash = hasher.finalize();

        if let Some(checkpoint) = checkpoint {
            if ker_hash != checkpoint.ker_hash {
                info!("TODO: Kernel hash mismatch, upgrade necessary");
                ker_hash = checkpoint.ker_hash;
            }
        }

        let mut serf = Self {
            version,
            ker_hash,
            arvo,
            context,
            event_num,
        };

        unsafe {
            serf.event_update(event_num, arvo);
            serf.preserve_event_update_leftovers();
        }
        serf
    }

    pub fn jam_checkpoint(&mut self, buff_index: bool) -> JammedCheckpoint {
        let version = self.version;
        let ker_hash = self.ker_hash;
        let event_num = self.event_num;
        let arvo = self.arvo.clone();
        let cold = self.context.cold;
        JammedCheckpoint::new(
            &mut self.stack(),
            version,
            buff_index,
            ker_hash,
            event_num,
            &cold,
            &arvo,
        )
    }

    /// Updates the Serf's state after an event.
    ///
    /// # Arguments
    ///
    /// * `new_event_num` - The new event number.
    /// * `new_arvo` - The new Arvo state.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it modifies the Serf's state directly.
    pub unsafe fn event_update(&mut self, new_event_num: u64, new_arvo: Noun) {
        self.arvo = new_arvo;
        self.event_num = new_event_num;

        self.context.cache = Hamt::new(&mut self.context.stack);
        self.context.scry_stack = D(0);
    }

    /// Preserves leftovers after an event update.
    ///
    /// # Safety
    ///
    /// This function is unsafe because it modifies the Serf's state directly.
    pub unsafe fn preserve_event_update_leftovers(&mut self) {
        let stack = &mut self.context.stack;
        stack.preserve(&mut self.context.warm);
        stack.preserve(&mut self.context.hot);
        stack.preserve(&mut self.context.cache);
        stack.preserve(&mut self.context.cold);
        stack.preserve(&mut self.arvo);
        stack.flip_top_frame(0);
    }

    /// Returns a mutable reference to the Nock stack.
    ///
    /// # Returns
    ///
    /// A mutable reference to the `NockStack`.
    pub fn stack(&mut self) -> &mut NockStack {
        &mut self.context.stack
    }

    /// Creates a poke swap noun.
    ///
    /// # Arguments
    ///
    /// * `eve` - The event number.
    /// * `mug` - The mug value.
    /// * `ovo` - The original noun.
    /// * `fec` - The effect noun.
    ///
    /// # Returns
    ///
    /// A noun representing the poke swap.
    pub fn poke_swap(&mut self, eve: u64, mug: u64, ovo: Noun, fec: Noun) -> Noun {
        T(
            self.stack(),
            &[D(tas!(b"poke")), D(tas!(b"swap")), D(eve), D(mug), ovo, fec],
        )
    }

    /// Creates a poke bail noun.
    ///
    /// # Arguments
    ///
    /// * `lud` - The lud noun.
    ///
    /// # Returns
    ///
    /// A noun representing the poke bail.
    pub fn poke_bail(&mut self, lud: Noun) -> Noun {
        T(self.stack(), &[D(tas!(b"poke")), D(tas!(b"bail")), lud])
    }
}

fn slot(noun: Noun, axis: u64) -> Result<Noun> {
    Ok(noun.slot(axis)?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::Path;
    use tempfile::TempDir;

    fn setup_kernel(jam: &str) -> (Kernel, TempDir) {
        let temp_dir = TempDir::new().expect("Failed to create temp directory");
        let snap_dir = temp_dir.path().to_path_buf();
        let jam_paths = JamPaths::new(&snap_dir);
        let jam_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("assets")
            .join(jam);
        let jam_bytes = fs::read(jam_path).expect(&format!("Failed to read {} file", jam));
        let kernel = Kernel::load(snap_dir, jam_paths, &jam_bytes, false);
        (kernel, temp_dir)
    }

    #[test]
    fn test_kernel_boot() {
        let _ = setup_kernel("dumb.jam");
    }

    // To test your own kernel, place a `kernel.jam` file in the `assets` directory
    // and uncomment the following test:
    //
    // #[test]
    // fn test_custom_kernel() {
    //     let (kernel, _temp_dir) = setup_kernel("kernel.jam");
    //     // Add your custom assertions here to test the kernel's behavior
    // }
}
