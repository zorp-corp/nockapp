#![allow(dead_code)]
use byteorder::{LittleEndian, WriteBytesExt};
use std::fs::File;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Instant;
use sword::hamt::Hamt;
use sword::interpreter::{self, inc, interpret, Error, Mote};
use sword::jets::cold::Cold;
use sword::jets::hot::{Hot, HotEntry, URBIT_HOT_STATE};
use sword::jets::list::util::zing;
use sword::jets::nock::util::mook;
use sword::jets::warm::Warm;
use sword::mem::NockStack;
use sword::mug::met3_usize;
use sword::noun::{Atom, Cell, DirectAtom, IndirectAtom, Noun, Slots, D, T};
use sword::persist::{pma_meta_get, pma_meta_set, pma_open, Persist};
use sword::trace::{path_to_cord, write_serf_trace_safe, TraceInfo};
use sword_macros::tas;

use crate::utils::slogger::CrownSlogger;
use crate::utils::{current_da, NOCK_STACK_SIZE};
use crate::{AtomExt, CrownError, NounExt, Result, ToBytesExt};

const PEEK_AXIS: u64 = 22;
const POKE_AXIS: u64 = 23;

#[repr(usize)]
enum BTMetaField {
    SnapshotVersion = 0,
    Snapshot = 1,
}

pub struct Kernel {
    pub serf: Serf,
    snap_dir: PathBuf,
    terminator: Arc<AtomicBool>,
}

struct Snapshot(pub(crate) *mut SnapshotMem);

#[repr(C)]
#[repr(packed)]
struct SnapshotMem {
    pub(crate) arvo: Noun,
    pub(crate) cold: Cold,
    pub(crate) event_num: u64,
}

impl Kernel {
    pub fn load_with_hot_state(
        snap_dir: PathBuf,
        kernel: &[u8],
        hot_state: &[HotEntry],
        trace: bool,
    ) -> Self {
        let serf = Serf::load(snap_dir.clone(), kernel, hot_state, trace);
        let terminator = Arc::new(AtomicBool::new(false));
        Self {
            serf,
            snap_dir,
            terminator,
        }
    }

    pub fn load(snap_dir: PathBuf, kernel: &[u8], trace: bool) -> Self {
        let serf = Serf::load(snap_dir.clone(), kernel, &[], trace);
        let terminator = Arc::new(AtomicBool::new(false));
        Self {
            serf,
            snap_dir,
            terminator,
        }
    }

    pub fn load_form(snap_dir: PathBuf, form: &[u8], trace: bool) -> Self {
        let serf = Serf::load_form(snap_dir.clone(), form, &[], trace);
        let terminator = Arc::new(AtomicBool::new(false));
        Self {
            serf,
            snap_dir,
            terminator,
        }
    }

    fn handle_live(&mut self, writ: Noun) -> Result<Noun> {
        let inner = writ.slot(6)?.as_direct()?;
        match inner.data() {
            tas!(b"exit") => {
                self.terminator.store(true, Ordering::Relaxed);
                Ok(D(0))
            }
            tas!(b"save") => {
                unsafe { self.serf.save() };
                Ok(D(0))
            }
            _ => Err(CrownError::KernelError(Some(D(0)))),
        }
    }

    fn handle_peek(&mut self, writ: Noun) -> Result<Noun> {
        let ovo = writ.slot(7)?;
        self.peek(ovo)
    }

    fn handle_poke(&mut self, writ: Noun) -> Result<Noun> {
        let job = writ.slot(7)?;
        self.do_poke(job)
    }

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

    pub fn goof(&mut self, mote: Mote, traces: Noun) -> Noun {
        let trace = zing(&mut self.serf.context.stack, traces).expect("serf: goof: zing failed");
        let tone = Cell::new(&mut self.serf.context.stack, D(2), trace);
        let tang = mook(&mut self.serf.context, tone, false)
            .expect("serf: goof: +mook crashed on bail")
            .tail();
        T(&mut self.serf.context.stack, &[D(mote as u64), tang])
    }

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

    fn poke_swap(&mut self, job: Noun, goof: Noun) -> Result<Noun> {
        let stack = &mut self.serf.context.stack;
        self.serf.context.cache = Hamt::<Noun>::new(stack);
        let job_cell = job.as_cell().expect("serf: poke: job not a cell");
        let job_now = job_cell.head().as_atom().expect("serf: poke: now not atom");
        let now = inc(stack, job_now).as_noun();
        let wire = T(stack, &[D(0), D(tas!(b"arvo")), D(0)]);
        let crud = DirectAtom::new_panic(tas!(b"crud"));
        let mut ovo = T(stack, &[now, wire, crud.as_noun(), goof, job_cell.tail()]);
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

        let event_num = D(1);
        let wire = T(stack, &[D(tas!(b"poke")), D(0)]);
        let poke = T(
            stack,
            &[event_num, wire, eny.as_noun(), our.as_noun(), now.as_noun(), cause],
        );

        self.do_poke(poke)
    }
}

pub struct Serf {
    pub arvo: Noun,
    pub context: interpreter::Context,
    pub event_num: u64,
}

impl Serf {
    fn new(
        kernel: Option<Noun>,
        event_num: u64,
        kernel_bytes: &[u8],
        constant_hot_state: &[HotEntry],
        trace_info: Option<TraceInfo>,
    ) -> Self {
        let hot_state = [URBIT_HOT_STATE, constant_hot_state].concat();
        let mut stack = NockStack::new(NOCK_STACK_SIZE, 0);
        let cache = Hamt::<Noun>::new(&mut stack);
        let mut cold = Cold::new(&mut stack);
        let hot = Hot::init(&mut stack, &hot_state);
        let warm = Warm::init(&mut stack, &mut cold, &hot);
        let slogger = std::boxed::Box::pin(CrownSlogger {});

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

        let arvo = kernel.unwrap_or_else(|| {
            let kernel_trap = Noun::cue_bytes_slice(&mut context.stack, kernel_bytes);
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
        });

        let mut serf = Self {
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

    fn new_form(
        kernel: Option<Noun>,
        event_num: u64,
        form_bytes: &[u8],
        constant_hot_state: &[HotEntry],
        trace_info: Option<TraceInfo>,
    ) -> Self {
        let hot_state = [URBIT_HOT_STATE, constant_hot_state].concat();
        let mut stack = NockStack::new(NOCK_STACK_SIZE, 0);
        let cache = Hamt::<Noun>::new(&mut stack);
        let mut cold = Cold::new(&mut stack);
        let hot = Hot::init(&mut stack, &hot_state);
        let warm = Warm::init(&mut stack, &mut cold, &hot);
        let slogger = std::boxed::Box::pin(CrownSlogger {});

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

        let arvo = kernel.unwrap_or_else(|| {
            let kernel_form = Noun::cue_bytes_slice(&mut context.stack, form_bytes);
            let arvo = if context.trace_info.is_some() {
                let start = Instant::now();
                let arvo = interpret(&mut context, D(0), kernel_form).unwrap(); // TODO better error
                write_serf_trace_safe(&mut context, "boot", start);
                arvo
            } else {
                interpret(&mut context, D(0), kernel_form).unwrap() // TODO better error
            };
            arvo
        });

        let mut serf = Self {
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

    pub fn load(
        snap_dir: PathBuf,
        kernel_bytes: &[u8],
        constant_hot_state: &[HotEntry],
        trace: bool,
    ) -> Self {
        let mut snap_path = snap_dir.clone();
        snap_path.push(".crown");
        snap_path.push("chk");
        std::fs::create_dir_all(&snap_path).unwrap();
        pma_open(snap_path).expect("serf: pma open failed");

        let snapshot_version = pma_meta_get(BTMetaField::SnapshotVersion as usize);

        let snapshot = match snapshot_version {
            0 => None,
            1 => Some(Snapshot(
                pma_meta_get(BTMetaField::Snapshot as usize) as *mut SnapshotMem
            )),
            _ => panic!("Unsupported snapshot version"),
        };

        let (arvo, event_num): (Option<Noun>, u64) =
            snapshot.map_or((None, 0), |snapshot| unsafe {
                let mem = snapshot.0;
                (Some((*mem).arvo), (*mem).event_num)
            });

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

        Self::new(
            arvo, event_num, kernel_bytes, constant_hot_state, trace_info,
        )
    }

    pub fn load_form(
        snap_dir: PathBuf,
        form_bytes: &[u8],
        constant_hot_state: &[HotEntry],
        trace: bool,
    ) -> Self {
        let mut snap_path = snap_dir.clone();
        snap_path.push(".crown");
        snap_path.push("chk");
        std::fs::create_dir_all(&snap_path).unwrap();
        pma_open(snap_path).expect("serf: pma open failed");

        let snapshot_version = pma_meta_get(BTMetaField::SnapshotVersion as usize);

        let snapshot = match snapshot_version {
            0 => None,
            1 => Some(Snapshot(
                pma_meta_get(BTMetaField::Snapshot as usize) as *mut SnapshotMem
            )),
            _ => panic!("Unsupported snapshot version"),
        };

        let (arvo, event_num): (Option<Noun>, u64) =
            snapshot.map_or((None, 0), |snapshot| unsafe {
                let mem = snapshot.0;
                (Some((*mem).arvo), (*mem).event_num)
            });

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

        Self::new_form(arvo, event_num, form_bytes, constant_hot_state, trace_info)
    }

    pub unsafe fn event_update(&mut self, new_event_num: u64, new_arvo: Noun) {
        self.arvo = new_arvo;
        self.event_num = new_event_num;
        self.save();

        self.context.cache = Hamt::new(&mut self.context.stack);
        self.context.scry_stack = D(0);
    }

    pub unsafe fn preserve_event_update_leftovers(&mut self) {
        let stack = &mut self.context.stack;
        stack.preserve(&mut self.context.warm);
        stack.preserve(&mut self.context.hot);
        stack.preserve(&mut self.context.cache);
        stack.flip_top_frame(0);
    }

    pub unsafe fn save(&mut self) {
        let handle = {
            let mut snapshot = Snapshot({
                let snapshot_mem_ptr: *mut SnapshotMem = self.context.stack.struct_alloc(1);

                // Save into PMA (does not sync)
                (*snapshot_mem_ptr).event_num = self.event_num;
                (*snapshot_mem_ptr).arvo = self.arvo;
                (*snapshot_mem_ptr).cold = self.context.cold;
                snapshot_mem_ptr
            });

            let handle = snapshot.save_to_pma(&mut self.context.stack);

            self.arvo = (*snapshot.0).arvo;
            self.event_num = (*snapshot.0).event_num;
            self.context.cold = (*snapshot.0).cold;

            handle
        };
        pma_meta_set(BTMetaField::SnapshotVersion as usize, 1);
        pma_meta_set(BTMetaField::Snapshot as usize, handle);
    }

    pub fn stack(&mut self) -> &mut NockStack {
        &mut self.context.stack
    }

    pub fn poke_swap(&mut self, eve: u64, mug: u64, ovo: Noun, fec: Noun) -> Noun {
        T(
            self.stack(),
            &[D(tas!(b"poke")), D(tas!(b"swap")), D(eve), D(mug), ovo, fec],
        )
    }

    pub fn poke_bail(&mut self, lud: Noun) -> Noun {
        T(self.stack(), &[D(tas!(b"poke")), D(tas!(b"bail")), lud])
    }
}

impl Persist for Snapshot {
    unsafe fn space_needed(&mut self, stack: &mut NockStack) -> usize {
        let mut arvo = (*(self.0)).arvo;
        let mut cold = (*(self.0)).cold;
        let arvo_space_needed = arvo.space_needed(stack);
        let cold_space_needed = cold.space_needed(stack);
        (((std::mem::size_of::<SnapshotMem>() + 7) >> 3) << 3)
            + arvo_space_needed
            + cold_space_needed
    }

    unsafe fn copy_to_buffer(&mut self, stack: &mut NockStack, buffer: &mut *mut u8) {
        let snapshot_buffer = *buffer as *mut SnapshotMem;
        std::ptr::copy_nonoverlapping(self.0, snapshot_buffer, 1);
        *self = Snapshot(snapshot_buffer);
        *buffer = snapshot_buffer.add(1) as *mut u8;

        let mut arvo = (*snapshot_buffer).arvo;
        arvo.copy_to_buffer(stack, buffer);
        (*snapshot_buffer).arvo = arvo;

        let mut cold = (*snapshot_buffer).cold;
        cold.copy_to_buffer(stack, buffer);
        (*snapshot_buffer).cold = cold;
    }

    unsafe fn handle_to_u64(&self) -> u64 {
        self.0 as u64
    }

    unsafe fn handle_from_u64(meta_handle: u64) -> Self {
        Snapshot(meta_handle as *mut SnapshotMem)
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
        let jam_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("assets")
            .join(jam);
        let jam_bytes = fs::read(jam_path).expect(&format!("Failed to read {} file", jam));
        let kernel = Kernel::load(snap_dir, &jam_bytes, false);
        (kernel, temp_dir)
    }

    #[test]
    fn test_kernel_boot() {
        let _ = setup_kernel("dumb.jam");
    }
}
