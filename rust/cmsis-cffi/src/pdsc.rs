use slog::{Logger, Drain};
use slog_term::{TermDecorator, FullFormat};
use slog_async::Async;
use std::borrow::Cow;
use std::os::raw::c_char;
use std::ffi::CStr;
use std::path::Path;

use failure::err_msg;

use cmsis_utils::ResultLogExt;
use cmsis_utils::parse::FromElem;
use pack_desc::{dump_devices, Package};

use pack_index::UpdateReturn;

cffi!{
    fn dump_pdsc_json(
        packs: *mut ParsedPacks,
        devices_dest: *const c_char,
        boards_dest: *const c_char,
    ) -> Result<()> {
        let decorator = TermDecorator::new().build();
        let drain = FullFormat::new(decorator).build().fuse();
        let drain = Async::new(drain).build().fuse();
        let log = Logger::root(drain, o!());
        let dev_dest: Option<Cow<str>> = if !devices_dest.is_null() {
            let fname = unsafe { CStr::from_ptr(devices_dest) }.to_string_lossy();
            Some(fname)
        } else {
            None
        };
        let brd_dest: Option<Cow<str>> = if !boards_dest.is_null() {
            let fname = unsafe { CStr::from_ptr(boards_dest) }.to_string_lossy();
            Some(fname)
        } else {
            None
        };
        with_from_raw!(let filenames = packs, {
            dump_devices(&filenames.0,
                         dev_dest.map(|d| d.to_string()),
                         brd_dest.map(|d| d.to_string()),
                         &log)
        })
    }
}

pub struct ParsedPacks(Vec<Package>);

impl ParsedPacks {
    pub fn iter(&self) -> impl Iterator<Item = &Package> {
        self.0.iter()
    }
}

cffi!{
    fn parse_packs(ptr: *mut UpdateReturn) -> Result<*mut ParsedPacks>{
        if !ptr.is_null() {
            with_from_raw!(let boxed = ptr,{
                let decorator = TermDecorator::new().build();
                let drain = FullFormat::new(decorator).build().fuse();
                let drain = Async::new(drain).build().fuse();
                let log = Logger::root(drain, o!());
                let pdsc_files = boxed.iter();
                Ok(Box::into_raw(Box::new(ParsedPacks(
                    pdsc_files
                        .filter_map(|input| Package::from_path(Path::new(input), &log).ok_warn(&log))
                        .collect()))))
            })
        } else {
            Err(err_msg("Null Passed into parse packs."))
        }
    }
}

cffi!{
    fn parse_packs_free(ptr: *mut ParsedPacks) {
        if !ptr.is_null() {
            drop(unsafe { Box::from_raw(ptr) })
        }
    }
}