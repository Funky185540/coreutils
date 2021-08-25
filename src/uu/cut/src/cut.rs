// This file is part of the uutils coreutils package.
//
// (c) Rolf Morel <rolfmorel@gmail.com>
//
// For the full copyright and license information, please view the LICENSE
// file that was distributed with this source code.

// spell-checker:ignore (ToDO) delim sourcefiles

#[macro_use]
extern crate uucore;

use bstr::io::BufReadExt;
use clap::{crate_version, App, Arg};
use std::error::Error;
use std::fmt::{Debug, Display};
use std::fs::File;
use std::io::{stdin, stdout, BufReader, BufWriter, Read, Write};
use std::path::Path;
use uucore::display::Quotable;

use self::searcher::Searcher;
use uucore::error::{UError, UResult, USimpleError};
use uucore::ranges::Range;
use uucore::InvalidEncodingHandling;


/* ****************************************************************************
 * Help text and option definitions
 * ****************************************************************************/

static NAME: &str = "cut";
static ABOUT: &str =
    "Print selected parts of lines from each FILE to standard output.

With no FILE, or when FILE is -, read standard input.

Mandatory arguments to long options are mandatory for short options too.";
static AFTER_HELP: &str =
"Use one, and only one of -b, -c or -f.  Each LIST is made up of one
range, or many ranges separated by commas.  Selected input is written
in the same order that it is read, and is written exactly once.
Each range is one of:

  N     N'th byte, character or field, counted from 1
  N-    from N'th byte, character or field, to end of line
  N-M   from N'th to M'th (included) byte, character or field
  -M    from first to M'th (included) byte, character or field

";

mod options {
    // Flags
    pub const COMPLEMENT: &str = "complement";
    pub const DONT_SPLIT_MULTIBYTES: &str = "n";
    pub const ONLY_DELIMITED: &str = "only-delimited";
    pub const ZERO_TERMINATED: &str = "zero-terminated";
    // Options
    pub const BYTES: &str = "bytes";
    pub const CHARACTERS: &str = "characters";
    pub const DELIMITER: &str = "delimiter";
    pub const FIELDS: &str = "fields";
    pub const OUTPUT_DELIMITER: &str = "output-delimiter";
    // File input
    pub const FILE: &str = "FILE";
}


/* ****************************************************************************
 * Error handling and custom error
 * ****************************************************************************/

#[derive(Debug)]
enum CutError {
    OnlyOneListAllowed(),
    NeedOneList(),
    InputDelimOnlyOnFields(),
    SuppressingOnlyOnFields(),
    DelimSingleChar(),
}

impl UError for CutError {
    fn code(&self) -> i32 {
        1
    }

    fn usage(&self) -> bool {
        true
    }
}

impl Error for CutError {}

impl Display for CutError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use CutError as CE;
        match self {
            CE::OnlyOneListAllowed() => write!(f, "only one type of list may be specified"),
            CE::NeedOneList() => {
                write!(f, "you must specify a list of bytes, characters, or fields")
            }
            CE::InputDelimOnlyOnFields() => write!(
                f,
                "an input delimiter may be specified only when operating on fields"
            ),
            CE::SuppressingOnlyOnFields() => write!(
                f,
                "suppressing non-delimited lines makes sense\n        only when operating on fields"
            ),
            CE::DelimSingleChar() => write!(f, "the delimiter must be a single character"),
        }
    }
}


/* ****************************************************************************
 * Custom data types and structures
 * ****************************************************************************/

struct Options {
    out_delim: Option<String>,
    zero_terminated: bool,
}

struct FieldOptions {
    delimiter: String, // one char long, String because of UTF8 representation
    out_delimiter: Option<String>,
    only_delimited: bool,
    zero_terminated: bool,
}

enum Mode {
    Bytes(Vec<Range>, Options),
    Characters(Vec<Range>, Options),
    Fields(Vec<Range>, FieldOptions),
}


/* ****************************************************************************
 * Helper functions
 * ****************************************************************************/

fn stdout_writer() -> Box<dyn Write> {
    if atty::is(atty::Stream::Stdout) {
        Box::new(stdout())
    } else {
        Box::new(BufWriter::new(stdout())) as Box<dyn Write>
    }
}

fn list_to_ranges(list: &str, complement: bool) -> Result<Vec<Range>, String> {
    if complement {
        Range::from_list(list).map(|r| uucore::ranges::complement(&r))
    } else {
        Range::from_list(list)
    }
}

fn cut_bytes<R: Read>(reader: R, ranges: &[Range], opts: &Options) -> i32 {
    let newline_char = if opts.zero_terminated { b'\0' } else { b'\n' };
    let buf_in = BufReader::new(reader);
    let mut out = stdout_writer();
    let delim = opts
        .out_delim
        .as_ref()
        .map_or("", String::as_str)
        .as_bytes();

    let res = buf_in.for_byte_record(newline_char, |line| {
        let mut print_delim = false;
        for &Range { low, high } in ranges {
            if low > line.len() {
                break;
            }
            if print_delim {
                out.write_all(delim)?;
            } else if opts.out_delim.is_some() {
                print_delim = true;
            }
            // change `low` from 1-indexed value to 0-index value
            let low = low - 1;
            let high = high.min(line.len());
            out.write_all(&line[low..high])?;
        }
        out.write_all(&[newline_char])?;
        Ok(true)
    });
    crash_if_err!(1, res);
    0
}

#[allow(clippy::cognitive_complexity)]
fn cut_fields_delimiter<R: Read>(
    reader: R,
    ranges: &[Range],
    delim: &str,
    only_delimited: bool,
    newline_char: u8,
    out_delim: &str,
) -> i32 {
    let buf_in = BufReader::new(reader);
    let mut out = stdout_writer();
    let input_delim_len = delim.len();

    let result = buf_in.for_byte_record_with_terminator(newline_char, |line| {
        let mut fields_pos = 1;
        let mut low_idx = 0;
        let mut delim_search = Searcher::new(line, delim.as_bytes()).peekable();
        let mut print_delim = false;

        if delim_search.peek().is_none() {
            if !only_delimited {
                out.write_all(line)?;
                if line[line.len() - 1] != newline_char {
                    out.write_all(&[newline_char])?;
                }
            }

            return Ok(true);
        }

        for &Range { low, high } in ranges {
            if low - fields_pos > 0 {
                low_idx = match delim_search.nth(low - fields_pos - 1) {
                    Some(index) => index + input_delim_len,
                    None => break,
                };
            }

            for _ in 0..=high - low {
                if print_delim {
                    out.write_all(out_delim.as_bytes())?;
                } else {
                    print_delim = true;
                }

                match delim_search.next() {
                    Some(high_idx) => {
                        let segment = &line[low_idx..high_idx];

                        out.write_all(segment)?;

                        low_idx = high_idx + input_delim_len;
                        fields_pos = high + 1;
                    }
                    None => {
                        let segment = &line[low_idx..];

                        out.write_all(segment)?;

                        if line[line.len() - 1] == newline_char {
                            return Ok(true);
                        }
                        break;
                    }
                }
            }
        }

        out.write_all(&[newline_char])?;
        Ok(true)
    });
    crash_if_err!(1, result);
    0
}

#[allow(clippy::cognitive_complexity)]
fn cut_fields<R: Read>(reader: R, ranges: &[Range], opts: &FieldOptions) -> i32 {
    let newline_char = if opts.zero_terminated { b'\0' } else { b'\n' };
    if let Some(ref o_delim) = opts.out_delimiter {
        return cut_fields_delimiter(
            reader,
            ranges,
            &opts.delimiter,
            opts.only_delimited,
            newline_char,
            o_delim,
        );
    }

    let buf_in = BufReader::new(reader);
    let mut out = stdout_writer();
    let delim_len = opts.delimiter.len();

    let result = buf_in.for_byte_record_with_terminator(newline_char, |line| {
        let mut fields_pos = 1;
        let mut low_idx = 0;
        let mut delim_search = Searcher::new(line, opts.delimiter.as_bytes()).peekable();
        let mut print_delim = false;

        if delim_search.peek().is_none() {
            if !opts.only_delimited {
                out.write_all(line)?;
                if line[line.len() - 1] != newline_char {
                    out.write_all(&[newline_char])?;
                }
            }

            return Ok(true);
        }

        for &Range { low, high } in ranges {
            if low - fields_pos > 0 {
                if let Some(delim_pos) = delim_search.nth(low - fields_pos - 1) {
                    low_idx = if print_delim {
                        delim_pos
                    } else {
                        delim_pos + delim_len
                    }
                } else {
                    break;
                }
            }

            match delim_search.nth(high - low) {
                Some(high_idx) => {
                    let segment = &line[low_idx..high_idx];

                    out.write_all(segment)?;

                    print_delim = true;
                    low_idx = high_idx;
                    fields_pos = high + 1;
                }
                None => {
                    let segment = &line[low_idx..line.len()];

                    out.write_all(segment)?;

                    if line[line.len() - 1] == newline_char {
                        return Ok(true);
                    }
                    break;
                }
            }
        }
        out.write_all(&[newline_char])?;
        Ok(true)
    });
    crash_if_err!(1, result);
    0
}

fn cut_files(mut filenames: Vec<String>, mode: Mode) -> UResult<()> {
    let mut stdin_read = false;
    let mut exit_code = 0;

    if filenames.is_empty() {
        filenames.push("-".to_owned());
    }

    for filename in &filenames {
        if filename == "-" {
            if stdin_read {
                continue;
            }

            exit_code |= match mode {
                Mode::Bytes(ref ranges, ref opts) => cut_bytes(stdin(), ranges, opts),
                Mode::Characters(ref ranges, ref opts) => cut_bytes(stdin(), ranges, opts),
                Mode::Fields(ref ranges, ref opts) => cut_fields(stdin(), ranges, opts),
            };

            stdin_read = true;
        } else {
            let path = Path::new(&filename[..]);

            if path.is_dir() {
                show_error!("{}: Is a directory", filename.maybe_quote());
                continue;
            }

            if path.metadata().is_err() {
                show_error!("{}: No such file or directory", filename.maybe_quote());
                continue;
            }

            let file = match File::open(&path) {
                Ok(f) => f,
                Err(e) => {
                    show_error!("opening {}: {}", filename.quote(), e);
                    continue;
                }
            };

            exit_code |= match mode {
                Mode::Bytes(ref ranges, ref opts) => cut_bytes(file, ranges, opts),
                Mode::Characters(ref ranges, ref opts) => cut_bytes(file, ranges, opts),
                Mode::Fields(ref ranges, ref opts) => cut_fields(file, ranges, opts),
            };
        }
    }

    uucore::error::set_exit_code(exit_code);
    Ok(())
}

/* ****************************************************************************
 * Main routine
 * ****************************************************************************/

#[uucore_procs::gen_uumain]
pub fn uumain(args: impl uucore::Args) -> UResult<()> {
    let args = args
        .collect_str(InvalidEncodingHandling::Ignore)
        .accept_any();

    let matches = uu_app().get_matches_from(args);

    let complement = matches.is_present(options::COMPLEMENT);

    let mode_parse = match (
        matches.value_of(options::BYTES),
        matches.value_of(options::CHARACTERS),
        matches.value_of(options::FIELDS),
    ) {
        (Some(byte_ranges), None, None) => list_to_ranges(byte_ranges, complement).map(|ranges| {
            Mode::Bytes(
                ranges,
                Options {
                    out_delim: Some(
                        matches
                            .value_of(options::OUTPUT_DELIMITER)
                            .unwrap_or_default()
                            .to_owned(),
                    ),
                    zero_terminated: matches.is_present(options::ZERO_TERMINATED),
                },
            )
        }),
        (None, Some(char_ranges), None) => list_to_ranges(char_ranges, complement).map(|ranges| {
            Mode::Characters(
                ranges,
                Options {
                    out_delim: Some(
                        matches
                            .value_of(options::OUTPUT_DELIMITER)
                            .unwrap_or_default()
                            .to_owned(),
                    ),
                    zero_terminated: matches.is_present(options::ZERO_TERMINATED),
                },
            )
        }),
        (None, None, Some(field_ranges)) => {
            match list_to_ranges(field_ranges, complement) {
                Err(e) => {
                    return Err(USimpleError::new(
                        1,
                        format!("failed processing list: {}", e),
                    ))
                }
                Ok(ranges) => {
                    let out_delim = match matches.value_of(options::OUTPUT_DELIMITER) {
                        Some(s) => {
                            if s.is_empty() {
                                Some("\0".to_owned())
                            } else {
                                Some(s.to_owned())
                            }
                        }
                        None => None,
                    };

                    let only_delimited = matches.is_present(options::ONLY_DELIMITED);
                    let zero_terminated = matches.is_present(options::ZERO_TERMINATED);

                    match matches.value_of(options::DELIMITER) {
                        Some(mut delim) => {
                            // GNU's `cut` supports `-d=` to set the delimiter to `=`.
                            // Clap parsing is limited in this situation, see:
                            // https://github.com/uutils/coreutils/issues/2424#issuecomment-863825242
                            // Since clap parsing handles `-d=` as delimiter explicitly set to "" and
                            // an empty delimiter is not accepted by GNU's `cut` (and makes no sense),
                            // we can use this as basis for a simple workaround:
                            if delim.is_empty() {
                                delim = "=";
                            }
                            if delim.chars().count() > 1 {
                                return Err(CutError::DelimSingleChar().into());
                            } else {
                                let delim = if delim.is_empty() {
                                    "\0".to_owned()
                                } else {
                                    delim.to_owned()
                                };

                                Ok(Mode::Fields(
                                    ranges,
                                    FieldOptions {
                                        delimiter: delim,
                                        out_delimiter: out_delim,
                                        only_delimited,
                                        zero_terminated,
                                    },
                                ))
                            }
                        }
                        None => Ok(Mode::Fields(
                            ranges,
                            FieldOptions {
                                delimiter: "\t".to_owned(),
                                out_delimiter: out_delim,
                                only_delimited,
                                zero_terminated,
                            },
                        )),
                    }
                }
            }
        }
        (ref b, ref c, ref f) if b.is_some() || c.is_some() || f.is_some() => {
            return Err(CutError::OnlyOneListAllowed().into())
        }
        _ => return Err(CutError::NeedOneList().into()),
    };

    let mode_parse = match mode_parse {
        Err(_) => mode_parse,
        Ok(mode) => match mode {
            Mode::Bytes(_, _) | Mode::Characters(_, _)
                if matches.is_present(options::DELIMITER) =>
            {
                return Err(CutError::InputDelimOnlyOnFields().into());
            }
            Mode::Bytes(_, _) | Mode::Characters(_, _)
                if matches.is_present(options::ONLY_DELIMITED) =>
            {
                return Err(CutError::SuppressingOnlyOnFields().into());
            }
            _ => Ok(mode),
        },
    };

    let files: Vec<String> = matches
        .values_of(options::FILE)
        .unwrap_or_default()
        .map(str::to_owned)
        .collect();

    match mode_parse {
        Ok(mode) => cut_files(files, mode),
        Err(err_msg) => {
            crash!(1, "{}", err_msg);
        }
    }
}

pub fn uu_app() -> App<'static, 'static> {
    App::new(uucore::util_name())
        .name(NAME)
        .version(crate_version!())
        .about(ABOUT)
        .after_help(AFTER_HELP)
        .arg(
            Arg::with_name(options::BYTES)
                .short("b")
                .long(options::BYTES)
                .takes_value(true)
                .help("select only these bytes")
                .allow_hyphen_values(true)
                .value_name("LIST"),
        )
        .arg(
            Arg::with_name(options::CHARACTERS)
                .short("c")
                .long(options::CHARACTERS)
                .help("select only these characters")
                .takes_value(true)
                .allow_hyphen_values(true)
                .value_name("LIST"),
        )
        .arg(
            Arg::with_name(options::DELIMITER)
                .short("d")
                .long(options::DELIMITER)
                .help("use DELIM instead of TAB for field delimiter")
                .takes_value(true)
                .value_name("DELIM"),
        )
        .arg(
            Arg::with_name(options::FIELDS)
                .short("f")
                .long(options::FIELDS)
                .help("select only these fields;  also print any line
  that contains no delimiter character, unless
  the -s option is specified")
                .takes_value(true)
                .allow_hyphen_values(true)
                .value_name("LIST"),
        )
        .arg(
            Arg::with_name(options::DONT_SPLIT_MULTIBYTES)
                .short(options::DONT_SPLIT_MULTIBYTES)
                .help("with -b: don't split multibyte characters")
                .takes_value(false),
        )
        .arg(
            Arg::with_name(options::COMPLEMENT)
                .long(options::COMPLEMENT)
                .help("complement the set of selected bytes, characters
  or fields")
                .takes_value(false),
        )
        .arg(
            Arg::with_name(options::ONLY_DELIMITED)
                .short("s")
                .long(options::ONLY_DELIMITED)
                .help("do not print lines not containing delimiters")
                .takes_value(false),
        )
        .arg(
            Arg::with_name(options::OUTPUT_DELIMITER)
            .long(options::OUTPUT_DELIMITER)
                .help("use STRING as the output delimiter
  the default is to use the input delimiter")
                .takes_value(true)
                .value_name("STRING"),
        )
        .arg(
            Arg::with_name(options::ZERO_TERMINATED)
                .short("z")
                .long(options::ZERO_TERMINATED)
                .help("line delimiter is NUL, not newline")
                .takes_value(false),
        )
        .arg(
            Arg::with_name(options::FILE)
                .hidden(false)
                .multiple(true)
        )
}
