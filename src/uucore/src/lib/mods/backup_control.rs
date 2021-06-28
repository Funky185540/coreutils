use std::{
    env,
    fs,
    io,
    path::{Path, PathBuf},
};
#[cfg(unix)]
use std::os::unix;
#[cfg(windows)]
use std::os::windows;

extern crate fs_extra;
use fs_extra::dir::{move_dir, CopyOptions as DirCopyOptions};


pub static BACKUP_CONTROL_VALUES: &[&str] = &[
    "simple", "never", "numbered", "t", "existing", "nil", "none", "off",
];

pub static BACKUP_CONTROL_LONG_HELP: &str = "The backup suffix is '~', unless set with --suffix or SIMPLE_BACKUP_SUFFIX. Here are the version control values:

none, off
    never make backups (even if --backup is given)

numbered, t
    make numbered backups

existing, nil
    numbered if numbered backups exist, simple otherwise

simple, never
    always make simple backups";

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BackupMode {
    NoBackup,
    SimpleBackup,
    NumberedBackup,
    ExistingBackup,
}

pub fn determine_backup_suffix(supplied_suffix: Option<&str>) -> String {
    if let Some(suffix) = supplied_suffix {
        String::from(suffix)
    } else {
        env::var("SIMPLE_BACKUP_SUFFIX").unwrap_or_else(|_| "~".to_owned())
    }
}

pub fn determine_backup_mode(backup_opt_exists: bool, backup_opt: Option<&str>) -> BackupMode {
    if backup_opt_exists {
        match backup_opt.map(String::from) {
            // default is existing, see:
            // https://www.gnu.org/software/coreutils/manual/html_node/Backup-options.html
            None => BackupMode::ExistingBackup,
            Some(mode) => match &mode[..] {
                "simple" | "never" => BackupMode::SimpleBackup,
                "numbered" | "t" => BackupMode::NumberedBackup,
                "existing" | "nil" => BackupMode::ExistingBackup,
                "none" | "off" => BackupMode::NoBackup,
                _ => panic!(), // cannot happen as it is managed by clap
            },
        }
    } else {
        BackupMode::NoBackup
    }
}

pub fn get_backup_path(
    backup_mode: BackupMode,
    backup_path: &Path,
    suffix: &str,
) -> Option<PathBuf> {
    match backup_mode {
        BackupMode::NoBackup => None,
        BackupMode::SimpleBackup => Some(simple_backup_path(backup_path, suffix)),
        BackupMode::NumberedBackup => Some(numbered_backup_path(backup_path)),
        BackupMode::ExistingBackup => Some(existing_backup_path(backup_path, suffix)),
    }
}

pub fn simple_backup_path(path: &Path, suffix: &str) -> PathBuf {
    let mut p = path.to_string_lossy().into_owned();
    p.push_str(suffix);
    PathBuf::from(p)
}

pub fn numbered_backup_path(path: &Path) -> PathBuf {
    for i in 1_u64.. {
        let path_str = &format!("{}.~{}~", path.to_string_lossy(), i);
        let path = Path::new(path_str);
        if !path.exists() {
            return path.to_path_buf();
        }
    }
    panic!("cannot create backup")
}

pub fn existing_backup_path(path: &Path, suffix: &str) -> PathBuf {
    let test_path_str = &format!("{}.~1~", path.to_string_lossy());
    let test_path = Path::new(test_path_str);
    if test_path.exists() {
        numbered_backup_path(path)
    } else {
        simple_backup_path(path, suffix)
    }
}

/// Perform a backup of a file, symlink or folder.
///
/// Given a backup mode, some path to perform a backup for, and a backup suffix,
/// this function creates a backup of `backup_path`. It is written such that it
/// attempts to rename the file to backup. If renaming fails, it falls back to
/// copying the file under a new name and removing the original version, or
/// moving it directly instead.
///
/// Returns the path that the file, symlink or folder has been backed up to, or
/// an error if any occured.
///
///
/// # Errors
///
/// This function may error if any of the underlying file operations error, e.g.
/// due to insufficient permissions. Refer to [`backup_symlink`],
/// [`backup_dir`], [`std::fs::copy`], [`std::fs::remove_file`] and
/// [`std::fs::symlink_metadata`] for futher information on errors to expect.
///
///
/// # Examples
///
/// This will backup `/foo/bar.txt` to `/foo/bar.txt~`.
///
/// ```
/// # // Create the file that we work with.
/// # use std::fs;
/// # fs::write("/foo/bar.txt", "baz!")?;
///
/// // Or obtain this from `determine_backup_mode`
/// let backup_mode = BackupMode::SimpleBackup;
/// let file = PathBuf::from("/foo/bar.txt");
/// // Or obtain this from `determine_backup_suffix`
/// let suffix = "~";
///
/// let ret = perform_backup(backup_mode, file, suffix);
/// if let Some(path) = ret {
///     println!("Backed up '{}' to '{}'", file, path);
/// } else {
///     eprintln!("Got an error while backing up '{}': {:?}", file, ret);
/// }
/// ```
pub fn perform_backup(
    backup_mode: BackupMode,
    file_path: &Path,
    suffix: &str,
) -> Result<PathBuf, io::Error> {
    if let Some(backup_to) = get_backup_path(backup_mode, file_path, suffix) {
        if fs::rename(file_path, backup_to).is_err() {
            // Get metadata without following symlinks
            let metadata = file_path.symlink_metadata()?;
            let file_type = metadata.file_type();

            if file_type.is_symlink() {
                backup_symlink(file_path, &backup_to)?;
            } else if file_type.is_dir() {
                backup_dir(file_path, &backup_to)?;
            } else {
                fs::copy(file_path, backup_to).and_then(|_| fs::remove_file(file_path))?;
            }
        }

        Ok(backup_to)
    } else {
        Ok(())
    }
}

/// Backup the given symlink (`from`) to the given destination (`to`).
///
/// # Errors
///
/// - **On Windows** returns an error when `from` is a dangling symlink.
/// - **On platforms other than Windows and Linux** returns an error
///   unconditionally, since symlinks aren't supported.
///
/// May return other errors if e.g. the user lacks permissions to perform the
/// requested operation, or files don't exist.
#[inline]
fn backup_symlink(from: &Path, to: &Path) -> io::Result<()> {
    let path_symlink_points_to = fs::read_link(from)?;
    #[cfg(unix)]
    {
        unix::fs::symlink(&path_symlink_points_to, &to).and_then(|_| fs::remove_file(&from))?;
    }
    #[cfg(windows)]
    {
        if path_symlink_points_to.exists() {
            // Copy symlink
            if path_symlink_points_to.is_dir() {
                windows::fs::symlink_dir(&path_symlink_points_to, &to)?;
            } else {
                windows::fs::symlink_file(&path_symlink_points_to, &to)?;
            }
            // Remove previous symlink
            fs::remove_file(&from)?;
        } else {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                "can't determine symlink type, since it is dangling",
            ));
        }
    }
    #[cfg(not(any(windows, unix)))]
    {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "your operating system does not support symlinks",
        ));
    }
    Ok(())
}

/// Backup the given dir (`from`) to the given destination (`to`).
///
/// # Errors
///
/// Returns an error if the user lacks permission to access the destination to
/// backup to.
#[inline]
fn backup_dir(from: &Path, to: &Path) -> io::Result<()> {
    // We remove the destination directory if it exists to match the
    // behavior of `fs::rename`. As far as I can tell, `fs_extra`'s
    // `move_dir` would otherwise behave differently.
    if to.exists() {
        fs::remove_dir_all(to)?;
    }
    let options = DirCopyOptions {
        // From the `fs_extra` documentation:
        // "Recursively copy a directory with a new name or place it
        // inside the destination. (same behaviors like cp -r in Unix)"
        copy_inside: true,
        ..DirCopyOptions::new()
    };
    if let Err(err) = move_dir(from, to, &options) {
        return match err.kind {
            fs_extra::error::ErrorKind::PermissionDenied => Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "Permission denied",
            )),
            _ => Err(io::Error::new(io::ErrorKind::Other, format!("{:?}", err))),
        };
    }
    Ok(())
}
