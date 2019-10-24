namespace Orchid.IO

open System.IO

module public Directory =

    let exists dir = Directory.Exists(dir)

    /// Returns a sequence of each file in the given directory
    let files dir: seq<FileInfo> =
        let dirInfo = new DirectoryInfo(dir)
        dirInfo.GetFiles()
        |> Seq.ofArray

    /// Returns a sequence of each child directory in the given directory
    let directories parent: seq<DirectoryInfo> =
        let dirInfo = new DirectoryInfo(parent)
        dirInfo.GetDirectories()
        |> Seq.ofArray

    /// Returns a sequence containing all files in the given directory that match the given predicate
    let fileFilter (pred: FileInfo -> bool)  dir =
        dir
        |> files
        |> Seq.filter pred

    /// Returns a sequence containing all child directories in the given parent that match the given predicate
    let directoryFilter (pred: DirectoryInfo -> bool) parent =
        parent
        |> directories
        |> Seq.filter pred



