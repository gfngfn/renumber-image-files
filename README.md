# `renum-files-exe`

This is a small tool for renumbering images developed for @gfngfn's personal use.

## How to Compile

```
$ stack build
```

## How to Install

The following command will install binary executable `renum-files-exe`:

```
$ stack install
```

## How to Use

### Checking

The following command checks whether the given directory contains files with inconsistent indices (e.g. containts both `foo004.jpg` and `foo004_01.jpg`):

```console
$ renum-files-exe --check source/
```

Filenames must match `\a+\d\d\d(\_\d\d)?\.\a+`.


### Normalization

The following command normalizes indices (i.e. eliminates all the gaps between indices):

```console
$ renum-files-exe --normalize target/
```


### Unification

```console
# the source directory, the contents of which have been added manually
$ ls source/
bar003.png
bar008_01.jpeg
bar008_02.jpeg
foo001.jpeg
foo004_01.png
foo004_02.png
foo042.jpg

# the target directory, where files are to be kept with normalized indices
$ ls target/
bar001.jpg
bar002_01.png
bar002_02.png
bar002_03.png
bar003.png
bar004.jpg
foo001_01.jpg
foo001_02.png
foo002.jpg

$ renum-files-exe --normalize source/ target/
# (some texts on stdout)

$ ls source/

$ ls target/
bar001.jpg
bar002_01.png
bar002_02.png
bar002_03.png
bar003.png
bar004.jpg
bar005.png     # <= source/bar003.png
bar006_01.jpg  # <= source/bar008_01.jpeg
bar006_02.jpg  # <= source/bar008_02.jpeg
foo001_01.jpg
foo001_02.png
foo002.jpg
foo003.jpg     # <= source/foo001.jpeg
foo004_01.png  # <= source/foo004_01.png
foo004_02.png  # <= source/foo004_02.png
foo005.jpg     # <= source/foo042.jpg
```


### Summing up

The following command counts how many indices are used for each tag:

```console
$ renum-files-exe --sum-up target/
```
