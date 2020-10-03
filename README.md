# fsmark

_A bookmarker for your filesystem_

## Introduction

fsmark is a tool which provides quick and easy access to potentially long path
names.

It is designed around being easy to pipe to other processes and providing the
smallest amount of typing overhead possible.

It does ***not*** have a GUI and is designed for a unix-ish terminal
environment. It can run on windows but is not very useful without powershell
(since `cmd.exe` cannot do pipes).

## Getting started

The executable name for fsmark is `fsm`, which will hereafter be used when
referring to fsmark.

### Concepts

fsm stores two types of information: entries and shelves. Each entry exists on a
shelf and must have a unique name in the context of that shelf (entries can have
conflicting names as long as they exist on different shelves).

fsm prioritizes entries over shelves. This means that `fsm add <path> -n <name>`
adds an entry to the selected shelf, while `fsm shelves add <name>` adds a new
shelf. To set the selected shelf add the `--shelf <name>` option, if this option
is not specified then the default shelf is used.

### Basic usage

A full list of the available commands can be viewed using `fsm --help`.

#### Add a path to the default shelf

```bash 
fsm add <path> -n <name>
```

Note: If `-n` is omitted then a dialog will be provided to choose a name for the
new entry.

#### Create a new shelf

```bash 
fsm shelves add <name> 
```

#### Add a path to a specific shelf

```bash 
fsm add <path> -n name --shelf <shelf name> 
```

#### Output the full path to an entry

```bash
fsm fp <name> 
```

#### Output a list of all entries on a shelf

```bash
fsm list --shelf <shelf name> 
```

Note: This will output all the paths of each entry, each on a separate line. To
view full information about each entry (such as its name) add `-f|--full`.

To narrow down the results slightly, the `-s|--search` option can be passed to
filter by name:

```bash
fsm list -s anything_starting_with_this* 
```

## Examples

The [fsm utils](/scripts/fsm_utils.sh) script contains sh functions for aliasing
frequently used fsm commands. It currently includes:

- `jmp`: Alias for `cd $(fsm fp <path>)`
- `catm`: Alias for `cat $(fsm fp <path>)`
- `editm`: Alias for `$EDITOR $(fsm fp <path>)`

The script is not included in the installer and doesn't work on windows, it is
more an example of how fsm can be utilized through wrapper scripts. (If anybody
can write an equivalent powershell script that would be awesome).

### Creating every directory listed on a specified shelf

```bash
mkdir -p $(fsm list --shelf my_shelf | xargs) 
```

### Using with `cd`

```bash
fsm add "~/work/cs/projects/hs" workspace cd $(fsm fp workspace) 
```

This will move your current directory to `~/work/cs/projects/hs`

### Using with `cat`

```bash 
fsm add "~/mydir/super/long/path/file.txt" my_file cat $(fsm fp my_file)
```

## Installing

If you are on windows you can grab the installer from the
[releases](https://github.com/0x00002a/fsmark/releases) page. If you are not on
windows then you can build it from source.

1. Install `stack`, this should be a simple matter of typing `<your package
manager> install stack`
2. Clone this repository
3. Run `stack install` in the root of this repository
4. Run `fsm version` to verify that fsm has installed correctly

## Philosophy & Design

The central design goal of fsm was to have something which was easier to type on
the command-line than a full path, and which could be used everywhere a full
path could be (e.g as input to `cd`). The interface of fsm is centered around a
few simple assumptions and principles:

1. Output is assumed to be used as input into another process unless otherwise
specified
2. Path information should be the default require the least typing/effort on the
part of the user
3. "Human output" should be available but not default

An example of these design principles in the interface is the `list` command: by
default, it outputs raw path information which can be fed to `xargs` or similar;
viewing information such as the names for each entry is a separate switch.

## License

This project is licensed under the GNU GPLv3. A copy of the license is included
with the sources [here](/LICENSE)

> Copyright (C) 2020 Natasha England-Elbro
>
> This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.
>
> This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
more details.
>
> You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
