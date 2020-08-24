# FSM

_File Shelving Manager_

## Introduction

fsm is a tool which provides quick access to lists ("shelves") of paths. Each path on a shelf has a name, allowing quick access to possibly long paths.

fsm is designed to have its output utilized through unix pipes. This allows further scripts to be built up around it. For this reason it doesn't really work when using `cmd.exe`, however it does work when using powershell (and I find it very useful for windows paths).

## Getting started

First [install](#installing) `fsm`. If you are on windows you may also need to add it to the path.

### Concepts

fsm stores two types of information: entries and shelves. Each entry exists on a shelf and must have a unique name in the context of that shelf (entries can have conflicting names as long as they exist on different shelves).

fsm prioritizes entries over shelves. This means that `fsm add <path> -n <name>` adds an entry to the selected shelf, while `fsm shelves add <name>` adds a new shelf. To set the selected shelf add the `--shelf <name>` option, if this option is not specified then the default shelf is used.

### Basic usage

A full list of the available commands can be viewed using `fsm --help`.

#### Add a path to the default shelf

```bash
fsm add <path> -n <name>
```

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

Note: This will output all the paths of each entry, each on a seperate line. To view full information about each entry (such as its name) add `--full`.

To narrow down the results slightly, the `-n|--name` option can be passed to filter by name:

```bash
fsm list -n anything_starting_with_this*
```

## Examples

### Creating every directory listed on a specified shelf

```bash
mkdir -p $(fsm list --shelf my_shelf | xargs)
```

### Using with `cd`

```bash
fsm add "~/work/cs/projects/hs" workspace
cd $(fsm fp workspace)
```

This will move your current directory to `~/work/cs/projects/hs`

### Using with `cat`

```bash
fsm add "~/mydir/super/long/path/file.txt" my_file
cat $(fsm fp my_file)
```

## Installing

If you are on windows you can grab the installer from the [releases](https://github.com/0x00002a/file-shelf/releases) page. If you are not on windows then you can build it from source.

1. Install `stack`, this should be a simple matter of typing `<your package manager> install stack`
2. Clone this repository
3. Run `stack install` in the root of this repository
4. Run `fsm version` to verify that fsm has installed correctly

## Philosophy & Design

The central design goal of fsm was to have something which was easier to type on the command-line than a full path. For this reason the interface of fsm is centered around two things:

1. The expectation that its output will be processed by another program, most of the time.
2. Path information should be the default output mode and require the least typing

As an example there is the `list` command, by default it outputs raw path information which can be fed to `xargs` or similar - requiring a flag to switch to human-readable results.

## License

This project is licensed under the GNU GPLv3. A copy of the license is included with the sources [here](/LICENSE)

>Copyright (C) 2020 Natasha England-Elbro
>
> This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
>
>This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
>
>You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

