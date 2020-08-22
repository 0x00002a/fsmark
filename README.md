# file-shelf
_Shelve your file paths_

## Introduction

file-shelf is a tool which provides quick access to lists ("shelves") of paths. Each path on a shelf has a name, allowing quick access to possibly long paths.

file-shelf is designed to have its output utilized through unix "pipes". This allows further scripts to be built up around it to do things like, automatic `cd`.

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

## Basic usage

A full list of the available commands can be viewed by doing `fsm --help`, `fsm <command> --help`, etc.

### Add a path to the default shelf

```bash
fsm add <path> -n <name>
```

### Create a new shelf

```bash
fsm shelves add <name>
```

### Add a path to a specific shelf

```bash
fsm add <path> -n name --shelf <shelf name>
```

### Output the full path to an entry

```bash
fsm fp <name>
```

### Output a list of all entries on a shelf

```bash
fsm list --shelf <shelf name>
```

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

