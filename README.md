# Scenco

Collection of encoding algorithms for conditional graphs. Various pieces of code currenctly available in https://github.com/allegroCoder/SCENCO and http://www.workcraft.org/ put together.

[![Build Status](https://travis-ci.org/tuura/scenco-core.svg?branch=master)](https://travis-ci.org/tuura/scenco-core) [![Build status](https://ci.appveyor.com/api/projects/status/k93mdkwlnxkgibwj/branch/master?svg=true)](https://ci.appveyor.com/project/snowleopard/scenco/branch/master)

Compositional specification of asynchronous circuits using behavioural concepts.

## Getting the sources

Navigate to a directory to store the library and run this command to clone the code:

`git clone https://github.com/tuura/scenco-core.git`

## Building Scenco from command line

Enter the scenco directory and compile Scenco using the following command:

```bash
ghc --make -isrc -iscenco -lstdc++ src/c++/scenco.cpp -O2 scenco/Main.hs -o scenco -Wall -fwarn-tabs
```

## Building Scenco from cabal

You can build the library and executables using Cabal.

## Using Scenco

The following usage info can be obtained by running `scenco --help`:

```
Usage: scenco [input file] [tech lib] [OPTIONS...]
  -e METHOD   --encoding=METHOD      Encoding method: sequential (default), single-literal, random, heuristic, exhaustive.
  -c FILE     --constraints=FILE     Encoding constraints
  -n INTEGER  --n-solutions=INTEGER  Number of solutions to generate (higher is better/slower)
  -o FILE     --output=FILE          Output file
  -m          --microcontroller      Optimise the resulting microcontroller instead of graph family
  -v FILE     --verilog=FILE         Write the microcontroller into a Verilog file
  -h          --help                 Show this help message
```

### Build

```bash
cabal configure --disable-tests
cabal build
```
### Test

```bash
cabal test --show-details=always
```

### Run

```
cabal run scenco -- [command line options]
```

## How to install ABC

"ABC is a growing software system for synthesis and verification of binary sequential logic circuits appearing in synchronous hardware designs. ABC combines scalable logic optimization based on And-Inverter Graphs (AIGs), optimal-delay DAG-based technology mapping for look-up tables and standard cells, and innovative algorithms for sequential synthesis and verification." For further information, and for the download either of the sources or the binary of the tool, please refer to the reference website:

http://www.eecs.berkeley.edu/~alanmi/abc/

We use `ABC` either for the synthesis and mapping process. Add the folder, where the binary is present, into the `PATH` variable of the system. Follow the following instructions depending on the OS that you use.

##### Linux & Mac OS X
1) Refer to the following website for the download and compilation of the most up to date version of the tool: http://www.eecs.berkeley.edu/~alanmi/abc/

2) Type and run in the terminal: `export PATH="$PATH:[ABC_absolute_folder_path]"` to add `ABC` into the system variable `PATH`

##### Windows
1) A ready-to-use binary version could be downloaded at the following link: http://www.eecs.berkeley.edu/~alanmi/abc/abc10216.exe

2) Type and run in the command line: `set "PATH=%PATH%;[ABC_absolute_folder_path]"` to add `ABC` into the system variable `PATH`

