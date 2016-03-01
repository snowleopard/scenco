# SCENCO

Collection of encoding algorithms for conditional graphs. Various pieces of code currenctly available in https://github.com/allegroCoder/SCENCO and http://www.workcraft.org/ put together.

[![Build Status](https://travis-ci.org/tuura/scenco.svg?branch=master)](https://travis-ci.org/tuura/scenco) [![Build status](https://ci.appveyor.com/api/projects/status/k93mdkwlnxkgibwj/branch/master?svg=true)](https://ci.appveyor.com/project/snowleopard/scenco/branch/master)

Compositional specification of asynchronous circuits using behavioural
concepts.

### Build

	cabal build

### Test

	cabal test --show-details=always

#### How to install ABC

"ABC is a growing software system for synthesis and verification of binary sequential logic circuits appearing in synchronous hardware designs. ABC combines scalable logic optimization based on And-Inverter Graphs (AIGs), optimal-delay DAG-based technology mapping for look-up tables and standard cells, and innovative algorithms for sequential synthesis and verification." For further information, and for the download either of the sources or the binary of the tool, please refer to the reference website:

http://www.eecs.berkeley.edu/~alanmi/abc/

We use `ABC` either for the synthesis and mapping process. All the functionalities provided by `SCENCO`, can be used only when ABC is properly installed. Add the folder, where the binary is present, into the `PATH` variable of the system. Follow the following instructions depending on the OS that you use.

###### Linux & Mac OS X 
1) Refer to the following website for the download and compilation of the most up to date version of the tool: http://www.eecs.berkeley.edu/~alanmi/abc/

2) Type and run in the terminal: `export PATH="$PATH:[ABC_absolute_folder_path]"` to add `ABC` into the system variable `PATH`

###### Windows
1) A ready-to-use binary version could be downloaded at the following link: http://www.eecs.berkeley.edu/~alanmi/abc/abc10216.exe

2) Type and run in the command line: `set "PATH=%PATH%;[ABC_absolute_folder_path]"` to add `ABC` into the system variable `PATH`

