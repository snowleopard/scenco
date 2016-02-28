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

We use `ABC` either for the synthesis and mapping process. All the functionalities provided by `SCENCO`, can be used only when ABC is installed in the system. Place the binary of ABC in the root folder of SCENCO in order to run the tool properly, or add the main folder where ABC is present into the `PATH` variable of the system.

- Linux & Mac OS X : for these two OSes, refer to the following website for the download and compilation of the most up to date version of the tool: http://www.eecs.berkeley.edu/~alanmi/abc/

- Windows: for those of you having Windows installed on your machine, a ready-to-use binary version could be downaload at the following link: http://www.eecs.berkeley.edu/~alanmi/abc/abc10216.exe

##### Note

Scenco might show up some issues running under Mac OS X. We are working for fixing them.
