# Demo
You can find some demo programs here. 
The programs were used to make the figures in [paper](https://doi.org)

# Without DCL
Some demo programs requires a drawing library DCL. 
If you don't install DCL, you can perform a test program `test_Rankine_ax.f90` to check the retrieval accuracy three techniques (i.e., GVTD-X/GVTD/GBVTD) for an analytical Rankine vortex. 
```
$ cd demo/
$ make  # -> build "test_Rankine_ax"
$ ./test_Rankine_ax < test_Rankine_ax.nml
... (running) ...
*** MESSAGE(main) *** : Pass the test  # -> If this message is displayed, the libGVTDX.a passed the test
```


# With DCL
If DCL ([Debian](http://www.gfd-dennou.org/arch/cc-env/debian-dennou/index.htm.en)/[Ubuntu](http://www.gfd-dennou.org/arch/cc-env/ubuntu-dennou/index.htm.en)/[Source](https://www.gfd-dennou.org/arch/dcl/)) is installed in your machine, you can use [sample programs](demo/sample.md), which were used to make the figures in the [paper](https://doi.org/), in `demo/`.
```
$ make -f Makefile.dcl  # -> build test_Rankine[1-6]_dcl
$ ./test_Rankine[1-6]_dcl < test_Rankine[1-6]_dcl.nml
```


## DCL installation
You need to install DCL ([Debian](http://www.gfd-dennou.org/arch/cc-env/debian-dennou/index.htm.en)/[Ubuntu](http://www.gfd-dennou.org/arch/cc-env/ubuntu-dennou/index.htm.en)/[Source](https://www.gfd-dennou.org/arch/dcl/)) which is a Fortran library for drawing. 


## Bulid
1. You edit `Mkinclude`, depending on your environment. 
```
FC = fortran compiler
FCFLAGS = options
DCLFC   = the path of "dclfrt" command in DCL
```
2. Run `make`


## USAGE
```
./test_Rankine1_dcl < test_Rankine1_dcl.nml
```

