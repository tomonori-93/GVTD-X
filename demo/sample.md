# Demo
You can find some demo programs here. 
The programs were used to make the figures in [paper](https://doi.org)


# DCL installation
You need to install DCL ([Debian](http://www.gfd-dennou.org/arch/cc-env/debian-dennou/index.htm.en)/[Ubuntu](http://www.gfd-dennou.org/arch/cc-env/ubuntu-dennou/index.htm.en)/[Source](https://www.gfd-dennou.org/arch/dcl/)) which is a Fortran library for drawing. 


# Bulid
1. You edit `Mkinclude`, depending on your environment. 
```
FC = fortran compiler
FCFLAGS = options
DCLFC   = the path of "dclfrt" command in DCL
```
2. Run `make`


# USAGE
```
./test_Rankine1_dcl < test_Rankine1_dcl.nml
```

