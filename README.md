<!--# ToRMHOWe (ともえ)
ToRMHOWe (Tropical-cyclone circulation Retrieval Method based on the Helmholtz decomposition from Observation of single-Doppler Weather radars) is a Fortran library to retrieve tangential and radial wind components in atmospheric vortices from single-Doppler radar observations. 

ToRMHOWe is pronounced as "To-Moe" and written as 鞆絵 or 巴 in Chinese characters, which is come from a Japanese traditional pattern. 
-->
# GVTD-HeCs
GVTD-HeCs (Generalized Velocity Track Display with the Helmholtz-decomposition-based Closure assumption) is a Fortran library to retrieve tangential and radial wind components in atmospheric vortices from single-Doppler radar observations. 

GVTD-HeCs is pronounced as "GVTD-X". 


# Methods
[Paper](https://doi.org/)

[Method descriptions](https://tomonori-93.github.io/GVTD-HeCs/ford-doc/index.html)

[Formulation and derivation (Japanese document)](doc/formulation.pdf)

# Images
![Test Image 1](image/image1.png)


# Installation
You can use the `configure` script with libtool: 
```
export FC=gfortran
export FCFLAGS="-ffree-line-length-none"
./configure --prefix=XXX --includedir=XXX/include  # XXX is the destination of the install
make
make install
```

### Tips
* You can use options for OpenMP in `FCFLAGS` to perform multi-threaded parallel processing (e.g., gfortran: `-fopenmp`, ifort: `-qopenmp`). 


# USAGE
You can use the subroutines and functions by linking the build library (`libGVTDX.a`) to your Fortran main programs (ex., `main.f90`) as follows: 
```
gfortran -IXXX/include main.f90 -LXXX -lGVTDX -o main  # -> The executable file of `main` will be built
```
* You can find [an example program](tools/GVTDX_Dradar.md) (`GVTDX_Dradar.f90`) in `tools/`


# Demo
If DCL ([Debian](http://www.gfd-dennou.org/arch/cc-env/debian-dennou/index.htm.en)/[Ubuntu](http://www.gfd-dennou.org/arch/cc-env/ubuntu-dennou/index.htm.en)/[Source](https://www.gfd-dennou.org/arch/dcl/)) is installed in your machine, you can use [sample programs](demo/sample.md), which were used to make the figures in the [paper](https://doi.org/), in `demo/`.

