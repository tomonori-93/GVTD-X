# How to use GVTD-X
You can use the subroutines and functions by linking the build library (`libGVTDX.a`) to your Fortran main programs (ex., `main.f90`) as follows: 
```
$ gfortran -IXXX/include main.f90 -LXXX -lGVTDX -o main  # -> The executable file of `main` will be built
```

* You can find [example programs](./) in `tools/`
  * There are sample programs with real data from single-Doppler radar observations. 

# Program list 
* [`GVTDX_Dradar`](docs/GVTDX_Dradar.md)
  * To retrieve TC circulations from single-Doppler radar observations by using GVTD-X (or GBVTD/GVTD). 
* [`GVTDX_budgets`](docs/GVTDX_budgets.md)
  * To calculate budget equations of angular momentum, vorticity, and enstrophy with output by `GVTDX_Dradar`. 

# Compiling
By running `make`, the programs will be compilied. 


# Tips
* You can use options for OpenMP in `FCFLAGS` to perform multi-threaded parallel processing (e.g., gfortran: `-fopenmp`, ifort: `-qopenmp`). 
