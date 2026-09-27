opt="-O3 -fPIC -g -fbacktrace -fbounds-check"
if [ -e lib ]; then
   rm -r lib
fi
cd src
rm *.mod *.o
mkdir ../lib
gfortran ${opt} -c sub_mod.f90
gfortran ${opt} -c gvtdx_main_mod.f90
gfortran ${opt} -c gbvtd_main_mod.f90
gfortran ${opt} -c gvtd_main_mod.f90
gfortran ${opt} -c tools_sub.f90
gfortran ${opt} -c retrieval_control.f90
gfortran ${opt} -c c_interface.f90

gfortran -shared *.o -o ../lib/libGVTDX.so
