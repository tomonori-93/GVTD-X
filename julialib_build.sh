if [ -e lib ]; then
   rm -r lib
fi
cd src
rm *.mod *.o
mkdir ../lib
gfortran -O3 -fPIC -c sub_mod.f90
gfortran -O3 -fPIC -c gvtdx_main_mod.f90
gfortran -O3 -fPIC -c gbvtd_main_mod.f90
gfortran -O3 -fPIC -c gvtd_main_mod.f90
gfortran -O3 -fPIC -c tools_sub.f90
gfortran -O3 -fPIC -c c_interface.f90

gfortran -shared *.o -o ../lib/liblibGVTDX.so
