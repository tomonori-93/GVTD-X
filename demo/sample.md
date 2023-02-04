# Demo
You can find some demo programs here. 
The programs were used to make the figures in [paper](https://doi.org)


# DCL installation
You need to install DCL ([Debian](http://www.gfd-dennou.org/arch/cc-env/debian-dennou/index.htm.en)/[Ubuntu](http://www.gfd-dennou.org/arch/cc-env/ubuntu-dennou/index.htm.en)/[Source](https://www.gfd-dennou.org/arch/dcl/)) which is a Fortran library for drawing. 


# Bulid
1. You edit `Mkinclude`, depending on your environment. 
2. Run `make`


# USAGE
```
./GVTDX_Dradar < GVTDX_Dradar.nml
```


# Namelist
You can find the template `GVTDX_Dradar.nml`: 
```
  integer :: nr_org            !! the radial grid number of the original data
  integer :: nt_org            !! the azimuthal grid number of the original data
  integer :: nz                !! the vertical grid number of the original data
  integer :: nrot              !! the rotating maximum wavenumber used in the retrieval
  integer :: ndiv              !! the divergent maximum wavenumber used in the retrieval
  integer :: nrdiv             !! radial grid number where the divergence is defined
  integer :: nthres_undef(2)   !! thresholds of the azimuthal sampling number to determine the (1) innermost and (2) outermost radii, respectively
  integer :: smooth_r          !! resampled radial grid number from the original data
  integer :: smooth_t          !! resampled azimuthal grid number from the original data
  integer :: nnz(2)            !! calculating vertical grid levels
  integer :: skip_min_t        !! threshold of the azimuthal sampling number to determine unused radii
  integer :: flag_GVTDX        !! 1: GVTD-X, 2: GVTD, 3: GBVTD
  real :: undefobs             !! undefined value for the original Doppler radar data
  double precision, dimension(nrdivmax) :: rdiv_t  !! radial grids where the divergence is defined
  double precision :: dpoint_x !! longitudinal position of the radar (degree)
  double precision :: dpoint_y !! latitudinal position of the radar (degree)
  double precision :: rmin     !! the innermost radius where Doppler velocity is defined in the original data (m)
  double precision :: dr       !! the radial grid spacing where Doppler velocity is defined (m)
  double precision :: tmin     !! the start of the azimuthal angle where Doppler velocity is defined in the original data (deg)
  double precision :: dt       !! the azimuthal angle interval where Doppler velocity is defined (deg)
  double precision :: zmin     !! the bottom height where Doppler velocity is defined in the original data (m)
  double precision :: dz       !! the vertical grid spacing where Doppler velocity is defined (m)
  character(1000) :: listname  !! ASCII file name listing file name and other information
```


# File format set in `listname`
Support ASCII format with `6+2*nz` columns and `nl+2` lines, where `nz` and `nl` are the vertical levels and times in the Doppler velocity data.
For example, as follows (`nz=2`,`nl=10`): 
```
'Filename'  'time'  'TC (LON)'  'TC (LAT)'  'Us-x'  'Vs-y'  'Um-x-z1km'  'Vm-y-z1km'  'Um-x-z2km'  'Vm-y-z2km'  
'Filename'  's'     'degree'    'degree'    'ms-1'  'ms-1'  'ms-1'       'ms-1'       'ms-1'       'ms-1'       
'data0.dat' 0.0     130.0       15.0        3.5     4.4     1.0          2.0          5.0          -1.0         
'data1.dat' 300.0   130.3       15.0        3.6     4.4     1.2          2.3          5.2          -1.1         
'data2.dat' 600.0   130.6       15.0        3.6     4.4     1.4          2.6          5.2          -1.1         
'data3.dat' 900.0   130.9       15.1        3.6     4.4     1.2          2.9          5.2          -1.2         
'data4.dat' 1200.0  131.3       15.1        3.6     4.4     1.2          2.2          5.2          -1.3         
'data5.dat' 1500.0  131.6       15.1        3.6     4.4     1.1          2.3          5.5          -1.1         
'data6.dat' 1800.0  131.9       15.3        3.7     4.4     1.1          2.2          5.5          -1.2         
'data7.dat' 2100.0  132.2       15.3        3.7     4.4     1.0          2.2          5.5          -1.1         
'data8.dat' 2400.0  132.5       15.3        3.7     4.4     1.0          2.1          5.5          -1.2         
'data9.dat' 2700.0  132.8       15.3        3.7     4.4     1.0          2.1          5.1          -1.0         
```
`dataX.dat` is the Doppler velocity data. 

Output file name is produced by appending the footer `.GVTDX` in each input file name


# Format of the Doppler velocity data files
Support the 4-byte binary format for the input Doppler velocity data as follows (`data.dat`): 
```
integer :: nr_org=100  ! radial grid number
integer :: nt_org=200  ! azimuthal grid number
integer :: nz=10       ! vertical grid number
real :: vel(nr_org,nt_org,nz)  ! Doppler velocity
...
open(unit=n,file='data.dat',access='direct',recl=4*nr_org*nt_org*nz)
write(unit=n,rec=1) (((vel(i,j,k),i=1,nr_org),j=1,nt_org),k=1,nz)
close(unit=n)
```

