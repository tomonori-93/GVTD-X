# Installation
You can use the `configure` script with libtool: 
```
$ export FC=gfortran
$ export FCFLAGS="-ffree-line-length-none"
$ ./configure --prefix=XXX --includedir=XXX/include  # XXX is the destination of the install
$ make
$ make install
```
