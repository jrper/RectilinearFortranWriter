# Fortran VTK Rectilinear Grid Writer


Example of a transparent (ish) Fortran interface for the vtkXMLPRectilinearGridWriter class. This is actually really simple in the serial case, but not quite as easy in parallel, especially if you aren't willing to let VTK decide how your grid gets decomposed.

The actual library code is in the `./writer` directory, with an example of it running in action in `./example`. Run the following in both directories
```
cmake .
make
```
then run `./example/example`, possibly under MPI and examine the output files.
