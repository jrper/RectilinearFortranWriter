program example1
  use writer
  use iso_c_binding
  use mpi

  implicit none

  integer, parameter :: NX=11, NY=11, NZ=1
  double precision :: data(1, NX, NY, NZ), x(NX), y(NY), z(NZ)
  integer :: i, j, k, idata(3, NX, NY, NZ), cdata(1, NX-1, NY-1, NZ)
  type(c_ptr) :: grid
  double precision, parameter :: DX = 0.1, DY=0.1, DZ=0.1

  integer :: ierr, rank, nprocs, offset(3), global_extent(6)

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  

  x = rank*NX*DX+[(i*DX, i=0, NX-1)]
  y = [(i*DY, i=0, NY-1)]
  z = [(i*DZ, i=0, NZ-1)]
  
  offset = [(NX-1)*rank,0,0]
  global_extent=[0,30,0,10,0,0]
  
  do k=1, NZ
     do j=1, NY
        do i=1, NX
           data(1, i, j, k) = (2*x(i))**2+y(j)**2
           idata(1, i ,j, k) = 2*i+j
           idata(2, i, j, k) = j
           idata(3, i, j, k) = -i
        end do
     end do
  end do

 do k=1, NZ
     do j=1, NY-1
        do i=1, NX-1
           cdata(1, i, j, k) = i**3+j**2
        end do
     end do
  end do

  call create_new_vtk_grid(grid, x, y, z, offset)
  call add_point_array(grid, "My scalar data", data)
  call add_point_array(grid, "My integer data", idata)
  call add_cell_array(grid, "My cell data", cdata)
  call write_vtk_grid(grid, fix_name("bob.pvtr"), nprocs, rank, global_extent)
  call destroy_vtk_grid(grid)

  call MPI_Finalize(ierr)

end program example1
