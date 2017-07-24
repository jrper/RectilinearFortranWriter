program example1
  use writer
  use iso_c_binding
  use mpi

  implicit none

  integer :: NX, NY, NZ
  double precision, allocatable :: data(:,:,:,:), x(:), y(:), z(:)
  integer :: i, j, k
  integer, allocatable :: idata(:,:,:,:), cdata(:,:,:,:)
  type(c_ptr) :: grid
  double precision, parameter :: DX = 0.1, DY=0.1, DZ=0.1

  integer :: ierr, rank, nprocs, offset(3)

  !!! Setup for a parallel dump if necessary
  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  NX = 10*(rank+1) + 1
  NY = 11
  NZ = 1

  allocate(data(1,NX,NY,NZ), x(NX), y(NY), z(NZ), &
       idata(3,NX,NY,NZ), cdata(1,NX-1,NY-1,NZ))

! this call only needed for vtk 6 and above
  call initialize_vtk()
  
! set up the coordinate vectors needed for the output dump.
  x = 5*(rank)*(rank+1)*DX+[(i*DX, i=0, NX-1)]
  y = [(i*DY, i=0, NY-1)]
  z = [(i*DZ, i=0, NZ-1)]
  
  offset = [5*(rank)*(rank+1),0,0]
 
! make some data
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

  !!! this call creates the vtk grid
  call create_new_vtk_grid(grid, x, y, z, offset)
  !!! these calls add some data
  call add_point_array(grid, "My scalar data", data)
  call add_point_array(grid, "My integer data", idata)
  call add_cell_array(grid, "My cell data", cdata)
  !!! this is the call which actually writes to disk
  if (nprocs == 1) then
     call write_vtk_grid(grid, fix_name("bob.vtr"), nprocs, rank)
  else
     call write_vtk_grid(grid, fix_name("bob.pvtr"), nprocs, rank)
  end if
  !!! cleanup in vtk
  call destroy_vtk_grid(grid)
  !!! cleanup MPI
  call MPI_Finalize(ierr)

end program example1
