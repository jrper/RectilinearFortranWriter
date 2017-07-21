module writer

  use iso_c_binding

  implicit none

  integer, parameter :: VTK_DOUBLE=11, VTK_FLOAT=10, VTK_INT=6, VTK_LONG=8

  interface

     subroutine create_vtk_grid(grid, dims, extent, x, y, z, datatype) bind(c)
       use iso_c_binding
       type(c_ptr), intent(out) :: grid
       integer(c_int), intent(in) :: dims(3), extent(6)
       type(c_ptr), intent(in), value :: x, y, z
       integer(c_int), intent(in), value :: datatype 
     end subroutine create_vtk_grid

     subroutine add_point_data(grid, name, ncomponents, data, datatype) bind(c)
       use iso_c_binding
       type(c_ptr), intent(in) :: grid
       character(c_char), intent(in) :: name(*)
       integer(c_int), intent(in), value :: ncomponents, datatype
       type(c_ptr), intent(in), value :: data
     end subroutine add_point_data

     subroutine add_cell_data(grid, name, ncomponents, dims, data, datatype) bind(c)
       use iso_c_binding
       type(c_ptr), intent(in) :: grid
       character(c_char), intent(in) :: name(*)
       integer(c_int), intent(in) :: dims(3)
       integer(c_int), intent(in), value :: ncomponents, datatype
       type(c_ptr), intent(in), value :: data
     end subroutine add_cell_data

     subroutine write_vtk_grid(grid, name, npieces, piece, global_extent) bind(c)
       use iso_c_binding
       type(c_ptr), intent(in) :: grid
       character(c_char), intent(in) :: name(*)
       integer(c_int), intent(in), value :: npieces, piece
       integer(c_int), intent(in) :: global_extent(6)
     end subroutine write_vtk_grid

     subroutine destroy_vtk_grid(grid) bind(c)
       use iso_c_binding
       type(c_ptr), intent(inout) :: grid
     end subroutine destroy_vtk_grid
     

  end interface

  interface add_point_array
     module procedure add_point_integer_data, add_point_double_data
  end interface add_point_array

  interface add_cell_array
     module procedure add_cell_integer_data, add_cell_double_data
  end interface add_cell_array

  interface create_new_vtk_grid
     module procedure create_double_vtk_grid, create_float_vtk_grid
  end interface create_new_vtk_grid

  interface ref
     module procedure dref, fref, iref
  end interface ref

  contains 

    subroutine create_double_vtk_grid(grid, x, y, z, offset)
      type(c_ptr), intent(inout) :: grid
      real(c_double), target :: x(:), y(:), z(:)
      integer, intent(in) :: offset(3)

      call create_vtk_grid(grid, [size(x), size(y), size(z)],&
           [offset(1), offset(1)+size(x)-1,&
           offset(2), offset(2)+size(y)-1,&
           offset(3), offset(3)+size(z)-1],&        
           ref(x), ref(y), ref(z), VTK_DOUBLE)

    end subroutine create_double_vtk_grid

    subroutine create_float_vtk_grid(grid, x, y, z, offset)
      type(c_ptr), intent(inout) :: grid
      real(c_float), target :: x(:), y(:), z(:)
      integer, intent(in) :: offset(3)

      call create_vtk_grid(grid, [size(x), size(y), size(z)],&
           [offset(1), offset(1)+size(x)-1,&
           offset(2), offset(2)+size(y)-1,&
           offset(3), offset(3)+size(z)-1],&
           ref(x), ref(y), ref(z), VTK_FLOAT)

    end subroutine create_float_vtk_grid
    
    subroutine add_point_integer_data(grid, name, data)
      type(c_ptr), intent(in) :: grid
      character(len=*), intent(in) :: name
      integer(c_int), target, intent(in) :: data(:,:,:,:)
    
      call add_point_data(grid, fix_name(name), size(data,1),&
           ref(data(:,1,1,1)), VTK_INT)

    end subroutine add_point_integer_data

    subroutine add_point_double_data(grid, name, data)
      type(c_ptr), intent(in) :: grid
      character(len=*), intent(in) :: name
      real(c_double), target, intent(in) :: data(:,:,:,:)
    
      call add_point_data(grid, fix_name(name), size(data,1),&
           ref(data(:,1,1,1)), VTK_DOUBLE)

    end subroutine add_point_double_data

    subroutine add_cell_integer_data(grid, name, data)
      type(c_ptr), intent(in) :: grid
      character(len=*), intent(in) :: name
      integer(c_int), target, intent(in) :: data(:,:,:,:)

      integer :: dims(4)

      dims = shape(data)
    
      call add_cell_data(grid, fix_name(name), dims(1),&
           dims(2:4), ref(data(:,1,1,1)), VTK_INT)

    end subroutine add_cell_integer_data

    subroutine add_cell_double_data(grid, name, data)
      type(c_ptr), intent(in) :: grid
      character(len=*), intent(in) :: name
      real(c_double), target, intent(in) :: data(:,:,:,:)

      integer :: dims(4)

      dims = shape(data)
    
      call add_cell_data(grid, fix_name(name), dims(1),&
           dims(2:4), ref(data(:,1,1,1)), VTK_DOUBLE)

    end subroutine add_cell_double_data

    function fix_name(name) result(out)
      
      character(len=*) :: name
      character(len=len(name)+1) :: out

      out = trim(name)//C_NULL_CHAR
      
    end function fix_name

    function dref(x)
      real(c_double), target :: x(*)
      type(c_ptr) :: dref

      dref = c_loc(x)

    end function dref

    function fref(x)
      real(c_float), target :: x(*)
      type(c_ptr) :: fref

      fref = c_loc(x)

    end function fref

    function iref(x)
      integer(c_int), target :: x(*)
      type(c_ptr) :: iref

      iref = c_loc(x)

    end function iref


end module writer
