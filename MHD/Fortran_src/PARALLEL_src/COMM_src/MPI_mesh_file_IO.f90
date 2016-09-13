!>@file  MPI_mesh_file_IO.f90
!!      module MPI_mesh_file_IO
!!
!!@author  H. Matsui
!!@date      Programmed in Aug., 2016
!!
!>@brief ASCII mesh file IO
!!
!!@verbatim
!!      subroutine mpi_read_mesh_file                                   &
!!     &         (nprocs_in, my_rank_IO, fem_IO)
!!        type(mesh_data), intent(inout) :: fem_IO
!!      subroutine mpi_read_mesh_geometry                               &
!!     &         (nprocs_in, smy_rank_IO, mesh_IO)
!!      subroutine mpi_read_node_size                                   &
!!     &         (nprocs_in, my_rank_IO, mesh_IO)
!!      subroutine mpi_read_geometry_size(nprocs_in, my_rank_IO, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine mpi_write_mesh_file(nprocs_in, my_rank_IO, fem_IO)
!!        type(mesh_data), intent(inout) :: fem_IO
!!@endverbatim
!
      module MPI_mesh_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_read_mesh_data
      use t_mesh_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_file                                     &
     &         (nprocs_in, my_rank_IO, fem_IO)
!
      use mesh_data_IO
      use groups_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(mesh_data), intent(inout) :: fem_IO
!
      integer(kind = kint) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
!
      call read_geometry_data                                           &
     &   (input_file_code, my_rank_IO, fem_IO%mesh, ierr)
      call read_mesh_groups(input_file_code, fem_IO%group)
      close(input_file_code)
!
      end subroutine mpi_read_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_geometry                                 &
     &         (nprocs_in, my_rank_IO, mesh_IO)
!
      use mesh_data_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
!
      integer(kind = kint) :: ierr
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_geometry_data                                           &
     &   (input_file_code, my_rank_IO, mesh_IO, ierr)
      close(input_file_code)
!
!
      end subroutine mpi_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_node_size                                     &
     &         (nprocs_in, my_rank_IO, mesh_IO)
!
      use mesh_data_IO
      use node_geometry_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
!
      integer(kind = kint) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_num_node(input_file_code, my_rank_IO, mesh_IO, ierr)
      close(input_file_code)
!
!
      end subroutine mpi_read_node_size
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geometry_size(nprocs_in, my_rank_IO, mesh_IO)
!
      use mesh_data_IO
      use element_connect_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(mesh_geometry), intent(inout) :: mesh_IO
!
      integer(kind = kint) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_num_node_ele                                            &
     &   (input_file_code, my_rank_IO, mesh_IO, ierr)
      close(input_file_code)
!
      end subroutine mpi_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_mesh_file(nprocs_in, my_rank_IO, fem_IO)
!
      use m_machine_parameter
      use m_fem_mesh_labels
      use m_read_boundary_data
      use mesh_data_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(mesh_data), intent(inout) :: fem_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &   'Write ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
!
      call write_geometry_data(input_file_code, my_rank_IO, fem_IO%mesh)
      call write_mesh_groups(input_file_code, fem_IO%group)
!
      close(input_file_code)
!
      end subroutine mpi_write_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module MPI_mesh_file_IO
