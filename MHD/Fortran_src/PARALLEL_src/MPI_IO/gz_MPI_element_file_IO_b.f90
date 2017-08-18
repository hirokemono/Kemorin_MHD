!>@file  gz_MPI_element_file_IO_b.f90
!!      module gz_MPI_element_file_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine gz_mpi_input_element_file_b                          &
!!     &         (nprocs_in, my_rank_IO, file_prefix, ele_mesh_IO, ierr)
!!      subroutine gz_mpi_input_surface_file_b                          &
!!     &         (nprocs_in, my_rank_IO, file_prefix, surf_mesh_IO, ierr)
!!      subroutine gz_mpi_input_edge_file_b                             &
!!     &         (nprocs_in, my_rank_IO, file_prefix, edge_mesh_IO, ierr)
!!
!!      subroutine gz_mpi_output_element_file_b                         &
!!     &         (nprocs_in, my_rank_IO, ele_mesh_IO)
!!      subroutine gz_mpi_output_surface_file_b                         &
!!     &         (nprocs_in, my_rank_IO, file_prefix, surf_mesh_IO)
!!      subroutine gz_mpi_output_edge_file_b                            &
!!     &         (nprocs_in, my_rank_IO, file_prefix, edge_mesh_IO)
!!        type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!!        type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!!@endverbatim
!!
!!@param my_rank_IO  MPI rank
!
      module gz_MPI_element_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_calypso_mpi_IO_param
      use t_read_mesh_data
      use set_mesh_file_names
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param
      character(len=kchara), private :: file_name
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_input_element_file_b                            &
     &         (nprocs_in, my_rank_IO, file_prefix, ele_mesh_IO, ierr)
!
      use gz_MPI_element_data_IO_b
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call set_ele_comm_file_name(file_prefix, iflag_single,            &
     &    my_rank_IO, file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Read ascii element comm file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call gz_mpi_read_ele_comm_table_b(IO_param, ele_mesh_IO%comm)
      call gz_mpi_read_ele_geometry_b(IO_param,                         &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_input_element_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_input_surface_file_b                            &
     &         (nprocs_in, my_rank_IO, file_prefix, surf_mesh_IO, ierr)
!
      use gz_MPI_surface_data_IO_b
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call set_surf_mesh_file_name(file_prefix, iflag_single,           &
     &    my_rank_IO, file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Read ascii surface mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call gz_mpi_read_surf_connect_b(IO_param, surf_mesh_IO%comm,      &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed)
      call gz_mpi_read_surf_geometry_b(IO_param,                        &
     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_input_surface_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_input_edge_file_b                               &
     &         (nprocs_in, my_rank_IO, file_prefix, edge_mesh_IO, ierr)
!
      use gz_MPI_edge_data_IO_b
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call set_edge_mesh_file_name(file_prefix, iflag_single,           &
     &    my_rank_IO, file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Read ascii edge mesh file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call gz_mpi_read_edge_connect_b(IO_param, edge_mesh_IO%comm,      &
     &    edge_mesh_IO%ele, edge_mesh_IO%sfed)
      call gz_mpi_read_edge_geometry_b(IO_param,                        &
     &    edge_mesh_IO%node, edge_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_input_edge_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_output_element_file_b                           &
     &         (nprocs_in, my_rank_IO, file_prefix, ele_mesh_IO)
!
      use gz_MPI_element_data_IO_b
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      call set_ele_comm_file_name(file_prefix, iflag_single,            &
     &    my_rank_IO, file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write ascii element comm file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call gz_mpi_write_ele_comm_table_b(IO_param, ele_mesh_IO%comm)
      call gz_mpi_write_ele_geometry_b(IO_param,                        &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_output_element_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_output_surface_file_b                           &
     &         (nprocs_in, my_rank_IO, file_prefix, surf_mesh_IO)
!
      use gz_MPI_surface_data_IO_b
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: surf_mesh_IO
!
!
      call set_surf_mesh_file_name(file_prefix, iflag_single,           &
     &    my_rank_IO, file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write ascii surface mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call gz_mpi_write_surf_connect_b(IO_param, surf_mesh_IO%comm,     &
     &   surf_mesh_IO%ele, surf_mesh_IO%sfed)
      call gz_mpi_write_surf_geometry_b(IO_param,                       &
     &    surf_mesh_IO%node, surf_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_output_surface_file_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_output_edge_file_b                              &
     &         (nprocs_in, my_rank_IO, file_prefix, edge_mesh_IO)
!
      use gz_MPI_edge_data_IO_b
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(surf_edge_IO_file), intent(inout) :: edge_mesh_IO
!
!
      call set_edge_mesh_file_name(file_prefix, iflag_single,           &
     &    my_rank_IO, file_name)
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) write(*,*)                &
     &  'Write ascii edge mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs_in, my_rank_IO, IO_param)
!
      call gz_mpi_write_edge_connect_b(IO_param, edge_mesh_IO%comm,     &
     &   edge_mesh_IO%ele, edge_mesh_IO%sfed)
      call gz_mpi_write_edge_geometry_b(IO_param,                       &
     &   edge_mesh_IO%node, edge_mesh_IO%sfed)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_output_edge_file_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_element_file_IO_b
