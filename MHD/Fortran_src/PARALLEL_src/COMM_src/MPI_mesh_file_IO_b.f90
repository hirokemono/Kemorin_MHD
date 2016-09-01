!>@file  MPI_mesh_file_IO_b.f90
!!      module MPI_mesh_file_IO_b
!!
!!@author  H. Matsui
!!@date      Programmed in Aug., 2016
!!
!>@brief ASCII mesh file IO
!!
!!@verbatim
!!      subroutine mpi_read_mesh_file_b(my_rank)
!!      subroutine mpi_read_mesh_geometry_b(my_rank)
!!
!!      subroutine mpi_read_node_size_b(my_rank)
!!      subroutine mpi_read_geometry_size_b(my_rank)
!!
!!      subroutine mpi_write_mesh_file_b(my_rank)
!!@endverbatim
!
      module MPI_mesh_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_read_mesh_data
      use binary_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_file_b(my_rank)
!
      use m_machine_parameter
      use m_read_boundary_data
      use mesh_data_IO_b
      use groups_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(mesh_file_name)
!
      call open_read_binary_file(mesh_file_name, my_rank)
!
      call read_geometry_data_b
!
!   read node group
      call read_group_data_b(bc_grp_IO)
!  read element group
      call read_group_data_b(mat_grp_IO)
!  read surface group
      call read_surf_grp_data_b(surf_grp_IO)
!
      call close_binary_file
!
      end subroutine mpi_read_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_geometry_b(my_rank)
!
      use mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(mesh_file_name)
!
      call open_read_binary_file(mesh_file_name, my_rank)
      call read_geometry_data_b
      call close_binary_file
!
      end subroutine mpi_read_mesh_geometry_b
!
!  ---------------------------------------------------------------------
!
       subroutine mpi_read_node_size_b(my_rank)
!
       use domain_data_IO_b
       use mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(mesh_file_name)
!
      call open_read_binary_file(mesh_file_name, my_rank)
      call read_domain_info_b
      call read_number_of_node_b
      call close_binary_file
!
      end subroutine mpi_read_node_size_b
!
!------------------------------------------------------------------
!
       subroutine mpi_read_geometry_size_b(my_rank)
!
       use domain_data_IO_b
       use mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary mesh file: ', trim(mesh_file_name)
!
      call open_read_binary_file(mesh_file_name, my_rank)
!
      call read_domain_info_b
      call read_number_of_node_b
      call read_geometry_info_b
!
      call read_number_of_element_b
      call close_binary_file
!
      end subroutine mpi_read_geometry_size_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_mesh_file_b(my_rank)
!
      use m_machine_parameter
      use m_read_boundary_data
      use mesh_data_IO_b
      use groups_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write binary mesh file: ', trim(mesh_file_name)
!
      call open_write_binary_file(mesh_file_name)
      call write_geometry_data_b
!
!   write node group
      call write_grp_data_b(bc_grp_IO)
!  write element group
      call write_grp_data_b(mat_grp_IO)
!  write surface group
      call write_surf_grp_data_b(surf_grp_IO)
!
      call close_binary_file
!
      end subroutine mpi_write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module MPI_mesh_file_IO_b
