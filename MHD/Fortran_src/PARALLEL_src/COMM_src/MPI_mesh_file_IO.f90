!>@file  MPI_mesh_file_IO.f90
!!      module MPI_mesh_file_IO
!!
!!@author  H. Matsui
!!@date      Programmed in Aug., 2016
!!
!>@brief ASCII mesh file IO
!!
!!@verbatim
!!      subroutine mpi_read_mesh_file(my_rank)
!!      subroutine mpi_read_mesh_geometry(my_rank)
!!
!!      subroutine mpi_read_node_size(my_rank)
!!      subroutine mpi_read_geometry_size(my_rank)
!!
!!      subroutine mpi_write_mesh_file(my_rank)
!!@endverbatim
!
      module MPI_mesh_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_comm_data_IO
      use m_read_mesh_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_file(my_rank)
!
      use m_machine_parameter
      use m_read_boundary_data
      use mesh_data_IO
      use groups_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
!
      call read_geometry_data
!
!   read node group
      call read_group_data(input_file_code, bc_grp_IO)
!  read element group
      call read_group_data(input_file_code, mat_grp_IO)
!  read surface group
      call read_surf_grp_data(input_file_code, surf_grp_IO)
!
      close(input_file_code)
!
      end subroutine mpi_read_mesh_file
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_geometry(my_rank)
!
      use mesh_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_geometry_data
      close(input_file_code)
!
!
      end subroutine mpi_read_mesh_geometry
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_node_size(my_rank)
!
      use domain_data_IO
      use node_geometry_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call read_domain_info(input_file_code, my_rank_IO, comm_IO)
      call read_number_of_node(input_file_code, nod_IO)
      close(input_file_code)
!
!
      end subroutine mpi_read_node_size
!
!------------------------------------------------------------------
!
      subroutine mpi_read_geometry_size(my_rank)
!
      use domain_data_IO
      use node_geometry_IO
      use element_connect_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
!
      call read_domain_info(input_file_code, my_rank_IO, comm_IO)
      call read_number_of_node(input_file_code, nod_IO)
      call read_geometry_info(input_file_code, nod_IO)
!
!  ----  read element data -------
!
      call read_number_of_element(input_file_code, ele_IO)
      close(input_file_code)
!
      end subroutine mpi_read_geometry_size
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_mesh_file(my_rank)
!
      use m_machine_parameter
      use m_fem_mesh_labels
      use m_read_boundary_data
      use mesh_data_IO
      use groups_IO
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii mesh file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
!
      call write_geometry_data
!
!   write node group
      write(input_file_code,'(a)', advance='NO') hd_fem_nodgrp()
      call write_grp_data(input_file_code, bc_grp_IO)
!
!  write element group
      write(input_file_code,'(a)', advance='NO') hd_fem_elegrp()
      call write_grp_data(input_file_code, mat_grp_IO)
!
!  write surface group
      write(input_file_code,'(a)', advance='NO') hd_fem_sfgrp()
      call write_surf_grp_data(input_file_code, surf_grp_IO)
!
      close(input_file_code)
!
      end subroutine mpi_write_mesh_file
!
!  ---------------------------------------------------------------------
!
      end module MPI_mesh_file_IO
