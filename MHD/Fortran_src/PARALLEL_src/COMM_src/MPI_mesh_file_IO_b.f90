!>@file   MPI_mesh_file_IO_b.f90
!!@brief  module MPI_mesh_file_IO_b
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine mpi_read_mesh_file_b(nprocs_in, id_rank)
!!      subroutine mpi_read_mesh_geometry_b(nprocs_in, id_rank)
!!
!!      subroutine mpi_read_node_size_b(nprocs_in, id_rank)
!!      subroutine mpi_read_geometry_size_b(nprocs_in, id_rank)
!!
!!      subroutine mpi_write_mesh_file_b(nprocs_in, id_rank)
!!@endverbatim
!
      module MPI_mesh_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_read_mesh_data
      use m_calypso_mpi_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_file_b(nprocs_in, id_rank)
!
      use m_machine_parameter
      use m_read_boundary_data
      use MPI_mesh_data_IO_b
      use MPI_groups_IO_b
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged mesh file: ', trim(mesh_file_name)
!
      call open_read_mpi_file_b(mesh_file_name, id_file, ioff_gl)
!
      call mpi_read_geometry_data_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
!   read node group
      call mpi_read_group_data_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, bc_grp_IO)
!  read element group
      call mpi_read_group_data_b                                        &
     &   (id_file, nprocs_in, id_rank, ioff_gl, mat_grp_IO)
!  read surface group
      call mpi_read_surf_grp_data_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl, surf_grp_IO)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_read_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_read_mesh_geometry_b(nprocs_in, id_rank)
!
      use MPI_mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged mesh file: ', trim(mesh_file_name)
!
      call open_read_mpi_file_b(mesh_file_name, id_file, ioff_gl)
      call mpi_read_geometry_data_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_read_mesh_geometry_b
!
!  ---------------------------------------------------------------------
!
       subroutine mpi_read_node_size_b(nprocs_in, id_rank)
!
       use domain_data_IO_b
       use MPI_mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged mesh file: ', trim(mesh_file_name)
!
      call open_read_mpi_file_b(mesh_file_name, id_file, ioff_gl)
      call mpi_read_domain_info_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_number_of_node_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_read_node_size_b
!
!------------------------------------------------------------------
!
       subroutine mpi_read_geometry_size_b(nprocs_in, id_rank)
!
       use domain_data_IO_b
       use MPI_mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged mesh file: ', trim(mesh_file_name)
!
      call open_read_mpi_file_b(mesh_file_name, id_file, ioff_gl)
!
      call mpi_read_domain_info_b                                       &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_number_of_node_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call mpi_read_geometry_info_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call mpi_read_number_of_element_b                                 &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_read_geometry_size_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_write_mesh_file_b(nprocs_in, id_rank)
!
      use m_machine_parameter
      use m_read_boundary_data
      use MPI_mesh_data_IO_b
      use MPI_groups_IO_b
!
      integer(kind = kint), intent(in) :: nprocs_in, id_rank
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped binary merged mesh file: ', trim(mesh_file_name)
!
      call open_write_mpi_file_b                                        &
     &   (mesh_file_name, nprocs_in, id_file, ioff_gl)
      call mpi_write_geometry_data_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
!   write node group
      call mpi_write_grp_data_b                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl, bc_grp_IO)
!  write element group
      call mpi_write_grp_data_b                                         &
     &   (id_file, nprocs_in, id_rank, ioff_gl, mat_grp_IO)
!  write surface group
      call mpi_write_surf_grp_data_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl, surf_grp_IO)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine mpi_write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module MPI_mesh_file_IO_b
