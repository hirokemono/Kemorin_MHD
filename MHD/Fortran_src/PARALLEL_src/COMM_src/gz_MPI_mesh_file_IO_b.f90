!>@file   gz_MPI_mesh_file_IO_b.f90
!!@brief  module gz_MPI_mesh_file_IO_b
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_mpi_read_mesh_file_b(nprocs_in, id_rank)
!!      subroutine gz_mpi_read_mesh_geometry_b(nprocs_in, id_rank)
!!
!!      subroutine gz_mpi_read_node_size_b(nprocs_in, id_rank)
!!      subroutine gz_mpi_read_geometry_size_b(nprocs_in, id_rank)
!!
!!      subroutine gz_mpi_write_mesh_file_b(nprocs_in, id_rank)
!!@endverbatim
!
      module gz_MPI_mesh_file_IO_b
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
      subroutine gz_mpi_read_mesh_file_b(nprocs_in, id_rank)
!
      use m_machine_parameter
      use m_read_boundary_data
      use gz_MPI_mesh_data_IO_b
      use gz_MPI_groups_IO_b
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
      call open_read_gz_mpi_file_b(mesh_file_name, ioff_gl)
!
      call gz_mpi_read_geometry_data_b                                  &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
!   read node group
      call gz_mpi_read_group_data_b                                     &
     &   (id_file, nprocs_in, id_rank, bc_grp_IO)
!  read element group
      call gz_mpi_read_group_data_b                                     &
     &   (id_file, nprocs_in, id_rank, mat_grp_IO)
!  read surface group
      call gz_mpi_read_surf_grp_data_b                                  &
     &    (id_file, nprocs_in, id_rank, surf_grp_IO)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine gz_mpi_read_mesh_geometry_b(nprocs_in, id_rank)
!
      use gz_MPI_mesh_data_IO_b
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
      call open_read_gz_mpi_file_b(mesh_file_name, id_file, ioff_gl)
      call gz_mpi_read_geometry_data_b                                  &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_mesh_geometry_b
!
!  ---------------------------------------------------------------------
!
       subroutine gz_mpi_read_node_size_b(nprocs_in, id_rank)
!
       use gz_domain_data_IO_b
       use gz_MPI_mesh_data_IO_b
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
      call open_read_gz_mpi_file_b(mesh_file_name, id_file, ioff_gl)
      call gz_mpi_read_domain_info_b
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call gz_mpi_read_number_of_node_b                                 &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_node_size_b
!
!------------------------------------------------------------------
!
       subroutine gz_mpi_read_geometry_size_b(nprocs_in, id_rank)
!
       use gz_domain_data_IO_b
       use gz_MPI_mesh_data_IO_b
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
      call open_read_gz_mpi_file_b(mesh_file_name, ioff_gl)
!
      call gz_mpi_read_domain_info_b
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call gz_mpi_read_number_of_node_b                                 &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call gz_mpi_read_geometry_info_b                                  &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
!
      call gz_mpi_read_number_of_element_b                              &
     &   (id_file, nprocs_in, id_rank, ioff_gl)
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_geometry_size_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_mesh_file_b(nprocs_in, id_rank)
!
      use m_machine_parameter
      use m_read_boundary_data
      use gz_MPI_mesh_data_IO_b
      use gz_MPI_groups_IO_b
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
      call open_write_gz_mpi_file_b                                     &
     &   (mesh_file_name, nprocs_in, id_file, ioff_gl)
      call gz_mpi_write_geometry_data_b(id_file, nprocs_in, ioff_gl)
!
!   write node group
      call gz_mpi_write_grp_data_b(id_file, ioff_gl, bc_grp_IO)
!  write element group
      call gz_mpi_write_grp_data_b(id_file, ioff_gl, mat_grp_IO)
!  write surface group
      call gz_mpi_write_surf_grp_data_b(id_file, ioff_gl, surf_grp_IO)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module gz_MPI_mesh_file_IO_b
