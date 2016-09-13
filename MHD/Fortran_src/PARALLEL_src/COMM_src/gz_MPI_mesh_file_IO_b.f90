!>@file   gz_MPI_mesh_file_IO_b.f90
!!@brief  module gz_MPI_mesh_file_IO_b
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine gz_mpi_read_mesh_file_b                              &
!!     &         (nprocs_in, my_rank_IO, fem_IO)
!!        type(mesh_data), intent(inout) :: fem_IO
!!
!!      subroutine gz_mpi_read_mesh_geometry_b                          &
!!     &         (nprocs_in, my_rank_IO, mesh_IO)
!!      subroutine gz_mpi_read_node_size_b                              &
!!     &         (nprocs_in, my_rank_IO, mesh_IO)
!!      subroutine gz_mpi_read_geometry_size_b                          &
!!     &         (nprocs_in, my_rank_IO, mesh_IO)
!!        type(mesh_geometry), intent(inout) :: mesh_IO
!!
!!      subroutine gz_mpi_write_mesh_file_b                             &
!!     &         (nprocs_in, my_rank_IO, fem_IO)
!!        type(mesh_data), intent(inout) :: fem_IO
!!@endverbatim
!
      module gz_MPI_mesh_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_calypso_mpi_IO
      use m_read_mesh_data
      use t_mesh_data
      use gz_MPI_mesh_data_IO_b
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_mpi_read_mesh_file_b                                &
     &         (nprocs_in, my_rank_IO, fem_IO)
!
      use m_machine_parameter
      use m_read_boundary_data
      use gz_MPI_binary_datum_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
!
      type(mesh_data), intent(inout) :: fem_IO
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged mesh file: ', trim(mesh_file_name)
!
      call open_read_gz_mpi_file_b(mesh_file_name, id_file, ioff_gl)
!
      call gz_mpi_read_geometry_data_b                                  &
     &   (id_file, nprocs_in, my_rank_IO, ioff_gl, fem_IO%mesh)
      call gz_mpi_read_mesh_groups_b                                    &
     &   (id_file, nprocs_in, my_rank_IO, ioff_gl, fem_IO%group)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      subroutine gz_mpi_read_mesh_geometry_b                            &
     &         (nprocs_in, my_rank_IO, mesh_IO)
!
      use gz_MPI_binary_datum_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
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
     &   (id_file, nprocs_in, my_rank_IO, ioff_gl, mesh_IO)
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_mesh_geometry_b
!
!  ---------------------------------------------------------------------
!
      subroutine gz_mpi_read_node_size_b                                &
     &         (nprocs_in, my_rank_IO, mesh_IO)
!
      use gz_MPI_binary_datum_IO
      use gz_MPI_domain_data_IO_b
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged mesh file: ', trim(mesh_file_name)
!
      call open_read_gz_mpi_file_b(mesh_file_name, id_file, ioff_gl)
      call gz_mpi_read_num_node_b                                       &
     &   (id_file, nprocs_in, my_rank_IO, ioff_gl, mesh_IO)
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_node_size_b
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_geometry_size_b                            &
     &         (nprocs_in, my_rank_IO, mesh_IO)
!
      use gz_MPI_binary_datum_IO
      use gz_MPI_domain_data_IO_b
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
!
      type(mesh_geometry), intent(inout) :: mesh_IO
!
      integer :: id_file
      integer(kind = kint_gl) :: ioff_gl
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary merged mesh file: ', trim(mesh_file_name)
!
      call open_read_gz_mpi_file_b(mesh_file_name, id_file, ioff_gl)
      call gz_mpi_read_num_node_ele_b                                   &
     &   (id_file, nprocs_in, my_rank_IO, ioff_gl, mesh_IO)
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_read_geometry_size_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_mesh_file_b                               &
     &         (nprocs_in, my_rank_IO, fem_IO)
!
      use m_machine_parameter
      use m_read_boundary_data
      use gz_MPI_binary_datum_IO
!
      integer(kind = kint), intent(in) :: nprocs_in, my_rank_IO
      type(mesh_data), intent(inout) :: fem_IO
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
      call gz_mpi_write_geometry_data_b                                 &
     &   (id_file, nprocs_in, ioff_gl, fem_IO%mesh)
      call gz_mpi_write_mesh_groups_b                                   &
     &   (id_file, nprocs_in, my_rank_IO, ioff_gl, fem_IO%group)
!
      call calypso_close_mpi_file(id_file)
!
      end subroutine gz_mpi_write_mesh_file_b
!
!  ---------------------------------------------------------------------
!
      end module gz_MPI_mesh_file_IO_b
