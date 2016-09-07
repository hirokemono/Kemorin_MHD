!>@file  gz_MPI_spherical_model_IO_b.f90
!!       module gz_MPI_spherical_model_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine gz_mpi_read_rank_4_sph_b                             &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!      subroutine gz_mpi_read_gl_reso_sph_b(id_file, ioff_gl)
!!      subroutine gz_mpi_read_gl_nodes_sph_b                           &
!!     &         (id_file, nprocs_in, id_rank, ioff_gl)
!!
!!      subroutine gz_mpi_write_rank_4_sph_b(id_file, ioff_gl)
!!      subroutine gz_mpi_write_gl_reso_sph_b(id_file, ioff_gl)
!!      subroutine gz_mpi_write_gl_nodes_sph_b(id_file, ioff_gl)
!!@endverbatim
!
      module gz_MPI_spherical_model_IO_b
!
      use m_precision
!
      use m_node_id_spherical_IO
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_rank_4_sph_b                               &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
!
      call gz_mpi_read_int_vector_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    ndir_sph_IO, sph_rank_IO)
!
      end subroutine gz_mpi_read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_gl_reso_sph_b(id_file, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_read_mul_inthead_b                                    &
     &   (id_file, ioff_gl, ndir_sph_IO, nidx_gl_sph_IO)
      call gz_mpi_read_one_inthead_b(id_file, ioff_gl, ltr_gl_IO)
!
      end subroutine gz_mpi_read_gl_reso_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_gl_nodes_sph_b                             &
     &         (id_file, nprocs_in, id_rank, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(in) :: id_rank, nprocs_in
!
      integer(kind=kint) :: nvect
!
!
      call gz_mpi_read_one_integer_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nnod_sph_IO)
!
      call allocate_nod_id_sph_IO
!
      call gz_mpi_read_int8_vector_b                                    &
     &   (id_file, nprocs_in, id_rank, ioff_gl,                         &
     &    nnod_sph_IO, inod_gl_sph_IO)
      nvect = nnod_sph_IO * ndir_sph_IO
      call gz_mpi_read_int_vector_b                                     &
     &   (id_file, nprocs_in, id_rank, ioff_gl, nvect, idx_gl_sph_IO)
!
      end subroutine gz_mpi_read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_rank_4_sph_b(id_file, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_write_int_vector_b                                    &
     &   (id_file, ioff_gl, ndir_sph_IO, sph_rank_IO)
!
      end subroutine gz_mpi_write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_gl_reso_sph_b(id_file, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
!
      call gz_mpi_write_mul_inthead_b                                   &
     &   (id_file, ioff_gl, ndir_sph_IO, nidx_gl_sph_IO)
      call gz_mpi_write_one_inthead_b(id_file, ioff_gl, ltr_gl_IO)
!
      end subroutine gz_mpi_write_gl_reso_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_gl_nodes_sph_b(id_file, ioff_gl)
!
      integer, intent(in) ::  id_file
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind = kint) ::  nvect
!
!
      call gz_mpi_write_one_integer_b(id_file, ioff_gl, nnod_sph_IO)
      call gz_mpi_write_int8_vector_b                                   &
     &   (id_file, ioff_gl, nnod_sph_IO, inod_gl_sph_IO)
      nvect = nnod_sph_IO * ndir_sph_IO
      call gz_mpi_write_int_vector_b                                    &
     &   (id_file, ioff_gl, nvect, idx_gl_sph_IO)
!
      call deallocate_nod_id_sph_IO
!
      end subroutine gz_mpi_write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module gz_MPI_spherical_model_IO_b
