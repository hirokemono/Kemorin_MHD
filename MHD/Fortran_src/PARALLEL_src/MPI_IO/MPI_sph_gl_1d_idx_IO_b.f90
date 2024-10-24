!>@file   MPI_sph_gl_1d_idx_IO_b.f90
!!@brief  module MPI_sph_gl_1d_idx_IO_b
!!
!!@author H.Matsui
!!@date      Programmed in Aug., 2016
!
!>@brief  Mesh file IO for gxipped format
!!
!!@verbatim
!!      subroutine mpi_read_rtp_gl_1d_table_b(IO_param, sph_IO)
!!      subroutine mpi_read_rj_gl_1d_table_b(IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine mpi_write_rtp_gl_1d_table_b(IO_param, sph_IO)
!!      subroutine mpi_write_rj_gl_1d_table_b (IO_param, sph_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module MPI_sph_gl_1d_idx_IO_b
!
      use m_precision
      use m_constants
!
      use t_node_id_spherical_IO
      use t_calypso_mpi_IO_param
      use MPI_binary_data_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------!
      subroutine mpi_read_rtp_gl_1d_table_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: num64
!
!
      sph_IO%numdir_sph = 3
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 1
      sph_IO%ncomp_table_1d(3) = 2
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      num64 = sph_IO%numdir_sph
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%nidx_sph)
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%ist_sph)
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%ied_sph)
!
      call alloc_idx_sph_1d1_IO(sph_IO)
      call alloc_idx_sph_1d2_IO(sph_IO)
      call alloc_idx_sph_1d3_IO(sph_IO)
!
      num64 = sph_IO%nidx_sph(1)
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%idx_gl_1)
      call mpi_read_1d_vector_b(IO_param, num64, sph_IO%r_gl_1)
!
      num64 = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%idx_gl_2)
!
      num64 = sph_IO%nidx_sph(3) * sph_IO%ncomp_table_1d(3)
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%idx_gl_3)
!
      end subroutine mpi_read_rtp_gl_1d_table_b
!
! -----------------------------------------------------------------------
!
      subroutine mpi_read_rj_gl_1d_table_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: num64
!
!
      sph_IO%numdir_sph = 2
      sph_IO%ncomp_table_1d(1) = 1
      sph_IO%ncomp_table_1d(2) = 3
!
      call alloc_num_idx_sph_IO(sph_IO)
!
      num64 = sph_IO%numdir_sph
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%nidx_sph)
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%ist_sph)
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%ied_sph)
!
      call alloc_idx_sph_1d1_IO(sph_IO)
      call alloc_idx_sph_1d2_IO(sph_IO)
!
      num64 = sph_IO%nidx_sph(1)
      call mpi_read_int_vector_b(IO_param, num64, sph_IO%idx_gl_1)
      call mpi_read_1d_vector_b(IO_param, num64, sph_IO%r_gl_1)
!
      num64 = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call mpi_read_int_vector_b                                        &
     &   (IO_param, num64, sph_IO%idx_gl_2)
!
      end subroutine mpi_read_rj_gl_1d_table_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_write_rtp_gl_1d_table_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = sph_IO%numdir_sph
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b(IO_param, num64, sph_IO%nidx_sph)
      call mpi_write_int_vector_b(IO_param, num64, sph_IO%ist_sph)
      call mpi_write_int_vector_b(IO_param, num64, sph_IO%ied_sph)
!
      num64 = sph_IO%nidx_sph(1)
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b(IO_param, num64, sph_IO%idx_gl_1)
      call mpi_write_1d_vector_b(IO_param, num64, sph_IO%r_gl_1)
!
      num64 = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b                                       &
     &   (IO_param, num64, sph_IO%idx_gl_2)
!
      num64 = sph_IO%nidx_sph(3) * sph_IO%ncomp_table_1d(3)
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b                                       &
     &   (IO_param, num64, sph_IO%idx_gl_3)
!
      end subroutine mpi_write_rtp_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      subroutine mpi_write_rj_gl_1d_table_b(IO_param, sph_IO)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint_gl) :: num64
!
!
!
      num64 = sph_IO%numdir_sph
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b(IO_param, num64, sph_IO%nidx_sph)
      call mpi_write_int_vector_b(IO_param, num64, sph_IO%ist_sph)
      call mpi_write_int_vector_b(IO_param, num64, sph_IO%ied_sph)
!
      num64 = sph_IO%nidx_sph(1)
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b(IO_param, num64, sph_IO%idx_gl_1)
      call mpi_write_1d_vector_b(IO_param, num64, sph_IO%r_gl_1)
!
      num64 = sph_IO%nidx_sph(2) * sph_IO%ncomp_table_1d(2)
      call istack64_4_parallel_data(num64, IO_param)
      call mpi_write_int_vector_b(IO_param, num64, sph_IO%idx_gl_2)
!
      end subroutine mpi_write_rj_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      end module MPI_sph_gl_1d_idx_IO_b
