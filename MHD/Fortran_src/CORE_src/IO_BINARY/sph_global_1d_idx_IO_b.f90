!sph_global_1d_idx_IO_b.f90
!      module sph_global_1d_idx_IO_b
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_rtp_gl_1d_table_b
!      subroutine read_rj_gl_1d_table_b
!
!      subroutine write_rtp_gl_1d_table_b
!      subroutine write_rj_gl_1d_table_b
!
      module sph_global_1d_idx_IO_b
!
      use m_precision
!
      use m_node_id_spherical_IO
      use binary_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------!
      subroutine read_rtp_gl_1d_table_b
!
      integer(kind = kint) :: nvect
!
!
      sph_IO1%numdir_sph = 3
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 1
      ncomp_itbl_1d_IO(3) = 2
!
      call read_mul_integer_b(sph_IO1%numdir_sph, nidx_sph_IO)
      call read_mul_integer_b(sph_IO1%numdir_sph, ist_sph_IO)
      call read_mul_integer_b(sph_IO1%numdir_sph, ied_sph_IO)
!
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
      call allocate_idx_sph_1d3_IO
!
      call read_mul_integer_b(nidx_sph_IO(1), idx_gl_1_IO)
      call read_1d_vector_b(nidx_sph_IO(1), r_gl_1_IO)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call read_mul_integer_b(nvect, idx_gl_2_IO)
!
      nvect = nidx_sph_IO(3) * ncomp_itbl_1d_IO(3)
      call read_mul_integer_b(nvect, idx_gl_3_IO)
!
      end subroutine read_rtp_gl_1d_table_b
!
! -----------------------------------------------------------------------
!
      subroutine read_rj_gl_1d_table_b
!
      integer(kind = kint) :: nvect
!
!
      sph_IO1%numdir_sph = 2
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 3
!
      call read_mul_integer_b(sph_IO1%numdir_sph, nidx_sph_IO)
      call read_mul_integer_b(sph_IO1%numdir_sph, ist_sph_IO)
      call read_mul_integer_b(sph_IO1%numdir_sph, ied_sph_IO)
!
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
!
      call read_mul_integer_b(nidx_sph_IO(1), idx_gl_1_IO)
      call read_1d_vector_b(nidx_sph_IO(1), r_gl_1_IO)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call read_mul_integer_b(nvect, idx_gl_2_IO)
!
      end subroutine read_rj_gl_1d_table_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rtp_gl_1d_table_b
!
      integer(kind = kint) :: nvect
!
!
      call write_mul_integer_b(sph_IO1%numdir_sph, nidx_sph_IO)
      call write_mul_integer_b(sph_IO1%numdir_sph, ist_sph_IO)
      call write_mul_integer_b(sph_IO1%numdir_sph, ied_sph_IO)
!
      call write_mul_integer_b(nidx_sph_IO(1), idx_gl_1_IO)
      call write_1d_vector_b(nidx_sph_IO(1), r_gl_1_IO)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call write_mul_integer_b(nvect, idx_gl_2_IO)
!
      nvect = nidx_sph_IO(3) * ncomp_itbl_1d_IO(3)
      call write_mul_integer_b(nvect, idx_gl_3_IO)
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine write_rtp_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      subroutine write_rj_gl_1d_table_b
!
      integer(kind = kint) :: nvect
!
!
      call write_mul_integer_b(sph_IO1%numdir_sph, nidx_sph_IO)
      call write_mul_integer_b(sph_IO1%numdir_sph, ist_sph_IO)
      call write_mul_integer_b(sph_IO1%numdir_sph, ied_sph_IO)
!
      call write_mul_integer_b(nidx_sph_IO(1), idx_gl_1_IO)
      call write_1d_vector_b(nidx_sph_IO(1), r_gl_1_IO)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call write_mul_integer_b(nvect, idx_gl_2_IO)
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      end subroutine write_rj_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      end module sph_global_1d_idx_IO_b
