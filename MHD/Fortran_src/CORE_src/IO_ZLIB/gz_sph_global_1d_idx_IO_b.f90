!gz_sph_global_1d_idx_IO_b.f90
!      module gz_sph_global_1d_idx_IO_b
!
!     Written by H. Matsui on July, 2007
!
!      subroutine gz_read_rtp_gl_1d_table_b
!      subroutine gz_read_rj_gl_1d_table_b
!
!      subroutine gz_write_rtp_gl_1d_table_b
!      subroutine gz_write_rj_gl_1d_table_b
!
      module gz_sph_global_1d_idx_IO_b
!
      use m_precision
!
      use m_node_id_spherical_IO
      use gz_binary_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------!
      subroutine gz_read_rtp_gl_1d_table_b
!
      integer(kind = kint) :: nvect
!
!
      ndir_sph_IO = 3
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 1
      ncomp_itbl_1d_IO(3) = 2
!
      call gz_read_fld_mul_inthead_b(ndir_sph_IO, nidx_sph_IO)
      call gz_read_fld_mul_inthead_b(ndir_sph_IO, ist_sph_IO)
      call gz_read_fld_mul_inthead_b(ndir_sph_IO, ied_sph_IO)
!
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
      call allocate_idx_sph_1d3_IO
!
      call gz_read_fld_mul_inthead_b(nidx_sph_IO(1), idx_gl_1_IO)
      call gz_read_fld_realarray_b(nidx_sph_IO(1), r_gl_1_IO)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call gz_read_fld_mul_inthead_b(nvect, idx_gl_2_IO)
!
      nvect = nidx_sph_IO(3) * ncomp_itbl_1d_IO(3)
      call gz_read_fld_mul_inthead_b(nvect, idx_gl_3_IO)
!
      end subroutine gz_read_rtp_gl_1d_table_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_rj_gl_1d_table_b
!
      integer(kind = kint) :: nvect
!
!
      ndir_sph_IO = 2
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 3
!
      call gz_read_fld_mul_inthead_b(ndir_sph_IO, nidx_sph_IO)
      call gz_read_fld_mul_inthead_b(ndir_sph_IO, ist_sph_IO)
      call gz_read_fld_mul_inthead_b(ndir_sph_IO, ied_sph_IO)
!
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
!
      call gz_read_fld_mul_inthead_b(nidx_sph_IO(1), idx_gl_1_IO)
      call gz_read_fld_realarray_b(nidx_sph_IO(1), r_gl_1_IO)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call gz_read_fld_mul_inthead_b(nvect, idx_gl_2_IO)
!
      end subroutine gz_read_rj_gl_1d_table_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_rtp_gl_1d_table_b
!
      integer(kind = kint) :: nvect
!
!
      call gz_write_fld_mul_inthead_b(ndir_sph_IO, nidx_sph_IO)
      call gz_write_fld_mul_inthead_b(ndir_sph_IO, ist_sph_IO)
      call gz_write_fld_mul_inthead_b(ndir_sph_IO, ied_sph_IO)
!
      call gz_write_fld_mul_inthead_b(nidx_sph_IO(1), idx_gl_1_IO)
      call gz_write_fld_realarray_b(nidx_sph_IO(1), r_gl_1_IO)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call gz_write_fld_mul_inthead_b(nvect, idx_gl_2_IO)
!
      nvect = nidx_sph_IO(3) * ncomp_itbl_1d_IO(3)
      call gz_write_fld_mul_inthead_b(nvect, idx_gl_3_IO)
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine gz_write_rtp_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_rj_gl_1d_table_b
!
      integer(kind = kint) :: nvect
!
!
      call gz_write_fld_mul_inthead_b(ndir_sph_IO, nidx_sph_IO)
      call gz_write_fld_mul_inthead_b(ndir_sph_IO, ist_sph_IO)
      call gz_write_fld_mul_inthead_b(ndir_sph_IO, ied_sph_IO)
!
      call gz_write_fld_mul_inthead_b(nidx_sph_IO(1), idx_gl_1_IO)
      call gz_write_fld_realarray_b(nidx_sph_IO(1), r_gl_1_IO)
!
      nvect = nidx_sph_IO(2) * ncomp_itbl_1d_IO(2)
      call gz_write_fld_mul_inthead_b(nvect, idx_gl_2_IO)
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      end subroutine gz_write_rj_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      end module gz_sph_global_1d_idx_IO_b
