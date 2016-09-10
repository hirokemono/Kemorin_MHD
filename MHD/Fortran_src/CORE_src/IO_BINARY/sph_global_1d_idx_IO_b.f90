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
      sph_IO1%ncomp_table_1d(1) = 1
      sph_IO1%ncomp_table_1d(2) = 1
      sph_IO1%ncomp_table_1d(3) = 2
!
      call read_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%nidx_sph)
      call read_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%ist_sph)
      call read_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%ied_sph)
!
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
      call allocate_idx_sph_1d3_IO
!
      call read_mul_integer_b(sph_IO1%nidx_sph(1), idx_gl_1_IO)
      call read_1d_vector_b(sph_IO1%nidx_sph(1), r_gl_1_IO)
!
      nvect = sph_IO1%nidx_sph(2) * sph_IO1%ncomp_table_1d(2)
      call read_mul_integer_b(nvect, idx_gl_2_IO)
!
      nvect = sph_IO1%nidx_sph(3) * sph_IO1%ncomp_table_1d(3)
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
      sph_IO1%ncomp_table_1d(1) = 1
      sph_IO1%ncomp_table_1d(2) = 3
!
      call read_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%nidx_sph)
      call read_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%ist_sph)
      call read_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%ied_sph)
!
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
!
      call read_mul_integer_b(sph_IO1%nidx_sph(1), idx_gl_1_IO)
      call read_1d_vector_b(sph_IO1%nidx_sph(1), r_gl_1_IO)
!
      nvect = sph_IO1%nidx_sph(2) * sph_IO1%ncomp_table_1d(2)
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
      call write_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%nidx_sph)
      call write_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%ist_sph)
      call write_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%ied_sph)
!
      call write_mul_integer_b(sph_IO1%nidx_sph(1), idx_gl_1_IO)
      call write_1d_vector_b(sph_IO1%nidx_sph(1), r_gl_1_IO)
!
      nvect = sph_IO1%nidx_sph(2) * sph_IO1%ncomp_table_1d(2)
      call write_mul_integer_b(nvect, idx_gl_2_IO)
!
      nvect = sph_IO1%nidx_sph(3) * sph_IO1%ncomp_table_1d(3)
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
      call write_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%nidx_sph)
      call write_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%ist_sph)
      call write_mul_integer_b(sph_IO1%numdir_sph, sph_IO1%ied_sph)
!
      call write_mul_integer_b(sph_IO1%nidx_sph(1), idx_gl_1_IO)
      call write_1d_vector_b(sph_IO1%nidx_sph(1), r_gl_1_IO)
!
      nvect = sph_IO1%nidx_sph(2) * sph_IO1%ncomp_table_1d(2)
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
