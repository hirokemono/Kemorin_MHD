!sph_global_1d_idx_IO_b.f90
!      module sph_global_1d_idx_IO_b
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_rtp_gl_1d_table_b(id_file)
!      subroutine read_rj_gl_1d_table_b(id_file)
!
!      subroutine write_rtp_gl_1d_table_b(id_file)
!      subroutine write_rj_gl_1d_table_b(id_file)
!
      module sph_global_1d_idx_IO_b
!
      use m_precision
!
      use m_node_id_spherical_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------!
      subroutine read_rtp_gl_1d_table_b(id_file)
!
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      ndir_sph_IO = 3
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 1
      ncomp_itbl_1d_IO(3) = 2
!
      read(id_file) nidx_sph_IO(1:ndir_sph_IO)
      read(id_file) ist_sph_IO(1:ndir_sph_IO)
      read(id_file) ied_sph_IO(1:ndir_sph_IO)
!
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
      call allocate_idx_sph_1d3_IO
!
      read(id_file) idx_gl_1_IO(1:nidx_sph_IO(1))
      read(id_file) r_gl_1_IO(1:nidx_sph_IO(1))
!
      do i = 1, ncomp_itbl_1d_IO(2)
        read(id_file) idx_gl_2_IO(1:nidx_sph_IO(2),i)
      end do
!
      do i = 1, ncomp_itbl_1d_IO(3)
        read(id_file) idx_gl_3_IO(1:nidx_sph_IO(3),i)
      end do
!
      end subroutine read_rtp_gl_1d_table_b
!
! -----------------------------------------------------------------------
!
      subroutine read_rj_gl_1d_table_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      ndir_sph_IO = 2
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 3
!
      read(id_file) nidx_sph_IO(1:ndir_sph_IO)
      read(id_file) ist_sph_IO(1:ndir_sph_IO)
      read(id_file) ied_sph_IO(1:ndir_sph_IO)
!
      call allocate_idx_sph_1d1_IO
      call allocate_idx_sph_1d2_IO
!
      read(id_file) idx_gl_1_IO(1:nidx_sph_IO(1))
      read(id_file) r_gl_1_IO(1:nidx_sph_IO(1))
!
      do i = 1, ncomp_itbl_1d_IO(2)
        read(id_file) idx_gl_2_IO(1:nidx_sph_IO(2),i)
      end do
!
      end subroutine read_rj_gl_1d_table_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rtp_gl_1d_table_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
!
      write(id_file) nidx_sph_IO(1:ndir_sph_IO)
      write(id_file) ist_sph_IO(1:ndir_sph_IO)
      write(id_file) ied_sph_IO(1:ndir_sph_IO)
!
      write(id_file) idx_gl_1_IO(1:nidx_sph_IO(1))
      write(id_file) r_gl_1_IO(1:nidx_sph_IO(1))
!
      do i = 1, ncomp_itbl_1d_IO(2)
        write(id_file) idx_gl_2_IO(1:nidx_sph_IO(2),i)
      end do
!
      do i = 1, ncomp_itbl_1d_IO(3)
        write(id_file) idx_gl_3_IO(1:nidx_sph_IO(3),i)
      end do
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine write_rtp_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      subroutine write_rj_gl_1d_table_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
      write(id_file) nidx_sph_IO(1:ndir_sph_IO)
      write(id_file) ist_sph_IO(1:ndir_sph_IO)
      write(id_file) ied_sph_IO(1:ndir_sph_IO)
!
      write(id_file) idx_gl_1_IO(1:nidx_sph_IO(1))
      write(id_file) r_gl_1_IO(1:nidx_sph_IO(1))
!
      do i = 1, ncomp_itbl_1d_IO(2)
        write(id_file) idx_gl_2_IO(1:nidx_sph_IO(2),i)
      end do
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      end subroutine write_rj_gl_1d_table_b
!
! ----------------------------------------------------------------------
!
      end module sph_global_1d_idx_IO_b
