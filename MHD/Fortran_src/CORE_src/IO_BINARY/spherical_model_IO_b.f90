!>@file  spherical_model_IO_b.f90
!!       module spherical_model_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine read_rank_4_sph_b(bbuf, sph_IO)
!!      subroutine read_gl_resolution_sph_b(bbuf, sph_IO)
!!      subroutine read_gl_nodes_sph_b(bbuf, sph_IO)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine write_rank_4_sph_b(sph_IO, bbuf)
!!      subroutine write_gl_resolution_sph_b(sph_IO, bbuf)
!!      subroutine write_gl_nodes_sph_b(sph_IO, bbuf)
!!        type(sph_IO_data), intent(in) :: sph_IO
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module spherical_model_IO_b
!
      use m_precision
!
      use t_node_id_spherical_IO
      use t_binary_IO_buffer
      use binary_IO
      use transfer_to_long_integers
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_rank_4_sph_b(bbuf, sph_IO)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call read_mul_integer_b                                           &
     &   (bbuf, cast_long(sph_IO%numdir_sph), sph_IO%sph_rank)
!
      end subroutine read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_resolution_sph_b(bbuf, sph_IO)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call read_mul_integer_b                                           &
     &   (bbuf, cast_long(sph_IO%numdir_sph), sph_IO%nidx_gl_sph)
      if(bbuf%ierr_bin .gt. 0) return
!
      call read_one_integer_b(bbuf, sph_IO%ltr_gl)
!
      end subroutine read_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_nodes_sph_b(bbuf, sph_IO)
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: num64
!
!
      call read_one_integer_b(bbuf, sph_IO%numnod_sph)
      if(bbuf%ierr_bin .gt. 0) return
!
      call alloc_nod_id_sph_IO(sph_IO)
!
      call read_mul_int8_b                                              &
     &   (bbuf, cast_long(sph_IO%numnod_sph), sph_IO%inod_gl_sph)
      if(bbuf%ierr_bin .ne. 0) return
!
      num64 = sph_IO%numnod_sph * sph_IO%numdir_sph
      call read_mul_integer_b(bbuf, num64, sph_IO%idx_gl_sph)
!
      end subroutine read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rank_4_sph_b(sph_IO, bbuf)
!
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_mul_integer_b                                          &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%sph_rank, bbuf)
!
      end subroutine write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_resolution_sph_b(sph_IO, bbuf)
!
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_mul_integer_b                                          &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%nidx_gl_sph, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_one_integer_b(sph_IO%ltr_gl, bbuf)
!
      end subroutine write_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_nodes_sph_b(sph_IO, bbuf)
!
      type(sph_IO_data), intent(in) :: sph_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: num64
!
!
      call write_one_integer_b(sph_IO%numnod_sph, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      call write_mul_int8_b                                             &
     &   (cast_long(sph_IO%numnod_sph), sph_IO%inod_gl_sph, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      num64 = sph_IO%numnod_sph * sph_IO%numdir_sph
      call write_mul_integer_b(num64, sph_IO%idx_gl_sph, bbuf)
!
      end subroutine write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module spherical_model_IO_b
