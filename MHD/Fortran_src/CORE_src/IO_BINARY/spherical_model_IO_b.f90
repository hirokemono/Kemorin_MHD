!>@file  spherical_model_IO_b.f90
!!       module spherical_model_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine read_rank_4_sph_b(bin_flags, sph_IO)
!!      subroutine read_gl_resolution_sph_b(bin_flags, sph_IO)
!!      subroutine read_gl_nodes_sph_b(bin_flags, sph_IO)
!!        type(file_IO_flags), intent(inout) :: bin_flags
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine write_rank_4_sph_b(sph_IO)
!!      subroutine write_gl_resolution_sph_b(sph_IO)
!!      subroutine write_gl_nodes_sph_b(sph_IO)
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module spherical_model_IO_b
!
      use m_precision
!
      use t_node_id_spherical_IO
      use binary_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_rank_4_sph_b(bin_flags, sph_IO)
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    sph_IO%numdir_sph, sph_IO%sph_rank, bin_flags%ierr_IO)
!
      end subroutine read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_resolution_sph_b(bin_flags, sph_IO)
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    sph_IO%numdir_sph, sph_IO%nidx_gl_sph, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    sph_IO%ltr_gl, bin_flags%ierr_IO)
!
      end subroutine read_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_nodes_sph_b(bin_flags, sph_IO)
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint) :: nvect
      integer(kind = kint_gl) :: num64
!
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    sph_IO%numnod_sph, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call alloc_nod_id_sph_IO(sph_IO)
!
      num64 = sph_IO%numnod_sph
      call read_mul_int8_b(bin_flags%iflag_bin_swap,                    &
     &    num64, sph_IO%inod_gl_sph, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    nvect, sph_IO%idx_gl_sph, bin_flags%ierr_IO)
!
      end subroutine read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rank_4_sph_b(sph_IO)
!
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      call write_mul_integer_b(sph_IO%numdir_sph, sph_IO%sph_rank)
!
      end subroutine write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_resolution_sph_b(sph_IO)
!
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      call write_mul_integer_b(sph_IO%numdir_sph, sph_IO%nidx_gl_sph)
      call write_one_integer_b(sph_IO%ltr_gl)
!
      end subroutine write_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_nodes_sph_b(sph_IO)
!
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint) ::  nvect
!
!
      write(*,*) 'sph_IO%numnod_sph', sph_IO%numnod_sph, sph_IO%numdir_sph
      call write_one_integer_b(sph_IO%numnod_sph)
!
      num64 = sph_IO%numnod_sph
      call write_mul_int8_b(num64, sph_IO%inod_gl_sph)
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      write(*,*) 'nvect', nvect, nvect*kint
      call write_mul_integer_b(nvect, sph_IO%idx_gl_sph)
!
      end subroutine write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module spherical_model_IO_b
