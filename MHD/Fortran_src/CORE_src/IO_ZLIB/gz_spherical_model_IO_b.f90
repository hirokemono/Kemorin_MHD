!>@file  gz_spherical_model_IO_b.f90
!!       module gz_spherical_model_IO_b
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief  Data IO routines for spectrum data
!!
!!@verbatim
!!      subroutine gz_read_rank_4_sph_b(gz_flags, sph_IO)
!!      subroutine gz_read_gl_resolution_sph_b(gz_flags, sph_IO)
!!      subroutine gz_read_gl_nodes_sph_b(gz_flags, sph_IO)
!!        type(file_IO_flags), intent(inout) :: gz_flags
!!        type(sph_IO_data), intent(inout) :: sph_IO
!!
!!      subroutine gz_write_rank_4_sph_b(sph_IO)
!!      subroutine gz_write_gl_resolution_sph_b(sph_IO)
!!      subroutine gz_write_gl_nodes_sph_b(sph_IO)
!!        type(sph_IO_data), intent(in) :: sph_IO
!!@endverbatim
!
      module gz_spherical_model_IO_b
!
      use m_precision
!
      use t_node_id_spherical_IO
      use gz_binary_IO
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
      subroutine gz_read_rank_4_sph_b(gz_flags, sph_IO)
!
      type(file_IO_flags), intent(inout) :: gz_flags
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_read_mul_integer_b(gz_flags%iflag_bin_swap,               &
     &    cast_long(sph_IO%numdir_sph), sph_IO%sph_rank,                &
     &    gz_flags%ierr_IO)
!
      end subroutine gz_read_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_gl_resolution_sph_b(gz_flags, sph_IO)
!
      type(file_IO_flags), intent(inout) :: gz_flags
      type(sph_IO_data), intent(inout) :: sph_IO
!
!
      call gz_read_mul_integer_b(gz_flags%iflag_bin_swap,               &
     &    cast_long(sph_IO%numdir_sph), sph_IO%nidx_gl_sph,             &
     &    gz_flags%ierr_IO)
      if(gz_flags%ierr_IO .gt. 0) return
!
      call gz_read_one_integer_b                                        &
     &   (gz_flags%iflag_bin_swap, sph_IO%ltr_gl, gz_flags%ierr_IO)
!
      end subroutine gz_read_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_gl_nodes_sph_b(gz_flags, sph_IO)
!
      type(file_IO_flags), intent(inout) :: gz_flags
      type(sph_IO_data), intent(inout) :: sph_IO
!
      integer(kind = kint_gl) :: nvect
!
!
      call gz_read_one_integer_b                                        &
     &   (gz_flags%iflag_bin_swap, sph_IO%numnod_sph, gz_flags%ierr_IO)
      if(gz_flags%ierr_IO .gt. 0) return
!
      call alloc_nod_id_sph_IO(sph_IO)
!
      call gz_read_mul_int8_b(gz_flags%iflag_bin_swap,                  &
     &    cast_long(sph_IO%numnod_sph), sph_IO%inod_gl_sph,             &
     &     gz_flags%ierr_IO)
      if(gz_flags%ierr_IO .gt. 0) return
!
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      call gz_read_mul_integer_b(gz_flags%iflag_bin_swap,               &
     &    nvect, sph_IO%idx_gl_sph, gz_flags%ierr_IO)
!
      end subroutine gz_read_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_rank_4_sph_b(sph_IO)
!
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%sph_rank)
!
      end subroutine gz_write_rank_4_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_gl_resolution_sph_b(sph_IO)
!
      type(sph_IO_data), intent(in) :: sph_IO
!
!
      call gz_write_mul_integer_b                                       &
     &   (cast_long(sph_IO%numdir_sph), sph_IO%nidx_gl_sph)
      call gz_write_one_integer_b(sph_IO%ltr_gl)
!
      end subroutine gz_write_gl_resolution_sph_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_gl_nodes_sph_b(sph_IO)
!
      type(sph_IO_data), intent(in) :: sph_IO
!
      integer(kind = kint_gl) ::  nvect
!
!
      call gz_write_one_integer_b(sph_IO%numnod_sph)
      call gz_write_mul_int8_b                                          &
     &   (cast_long(sph_IO%numnod_sph), sph_IO%inod_gl_sph)
      nvect = sph_IO%numnod_sph * sph_IO%numdir_sph
      call gz_write_mul_integer_b(nvect, sph_IO%idx_gl_sph)
!
      end subroutine gz_write_gl_nodes_sph_b
!
! -----------------------------------------------------------------------
!
      end module gz_spherical_model_IO_b
