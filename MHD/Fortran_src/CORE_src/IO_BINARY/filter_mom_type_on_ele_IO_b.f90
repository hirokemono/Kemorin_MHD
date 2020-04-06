!filter_mom_type_on_ele_IO_b.f90
!     module filter_mom_type_on_ele_IO_b
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!!      subroutine read_base_filter_info_type_b(bbuf, filter_conf)
!!      subroutine write_base_filter_info_type_b(filter_conf, bbuf)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(filter_config_type), intent(inout) ::  filter_conf
!!
!!      subroutine read_elen_ele_type_b(bbuf, nele_fmom, elen_ele)
!!      subroutine write_elen_ele_type_b(nele_fmom, elen_ele, bbuf)
!!        integer(kind = kint), intent(in) :: nele_fmom
!!        type(elen_ele_diffs_type), intent(in) :: elen_ele
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!
!!      subroutine read_filter_moms_ele_type_b                          &
!!     &         (bbuf, nele_fmom, mom_ele)
!!      subroutine write_filter_moms_ele_type_b                         &
!!     &         (nele_fmom, mom_ele, bbuf)
!!        integer(kind = kint), intent(in) :: nele_fmom
!!        type(ele_mom_diffs_type), intent(inout) :: mom_ele
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!
      module filter_mom_type_on_ele_IO_b
!
      use m_precision
      use t_binary_IO_buffer
      use binary_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_base_filter_info_type_b(bbuf, filter_conf)
!
      use t_filter_elength
      use filter_moments_IO_b
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(filter_config_type), intent(inout) ::  filter_conf
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = filter_conf%nf_type
      call read_mul_character_b                                         &
     &   (bbuf, filter_conf%nf_type, filter_conf%filter_type)
      if(bbuf%ierr_bin .ne. 0) return
      call read_1d_vector_b(bbuf, num64, filter_conf%f_width)
      if(bbuf%ierr_bin .ne. 0) return
      call read_2d_vector_b                                             &
     &   (bbuf, num64, ithree, filter_conf%xmom_1d_org(1,0))
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine read_base_filter_info_type_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_base_filter_info_type_b(filter_conf, bbuf)
!
      use t_filter_elength
      use filter_moments_IO_b
!
      type(filter_config_type), intent(in) ::  filter_conf
      type(binary_IO_buffer), intent(inout) :: bbuf
!
      integer(kind = kint_gl) :: num64
!
!
      call write_mul_character_b                                        &
     &   (filter_conf%nf_type, filter_conf%filter_type, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      num64 = filter_conf%nf_type
      call write_1d_vector_b(num64, filter_conf%f_width, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_2d_vector_b                                            &
     &   (num64, ithree, filter_conf%xmom_1d_org(1,0), bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine write_base_filter_info_type_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elen_ele_type_b(bbuf, nele_fmom, elen_ele)
!
      use t_filter_elength
      use filter_moments_IO_b
!
      integer(kind = kint_gl), intent(in) :: nele_fmom
      type(elen_ele_diffs_type), intent(inout) :: elen_ele
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call read_elength_b(bbuf, nele_fmom, elen_ele%moms%f_x2,          &
     &    elen_ele%moms%f_y2, elen_ele%moms%f_z2)
      if(bbuf%ierr_bin .ne. 0) return
      call read_elength_b(bbuf, nele_fmom, elen_ele%moms%f_xy,          &
     &    elen_ele%moms%f_yz, elen_ele%moms%f_zx)
      if(bbuf%ierr_bin .ne. 0) return
!
      call read_mom_coefs_dx_b(bbuf, nele_fmom, elen_ele%diff%df_x2,    &
     &    elen_ele%diff%df_y2, elen_ele%diff%df_z2)
      if(bbuf%ierr_bin .ne. 0) return
      call read_mom_coefs_dx_b(bbuf, nele_fmom, elen_ele%diff%df_xy,    &
     &    elen_ele%diff%df_yz, elen_ele%diff%df_zx)
      if(bbuf%ierr_bin .ne. 0) return
!
      call read_mom_coefs_dx_b(bbuf, nele_fmom, elen_ele%diff2%df_x2,   &
     &    elen_ele%diff2%df_y2, elen_ele%diff2%df_z2)
      if(bbuf%ierr_bin .ne. 0) return
      call read_mom_coefs_dx_b(bbuf, nele_fmom, elen_ele%diff2%df_xy,   &
     &    elen_ele%diff2%df_yz, elen_ele%diff2%df_zx)
!
      end subroutine read_elen_ele_type_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_elen_ele_type_b(nele_fmom, elen_ele, bbuf)
!
      use t_filter_elength
      use filter_moments_IO_b
!
      integer(kind = kint_gl), intent(in) :: nele_fmom
      type(elen_ele_diffs_type), intent(in) :: elen_ele
!
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!    output coefs for filters for each element
!
!      write(id_file,'(a)')  '! dx^2 for each element'
!      write(id_file,'(a)')  '! element ID, length of x, y, z'
      call write_elength_b(nele_fmom, elen_ele%moms%f_x2,               &
     &    elen_ele%moms%f_y2, elen_ele%moms%f_z2, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!      write(id_file,'(a)')  '! dxdy for each element'
!      write(id_file,'(a)')  '! element ID, length of x, y, z'
      call write_elength_b(nele_fmom, elen_ele%moms%f_xy,               &
     &    elen_ele%moms%f_yz, elen_ele%moms%f_zx, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
!      write(id_file,'(a)')  '! 1st derivative of dx^2 for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(nele_fmom, elen_ele%diff%df_x2,         &
     &    elen_ele%diff%df_y2, elen_ele%diff%df_z2, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
!      write(id_file,'(a)')  '! 1st derivative of dxdy for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(nele_fmom, elen_ele%diff%df_xy,         &
     &    elen_ele%diff%df_yz, elen_ele%diff%df_zx, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
!      write(id_file,'(a)')  '! 2nd derivative of dx^2 for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(nele_fmom, elen_ele%diff2%df_x2,        &
     &    elen_ele%diff2%df_y2, elen_ele%diff2%df_z2, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
!      write(id_file,'(a)')  '! 2nd derivative of dxdy for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(nele_fmom, elen_ele%diff2%df_xy,        &
     &    elen_ele%diff2%df_yz, elen_ele%diff2%df_zx, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine write_elen_ele_type_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_moms_ele_type_b                            &
     &         (bbuf, nele_fmom, mom_ele)
!
      use t_filter_moments
      use filter_moments_IO_b
!
      integer(kind = kint_gl), intent(in) :: nele_fmom
      type(ele_mom_diffs_type), intent(inout) :: mom_ele
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call read_elength_b(bbuf, nele_fmom, mom_ele%moms%f_x2,           &
     &    mom_ele%moms%f_y2, mom_ele%moms%f_z2)
      if(bbuf%ierr_bin .ne. 0) return
      call read_elength_b(bbuf, nele_fmom, mom_ele%moms%f_xy,           &
     &    mom_ele%moms%f_yz, mom_ele%moms%f_zx)
      if(bbuf%ierr_bin .ne. 0) return
      call read_elength_b(bbuf, nele_fmom, mom_ele%moms%f_x,            &
     &    mom_ele%moms%f_y, mom_ele%moms%f_z)
      if(bbuf%ierr_bin .ne. 0) return
!
!
      call read_mom_coefs_dx_b(bbuf, nele_fmom, mom_ele%diff%df_x2,     &
     &    mom_ele%diff%df_y2, mom_ele%diff%df_z2)
      if(bbuf%ierr_bin .ne. 0) return
      call read_mom_coefs_dx_b(bbuf, nele_fmom, mom_ele%diff%df_xy,     &
     &    mom_ele%diff%df_yz, mom_ele%diff%df_zx)
      if(bbuf%ierr_bin .ne. 0) return
      call read_mom_coefs_dx_b(bbuf, nele_fmom, mom_ele%diff%df_x,      &
     &    mom_ele%diff%df_y, mom_ele%diff2%df_x2)
      if(bbuf%ierr_bin .ne. 0) return
!
!
      call read_mom_coefs_dx_b(bbuf, nele_fmom, mom_ele%diff2%df_x2,    &
     &    mom_ele%diff2%df_y2, mom_ele%diff2%df_z2)
      if(bbuf%ierr_bin .ne. 0) return
      call read_mom_coefs_dx_b(bbuf, nele_fmom, mom_ele%diff2%df_xy,    &
     &    mom_ele%diff2%df_yz, mom_ele%diff2%df_zx)
      if(bbuf%ierr_bin .ne. 0) return
      call read_mom_coefs_dx_b(bbuf, nele_fmom, mom_ele%diff2%df_x,     &
     &    mom_ele%diff2%df_y, mom_ele%diff2%df_z)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine read_filter_moms_ele_type_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moms_ele_type_b                           &
     &         (nele_fmom, mom_ele, bbuf)
!
      use t_filter_moments
      use filter_moments_IO_b
!
      integer(kind = kint_gl), intent(in) :: nele_fmom
      type(ele_mom_diffs_type), intent(in) :: mom_ele
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
!      write(id_file,'(a)')  '! Second filter moments for each element'
!      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength_b(nele_fmom, mom_ele%moms%f_x2,                &
     &    mom_ele%moms%f_y2, mom_ele%moms%f_z2, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!     &     '! product of first order moment in 2 direction' 
!      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength_b(nele_fmom, mom_ele%moms%f_xy,                &
     &    mom_ele%moms%f_yz, mom_ele%moms%f_zx, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!      write(id_file,'(a)')  '! first filter moments for each element'
!      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength_b(nele_fmom, mom_ele%moms%f_x,                 &
     &    mom_ele%moms%f_y, mom_ele%moms%f_z, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
!     &     '! 1st diff. of Second filter moments for each element'
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff%df_x2,          &
     &    mom_ele%diff%df_y2, mom_ele%diff%df_z2, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff%df_xy,          &
     &    mom_ele%diff%df_yz, mom_ele%diff%df_zx, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff%df_x,           &
     &    mom_ele%diff%df_y, mom_ele%diff2%df_x2, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
!
!     &     '! 2nd diff. of Second filter moments for each element'
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff2%df_x2,         &
     &    mom_ele%diff2%df_y2, mom_ele%diff2%df_z2, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff2%df_xy,         &
     &    mom_ele%diff2%df_yz, mom_ele%diff2%df_zx, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff2%df_x,          &
     &    mom_ele%diff2%df_y, mom_ele%diff2%df_z, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
      end subroutine write_filter_moms_ele_type_b
!
!  ---------------------------------------------------------------------
!
      end module filter_mom_type_on_ele_IO_b
