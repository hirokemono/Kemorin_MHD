!filter_mom_type_on_ele_IO_b.f90
!     module filter_mom_type_on_ele_IO_b
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!!      subroutine read_base_filter_info_type_b(bflag, filter_conf)
!!      subroutine write_base_filter_info_type_b(filter_conf, bflag)
!!        type(binary_IO_flags), intent(inout) :: bflag
!!        type(filter_config_type), intent(inout) ::  filter_conf
!!
!!      subroutine read_elen_ele_type_b(bflag, nele_fmom, elen_ele)
!!      subroutine write_elen_ele_type_b(nele_fmom, elen_ele, bflag)
!!        integer(kind = kint), intent(in) :: nele_fmom
!!        type(elen_ele_diffs_type), intent(in) :: elen_ele
!!        type(binary_IO_flags), intent(inout) :: bflag
!!
!!      subroutine read_filter_moms_ele_type_b                          &
!!     &         (bflag, nele_fmom, mom_ele)
!!      subroutine write_filter_moms_ele_type_b                         &
!!     &         (nele_fmom, mom_ele, bflag)
!!        integer(kind = kint), intent(in) :: nele_fmom
!!        type(ele_mom_diffs_type), intent(inout) :: mom_ele
!
      module filter_mom_type_on_ele_IO_b
!
      use m_precision
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
      subroutine read_base_filter_info_type_b(bflag, filter_conf)
!
      use t_filter_elength
      use filter_moments_IO_b
!
      type(binary_IO_flags), intent(inout) :: bflag
      type(filter_config_type), intent(inout) ::  filter_conf
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = filter_conf%nf_type
      call read_mul_character_b                                         &
     &   (bflag, filter_conf%nf_type, filter_conf%filter_type)
      if(bflag%ierr_IO .ne. 0) return
      call read_1d_vector_b(bflag, num64, filter_conf%f_width)
      if(bflag%ierr_IO .ne. 0) return
      call read_2d_vector_b                                             &
     &   (bflag, num64, ithree, filter_conf%xmom_1d_org(1,0))
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_base_filter_info_type_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_base_filter_info_type_b(filter_conf, bflag)
!
      use t_filter_elength
      use filter_moments_IO_b
!
      type(filter_config_type), intent(in) ::  filter_conf
      type(binary_IO_flags), intent(inout) :: bflag
!
      integer(kind = kint_gl) :: num64
!
!
      call write_mul_character_b                                        &
     &   (filter_conf%nf_type, filter_conf%filter_type, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      num64 = filter_conf%nf_type
      call write_1d_vector_b(num64, filter_conf%f_width, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_2d_vector_b                                            &
     &   (num64, ithree, filter_conf%xmom_1d_org(1,0), bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_base_filter_info_type_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elen_ele_type_b(bflag, nele_fmom, elen_ele)
!
      use t_filter_elength
      use filter_moments_IO_b
!
      integer(kind = kint_gl), intent(in) :: nele_fmom
      type(binary_IO_flags), intent(inout) :: bflag
      type(elen_ele_diffs_type), intent(inout) :: elen_ele
!
!
      call read_elength_b(bflag, nele_fmom, elen_ele%moms%f_x2,         &
     &    elen_ele%moms%f_y2, elen_ele%moms%f_z2)
      if(bflag%ierr_IO .ne. 0) return
      call read_elength_b(bflag, nele_fmom, elen_ele%moms%f_xy,         &
     &    elen_ele%moms%f_yz, elen_ele%moms%f_zx)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_mom_coefs_dx_b(bflag, nele_fmom, elen_ele%diff%df_x2,   &
     &    elen_ele%diff%df_y2, elen_ele%diff%df_z2)
      if(bflag%ierr_IO .ne. 0) return
      call read_mom_coefs_dx_b(bflag, nele_fmom, elen_ele%diff%df_xy,   &
     &    elen_ele%diff%df_yz, elen_ele%diff%df_zx)
      if(bflag%ierr_IO .ne. 0) return
!
      call read_mom_coefs_dx_b(bflag, nele_fmom, elen_ele%diff2%df_x2,  &
     &    elen_ele%diff2%df_y2, elen_ele%diff2%df_z2)
      if(bflag%ierr_IO .ne. 0) return
      call read_mom_coefs_dx_b(bflag, nele_fmom, elen_ele%diff2%df_xy,  &
     &    elen_ele%diff2%df_yz, elen_ele%diff2%df_zx)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_elen_ele_type_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_elen_ele_type_b(nele_fmom, elen_ele, bflag)
!
      use t_filter_elength
      use filter_moments_IO_b
!
      integer(kind = kint_gl), intent(in) :: nele_fmom
      type(elen_ele_diffs_type), intent(in) :: elen_ele
!
      type(binary_IO_flags), intent(inout) :: bflag
!
!    output coefs for filters for each element
!
!      write(id_file,'(a)')  '! dx^2 for each element'
!      write(id_file,'(a)')  '! element ID, length of x, y, z'
      call write_elength_b(nele_fmom, elen_ele%moms%f_x2,               &
     &    elen_ele%moms%f_y2, elen_ele%moms%f_z2, bflag)
      if(bflag%ierr_IO .ne. 0) return
!      write(id_file,'(a)')  '! dxdy for each element'
!      write(id_file,'(a)')  '! element ID, length of x, y, z'
      call write_elength_b(nele_fmom, elen_ele%moms%f_xy,               &
     &    elen_ele%moms%f_yz, elen_ele%moms%f_zx, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(id_file,'(a)')  '! 1st derivative of dx^2 for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(nele_fmom, elen_ele%diff%df_x2,         &
     &    elen_ele%diff%df_y2, elen_ele%diff%df_z2, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(id_file,'(a)')  '! 1st derivative of dxdy for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(nele_fmom, elen_ele%diff%df_xy,         &
     &    elen_ele%diff%df_yz, elen_ele%diff%df_zx, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(id_file,'(a)')  '! 2nd derivative of dx^2 for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(nele_fmom, elen_ele%diff2%df_x2,        &
     &    elen_ele%diff2%df_y2, elen_ele%diff2%df_z2, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(id_file,'(a)')  '! 2nd derivative of dxdy for each element'
!     &    '! direction of diffrenciate, ele ID, length of x, y, z'
      call write_mom_coefs_dx_b(nele_fmom, elen_ele%diff2%df_xy,        &
     &    elen_ele%diff2%df_yz, elen_ele%diff2%df_zx, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_elen_ele_type_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_moms_ele_type_b                            &
     &         (bflag, nele_fmom, mom_ele)
!
      use t_filter_moments
      use filter_moments_IO_b
!
      integer(kind = kint_gl), intent(in) :: nele_fmom
      type(binary_IO_flags), intent(inout) :: bflag
      type(ele_mom_diffs_type), intent(inout) :: mom_ele
!
!
      call read_elength_b(bflag, nele_fmom, mom_ele%moms%f_x2,          &
     &    mom_ele%moms%f_y2, mom_ele%moms%f_z2)
      if(bflag%ierr_IO .ne. 0) return
      call read_elength_b(bflag, nele_fmom, mom_ele%moms%f_xy,          &
     &    mom_ele%moms%f_yz, mom_ele%moms%f_zx)
      if(bflag%ierr_IO .ne. 0) return
      call read_elength_b(bflag, nele_fmom, mom_ele%moms%f_x,           &
     &    mom_ele%moms%f_y, mom_ele%moms%f_z)
      if(bflag%ierr_IO .ne. 0) return
!
!
      call read_mom_coefs_dx_b(bflag, nele_fmom, mom_ele%diff%df_x2,    &
     &    mom_ele%diff%df_y2, mom_ele%diff%df_z2)
      if(bflag%ierr_IO .ne. 0) return
      call read_mom_coefs_dx_b(bflag, nele_fmom, mom_ele%diff%df_xy,    &
     &    mom_ele%diff%df_yz, mom_ele%diff%df_zx)
      if(bflag%ierr_IO .ne. 0) return
      call read_mom_coefs_dx_b(bflag, nele_fmom, mom_ele%diff%df_x,     &
     &    mom_ele%diff%df_y, mom_ele%diff2%df_x2)
      if(bflag%ierr_IO .ne. 0) return
!
!
      call read_mom_coefs_dx_b(bflag, nele_fmom, mom_ele%diff2%df_x2,   &
     &    mom_ele%diff2%df_y2, mom_ele%diff2%df_z2)
      if(bflag%ierr_IO .ne. 0) return
      call read_mom_coefs_dx_b(bflag, nele_fmom, mom_ele%diff2%df_xy,   &
     &    mom_ele%diff2%df_yz, mom_ele%diff2%df_zx)
      if(bflag%ierr_IO .ne. 0) return
      call read_mom_coefs_dx_b(bflag, nele_fmom, mom_ele%diff2%df_x,    &
     &    mom_ele%diff2%df_y, mom_ele%diff2%df_z)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_filter_moms_ele_type_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moms_ele_type_b                           &
     &         (nele_fmom, mom_ele, bflag)
!
      use t_filter_moments
      use filter_moments_IO_b
!
      integer(kind = kint_gl), intent(in) :: nele_fmom
      type(ele_mom_diffs_type), intent(in) :: mom_ele
      type(binary_IO_flags), intent(inout) :: bflag
!
!
!      write(id_file,'(a)')  '! Second filter moments for each element'
!      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength_b(nele_fmom, mom_ele%moms%f_x2,                &
     &    mom_ele%moms%f_y2, mom_ele%moms%f_z2, bflag)
      if(bflag%ierr_IO .ne. 0) return
!     &     '! product of first order moment in 2 direction' 
!      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength_b(nele_fmom, mom_ele%moms%f_xy,                &
     &    mom_ele%moms%f_yz, mom_ele%moms%f_zx, bflag)
      if(bflag%ierr_IO .ne. 0) return
!      write(id_file,'(a)')  '! first filter moments for each element'
!      write(id_file,'(a)')  '! element ID, x, y, z direction'
      call write_elength_b(nele_fmom, mom_ele%moms%f_x,                 &
     &    mom_ele%moms%f_y, mom_ele%moms%f_z, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!     &     '! 1st diff. of Second filter moments for each element'
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff%df_x2,          &
     &    mom_ele%diff%df_y2, mom_ele%diff%df_z2, bflag)
      if(bflag%ierr_IO .ne. 0) return
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff%df_xy,          &
     &    mom_ele%diff%df_yz, mom_ele%diff%df_zx, bflag)
      if(bflag%ierr_IO .ne. 0) return
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff%df_x,           &
     &    mom_ele%diff%df_y, mom_ele%diff2%df_x2, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!
!     &     '! 2nd diff. of Second filter moments for each element'
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff2%df_x2,         &
     &    mom_ele%diff2%df_y2, mom_ele%diff2%df_z2, bflag)
      if(bflag%ierr_IO .ne. 0) return
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff2%df_xy,         &
     &    mom_ele%diff2%df_yz, mom_ele%diff2%df_zx, bflag)
      if(bflag%ierr_IO .ne. 0) return
!     &     '! direction of difference, element ID, x, y, z direction'
      call write_mom_coefs_dx_b(nele_fmom, mom_ele%diff2%df_x,          &
     &    mom_ele%diff2%df_y, mom_ele%diff2%df_z, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_filter_moms_ele_type_b
!
!  ---------------------------------------------------------------------
!
      end module filter_mom_type_on_ele_IO_b
