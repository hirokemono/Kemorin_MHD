!>@file   PSF_control_label_to_c.f90
!!@brief  module PSF_control_label_to_c
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for isuosurfaces
!!
!!@verbatim
!!      integer(c_int) function num_label_iso_ctl_f() bind(c)
!!      integer(c_int) function num_label_iso_ctl_w_dpl_f() bind(c)
!!      integer(c_int) function num_label_iso_define_control_f() bind(c)
!!      integer(c_int) function num_label_iso_type_f() bind(c)
!!
!!      subroutine set_label_iso_ctl_w_dpl_f(names)  bind(c)
!!      subroutine set_label_iso_define_control_f(names)  bind(c)
!!      subroutine set_label_iso_type_f(names)  bind(c)
!!
!!      integer(c_int) function num_label_fld_on_psf_control_f() bind(c)
!!      integer(c_int) function num_label_psf_define_control_f() bind(c)
!!      integer(c_int) function num_label_psf_ctl_w_dpl_f() bind(c)
!!      integer(c_int) function num_label_psf_def_type_f() bind(c)
!!      integer(c_int) function num_label_psf_def_type_grp_f() bind(c)
!!      integer(c_int) function num_label_psf_dirs_f() bind(c)
!!      integer(c_int) function num_label_psf_coefs_f() bind(c)
!!      subroutine set_label_fld_on_psf_control_f(names)  bind(c)
!!      subroutine set_label_psf_define_control_f(names)  bind(c)
!!      subroutine set_label_psf_ctl_w_dpl_f(names)  bind(c)
!!      subroutine set_label_psf_def_type_grp_f(names)  bind(c)
!!      subroutine set_label_psf_dirs_f(names)  bind(c)
!!      subroutine set_label_psf_coefs_f(names)  bind(c)
!!
!!      subroutine set_primary_section_coef_flag_f(names)  bind(c)
!!@endverbatim
!
      module PSF_control_label_to_c
!
      use ISO_C_BINDING
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_iso_ctl_f() bind(c)
!
      use read_iso_control_data
!
      num_label_iso_ctl_f = num_label_iso_ctl()
      return
      end function num_label_iso_ctl_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_iso_ctl_w_dpl_f() bind(c)
!
      use read_iso_control_data
!
      num_label_iso_ctl_w_dpl_f = num_label_iso_ctl_w_dpl()
      return
      end function num_label_iso_ctl_w_dpl_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_iso_define_control_f() bind(c)
!
      use t_control_data_4_iso_def
!
      num_label_iso_define_control_f = num_label_iso_define_control()
      return
      end function num_label_iso_define_control_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_iso_type_f() bind(c)
!
      use t_control_params_4_iso
!
      num_label_iso_type_f = num_label_iso_type()
      return
      end function num_label_iso_type_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_label_iso_ctl_w_dpl_f(names)  bind(c)
!
      use read_iso_control_data
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_iso_ctl_w_dpl(names)
      end subroutine set_label_iso_ctl_w_dpl_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_iso_define_control_f(names)  bind(c)
!
      use t_control_data_4_iso_def
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_iso_define_control(names)
      end subroutine set_label_iso_define_control_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_iso_type_f(names)  bind(c)
!
      use t_control_params_4_iso
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_iso_type(names)
      end subroutine set_label_iso_type_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_fld_on_psf_control_f() bind(c)
!
      use t_control_data_4_fld_on_psf
!
      num_label_fld_on_psf_control_f = num_label_fld_on_psf_control()
      return
      end function num_label_fld_on_psf_control_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_psf_define_control_f() bind(c)
!
      use t_control_data_4_psf_def
!
      num_label_psf_define_control_f = num_label_psf_define_control()
      return
      end function num_label_psf_define_control_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_psf_ctl_w_dpl_f() bind(c)
!
      use t_control_data_4_psf
!
      num_label_psf_ctl_w_dpl_f = num_label_psf_ctl_w_dpl()
      return
      end function num_label_psf_ctl_w_dpl_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_psf_def_type_f() bind(c)
!
      use set_coefs_of_sections
!
      num_label_psf_def_type_f = num_label_psf_def_type()
      return
      end function num_label_psf_def_type_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_psf_def_type_grp_f() bind(c)
!
      use set_coefs_of_sections
!
      num_label_psf_def_type_grp_f = num_label_psf_def_type_grp()
      return
      end function num_label_psf_def_type_grp_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_psf_dirs_f() bind(c)
!
      use m_section_coef_flags
!
      num_label_psf_dirs_f = num_label_psf_dirs()
      return
      end function num_label_psf_dirs_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_psf_coefs_f() bind(c)
!
      use m_section_coef_flags
!
      num_label_psf_coefs_f = num_label_psf_coefs()
      return
      end function num_label_psf_coefs_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_label_fld_on_psf_control_f(names)  bind(c)
!
      use t_control_data_4_fld_on_psf
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_fld_on_psf_control(names)
      end subroutine set_label_fld_on_psf_control_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_psf_define_control_f(names)  bind(c)
!
      use t_control_data_4_psf_def
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_psf_define_control(names)
      end subroutine set_label_psf_define_control_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_psf_ctl_w_dpl_f(names)  bind(c)
!
      use t_control_data_4_psf
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_psf_ctl_w_dpl(names)
      end subroutine set_label_psf_ctl_w_dpl_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_psf_def_type_grp_f(names)  bind(c)
!
      use set_coefs_of_sections
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_psf_def_type_grp(names)
      end subroutine set_label_psf_def_type_grp_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_psf_dirs_f(names)  bind(c)
!
      use m_section_coef_flags
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_psf_dirs(names)
      end subroutine set_label_psf_dirs_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_psf_coefs_f(names)  bind(c)
!
      use m_section_coef_flags
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_psf_coefs(names)
      end subroutine set_label_psf_coefs_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_primary_section_coef_flag_f(names)  bind(c)
!
      use skip_comment_f
      use m_section_coef_flags
!
      character(C_CHAR), intent(inout) :: names(*)
      character(len=kchara) :: name_f
!
      name_f = fill_from_null(names(1))
      call set_primary_section_coef_flag(name_f)
!
      end subroutine set_primary_section_coef_flag_f
!
!  ---------------------------------------------------------------------
!
      end module PSF_control_label_to_c
