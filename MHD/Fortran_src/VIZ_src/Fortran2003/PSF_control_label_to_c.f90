!>@file   PSF_control_label_to_c.f90
!!@brief  module PSF_control_label_to_c
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for isuosurfaces
!!
!!@verbatim
!!      integer(c_int) function num_label_iso_type_f() bind(c)
!!      subroutine set_label_iso_type_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_psf_def_type_f() bind(c)
!!      integer(c_int) function num_label_psf_def_type_grp_f() bind(c)
!!      integer(c_int) function num_label_psf_dirs_f() bind(c)
!!      integer(c_int) function num_label_psf_coefs_f() bind(c)
!!      subroutine set_label_psf_def_type_grp_f(names_c)  bind(c)
!!      subroutine set_label_psf_dirs_f(names_c)  bind(c)
!!      subroutine set_label_psf_coefs_f(names_c)  bind(c)
!!
!!      subroutine set_primary_section_coef_flag_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_fline_start_flags_f() bind(c)
!!      integer(c_int) function num_fline_direction_flags_f() bind(c)
!!      integer(c_int) function num_fline_seeds_flags_f() bind(c)
!!
!!      subroutine set_fline_start_flags_f(names_c)  bind(c)
!!      subroutine set_fline_direction_flags_f(names_c)  bind(c)
!!      subroutine set_fline_seeds_flags_f(names_c)  bind(c)
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
      integer(c_int) function num_label_iso_type_f() bind(c)
!
      use t_control_params_4_iso
!
      num_label_iso_type_f = num_label_iso_type()
      return
      end function num_label_iso_type_f
!
! ----------------------------------------------------------------------
!
      subroutine set_label_iso_type_f(names_c)  bind(c)
!
      use t_control_params_4_iso
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_iso_type()])
      call set_label_iso_type(name_f)
      end subroutine set_label_iso_type_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
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
      subroutine set_label_psf_def_type_grp_f(names_c)  bind(c)
!
      use set_coefs_of_sections
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_psf_def_type_grp()])
      call set_label_psf_def_type_grp(name_f)
      end subroutine set_label_psf_def_type_grp_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_psf_dirs_f(names_c)  bind(c)
!
      use m_section_coef_flags
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_psf_dirs()])
      call set_label_psf_dirs(name_f)
      end subroutine set_label_psf_dirs_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_psf_coefs_f(names_c)  bind(c)
!
      use m_section_coef_flags
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_psf_coefs()])
      call set_label_psf_coefs(name_f)
      end subroutine set_label_psf_coefs_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_primary_section_coef_flag_f(names_c)  bind(c)
!
      use skip_comment_f
      use m_section_coef_flags
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [1])
      name_f(1) = fill_from_null(name_f(1))
      call set_primary_section_coef_flag(name_f(1))
!
      end subroutine set_primary_section_coef_flag_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_fline_start_flags_f() bind(c)
!
      use t_control_params_4_fline
!
      num_fline_start_flags_f = num_fline_start_flags()
      return
      end function num_fline_start_flags_f
!
! ----------------------------------------------------------------------
!
      subroutine set_fline_start_flags_f(names_c)  bind(c)
!
      use t_control_params_4_fline
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_fline_start_flags()])
      call set_fline_start_flags(name_f)
      end subroutine set_fline_start_flags_f
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_fline_direction_flags_f() bind(c)
!
      use t_control_params_4_fline
!
      num_fline_direction_flags_f = num_fline_direction_flags()
      return
      end function num_fline_direction_flags_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_fline_seeds_flags_f() bind(c)
!
      use t_control_params_4_fline
!
      num_fline_seeds_flags_f = num_fline_seeds_flags()
      return
      end function num_fline_seeds_flags_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_fline_direction_flags_f(names_c)  bind(c)
!
      use t_control_params_4_fline
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_fline_direction_flags()])
      call set_fline_direction_flags(name_f)
      end subroutine set_fline_direction_flags_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_seeds_flags_f(names_c)  bind(c)
!
      use t_control_params_4_fline
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_fline_seeds_flags()])
      call set_fline_seeds_flags(name_f)
      end subroutine set_fline_seeds_flags_f
!
!  ---------------------------------------------------------------------
!
      end module PSF_control_label_to_c
