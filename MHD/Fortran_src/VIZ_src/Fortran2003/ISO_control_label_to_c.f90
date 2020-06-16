!>@file   ISO_control_label_to_c.f90
!!@brief  module ISO_control_label_to_c
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for isuosurfaces
!!
!!@verbatim
!!      integer(c_int) function num_label_iso_ctl_f() bind(c)
!!      integer(c_int) function num_label_iso_ctl_w_dpl_f() bind(c)
!!      integer(c_int) function num_label_iso_define_control_f() bind(c)
!!      integer(c_int) function num_label_fld_on_iso_control_f() bind(c)
!!      integer(c_int) function num_label_iso_type_f() bind(c)
!!
!!      subroutine set_label_iso_ctl_w_dpl_f(names)  bind(c)
!!      subroutine set_label_iso_define_control_f(names)  bind(c)
!!      subroutine set_label_fld_on_iso_control_f(names)  bind(c)
!!      subroutine set_label_iso_type_f(names)  bind(c)
!!@endverbatim
!
      module ISO_control_label_to_c
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
      use t_control_data_4_iso
!
      num_label_iso_ctl_f = num_label_iso_ctl()
      return
      end function num_label_iso_ctl_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_iso_ctl_w_dpl_f() bind(c)
!
      use t_control_data_4_iso
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
      integer(c_int) function num_label_fld_on_iso_control_f() bind(c)
!
      use t_control_data_4_fld_on_iso
!
      num_label_fld_on_iso_control_f = num_label_fld_on_iso_control()
      return
      end function num_label_fld_on_iso_control_f
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
      use t_control_data_4_iso
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
      subroutine set_label_fld_on_iso_control_f(names)  bind(c)
!
      use t_control_data_4_fld_on_iso
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_fld_on_iso_control(names)
      end subroutine set_label_fld_on_iso_control_f
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
!
      end module ISO_control_label_to_c
