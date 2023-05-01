!>@file   FLINE_control_label_to_c.f90
!!@brief  module FLINE_control_label_to_c
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for field lines
!!
!!@verbatim
!!      integer(c_int) function num_label_fline_ctl_f() bind(c)
!!      integer(c_int) function num_fline_start_flags_f() bind(c)
!!      integer(c_int) function num_fline_direction_flags_f() bind(c)
!!      integer(c_int) function num_fline_seeds_flags_f() bind(c)
!!
!!      subroutine set_label_fline_ctl_f(names)  bind(c)
!!      subroutine set_fline_start_flags_f(names)  bind(c)
!!      subroutine set_fline_direction_flags_f(names)  bind(c)
!!      subroutine set_fline_seeds_flags_f(names)  bind(c)
!!@endverbatim
!
      module FLINE_control_label_to_c
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
      integer(c_int) function num_label_fline_ctl_f() bind(c)
!
      use ctl_data_field_line_IO
!
      num_label_fline_ctl_f = num_label_fline_ctl()
      return
      end function num_label_fline_ctl_f
!
! ----------------------------------------------------------------------
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
      subroutine set_label_fline_ctl_f(names)  bind(c)
!
      use ctl_data_field_line_IO
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_fline_ctl(names)
      end subroutine set_label_fline_ctl_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_flags_f(names)  bind(c)
!
      use t_control_params_4_fline
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_fline_start_flags(names)
      end subroutine set_fline_start_flags_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_direction_flags_f(names)  bind(c)
!
      use t_control_params_4_fline
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_fline_direction_flags(names)
      end subroutine set_fline_direction_flags_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_seeds_flags_f(names)  bind(c)
!
      use t_control_params_4_fline
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_fline_seeds_flags(names)
      end subroutine set_fline_seeds_flags_f
!
!  ---------------------------------------------------------------------
!
      end module FLINE_control_label_to_c
