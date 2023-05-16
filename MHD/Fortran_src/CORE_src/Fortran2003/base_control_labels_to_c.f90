!>@file   base_control_labels_to_c.f90
!!@brief  module base_control_labels_to_c
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for field lines
!!
!!@verbatim
!!      integer(c_int) function num_label_time_step_ctl_f() bind(c)
!!      integer(c_int) function num_label_time_step_ctl_w_dep_f() bind(c)
!!      subroutine set_label_time_step_ctl_f(names)  bind(c)
!!@endverbatim
!
      module base_control_labels_to_c
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
      integer(c_int) function num_label_time_step_ctl_f() bind(c)
!
      use ctl_data_4_time_steps_IO
!
      num_label_time_step_ctl_f = num_label_time_step_ctl()
      return
      end function num_label_time_step_ctl_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_time_step_ctl_w_dep_f() bind(c)
!
      use ctl_data_4_time_steps_IO
!
      num_label_time_step_ctl_w_dep_f = num_label_time_step_ctl_w_dep()
      return
      end function num_label_time_step_ctl_w_dep_f
!
! ----------------------------------------------------------------------
!
      subroutine set_label_time_step_ctl_f(names)  bind(c)
!
      use ctl_data_4_time_steps_IO
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_time_step_ctl(names)
      end subroutine set_label_time_step_ctl_f
!
!  ---------------------------------------------------------------------
!
      end module base_control_labels_to_c
