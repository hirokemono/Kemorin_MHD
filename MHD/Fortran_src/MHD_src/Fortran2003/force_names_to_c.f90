!>@file   force_names_to_c.f90
!!        module force_names_to_c
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Routines to tell force names into C programs
!!
!!@verbatim
!!      integer(c_int) function num_advection_controls_f() bind(c)
!!      subroutine set_advection_control_labels_f                       &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_force_controls_f() bind(c)
!!      subroutine set_force_control_labels_f                           &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!@endverbatim
!
      module force_names_to_c
!
      use m_precision
      use ISO_C_BINDING
      use t_base_field_labels
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_advection_controls_f() bind(c)
!
      use m_force_control_labels
!
      num_advection_controls_f = num_advection_controls()
      return
      end function num_advection_controls_f
!
! ----------------------------------------------------------------------
!
      subroutine set_advection_control_labels_f                         &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_force_control_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_advection_control_labels                                 &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_advection_control_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_force_controls_f() bind(c)
!
      use m_force_control_labels
!
      num_force_controls_f = num_force_controls()
      return
      end function num_force_controls_f
!
! ----------------------------------------------------------------------
!
      subroutine set_force_control_labels_f                             &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_force_control_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_force_control_labels                                     &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_force_control_labels_f
!
! ----------------------------------------------------------------------
!
      end module force_names_to_c
