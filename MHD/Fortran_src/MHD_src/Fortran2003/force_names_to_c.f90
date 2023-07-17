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
!!
!!      type(c_ptr) function c_link_force_list_to_ctl(c_ctl)            &
!!     &           bind(C, NAME='c_link_force_list_to_ctl')
!!      type(c_ptr) function c_link_sph_force_list_to_ctl(c_ctl)        &
!!     &           bind(C, NAME='c_link_sph_force_list_to_ctl')
!!      type(c_ptr) function c_link_reftemp_list_to_ctl(c_ctl)          &
!!     &           bind(C, NAME='c_link_reftemp_list_to_ctl')
!!      type(c_ptr) function c_link_sph_reftemp_list_to_ctl(c_ctl)      &
!!     &           bind(C, NAME='c_link_sph_reftemp_list_to_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
!
      module force_names_to_c
!
      use m_precision
      use ISO_C_BINDING
      use t_base_field_labels
      use t_control_array_character
!
      implicit none
!
      type(ctl_array_chara), save, private, target :: force_list
      type(ctl_array_chara), save, private, target :: sph_force_list
!
      type(ctl_array_chara), save, private, target :: temp_mdl_list
      type(ctl_array_chara), save, private, target :: sph_temp_mdl_list
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_advection_controls()])
      call c_f_pointer(field_math_c, math, [num_advection_controls()])
      call set_advection_control_labels(n_comps_c(1), field, math)
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_force_controls()])
      call c_f_pointer(field_math_c, math, [num_force_controls()])
      call set_force_control_labels(n_comps_c(1), field, math)
!
      end subroutine set_force_control_labels_f
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_force_list_to_ctl(c_ctl)              &
     &           bind(C, NAME='c_link_force_list_to_ctl')
      use m_force_control_labels
      type(c_ptr), value, intent(in) :: c_ctl
!
      call set_force_list_array(force_list)
      c_link_force_list_to_ctl = C_loc(force_list)
!
      end function c_link_force_list_to_ctl
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_sph_force_list_to_ctl(c_ctl)          &
     &           bind(C, NAME='c_link_sph_force_list_to_ctl')
      use m_force_control_labels
      type(c_ptr), value, intent(in) :: c_ctl
!
      call set_sph_force_list_array(sph_force_list)
      c_link_sph_force_list_to_ctl = C_loc(sph_force_list)
!
      end function c_link_sph_force_list_to_ctl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_reftemp_list_to_ctl(c_ctl)            &
     &           bind(C, NAME='c_link_reftemp_list_to_ctl')
      use t_reference_scalar_param
      type(c_ptr), value, intent(in) :: c_ctl
!
      call set_reftemp_list_array(temp_mdl_list)
      c_link_reftemp_list_to_ctl = C_loc(temp_mdl_list)
!
      end function c_link_reftemp_list_to_ctl
!
! ----------------------------------------------------------------------
!
      end module force_names_to_c
