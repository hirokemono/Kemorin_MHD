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
!!      type(c_ptr) function c_link_force_list_to_ctl(c_ctl)            &
!!     &           bind(C, NAME='c_link_force_list_to_ctl')
!!      type(c_ptr) function c_link_sph_force_list_to_ctl(c_ctl)        &
!!     &           bind(C, NAME='c_link_sph_force_list_to_ctl')
!!      type(c_ptr) function c_link_filter_force_list(c_ctl)            &
!!     &           bind(C, NAME='c_link_filter_force_list')
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
      type(ctl_array_chara), save, private, target :: fil_frc_list
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
      type(c_ptr) function c_link_force_list_to_ctl(c_ctl)              &
     &           bind(C, NAME='c_link_force_list_to_ctl')
      use m_force_control_labels
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(force_list%c_tbl))                             &
     &          call set_force_list_array(force_list)
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
      if(.not. allocated(sph_force_list%c_tbl))                         &
     &           call set_sph_force_list_array(sph_force_list)
      c_link_sph_force_list_to_ctl = C_loc(sph_force_list)
!
      end function c_link_sph_force_list_to_ctl
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_filter_force_list(c_ctl)              &
     &           bind(C, NAME='c_link_filter_force_list')
      use m_force_control_labels
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(fil_frc_list%c_tbl))                           &
     &          call set_filter_force_list_array(fil_frc_list)
      c_link_filter_force_list = C_loc(fil_frc_list)
!
      end function c_link_filter_force_list
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_reftemp_list_to_ctl(c_ctl)            &
     &           bind(C, NAME='c_link_reftemp_list_to_ctl')
      use t_reference_scalar_param
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(temp_mdl_list%c_tbl))                          &
     &           call set_reftemp_list_array(temp_mdl_list)
      c_link_reftemp_list_to_ctl = C_loc(temp_mdl_list)
!
      end function c_link_reftemp_list_to_ctl
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_sph_reftemp_list_to_ctl(c_ctl)        &
     &           bind(C, NAME='c_link_sph_reftemp_list_to_ctl')
      use t_reference_scalar_param
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(sph_temp_mdl_list%c_tbl))                      &
     &           call set_sph_reftemp_list_array(sph_temp_mdl_list)
      c_link_sph_reftemp_list_to_ctl = C_loc(sph_temp_mdl_list)
!
      end function c_link_sph_reftemp_list_to_ctl
!
! ----------------------------------------------------------------------
!
      end module force_names_to_c
