!>@file   component_names_to_c.f90
!!        module component_names_to_c
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Routines to tell force names into C programs
!!
!!@verbatim
!!      subroutine set_primary_componnet_flag_f(input_flag_c) bind(c)
!!        character(C_CHAR), intent(inout) :: input_flag(*)
!!
!!      type(c_ptr) function c_link_xyz_dir_list_to_ctl()               &
!!     &           bind(C, NAME='c_link_xyz_dir_list_to_ctl')
!!      type(c_ptr) function c_link_xyzw_dir_list_to_ctl()              &
!!     &           bind(C, NAME='c_link_xyzw_dir_list_to_ctl')
!!
!!      type(c_ptr) function c_link_scalar_dir_list_to_ctl()            &
!!     &           bind(C, NAME='c_link_scalar_dir_list_to_ctl')
!!      type(c_ptr) function c_link_vector_dir_list_to_ctl()            &
!!     &           bind(C, NAME='c_link_vector_dir_list_to_ctl')
!!      type(c_ptr) function c_link_stensor_dir_list_to_ctl()           &
!!     &           bind(C, NAME='c_link_stensor_dir_list_to_ctl')
!!      type(c_ptr) function c_link_atensor_dir_list_to_ctl()           &
!!     &           bind(C, NAME='c_link_atensor_dir_list_to_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
!
      module component_names_to_c
!
      use m_precision
      use ISO_C_BINDING
      use t_base_field_labels
      use t_control_array_chara2int
!
      implicit none
!
      type(ctl_array_c2i), save, private, target :: xyz_dir_list
      type(ctl_array_c2i), save, private, target :: xyzw_dir_list
!
      type(ctl_array_c2i), save, private, target :: scl_dir_list
      type(ctl_array_c2i), save, private, target :: vec_dir_list
      type(ctl_array_c2i), save, private, target :: str_dir_list
      type(ctl_array_c2i), save, private, target :: ast_dir_list
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_primary_componnet_flag_f(input_flag_c) bind(c)
!
      use m_more_component_flags
!
      type(C_ptr), value :: input_flag_c
      character(len=kchara), pointer :: names(:)
!
      call c_f_pointer(input_flag_c,names,[1])
      call set_primary_componnet_flag(names(1))
!
      end subroutine set_primary_componnet_flag_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_xyz_dir_list_to_ctl()                 &
     &           bind(C, NAME='c_link_xyz_dir_list_to_ctl')
      use m_component_flags
!
      if(.not. allocated(xyz_dir_list%c1_tbl))                          &
     &    call set_xyz_direction_array(xyz_dir_list)
      c_link_xyz_dir_list_to_ctl = C_loc(xyz_dir_list)
!
      end function c_link_xyz_dir_list_to_ctl
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_xyzw_dir_list_to_ctl()                &
     &           bind(C, NAME='c_link_xyzw_dir_list_to_ctl')
      use m_component_flags
!
      if(.not. allocated(xyzw_dir_list%c1_tbl))                         &
     &    call set_xyzw_direction_array(xyzw_dir_list)
      c_link_xyzw_dir_list_to_ctl = C_loc(xyzw_dir_list)
!
      end function c_link_xyzw_dir_list_to_ctl
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_scalar_dir_list_to_ctl()              &
     &           bind(C, NAME='c_link_scalar_dir_list_to_ctl')
      use m_component_flags
!
      if(.not. allocated(scl_dir_list%c1_tbl))                          &
     &    call set_scalar_direction_array(scl_dir_list)
      c_link_scalar_dir_list_to_ctl = C_loc(scl_dir_list)
!
      end function c_link_scalar_dir_list_to_ctl
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_vector_dir_list_to_ctl()              &
     &           bind(C, NAME='c_link_vector_dir_list_to_ctl')
      use m_component_flags
!
      if(.not. allocated(vec_dir_list%c1_tbl))                          &
     &    call set_vector_direction_array(vec_dir_list)
      c_link_vector_dir_list_to_ctl = C_loc(vec_dir_list)
!
      end function c_link_vector_dir_list_to_ctl
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_stensor_dir_list_to_ctl()             &
     &           bind(C, NAME='c_link_stensor_dir_list_to_ctl')
      use m_component_flags
!
      if(.not. allocated(str_dir_list%c1_tbl))                          &
     &    call set_sym_tensor_direction_array(str_dir_list)
      c_link_stensor_dir_list_to_ctl = C_loc(str_dir_list)
!
      end function c_link_stensor_dir_list_to_ctl
!
! ----------------------------------------------------------------------
!
      type(c_ptr) function c_link_atensor_dir_list_to_ctl()             &
     &           bind(C, NAME='c_link_atensor_dir_list_to_ctl')
      use m_component_flags
!
      if(.not. allocated(ast_dir_list%c1_tbl))                          &
     &    call set_asym_tensor_direction_array(ast_dir_list)
      c_link_atensor_dir_list_to_ctl = C_loc(ast_dir_list)
!
      end function c_link_atensor_dir_list_to_ctl
!
! ----------------------------------------------------------------------
!
      end module component_names_to_c
