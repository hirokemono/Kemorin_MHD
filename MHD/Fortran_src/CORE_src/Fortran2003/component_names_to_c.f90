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
!!      integer(c_int) function num_flag_scalar_comp_f() bind(c)
!!      subroutine set_flag_scalar_comp_f                               &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_flag_vector_comp_f() bind(c)
!!      subroutine set_flag_vector_comp_f                               &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_flag_sym_tensor_comp_f() bind(c)
!!      subroutine set_flag_sym_tensor_comp_f                           &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_flag_asym_tensor_comp_f() bind(c)
!!      subroutine set_flag_asym_tensor_comp_f                          &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      subroutine set_primary_componnet_flag_f(input_flag_c) bind(c)
!!        character(C_CHAR), intent(inout) :: input_flag(*)
!!
!!      type(c_ptr) function c_link_xyz_dir_list_to_ctl(c_ctl)          &
!!     &           bind(C, NAME='c_link_xyz_dir_list_to_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
!
      module component_names_to_c
!
      use m_precision
      use ISO_C_BINDING
      use t_base_field_labels
      use t_control_array_character
!
      implicit none
!
      type(ctl_array_chara), save, private, target :: xyz_dir_list
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_flag_scalar_comp_f() bind(c)
!
      use m_component_flags
!
      num_flag_scalar_comp_f = num_flag_scalar_comp()
      return
      end function num_flag_scalar_comp_f
!
! ----------------------------------------------------------------------
!
      subroutine set_flag_scalar_comp_f                                 &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_component_flags
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_flag_scalar_comp()])
      call c_f_pointer(field_math_c, math,  [num_flag_scalar_comp()])
      call set_flag_scalar_comp(n_comps_c(1), field, math)
!
      end subroutine set_flag_scalar_comp_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_flag_vector_comp_f() bind(c)
!
      use m_component_flags
!
      num_flag_vector_comp_f = num_flag_vector_comp()
      return
      end function num_flag_vector_comp_f
!
! ----------------------------------------------------------------------
!
      subroutine set_flag_vector_comp_f                                 &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_component_flags
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_flag_vector_comp()])
      call c_f_pointer(field_math_c, math,  [num_flag_vector_comp()])
      call set_flag_vector_comp(n_comps_c(1), field, math)
!
      end subroutine set_flag_vector_comp_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_flag_sym_tensor_comp_f() bind(c)
!
      use m_component_flags
!
      num_flag_sym_tensor_comp_f = num_flag_sym_tensor_comp()
      return
      end function num_flag_sym_tensor_comp_f
!
! ----------------------------------------------------------------------
!
      subroutine set_flag_sym_tensor_comp_f                             &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_component_flags
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field,                             &
     &                 [num_flag_sym_tensor_comp()])
      call c_f_pointer(field_math_c, math,                              &
     &                 [num_flag_sym_tensor_comp()])
      call set_flag_sym_tensor_comp(n_comps_c(1), field, math)
!
      end subroutine set_flag_sym_tensor_comp_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_flag_asym_tensor_comp_f() bind(c)
!
      use m_component_flags
!
      num_flag_asym_tensor_comp_f = num_flag_asym_tensor_comp()
      return
      end function num_flag_asym_tensor_comp_f
!
! ----------------------------------------------------------------------
!
      subroutine set_flag_asym_tensor_comp_f                            &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_component_flags
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field,                             &
     &                 [num_flag_asym_tensor_comp_f()])
      call c_f_pointer(field_math_c, math,                              &
     &                 [num_flag_asym_tensor_comp_f()])
      call set_flag_asym_tensor_comp(n_comps_c(1), field, math)
!
      end subroutine set_flag_asym_tensor_comp_f
!
! ----------------------------------------------------------------------
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
      type(c_ptr) function c_link_xyz_dir_list_to_ctl(c_ctl)            &
     &           bind(C, NAME='c_link_xyz_dir_list_to_ctl')
      use m_component_flags
      type(c_ptr), value, intent(in) :: c_ctl
!
      call set_xyz_direction_array(xyz_dir_list)
      c_link_xyz_dir_list_to_ctl = C_loc(xyz_dir_list)
!
      end function c_link_xyz_dir_list_to_ctl
!
! ----------------------------------------------------------------------
!
      end module component_names_to_c
