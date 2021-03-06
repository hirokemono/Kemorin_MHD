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
!!      subroutine set_primary_componnet_flag_f(input_flag) bind(c)
!!        character(C_CHAR), intent(inout) :: input_flag(*)
!!@endverbatim
!
      module component_names_to_c
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
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_flag_scalar_comp                                         &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
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
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_flag_vector_comp                                         &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
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
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_flag_sym_tensor_comp                                     &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
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
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_flag_asym_tensor_comp                                    &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_flag_asym_tensor_comp_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_primary_componnet_flag_f(input_flag) bind(c)
!
      use m_more_component_flags
!
      character(C_CHAR), intent(inout) :: input_flag(*)
!
      call set_primary_componnet_flag(input_flag(1))
!
      end subroutine set_primary_componnet_flag_f
!
! ----------------------------------------------------------------------
!
      end module component_names_to_c
