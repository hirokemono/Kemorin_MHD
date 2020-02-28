!
!
!!      integer(c_int) function num_divergence_fields_f() bind(c)
!!      subroutine set_divergence_field_labels_f                        &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_gradient_fields_f() bind(c)
!!      subroutine set_gradient_field_labels_f                          &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_filtered_forces_f() bind(c)
!!      subroutine set_filtered_force_labels_f                          &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_rot_filtered_forces_f() bind(c)
!!      subroutine rot_filtered_force_labels_f                          &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_div_filtered_forces_f() bind(c)
!!      subroutine div_filtered_force_labels_f                          &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_filtered_ene_fluxes_f() bind(c)
!!      subroutine set_filtered_ene_flax_labels_f                       &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_wide_filter_fields_f() bind(c)
!!      subroutine set_wide_filter_field_labels_f                       &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_double_filter_fields_f() bind(c)
!!      subroutine set_dbl_filter_field_labels_f                        &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_difference_vector_f() bind(c)
!!      subroutine set_differnce_vector_labels_f                        &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_diff_filter_vector_f() bind(c)
!!      subroutine set_diff_filter_vect_labels_f                        &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      module field_names_to_c
!
      use m_precision
      use iso_c_binding
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
      integer(c_int) function lengthchara_f()  &
     &       bind(c, name='lengthchara_f')
!
      lengthchara_f = kchara
      return
      end function lengthchara_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_base_fields_f() bind(c)
!
      num_base_fields_f = num_base_fields()
      return
      end function num_base_fields_f
!
! ----------------------------------------------------------------------
!
      subroutine set_base_field_names_f(field_name_c) bind(c)
!
      use t_base_field_labels
!
      character(C_CHAR), intent(inout) :: field_name_c(*)
!
      call set_base_field_names(field_name_c(1))
!
      end subroutine set_base_field_names_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_divergence_fields_f() bind(c)
!
      use t_grad_field_labels
!
      num_divergence_fields_f = num_divergence_fields()
      return
      end function num_divergence_fields_f
!
! ----------------------------------------------------------------------
!
      subroutine set_divergence_field_labels_f                          &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use t_grad_field_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_divergence_field_labels                                  &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_divergence_field_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_gradient_fields_f() bind(c)
!
      use t_grad_field_labels
!
      num_gradient_fields_f = num_gradient_fields()
      return
      end function num_gradient_fields_f
!
! ----------------------------------------------------------------------
!
      subroutine set_gradient_field_labels_f                            &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use t_grad_field_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_gradient_field_labels                                    &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_gradient_field_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_filtered_forces_f() bind(c)
!
      use m_filtered_force_labels
!
      num_filtered_forces_f = num_filtered_forces()
      return
      end function num_filtered_forces_f
!
! ----------------------------------------------------------------------
!
      subroutine set_filtered_force_labels_f                            &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_filtered_force_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_filtered_force_labels                                    &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_filtered_force_labels_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_rot_filtered_forces_f() bind(c)
!
      use m_rot_filtered_force_labels
!
      num_rot_filtered_forces_f = num_rot_filtered_forces()
      return
      end function num_rot_filtered_forces_f
!
! ----------------------------------------------------------------------
!
      subroutine rot_filtered_force_labels_f                            &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_rot_filtered_force_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_rot_filtered_force_labels                                &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine rot_filtered_force_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_div_filtered_forces_f() bind(c)
!
      use m_div_filtered_force_labels
!
      num_div_filtered_forces_f = num_div_filtered_forces()
      return
      end function num_div_filtered_forces_f
!
! ----------------------------------------------------------------------
!
      subroutine div_filtered_force_labels_f                            &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_div_filtered_force_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_div_filtered_force_labels                                &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine div_filtered_force_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_filtered_ene_fluxes_f() bind(c)
!
      use m_filtered_ene_flux_labels
!
      num_filtered_ene_fluxes_f = num_filtered_ene_fluxes()
      return
      end function num_filtered_ene_fluxes_f
!
! ----------------------------------------------------------------------
!
      subroutine set_filtered_ene_flax_labels_f                         &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_filtered_ene_flux_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_filtered_ene_flax_labels                                 &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_filtered_ene_flax_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_wide_filter_fields_f() bind(c)
!
      use m_wide_filter_field_labels
!
      num_wide_filter_fields_f = num_wide_filter_fields()
      return
      end function num_wide_filter_fields_f
!
! ----------------------------------------------------------------------
!
      subroutine set_wide_filter_field_labels_f                         &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_wide_filter_field_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_wide_filter_field_labels                                 &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_wide_filter_field_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_double_filter_fields_f() bind(c)
!
      use m_dble_filter_field_labels
!
      num_double_filter_fields_f = num_double_filter_fields()
      return
      end function num_double_filter_fields_f
!
! ----------------------------------------------------------------------
!
      subroutine set_dbl_filter_field_labels_f                          &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_dble_filter_field_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_double_filter_field_labels                               &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_dbl_filter_field_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_difference_vector_f() bind(c)
!
      use t_diff_vector_labels
!
      num_difference_vector_f = num_difference_vector()
      return
      end function num_difference_vector_f
!
! ----------------------------------------------------------------------
!
      subroutine set_differnce_vector_labels_f                          &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use t_diff_vector_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_differnce_vector_labels                                  &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_differnce_vector_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_diff_filter_vector_f() bind(c)
!
      use m_diff_filter_vect_labels
!
      num_diff_filter_vector_f = num_diff_filter_vector()
      return
      end function num_diff_filter_vector_f
!
! ----------------------------------------------------------------------
!
      subroutine set_diff_filter_vect_labels_f                          &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_diff_filter_vect_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_diff_filter_vect_labels                                  &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_diff_filter_vect_labels_f
!
! ----------------------------------------------------------------------
!
      end module field_names_to_c

