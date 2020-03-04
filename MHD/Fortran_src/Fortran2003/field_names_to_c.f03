!
!
!!      integer(c_int) function num_base_forces_f() bind(c)
!!      subroutine set_base_force_labels_f                              &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_divergence_fields_f() bind(c)
!!      subroutine set_divergence_field_labels_f                        &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_gradient_fields_f() bind(c)
!!      subroutine set_gradient_field_labels_f                          &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_SGS_terms_f()                       &
!!     &              bind(c, name="num_SGS_terms_f")
!!      subroutine set_SGS_term_labels_f                                &
!!     &         (n_comps_c, field_name_c, field_math_c)                &
!!     &          bind(c, name="set_SGS_term_labels_f")
!!
!!      integer(c_int) function num_diff_SGS_terms_f()                  &
!!     &              bind(c, name="num_diff_SGS_terms_f")
!!      subroutine set_diff_SGS_term_labels_f                           &
!!     &         (n_comps_c, field_name_c, field_math_c)                &
!!     &          bind(c, name="set_diff_SGS_term_labels_f")
!!
!!      integer(c_int) function num_SGS_energy_fluxes_f()               &
!!     &              bind(c, name="num_SGS_energy_fluxes_f")
!!      subroutine set_SGS_energy_flux_labels_f                         &
!!     &         (n_comps_c, field_name_c, field_math_c)                &
!!     &          bind(c, name="set_SGS_energy_flux_labels_f")
!!
!!      integer(c_int) function num_SGS_model_coefs_f()                 &
!!     &              bind(c, name="num_SGS_model_coefs_f")
!!      subroutine set_SGS_model_coefs_labels_f                         &
!!     &         (n_comps_c, field_name_c, field_math_c)                &
!!     &          bind(c, name="set_SGS_model_coefs_labels_f")
!!
!!      integer(c_int) function num_dynamic_SGS_work_f()                &
!!     &              bind(c, name="num_dynamic_SGS_work_f")
!!      subroutine set_dynamic_SGS_work_labels_f                        &
!!     &         (n_comps_c, field_name_c, field_math_c)                &
!!     &          bind(c, name="set_dynamic_SGS_work_labels_f")
!!
!!      integer(c_int) function num_div_filter_fields_f() bind(c)
!!      subroutine set_div_filter_field_labels_f                        &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_grad_filter_fields_f() bind(c)
!!      subroutine set_grad_filter_field_labels_f                       &
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
!!
!!      integer(c_int) function num_wide_SGS_terms_f()                  &
!!     &          bind(c, name="num_wide_SGS_terms_f")
!!      subroutine set_wide_SGS_term_labels_f                           &
!!     &         (n_comps_c, field_name_c, field_math_c)                &
!!     &          bind(c, name="set_wide_SGS_term_labels_f")
!!
!!      integer(c_int) function num_force_w_SGS_f()                     &
!!     &          bind(c, name="num_force_w_SGS_f")
!!      subroutine set_force_with_SGS_labels_f                          &
!!     &         (n_comps_c, field_name_c, field_math_c)                &
!!     &          bind(c, name="set_force_with_SGS_labels_f")
!!
!!      integer(c_int) function num_true_SGS_terms_f()                  &
!!     &          bind(c, name="num_true_SGS_terms_f")
!!      subroutine set_true_SGS_term_labels_f                           &
!!     &         (n_comps_c, field_name_c, field_math_c)                &
!!     &          bind(c, name="set_true_SGS_term_labels_f")
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
      integer(c_int) function num_base_forces_f() bind(c)
!
      use t_base_force_labels
!
      num_base_forces_f = num_base_forces()
      return
      end function num_base_forces_f
!
! ----------------------------------------------------------------------
!
      subroutine set_base_force_labels_f                                &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use t_base_force_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_base_force_labels                                        &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_base_force_labels_f
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
      integer(c_int) function num_SGS_terms_f()                         &
     &              bind(c, name="num_SGS_terms_f")
!
      use t_SGS_term_labels
!
      num_SGS_terms_f = num_SGS_terms()
      return
      end function num_SGS_terms_f
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_term_labels_f                                  &
     &         (n_comps_c, field_name_c, field_math_c)                  &
     &          bind(c, name="set_SGS_term_labels_f")
!
      use t_SGS_term_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_SGS_term_labels                                          &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_SGS_term_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_diff_SGS_terms_f()                    &
     &              bind(c, name="num_diff_SGS_terms_f")
!
      use m_diff_SGS_term_labels
!
      num_diff_SGS_terms_f = num_diff_SGS_terms()
      return
      end function num_diff_SGS_terms_f
!
! ----------------------------------------------------------------------
!
      subroutine set_diff_SGS_term_labels_f                             &
     &         (n_comps_c, field_name_c, field_math_c)                  &
     &          bind(c, name="set_diff_SGS_term_labels_f")
!
      use m_diff_SGS_term_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_diff_SGS_term_labels                                     &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_diff_SGS_term_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_SGS_energy_fluxes_f()                 &
     &              bind(c, name="num_SGS_energy_fluxes_f")
!
      use t_SGS_enegy_flux_labels
!
      num_SGS_energy_fluxes_f = num_SGS_energy_fluxes()
      return
      end function num_SGS_energy_fluxes_f
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_energy_flux_labels_f                           &
     &         (n_comps_c, field_name_c, field_math_c)                  &
     &          bind(c, name="set_SGS_energy_flux_labels_f")
!
      use t_SGS_enegy_flux_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_SGS_energy_flux_labels                                   &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_SGS_energy_flux_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_SGS_model_coefs_f()                   &
     &              bind(c, name="num_SGS_model_coefs_f")
!
      use t_SGS_model_coef_labels
!
      num_SGS_model_coefs_f = num_SGS_model_coefs()
      return
      end function num_SGS_model_coefs_f
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_model_coefs_labels_f                           &
     &         (n_comps_c, field_name_c, field_math_c)                  &
     &          bind(c, name="set_SGS_model_coefs_labels_f")
!
      use t_SGS_model_coef_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_SGS_model_coefs_labels                                   &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_SGS_model_coefs_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_dynamic_SGS_work_f()                  &
     &              bind(c, name="num_dynamic_SGS_work_f")
!
      use t_SGS_model_coef_labels
!
      num_dynamic_SGS_work_f = num_dynamic_SGS_work()
      return
      end function num_dynamic_SGS_work_f
!
! ----------------------------------------------------------------------
!
      subroutine set_dynamic_SGS_work_labels_f                          &
     &         (n_comps_c, field_name_c, field_math_c)                  &
     &          bind(c, name="set_dynamic_SGS_work_labels_f")
!
      use t_SGS_model_coef_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_dynamic_SGS_work_labels                                  &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_dynamic_SGS_work_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_div_filter_fields_f() bind(c)
!
      use m_grad_filter_field_labels
!
      num_div_filter_fields_f = num_div_filter_fields()
      return
      end function num_div_filter_fields_f
!
! ----------------------------------------------------------------------
!
      subroutine set_div_filter_field_labels_f                          &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_grad_filter_field_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_div_filter_field_labels                                  &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_div_filter_field_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_grad_filter_fields_f() bind(c)
!
      use m_grad_filter_field_labels
!
      num_grad_filter_fields_f = num_grad_filter_fields()
      return
      end function num_grad_filter_fields_f
!
! ----------------------------------------------------------------------
!
      subroutine set_grad_filter_field_labels_f                         &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_grad_filter_field_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_grad_filter_field_labels                                 &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_grad_filter_field_labels_f
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
! ----------------------------------------------------------------------
!
      integer(c_int) function num_wide_SGS_terms_f()                    &
     &          bind(c, name="num_wide_SGS_terms_f")
!
      use m_wide_SGS_term_labels
!
      num_wide_SGS_terms_f = num_wide_SGS_terms()
      return
      end function num_wide_SGS_terms_f
!
! ----------------------------------------------------------------------
!
      subroutine set_wide_SGS_term_labels_f                             &
     &         (n_comps_c, field_name_c, field_math_c)                  &
     &          bind(c, name="set_wide_SGS_term_labels_f")
!
      use m_wide_SGS_term_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_wide_SGS_term_labels                                     &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_wide_SGS_term_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_true_SGS_terms_f()                    &
     &          bind(c, name="num_true_SGS_terms_f")
!
      use m_true_SGS_term_labels
!
      num_true_SGS_terms_f = num_true_SGS_terms()
      return
      end function num_true_SGS_terms_f
!
! ----------------------------------------------------------------------
!
      subroutine set_true_SGS_term_labels_f                             &
     &         (n_comps_c, field_name_c, field_math_c)                  &
     &          bind(c, name="set_true_SGS_term_labels_f")
!
      use m_true_SGS_term_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_true_SGS_term_labels                                     &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine set_true_SGS_term_labels_f
!
! ----------------------------------------------------------------------
!
      end module field_names_to_c

