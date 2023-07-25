!>@file   field_names_to_c.f90
!!        module field_names_to_c
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Routines to tell field names into C programs
!!
!!@verbatim
!!      integer(c_int) function lengthchara_f() bind(c)
!!
!!      subroutine set_base_field_names_f                               &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!      type(C_ptr) function c_link_base_field_names_to_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_link_base_field_names_to_ctl')
!!      type(C_ptr) function c_link_time_evo_list_to_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_link_time_evo_list_to_ctl')
!!      type(c_ptr), value, intent(in) :: c_ctl
!!
!!      integer(c_int) function num_base_forces_f() bind(c)
!!      subroutine set_base_force_labels_f                              &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_rot_forces_f() bind(c)
!!      subroutine set_rot_force_labels_f                               &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_div_forces_f() bind(c)
!!      subroutine set_div_force_labels_f                               &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_energy_fluxes_f() bind(c)
!!      subroutine set_energy_flux_names_f                              &
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
!!      integer(c_int) function num_field_products_f() bind(c)
!!      subroutine set_field_product_labels_f                           &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_base_diffusions_f() bind(c)
!!      subroutine set_base_diffusion_labels_f                          &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_base_diffusivities_f() bind(c)
!!      subroutine set_base_diffusivity_labels_f                        &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_work_4_explicit_f() bind(c)
!!      subroutine set_work_4_explicit_labels_f                         &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_check_fields_f() bind(c)
!!      subroutine set_check_fields_labels_f                            &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!
!!      integer(c_int) function num_fieldss_w_symmetry_f() bind(c)
!!      subroutine set_fields_w_sym_labels_f                            &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_forces_w_symmetry_f() bind(c)
!!      subroutine set_forces_w_sym_labels_f                            &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_energy_fluxes_w_symmetry_f() bind(c)
!!      subroutine set_ene_flux_w_sym_labels_f                          &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_field_comp_list_f()                 &
!!     &          bind(c, name="num_field_comp_list_f")
!!      subroutine set_field_component_labels_f                         &
!!     &         (n_comps_c, field_name_c, field_math_c)                &
!!     &          bind(c, name="set_field_component_labels_f")
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
!!      integer(c_int) function num_filter_fields_f() bind(c)
!!      subroutine set_filter_field_labels_f                            &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_div_filter_fields_f() bind(c)
!!      subroutine set_div_filter_field_labels_f                        &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_grad_filter_fields_f() bind(c)
!!      subroutine set_grad_filter_field_labels_f                       &
!!     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!!
!!      integer(c_int) function num_filter_force_f() bind(c)
!!      subroutine set_filter_force_labels_f                            &
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
!!@endverbatim
!
      module field_names_to_c
!
      use m_precision
      use ISO_C_BINDING
      use t_base_field_labels
      use t_control_array_character
      use t_control_array_chara2int
!
      implicit none
!
      type(ctl_array_c2i), save, target :: base_fld_list
      type(ctl_array_chara), save, private, target :: tevo_fld_list
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(c_int) function lengthchara_f() bind(c)
!
      lengthchara_f = kchara
      return
      end function lengthchara_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_base_field_names_f                                 &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_base_field_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_base_fields()])
      call c_f_pointer(field_math_c, math, [num_base_fields()])
      call set_base_field_names(n_comps_c(1), field, math)
!
      end subroutine set_base_field_names_f
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_base_field_names_to_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_link_base_field_names_to_ctl')
      use m_base_field_labels
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(base_fld_list%c1_tbl))                         &
     &           call set_base_field_names_to_ctl(base_fld_list)
      c_link_base_field_names_to_ctl = C_loc(base_fld_list)
      end function c_link_base_field_names_to_ctl
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_time_evo_list_to_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_link_time_evo_list_to_ctl')
      use m_base_field_labels
      type(c_ptr), value, intent(in) :: c_ctl
!
      if(.not. allocated(tevo_fld_list%c_tbl))                          &
     &      call time_evolution_list_array(tevo_fld_list)
      c_link_time_evo_list_to_ctl = C_loc(tevo_fld_list)
      end function c_link_time_evo_list_to_ctl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_base_forces_f() bind(c)
!
      use m_base_force_labels
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
      use m_base_force_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_base_forces()])
      call c_f_pointer(field_math_c, math, [num_base_forces()])
      call set_base_force_labels(n_comps_c(1), field, math)
!
      end subroutine set_base_force_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_rot_forces_f() bind(c)
!
      use m_rot_force_labels
!
      num_rot_forces_f = num_rot_forces()
      return
      end function num_rot_forces_f
!
! ----------------------------------------------------------------------
!
      subroutine set_rot_force_labels_f                                 &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_rot_force_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_rot_forces()])
      call c_f_pointer(field_math_c, math, [num_rot_forces()])
      call set_rot_force_labels(n_comps_c(1), field, math)
!
      end subroutine set_rot_force_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_div_forces_f() bind(c)
!
      use m_div_force_labels
!
      num_div_forces_f = num_div_forces()
      return
      end function num_div_forces_f
!
! ----------------------------------------------------------------------
!
      subroutine set_div_force_labels_f                                 &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_div_force_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_div_forces()])
      call c_f_pointer(field_math_c, math, [num_div_forces()])
      call set_div_force_labels(n_comps_c(1), field, math)
!
      end subroutine set_div_force_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_energy_fluxes_f() bind(c)
!
      use m_energy_flux_labels
!
      num_energy_fluxes_f = num_energy_fluxes()
      return
      end function num_energy_fluxes_f
!
! ----------------------------------------------------------------------
!
      subroutine set_energy_flux_names_f                                &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_energy_flux_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_energy_fluxes()])
      call c_f_pointer(field_math_c, math, [num_energy_fluxes()])
      call set_energy_flux_names(n_comps_c(1), field, math)
!
      end subroutine set_energy_flux_names_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_divergence_fields_f() bind(c)
!
      use m_grad_field_labels
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
      use m_grad_field_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_divergence_fields()])
      call c_f_pointer(field_math_c, math, [num_divergence_fields()])
      call set_divergence_field_labels(n_comps_c(1), field, math)
!
      end subroutine set_divergence_field_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_gradient_fields_f() bind(c)
!
      use m_grad_field_labels
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
      use m_grad_field_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_gradient_fields()])
      call c_f_pointer(field_math_c, math, [num_gradient_fields()])
      call set_gradient_field_labels(n_comps_c(1), field, math)
!
      end subroutine set_gradient_field_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_field_products_f() bind(c)
!
      use m_field_product_labels
!
      num_field_products_f = num_field_products()
      return
      end function num_field_products_f
!
! ----------------------------------------------------------------------
!
      subroutine set_field_product_labels_f                             &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_field_product_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_field_products()])
      call c_f_pointer(field_math_c, math, [num_field_products()])
      call set_field_product_labels(n_comps_c(1), field, math)
!
      end subroutine set_field_product_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_base_diffusions_f() bind(c)
!
      use m_diffusion_term_labels
!
      num_base_diffusions_f = num_base_diffusions()
      return
      end function num_base_diffusions_f
!
! ----------------------------------------------------------------------
!
      subroutine set_base_diffusion_labels_f                            &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_diffusion_term_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_base_diffusions()])
      call c_f_pointer(field_math_c, math, [num_base_diffusions()])
      call set_base_diffusion_labels(n_comps_c(1), field, math)
!
      end subroutine set_base_diffusion_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_base_diffusivities_f() bind(c)
!
      use m_diffusion_term_labels
!
      num_base_diffusivities_f = num_base_diffusivities()
      return
      end function num_base_diffusivities_f
!
! ----------------------------------------------------------------------
!
      subroutine set_base_diffusivity_labels_f                          &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_diffusion_term_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_base_diffusivities()])
      call c_f_pointer(field_math_c, math, [num_base_diffusivities()])
      call set_base_diffusivity_labels(n_comps_c(1), field, math)
!
      end subroutine set_base_diffusivity_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_work_4_explicit_f() bind(c)
!
      use m_explicit_term_labels
!
      num_work_4_explicit_f = num_work_4_explicit()
      return
      end function num_work_4_explicit_f
!
! ----------------------------------------------------------------------
!
      subroutine set_work_4_explicit_labels_f                           &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_explicit_term_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_work_4_explicit()])
      call c_f_pointer(field_math_c, math, [num_work_4_explicit()])
      call set_work_4_explicit_labels(n_comps_c(1), field, math)
!
      end subroutine set_work_4_explicit_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_check_fields_f() bind(c)
!
      use m_explicit_term_labels
!
      num_check_fields_f = num_check_fields()
      return
      end function num_check_fields_f
!
! ----------------------------------------------------------------------
!
      subroutine set_check_fields_labels_f                              &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_explicit_term_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_check_fields()])
      call c_f_pointer(field_math_c, math, [num_check_fields()])
      call set_check_fields_labels(n_comps_c(1), field, math)
!
      end subroutine set_check_fields_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_fieldss_w_symmetry_f() bind(c)
!
      use m_field_w_symmetry_labels
!
      num_fieldss_w_symmetry_f = num_fields_w_symmetry()
      return
      end function num_fieldss_w_symmetry_f
!
! ----------------------------------------------------------------------
!
      subroutine set_fields_w_sym_labels_f                              &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_field_w_symmetry_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_fields_w_symmetry()])
      call c_f_pointer(field_math_c, math, [num_fields_w_symmetry()])
      call set_field_w_symmetry_labels(n_comps_c(1), field, math)
!
      end subroutine set_fields_w_sym_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_forces_w_symmetry_f() bind(c)
!
      use m_force_w_sym_labels
!
      num_forces_w_symmetry_f = num_forces_w_symmetry()
      return
      end function num_forces_w_symmetry_f
!
! ----------------------------------------------------------------------
!
      subroutine set_forces_w_sym_labels_f                              &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_force_w_sym_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_forces_w_symmetry()])
      call c_f_pointer(field_math_c, math, [num_forces_w_symmetry()])
      call set_force_w_symmetry_names(n_comps_c(1), field, math)
!
      end subroutine set_forces_w_sym_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_energy_fluxes_w_sym_org_f() bind(c)
!
      use m_energy_flux_w_sym_labels
!
      num_energy_fluxes_w_sym_org_f = num_ene_fluxes_w_symmetry()
      return
      end function num_energy_fluxes_w_sym_org_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ene_flux_w_sym_labels_org_f                        &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_energy_flux_w_sym_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field,                             &
     &                 [num_ene_fluxes_w_symmetry()])
      call c_f_pointer(field_math_c, math,                              &
     &                 [num_ene_fluxes_w_symmetry()])
      call set_ene_flux_w_symmetry_names(n_comps_c(1), field, math)
!
      end subroutine set_ene_flux_w_sym_labels_org_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_energy_fluxes_w_symmetry_f() bind(c)
!
      use m_sym_ene_flux_labels
!
      num_energy_fluxes_w_symmetry_f = num_sym_ene_fluxes()
      return
      end function num_energy_fluxes_w_symmetry_f
!
! ----------------------------------------------------------------------
!
      subroutine set_ene_flux_w_sym_labels_f                            &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_sym_ene_flux_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field,                             &
     &                 [num_sym_ene_fluxes()])
      call c_f_pointer(field_math_c, math,                              &
     &                 [num_sym_ene_fluxes()])
      call set_sym_ene_flux_labels(n_comps_c(1), field, math)
!
      end subroutine set_ene_flux_w_sym_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_field_comp_list_f()                   &
     &          bind(c, name="num_field_comp_list_f")
!
      use m_field_component_labels
!
      num_field_comp_list_f = num_field_comp_list()
      return
      end function num_field_comp_list_f
!
! ----------------------------------------------------------------------
!
      subroutine set_field_component_labels_f                           &
     &         (n_comps_c, field_name_c, field_math_c)                  &
     &          bind(c, name="set_field_component_labels_f")
!
      use m_field_component_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_field_comp_list()])
      call c_f_pointer(field_math_c, math, [num_field_comp_list()])
      call set_field_component_labels(n_comps_c(1), field, math)
!
      end subroutine set_field_component_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_SGS_terms_f()                         &
     &              bind(c, name="num_SGS_terms_f")
!
      use m_SGS_term_labels
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
      use m_SGS_term_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_SGS_terms()])
      call c_f_pointer(field_math_c, math, [num_SGS_terms()])
      call set_SGS_term_labels(n_comps_c(1), field, math)
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_diff_SGS_terms()])
      call c_f_pointer(field_math_c, math, [num_diff_SGS_terms()])
      call set_diff_SGS_term_labels(n_comps_c(1), field, math)
!
      end subroutine set_diff_SGS_term_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_SGS_energy_fluxes_f()                 &
     &              bind(c, name="num_SGS_energy_fluxes_f")
!
      use m_SGS_enegy_flux_labels
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
      use m_SGS_enegy_flux_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_SGS_energy_fluxes()])
      call c_f_pointer(field_math_c, math, [num_SGS_energy_fluxes()])
      call set_SGS_energy_flux_labels(n_comps_c(1), field, math)
!
      end subroutine set_SGS_energy_flux_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_SGS_model_coefs_f()                   &
     &              bind(c, name="num_SGS_model_coefs_f")
!
      use m_SGS_model_coef_labels
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
      use m_SGS_model_coef_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_SGS_model_coefs()])
      call c_f_pointer(field_math_c, math, [num_SGS_model_coefs()])
      call set_SGS_model_coefs_labels(n_comps_c(1), field, math)
!
      end subroutine set_SGS_model_coefs_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_dynamic_SGS_work_f()                  &
     &              bind(c, name="num_dynamic_SGS_work_f")
!
      use m_SGS_model_coef_labels
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
      use m_SGS_model_coef_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_dynamic_SGS_work()])
      call c_f_pointer(field_math_c, math, [num_dynamic_SGS_work()])
      call set_dynamic_SGS_work_labels(n_comps_c(1), field, math)
!
      end subroutine set_dynamic_SGS_work_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_filter_fields_f() bind(c)
!
      use m_filtered_field_labels
!
      num_filter_fields_f = num_filter_fields()
      return
      end function num_filter_fields_f
!
! ----------------------------------------------------------------------
!
      subroutine set_filter_field_labels_f                              &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_filtered_field_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_filter_fields()])
      call c_f_pointer(field_math_c, math, [num_filter_fields()])
      call set_filter_field_labels(n_comps_c(1), field, math)
!
      end subroutine set_filter_field_labels_f
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_div_filter_fields()])
      call c_f_pointer(field_math_c, math, [num_div_filter_fields()])
      call set_div_filter_field_labels(n_comps_c(1), field, math)
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_grad_filter_fields()])
      call c_f_pointer(field_math_c, math, [num_grad_filter_fields()])
      call set_grad_filter_field_labels(n_comps_c(1), field, math)
!
      end subroutine set_grad_filter_field_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_filter_force_f() bind(c)
!
      use m_filtered_force_labels
!
      num_filter_force_f = num_filter_force()
      return
      end function num_filter_force_f
!
! ----------------------------------------------------------------------
!
      subroutine set_filter_force_labels_f                              &
     &         (n_comps_c, field_name_c, field_math_c) bind(c)
!
      use m_filtered_force_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_filter_force()])
      call c_f_pointer(field_math_c, math, [num_filter_force()])
      call set_filter_force_labels(n_comps_c(1), field, math)
!
      end subroutine set_filter_force_labels_f
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field,[num_rot_filtered_forces()])
      call c_f_pointer(field_math_c, math, [num_rot_filtered_forces()])
      call set_rot_filtered_force_labels(n_comps_c(1), field, math)
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field,[num_div_filtered_forces()])
      call c_f_pointer(field_math_c, math, [num_div_filtered_forces()])
      call set_div_filtered_force_labels(n_comps_c(1), field, math)
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field,[num_filtered_ene_fluxes()])
      call c_f_pointer(field_math_c, math, [num_filtered_ene_fluxes()])
      call set_filtered_ene_flax_labels(n_comps_c(1), field, math)
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_wide_filter_fields()])
      call c_f_pointer(field_math_c, math, [num_wide_filter_fields()])
      call set_wide_filter_field_labels(n_comps_c(1), field, math)
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field,                             &
     &                 [num_double_filter_fields()])
      call c_f_pointer(field_math_c, math,                              &
     &                 [num_double_filter_fields()])
      call set_double_filter_field_labels(n_comps_c(1), field, math)
!
      end subroutine set_dbl_filter_field_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_difference_vector_f() bind(c)
!
      use m_diff_vector_labels
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
      use m_diff_vector_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_difference_vector()])
      call c_f_pointer(field_math_c, math, [num_difference_vector()])
      call set_differnce_vector_labels(n_comps_c(1), field, math)
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_diff_filter_vector()])
      call c_f_pointer(field_math_c, math, [num_diff_filter_vector()])
      call set_diff_filter_vect_labels(n_comps_c(1), field, math)
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_wide_SGS_terms()])
      call c_f_pointer(field_math_c, math, [num_wide_SGS_terms()])
      call set_wide_SGS_term_labels(n_comps_c(1), field, math)
!
      end subroutine set_wide_SGS_term_labels_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_force_w_SGS_f()                       &
     &          bind(c, name="num_force_w_SGS_f")
!
      use m_force_w_SGS_labels
!
      num_force_w_SGS_f = num_force_w_SGS()
      return
      end function num_force_w_SGS_f
!
! ----------------------------------------------------------------------
!
      subroutine set_force_with_SGS_labels_f                            &
     &         (n_comps_c, field_name_c, field_math_c)                  &
     &          bind(c, name="set_force_with_SGS_labels_f")
!
      use m_force_w_SGS_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_force_w_SGS()])
      call c_f_pointer(field_math_c, math, [num_force_w_SGS()])
      call set_force_with_SGS_labels(n_comps_c(1), field, math)
!
      end subroutine set_force_with_SGS_labels_f
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
      type(C_ptr), value :: field_name_c
      type(C_ptr), value :: field_math_c
!
      character(len=kchara), pointer :: field(:)
      character(len=kchara), pointer :: math(:)
!
      call c_f_pointer(field_name_c, field, [num_true_SGS_terms()])
      call c_f_pointer(field_math_c, math, [num_true_SGS_terms()])
      call set_true_SGS_term_labels(n_comps_c(1), field, math)
!
      end subroutine set_true_SGS_term_labels_f
!
! ----------------------------------------------------------------------
!
      end module field_names_to_c
