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
!!      type(C_ptr) function c_link_base_field_names()                  &
!!     &          bind(C, NAME = 'c_link_base_field_names')
!!      type(C_ptr) function c_link_time_evo_list_to_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_link_time_evo_list_to_ctl')
!!
!!      type(C_ptr) function c_link_base_force_names()                  &
!!     &          bind(C, NAME = 'c_link_base_force_names')
!!      type(C_ptr) function c_link_rot_force_names()                   &
!!     &          bind(C, NAME = 'c_link_rot_force_names')
!!      type(C_ptr) function c_link_div_force_names()                   &
!!     &          bind(C, NAME = 'c_link_div_force_names')
!!
!!      type(C_ptr) function c_link_energy_flux_names()                 &
!!     &          bind(C, NAME = 'c_link_energy_flux_names')
!!  !!
!!      type(C_ptr) function c_link_divergence_field_names()            &
!!     &          bind(C, NAME = 'c_link_divergence_field_names')
!!      type(C_ptr) function c_link_gradient_field_names()              &
!!     &          bind(C, NAME = 'c_link_gradient_field_names')
!!      type(C_ptr) function c_link_field_product_names()               &
!!     &          bind(C, NAME = 'c_link_field_product_names')
!!      type(C_ptr) function c_link_base_diffusion_names()              &
!!     &          bind(C, NAME = 'c_link_base_diffusion_names')
!!      type(C_ptr) function c_link_base_diffusivity_names()            &
!!     &          bind(C, NAME = 'c_link_base_diffusivity_names')
!!
!!      type(C_ptr) function c_link_explicit_work_names()               &
!!     &          bind(C, NAME = 'c_link_explicit_work_names')
!!      type(C_ptr) function c_link_check_fields_names()                &
!!     &          bind(C, NAME = 'c_link_check_fields_names')
!!      type(C_ptr) function c_link_differnce_vector_names()            &
!!     &          bind(C, NAME = 'c_link_differnce_vector_names')
!!      type(C_ptr) function c_link_field_component_names()             &
!!     &          bind(C, NAME = 'c_link_field_component_names')
!!
!!      type(C_ptr) function c_link_field_w_symmetry_names()            &
!!     &          bind(C, NAME = 'c_link_field_w_symmetry_names')
!!      type(C_ptr) function c_link_force_w_symmetry_names()            &
!!     &          bind(C, NAME = 'c_link_force_w_symmetry_names')
!!      type(C_ptr) function c_link_sym_ene_flux_names()                &
!!     &          bind(C, NAME = 'c_link_sym_ene_flux_names')
!!      type(C_ptr) function c_link_sym_ene_flux_org_names()            &
!!     &          bind(C, NAME = 'c_link_sym_ene_flux_org_names')
!!
!!      type(C_ptr) function c_link_SGS_term_names()                    &
!!     &          bind(C, NAME = 'c_link_SGS_term_names')
!!      type(C_ptr) function c_link_SGS_energy_flux_names()             &
!!     &          bind(C, NAME = 'c_link_SGS_energy_flux_names')
!!      type(C_ptr) function c_link_diff_SGS_term_names()               &
!!     &          bind(C, NAME = 'c_link_diff_SGS_term_names')
!!      type(C_ptr) function c_link_SGS_model_coefs_names()             &
!!     &          bind(C, NAME = 'c_link_SGS_model_coefs_names')
!!      type(C_ptr) function c_link_dynamic_SGS_work_names()            &
!!     &          bind(C, NAME = 'c_link_dynamic_SGS_work_names')
!!      type(C_ptr) function c_link_filter_field_names()                &
!!     &          bind(C, NAME = 'c_link_filter_field_names')
!!
!!      type(C_ptr) function c_link_div_filter_field_names()            &
!!     &          bind(C, NAME = 'c_link_div_filter_field_names')
!!      type(C_ptr) function c_link_grad_filter_field_names()           &
!!     &          bind(C, NAME = 'c_link_grad_filter_field_names')
!!
!!      type(C_ptr) function c_link_filter_force_names()                &
!!     &          bind(C, NAME = 'c_link_filter_force_names')
!!      type(C_ptr) function c_link_rot_filter_force_names()            &
!!     &          bind(C, NAME = 'c_link_rot_filter_force_names')
!!      type(C_ptr) function c_link_div_filter_force_names()            &
!!     &          bind(C, NAME = 'c_link_div_filter_force_names')
!!      type(C_ptr) function c_link_filter_eflux_names()                &
!!     &          bind(C, NAME = 'c_link_filter_eflux_names')
!!      type(C_ptr) function c_link_diff_filter_vect_names()            &
!!     &          bind(C, NAME = 'c_link_diff_filter_vect_names')
!!      type(C_ptr) function c_link_wide_SGS_term_names()               &
!!     &          bind(C, NAME = 'c_link_wide_SGS_term_names')
!!      type(C_ptr) function c_link_force_with_SGS_names()              &
!!     &          bind(C, NAME = 'c_link_force_with_SGS_names')
!!      type(C_ptr) function c_link_true_SGS_term_names()               &
!!     &          bind(C, NAME = 'c_link_true_SGS_term_names')
!!
!!      type(C_ptr) function c_link_wide_filter_field_names()           &
!!     &          bind(C, NAME = 'c_link_wide_filter_field_names')
!!      type(C_ptr) function c_link_dbl_filter_field_names()            &
!!     &          bind(C, NAME = 'c_link_dbl_filter_field_names')
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
      type(ctl_array_c2i), save, private, target :: base_fld_list
      type(ctl_array_c2i), save, private, target :: grad_fld_list
      type(ctl_array_c2i), save, private, target :: div_fld_list
      type(ctl_array_c2i), save, private, target :: base_frc_list
      type(ctl_array_c2i), save, private, target :: rot_frc_list
      type(ctl_array_c2i), save, private, target :: div_frc_list
      type(ctl_array_c2i), save, private, target :: ene_flux_list
      type(ctl_array_c2i), save, private, target :: fld_prod_list
      type(ctl_array_c2i), save, private, target :: diffusion_list
      type(ctl_array_c2i), save, private, target :: diffusivity_list
      type(ctl_array_c2i), save, private, target :: exp_work_list
      type(ctl_array_c2i), save, private, target :: chk_fld_list
      type(ctl_array_c2i), save, private, target :: fld_comp_list
      type(ctl_array_c2i), save, private, target :: diff_vect_list
!
      type(ctl_array_c2i), save, private, target :: sym_fld_list
      type(ctl_array_c2i), save, private, target :: sym_frc_list
      type(ctl_array_c2i), save, private, target :: sym_flux_list
      type(ctl_array_c2i), save, private, target :: sym_flux_org_list
!
      type(ctl_array_c2i), save, private, target :: SGS_term_list
      type(ctl_array_c2i), save, private, target :: SGS_eflux_list
      type(ctl_array_c2i), save, private, target :: diff_SGS_list
      type(ctl_array_c2i), save, private, target :: SGS_Csim_list
      type(ctl_array_c2i), save, private, target :: dSGS_work_list
      type(ctl_array_c2i), save, private, target :: filter_fld_list
      type(ctl_array_c2i), save, private, target :: fil_grad_fld_list
      type(ctl_array_c2i), save, private, target :: fil_div_fld_list
      type(ctl_array_c2i), save, private, target :: filter_frc_list
      type(ctl_array_c2i), save, private, target :: fil_rot_frc_list
      type(ctl_array_c2i), save, private, target :: fil_div_frc_list
      type(ctl_array_c2i), save, private, target :: fil_eflux_list
!
      type(ctl_array_c2i), save, private, target :: w_fil_fld_list
      type(ctl_array_c2i), save, private, target :: d_fil_fld_list
!
      type(ctl_array_c2i), save, private, target :: diff_fil_vect_list
      type(ctl_array_c2i), save, private, target :: wide_SGS_term_list
      type(ctl_array_c2i), save, private, target :: force_w_SGS_list
      type(ctl_array_c2i), save, private, target :: true_SGS_list
!
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
      type(C_ptr) function c_link_base_field_names()                    &
     &          bind(C, NAME = 'c_link_base_field_names')
      use m_base_field_labels
!
      if(.not. allocated(base_fld_list%c1_tbl))                         &
     &           call set_base_field_names(base_fld_list)
      c_link_base_field_names = C_loc(base_fld_list)
      end function c_link_base_field_names
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
!
      type(C_ptr) function c_link_base_force_names()                    &
     &          bind(C, NAME = 'c_link_base_force_names')
      use m_base_force_labels
!
      if(.not. allocated(base_frc_list%c1_tbl))                         &
     &           call set_base_force_names_to_ctl(base_frc_list)
      c_link_base_force_names = C_loc(base_frc_list)
      end function c_link_base_force_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_rot_force_names()                     &
     &          bind(C, NAME = 'c_link_rot_force_names')
      use m_rot_force_labels
!
      if(.not. allocated(rot_frc_list%c1_tbl))                          &
     &           call set_rot_force_names(rot_frc_list)
      c_link_rot_force_names = C_loc(rot_frc_list)
      end function c_link_rot_force_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_div_force_names()                     &
     &          bind(C, NAME = 'c_link_div_force_names')
      use m_div_force_labels
!
      if(.not. allocated(div_frc_list%c1_tbl))                          &
     &           call set_div_force_names(div_frc_list)
      c_link_div_force_names = C_loc(div_frc_list)
      end function c_link_div_force_names
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_energy_flux_names()                   &
     &          bind(C, NAME = 'c_link_energy_flux_names')
      use m_energy_flux_labels
!
      if(.not. allocated(ene_flux_list%c1_tbl))                         &
     &           call set_energy_flux_names(ene_flux_list)
      c_link_energy_flux_names = C_loc(ene_flux_list)
      end function c_link_energy_flux_names
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_divergence_field_names()              &
     &          bind(C, NAME = 'c_link_divergence_field_names')
      use m_grad_field_labels
!
      if(.not. allocated(div_fld_list%c1_tbl))                          &
     &           call set_divergence_field_names(div_fld_list)
      c_link_divergence_field_names = C_loc(div_fld_list)
      end function c_link_divergence_field_names
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_gradient_field_names()                &
     &          bind(C, NAME = 'c_link_gradient_field_names')
      use m_grad_field_labels
!
      if(.not. allocated(grad_fld_list%c1_tbl))                         &
     &           call set_gradient_field_names(grad_fld_list)
      c_link_gradient_field_names = C_loc(grad_fld_list)
      end function c_link_gradient_field_names
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_field_product_names()                 &
     &          bind(C, NAME = 'c_link_field_product_names')
      use m_field_product_labels
!
      if(.not. allocated(fld_prod_list%c1_tbl))                         &
     &           call set_field_product_names(fld_prod_list)
      c_link_field_product_names = C_loc(fld_prod_list)
      end function c_link_field_product_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_base_diffusion_names()                &
     &          bind(C, NAME = 'c_link_base_diffusion_names')
      use m_diffusion_term_labels
!
      if(.not. allocated(diffusion_list%c1_tbl))                        &
     &           call set_base_diffusion_names(diffusion_list)
      c_link_base_diffusion_names = C_loc(diffusion_list)
      end function c_link_base_diffusion_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_base_diffusivity_names()              &
     &          bind(C, NAME = 'c_link_base_diffusivity_names')
      use m_diffusion_term_labels
!
      if(.not. allocated(diffusivity_list%c1_tbl))                      &
     &           call set_base_diffusivity_names(diffusivity_list)
      c_link_base_diffusivity_names = C_loc(diffusivity_list)
      end function c_link_base_diffusivity_names
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_explicit_work_names()                 &
     &          bind(C, NAME = 'c_link_explicit_work_names')
      use m_explicit_term_labels
!
      if(.not. allocated(exp_work_list%c1_tbl))                         &
     &           call set_explicit_work_names(exp_work_list)
      c_link_explicit_work_names = C_loc(exp_work_list)
      end function c_link_explicit_work_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_check_fields_names()                  &
     &          bind(C, NAME = 'c_link_check_fields_names')
      use m_explicit_term_labels
!
      if(.not. allocated(chk_fld_list%c1_tbl))                          &
     &           call set_check_fields_names(chk_fld_list)
      c_link_check_fields_names = C_loc(chk_fld_list)
      end function c_link_check_fields_names
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_field_w_symmetry_names()              &
     &          bind(C, NAME = 'c_link_field_w_symmetry_names')
      use m_field_w_symmetry_labels
!
      if(.not. allocated(sym_fld_list%c1_tbl))                          &
     &           call set_field_w_symmetry_names(sym_fld_list)
      c_link_field_w_symmetry_names = C_loc(sym_fld_list)
      end function c_link_field_w_symmetry_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_force_w_symmetry_names()              &
     &          bind(C, NAME = 'c_link_force_w_symmetry_names')
      use m_force_w_sym_labels
!
      if(.not. allocated(sym_frc_list%c1_tbl))                          &
     &           call set_force_w_symmetry_names(sym_frc_list)
      c_link_force_w_symmetry_names = C_loc(sym_frc_list)
      end function c_link_force_w_symmetry_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_sym_ene_flux_names()                  &
     &          bind(C, NAME = 'c_link_sym_ene_flux_names')
      use m_sym_ene_flux_labels
!
      if(.not. allocated(sym_flux_list%c1_tbl))                         &
     &           call set_sym_ene_flux_names(sym_flux_list)
      c_link_sym_ene_flux_names = C_loc(sym_flux_list)
      end function c_link_sym_ene_flux_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_sym_ene_flux_org_names()              &
     &          bind(C, NAME = 'c_link_sym_ene_flux_org_names')
      use m_energy_flux_w_sym_labels
!
      if(.not. allocated(sym_flux_org_list%c1_tbl))                     &
     &           call set_ene_flux_w_symmetry_names(sym_flux_org_list)
      c_link_sym_ene_flux_org_names = C_loc(sym_flux_org_list)
      end function c_link_sym_ene_flux_org_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_field_component_names()               &
     &          bind(C, NAME = 'c_link_field_component_names')
      use m_field_component_labels
!
      if(.not. allocated(fld_comp_list%c1_tbl))                         &
     &           call set_field_component_names(fld_comp_list)
      c_link_field_component_names = C_loc(fld_comp_list)
      end function c_link_field_component_names
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_SGS_term_names()                      &
     &          bind(C, NAME = 'c_link_SGS_term_names')
      use m_SGS_term_labels
!
      if(.not. allocated(SGS_term_list%c1_tbl))                         &
     &           call set_SGS_term_names(SGS_term_list)
      c_link_SGS_term_names = C_loc(SGS_term_list)
      end function c_link_SGS_term_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_SGS_energy_flux_names()               &
     &          bind(C, NAME = 'c_link_SGS_energy_flux_names')
      use m_SGS_enegy_flux_labels
!
      if(.not. allocated(SGS_eflux_list%c1_tbl))                        &
     &           call set_SGS_energy_flux_names(SGS_eflux_list)
      c_link_SGS_energy_flux_names = C_loc(SGS_eflux_list)
      end function c_link_SGS_energy_flux_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_diff_SGS_term_names()                 &
     &          bind(C, NAME = 'c_link_diff_SGS_term_names')
      use m_diff_SGS_term_labels
!
      if(.not. allocated(diff_SGS_list%c1_tbl))                         &
     &           call set_diff_SGS_term_names(diff_SGS_list)
      c_link_diff_SGS_term_names = C_loc(diff_SGS_list)
      end function c_link_diff_SGS_term_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_SGS_model_coefs_names()               &
     &          bind(C, NAME = 'c_link_SGS_model_coefs_names')
      use m_SGS_model_coef_labels
!
      if(.not. allocated(SGS_Csim_list%c1_tbl))                         &
     &           call set_SGS_model_coefs_names(SGS_Csim_list)
      c_link_SGS_model_coefs_names = C_loc(SGS_Csim_list)
      end function c_link_SGS_model_coefs_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_dynamic_SGS_work_names()              &
     &          bind(C, NAME = 'c_link_dynamic_SGS_work_names')
      use m_SGS_model_coef_labels
!
      if(.not. allocated(dSGS_work_list%c1_tbl))                        &
     &           call set_dynamic_SGS_work_names(dSGS_work_list)
      c_link_dynamic_SGS_work_names = C_loc(dSGS_work_list)
      end function c_link_dynamic_SGS_work_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_filter_field_names()                  &
     &          bind(C, NAME = 'c_link_filter_field_names')
      use m_filtered_field_labels
!
      if(.not. allocated(filter_fld_list%c1_tbl))                       &
     &           call set_filter_field_names(filter_fld_list)
      c_link_filter_field_names = C_loc(filter_fld_list)
      end function c_link_filter_field_names
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_div_filter_field_names()              &
     &          bind(C, NAME = 'c_link_div_filter_field_names')
      use m_grad_filter_field_labels
!
      if(.not. allocated(fil_div_fld_list%c1_tbl))                      &
     &           call set_div_filtered_field_names(fil_div_fld_list)
      c_link_div_filter_field_names = C_loc(fil_div_fld_list)
      end function c_link_div_filter_field_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_grad_filter_field_names()             &
     &          bind(C, NAME = 'c_link_grad_filter_field_names')
      use m_grad_filter_field_labels
!
      if(.not. allocated(fil_grad_fld_list%c1_tbl))                     &
     &           call set_grad_filtered_field_names(fil_grad_fld_list)
      c_link_grad_filter_field_names = C_loc(fil_grad_fld_list)
      end function c_link_grad_filter_field_names
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_filter_force_names()                  &
     &          bind(C, NAME = 'c_link_filter_force_names')
      use m_filtered_force_labels
!
      if(.not. allocated(filter_frc_list%c1_tbl))                       &
     &           call set_filter_force_names(filter_frc_list)
      c_link_filter_force_names = C_loc(filter_frc_list)
      end function c_link_filter_force_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_rot_filter_force_names()              &
     &          bind(C, NAME = 'c_link_rot_filter_force_names')
      use m_rot_filtered_force_labels
!
      if(.not. allocated(fil_rot_frc_list%c1_tbl))                      &
     &           call set_rot_filtered_force_names(fil_rot_frc_list)
      c_link_rot_filter_force_names = C_loc(fil_rot_frc_list)
      end function c_link_rot_filter_force_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_div_filter_force_names()              &
     &          bind(C, NAME = 'c_link_div_filter_force_names')
      use m_rot_filtered_force_labels
!
      if(.not. allocated(fil_div_frc_list%c1_tbl))                      &
     &           call set_rot_filtered_force_names(fil_div_frc_list)
      c_link_div_filter_force_names = C_loc(fil_div_frc_list)
      end function c_link_div_filter_force_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_filter_eflux_names()                  &
     &          bind(C, NAME = 'c_link_filter_eflux_names')
      use m_filtered_ene_flux_labels
!
      if(.not. allocated(fil_eflux_list%c1_tbl))                        &
     &           call set_filtered_ene_flux_names(fil_eflux_list)
      c_link_filter_eflux_names = C_loc(fil_eflux_list)
      end function c_link_filter_eflux_names
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_wide_filter_field_names()             &
     &          bind(C, NAME = 'c_link_wide_filter_field_names')
      use m_wide_filter_field_labels
!
      if(.not. allocated(w_fil_fld_list%c1_tbl))                        &
     &           call set_wide_filter_field_names(w_fil_fld_list)
      c_link_wide_filter_field_names = C_loc(w_fil_fld_list)
      end function c_link_wide_filter_field_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_dbl_filter_field_names()              &
     &          bind(C, NAME = 'c_link_dbl_filter_field_names')
      use m_dble_filter_field_labels
!
      if(.not. allocated(d_fil_fld_list%c1_tbl))                        &
     &           call set_double_filter_field_names(d_fil_fld_list)
      c_link_dbl_filter_field_names = C_loc(d_fil_fld_list)
      end function c_link_dbl_filter_field_names
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_differnce_vector_names()              &
     &          bind(C, NAME = 'c_link_differnce_vector_names')
      use m_diff_vector_labels
!
      if(.not. allocated(diff_vect_list%c1_tbl))                        &
     &           call set_differnce_vector_names(diff_vect_list)
      c_link_differnce_vector_names = C_loc(diff_vect_list)
      end function c_link_differnce_vector_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_diff_filter_vect_names()              &
     &          bind(C, NAME = 'c_link_diff_filter_vect_names')
      use m_diff_filter_vect_labels
!
      if(.not. allocated(diff_fil_vect_list%c1_tbl))                    &
     &           call set_diff_filter_vect_names(diff_fil_vect_list)
      c_link_diff_filter_vect_names = C_loc(diff_fil_vect_list)
      end function c_link_diff_filter_vect_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_wide_SGS_term_names()                 &
     &          bind(C, NAME = 'c_link_wide_SGS_term_names')
      use m_wide_SGS_term_labels
!
      if(.not. allocated(wide_SGS_term_list%c1_tbl))                    &
     &           call set_wide_SGS_term_names(wide_SGS_term_list)
      c_link_wide_SGS_term_names = C_loc(wide_SGS_term_list)
      end function c_link_wide_SGS_term_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_force_with_SGS_names()                &
     &          bind(C, NAME = 'c_link_force_with_SGS_names')
      use m_force_w_SGS_labels
!
      if(.not. allocated(force_w_SGS_list%c1_tbl))                      &
     &           call set_force_with_SGS_names(force_w_SGS_list)
      c_link_force_with_SGS_names = C_loc(force_w_SGS_list)
      end function c_link_force_with_SGS_names
!
! ----------------------------------------------------------------------
!
      type(C_ptr) function c_link_true_SGS_term_names()                 &
     &          bind(C, NAME = 'c_link_true_SGS_term_names')
      use m_true_SGS_term_labels
!
      if(.not. allocated(true_SGS_list%c1_tbl))                         &
     &           call set_true_SGS_term_names(true_SGS_list)
      c_link_true_SGS_term_names = C_loc(true_SGS_list)
      end function c_link_true_SGS_term_names
!
! ----------------------------------------------------------------------
!
      end module field_names_to_c
