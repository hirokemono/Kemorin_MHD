!>@file   m_MHD_field_group.f90
!!        module m_MHD_field_group
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      integer(c_int) function count_MHD_field_groups_f()              &
!!     &          bind(C, name = 'count_MHD_field_groups_f')
!!      integer(c_int) function count_MHD_sym_field_groups_f()          &
!!     &           bind(C, name = 'count_MHD_sym_field_groups_f')
!!      integer(c_int) function count_SGS_MHD_field_groups_f()          &
!!     &           bind(C, name = 'count_SGS_MHD_field_groups_f')
!!
!!      subroutine MHD_field_groups_f(nfld_group_c, field_group_c)      &
!!     &          bind(C, name = 'MHD_field_groups_f')
!!      subroutine MHD_sym_field_groups_f(nfld_group_c, field_group_c)  &
!!     &           bind(C, name = 'MHD_sym_field_groups_f')
!!      subroutine SGS_MHD_field_groups_f(nfld_group_c, field_group_c)  &
!!     &           bind(C, name = 'SGS_MHD_field_groups_f')
!!@endverbatim
!!
      module m_MHD_field_group
!
      use ISO_C_BINDING
!
      use m_precision
!
      use t_base_field_labels
      use t_base_force_labels
      use t_diffusion_term_labels
      use t_energy_flux_labels
      use t_grad_field_labels
      use t_field_product_labels
      use t_explicit_term_labels
      use t_diff_vector_labels
!
      use m_rot_force_labels
      use m_div_force_labels
!
      use m_field_w_symmetry_labels
      use m_force_w_sym_labels
      use m_energy_flux_w_sym_labels
!
      use m_field_w_symmetry_labels
      use m_force_w_sym_labels
      use m_energy_flux_w_sym_labels
!
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels
      use t_diff_vector_labels
!
      use m_diff_SGS_term_labels
      use m_filtered_field_labels
      use m_filtered_force_labels
      use m_filtered_ene_flux_labels
      use m_diff_filter_vect_labels
      use m_div_filtered_force_labels
      use m_rot_filtered_force_labels
      use m_grad_filter_field_labels
      use m_wide_SGS_term_labels
      use m_wide_filter_field_labels
      use m_dble_filter_field_labels
      use m_force_w_SGS_labels
      use m_true_SGS_term_labels

      implicit  none
! 
      integer(kind = kint), parameter :: ngrp_MHD_fields = 12
      character(len = kchara), parameter                                &
     &              :: MHD_field_group(ngrp_MHD_fields)                 &
     &                  = (/"Base_fields                 ",             &
     &                      "Forces                      ",             &
     &                      "Enegy_fluxes                ",             &
     &                      "Diffusion_terms             ",             &
     &                      "Rotation_of_forces          ",             &
     &                      "Divergence_of_forces        ",             &
     &                      "Products_of_fields          ",             &
     &                      "Gradient_of_scalar          ",             &
     &                      "Divergence_of_fields        ",             &
     &                      "Diffusivities               ",             &
     &                      "Work_area_for_explicit_terms",             &
     &                      "Field_Checks                "/)
!
      integer(kind = kint), parameter :: ngrp_MHD_sym_fields = 3
      character(len = kchara), parameter                                &
     &              :: MHD_sym_field_group(ngrp_MHD_sym_fields)         &
     &                  = (/"Fields_w_symmtery       ",                 &
     &                      "Forces_w_symmtery       ",                 &
     &                      "Energy_fluxes_w_symmtery"/)
!
      integer(kind = kint), parameter :: ngrp_SGS_MHD_fields = 19
      character(len = kchara), parameter                                &
     &              :: SGS_MHD_field_group(ngrp_SGS_MHD_fields)         &
     &                  = (/"SGS_terms                         ",       &
     &                      "Energy_fluxes_by_SGS_term         ",       &
     &                      "Differential_of_SGS_terms         ",       &
     &                      "Model_coefficients_of_SGS_term    ",       &
     &                      "Filtered_fields                   ",       &
     &                      "Filtered_force                    ",       &
     &                      "Energy_fluxes_by_filtered_force   ",       &
     &                      "Rotation_of_Filtered_force        ",       &
     &                      "Divergence_of_Filtered_force      ",       &
     &                      "Gradient_of_filtered_fields       ",       &
     &                      "Divergence_of_filtered_fields     ",       &
     &                      "Wide_filtered_fields              ",       &
     &                      "Double_filtered_fields            ",       &
     &                      "Diffreenciation_of_vector         ",       &
     &                      "Diffreenciation_of_filtered_vector",       &
     &                      "SGS_terms_by_wider_filtering      ",       &
     &                      "Forces_including_SGS_term         ",       &
     &                      "True_SGS_terms                    ",       &
     &                      "Work_area_for_SGS_model           "/)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(c_int) function count_MHD_field_groups_f()                &
     &          bind(C, name = 'count_MHD_field_groups_f')
!
      use t_grad_field_labels
!
!
      count_MHD_field_groups_f = ngrp_MHD_fields
!
      end function count_MHD_field_groups_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function count_MHD_sym_field_groups_f()            &
     &           bind(C, name = 'count_MHD_sym_field_groups_f')
!
      use t_grad_field_labels
!
!
      count_MHD_sym_field_groups_f = ngrp_MHD_sym_fields
!
      end function count_MHD_sym_field_groups_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function count_SGS_MHD_field_groups_f()            &
     &           bind(C, name = 'count_SGS_MHD_field_groups_f')
!
      use t_grad_field_labels
!
!
      count_SGS_MHD_field_groups_f = ngrp_SGS_MHD_fields
!
      end function count_SGS_MHD_field_groups_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine MHD_field_groups_f(nfld_group_c, field_group_c)        &
     &          bind(C, name = 'MHD_field_groups_f')
!
      use m_base_field_labels
      use m_base_force_labels
      use m_diffusion_term_labels
      use t_grad_field_labels
!
      integer(c_int), intent(inout) :: nfld_group_c(*)
      character(C_CHAR), intent(inout) :: field_group_c(*)
!
!
      call copy_filxed_lengh_chara                                      &
     &   (ngrp_MHD_fields, MHD_field_group, field_group_c)
!
!
      nfld_group_c( 1) = num_base_fields()
      nfld_group_c( 2) = num_base_forces()
      nfld_group_c( 3) = num_energy_fluxes()
      nfld_group_c( 4) = num_base_diffusions()
      nfld_group_c( 5) = num_rot_forces()
      nfld_group_c( 6) = num_div_forces()
      nfld_group_c( 7) = num_field_products()
      nfld_group_c( 8) = num_gradient_fields()
      nfld_group_c( 9) = num_divergence_fields()
      nfld_group_c(10) = num_base_diffusivities()
      nfld_group_c(11) = num_work_4_explicit()
      nfld_group_c(12) = num_check_fields()
!
      end subroutine MHD_field_groups_f
!
! ----------------------------------------------------------------------
!
      subroutine MHD_sym_field_groups_f(nfld_group_c, field_group_c)    &
     &           bind(C, name = 'MHD_sym_field_groups_f')
!
      use t_grad_field_labels
!
      integer(c_int), intent(inout) :: nfld_group_c(*)
      character(C_CHAR), intent(inout) :: field_group_c(*)
!
!
      call copy_filxed_lengh_chara                                      &
     &   (ngrp_MHD_sym_fields, MHD_sym_field_group, field_group_c)
!
      nfld_group_c( 1) = num_fields_w_symmetry()
      nfld_group_c( 2) = num_forces_w_symmetry()
      nfld_group_c( 3) = num_ene_fluxes_w_symmetry()
!
      end subroutine MHD_sym_field_groups_f
!
! ----------------------------------------------------------------------
!
      subroutine SGS_MHD_field_groups_f(nfld_group_c, field_group_c)    &
     &           bind(C, name = 'SGS_MHD_field_groups_f')
!
      use m_diff_vector_labels
      use t_grad_field_labels
!
      integer(c_int), intent(inout) :: nfld_group_c(*)
      character(C_CHAR), intent(inout) :: field_group_c(*)
!
!
      call copy_filxed_lengh_chara                                      &
     &   (ngrp_SGS_MHD_fields, SGS_MHD_field_group, field_group_c)
!
      nfld_group_c( 1) = num_SGS_terms()
      nfld_group_c( 2) = num_SGS_energy_fluxes()
      nfld_group_c( 3) = num_diff_SGS_terms()
      nfld_group_c( 4) = num_SGS_model_coefs()
      nfld_group_c( 5) = num_filter_fields()
      nfld_group_c( 6) = num_filtered_forces()
      nfld_group_c( 7) = num_filtered_ene_fluxes()
      nfld_group_c( 8) = num_rot_filtered_forces()
      nfld_group_c( 9) = num_div_filtered_forces()
      nfld_group_c(10) = num_grad_filter_fields()
      nfld_group_c(11) = num_div_filter_fields()
      nfld_group_c(12) = num_wide_filter_fields()
      nfld_group_c(13) = num_double_filter_fields()
      nfld_group_c(14) = num_difference_vector()
      nfld_group_c(15) = num_diff_filter_vector()
      nfld_group_c(16) = num_wide_SGS_terms()
      nfld_group_c(17) = num_force_w_SGS()
      nfld_group_c(18) = num_true_SGS_terms()
      nfld_group_c(19) = num_dynamic_SGS_work()
!
      end subroutine SGS_MHD_field_groups_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_filxed_lengh_chara(num, field_in, field_out)
!
      integer(kind = kint), intent(in) :: num
      character(len = kchara), intent(in) :: field_in(num)
      character(len = kchara), intent(inout) :: field_out(num)
!
      integer(kind = kint) :: i
!
      do i = 1, num
        write(field_out(i), '(a,a1)') trim(field_in(i)) // char(0)
      end do
!
      end subroutine copy_filxed_lengh_chara
!
! ----------------------------------------------------------------------
!
      end module m_MHD_field_group
