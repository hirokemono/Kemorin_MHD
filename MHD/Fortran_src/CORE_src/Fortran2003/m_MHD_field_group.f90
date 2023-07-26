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
      use m_grad_field_labels
      use m_energy_flux_labels
      use m_field_product_labels
      use m_explicit_term_labels
      use field_names_to_c
!
      integer(c_int), intent(inout) :: nfld_group_c(*)
      type(C_ptr), value :: field_group_c
!
      type(C_ptr) :: base_fld_c
!
      character(len=kchara), pointer :: field_grp(:)
!
      call c_f_pointer(field_group_c, field_grp, [ngrp_MHD_fields])
      call copy_filxed_lengh_chara                                      &
     &   (ngrp_MHD_fields, MHD_field_group, field_grp)
!
!
      base_fld_c =  c_link_base_field_names(field_group_c)
      base_fld_c =  c_link_base_force_names(field_group_c)
      base_fld_c =  c_link_energy_flux_names(field_group_c)
      base_fld_c =  c_link_base_diffusion_names(field_group_c)
      base_fld_c =  c_link_rot_force_names(field_group_c)
      base_fld_c =  c_link_div_force_names(field_group_c)
      base_fld_c =  c_link_field_product_names(field_group_c)
      base_fld_c =  c_link_gradient_field_names(field_group_c)
      base_fld_c =  c_link_divergence_field_names(field_group_c)
      base_fld_c =  c_link_base_diffusivity_names(field_group_c)
      base_fld_c =  c_link_explicit_work_names(field_group_c)
      base_fld_c =  c_link_check_fields_names(field_group_c)
      base_fld_c =  c_link_differnce_vector_names(field_group_c)
      nfld_group_c( 1) = base_fld_list%num
      nfld_group_c( 2) = base_frc_list%num
      nfld_group_c( 3) = ene_flux_list%num
      nfld_group_c( 4) = diffusion_list%num
      nfld_group_c( 5) = rot_frc_list%num
      nfld_group_c( 6) = div_frc_list%num
      nfld_group_c( 7) = fld_prod_list%num
      nfld_group_c( 8) = grad_fld_list%num
      nfld_group_c( 9) = div_fld_list%num
      nfld_group_c(10) = diffusivity_list%num
      nfld_group_c(11) = exp_work_list%num
      nfld_group_c(12) = chk_fld_list%num
      nfld_group_c(13) = diff_vect_list%num
!
      end subroutine MHD_field_groups_f
!
! ----------------------------------------------------------------------
!
      subroutine MHD_sym_field_groups_f(nfld_group_c, field_group_c)    &
     &           bind(C, name = 'MHD_sym_field_groups_f')
!
      use t_grad_field_labels
      use field_names_to_c
!
      integer(c_int), intent(inout) :: nfld_group_c(*)
      type(C_ptr), value :: field_group_c
!
      type(C_ptr) :: base_fld_c
      character(len=kchara), pointer :: field_grp(:)
!
      call c_f_pointer(field_group_c, field_grp, [ngrp_MHD_sym_fields])
      call copy_filxed_lengh_chara                                      &
     &   (ngrp_MHD_sym_fields, MHD_sym_field_group, field_grp)
!
      base_fld_c =  c_link_field_w_symmetry_names(field_group_c)
      base_fld_c =  c_link_force_w_symmetry_names(field_group_c)
      base_fld_c =  c_link_sym_ene_flux_names(field_group_c)
      nfld_group_c( 1) = sym_fld_list%num
      nfld_group_c( 2) = sym_frc_list%num
      nfld_group_c( 3) = sym_flux_list%num
!
      end subroutine MHD_sym_field_groups_f
!
! ----------------------------------------------------------------------
!
      subroutine SGS_MHD_field_groups_f(nfld_group_c, field_group_c)    &
     &           bind(C, name = 'SGS_MHD_field_groups_f')
!
      use m_diff_vector_labels
      use m_SGS_term_labels
      use m_SGS_model_coef_labels
      use m_SGS_enegy_flux_labels
      use field_names_to_c
!
      integer(c_int), intent(inout) :: nfld_group_c(*)
      type(C_ptr), value :: field_group_c
!
      type(C_ptr) :: base_fld_c
      character(len=kchara), pointer :: field_grp(:)
!
      call c_f_pointer(field_group_c, field_grp, [ngrp_SGS_MHD_fields])
      call copy_filxed_lengh_chara                                      &
     &   (ngrp_SGS_MHD_fields, SGS_MHD_field_group, field_grp)
!
      base_fld_c =  c_link_SGS_term_names(field_group_c)
      base_fld_c =  c_link_SGS_energy_flux_names(field_group_c)
      base_fld_c =  c_link_diff_SGS_term_names(field_group_c)
      base_fld_c =  c_link_SGS_model_coefs_names(field_group_c)
      base_fld_c =  c_link_filter_field_names(field_group_c)
      base_fld_c =  c_link_filter_force_names(field_group_c)
      base_fld_c =  c_link_filter_eflux_names(field_group_c)
      base_fld_c =  c_link_rot_filter_force_names(field_group_c)
      base_fld_c =  c_link_div_filter_force_names(field_group_c)
      base_fld_c =  c_link_grad_filter_field_names(field_group_c)
      base_fld_c =  c_link_div_filter_field_names(field_group_c)
      base_fld_c =  c_link_wide_filter_field_names(field_group_c)
      base_fld_c =  c_link_dbl_filter_field_names(field_group_c)
      base_fld_c =  c_link_diff_filter_vect_names(field_group_c)
      base_fld_c =  c_link_wide_SGS_term_names(field_group_c)
      base_fld_c =  c_link_force_with_SGS_names(field_group_c)
      base_fld_c =  c_link_true_SGS_term_names(field_group_c)
      base_fld_c =  c_link_dynamic_SGS_work_names(field_group_c)
      nfld_group_c( 1) = SGS_term_list%num
      nfld_group_c( 2) = SGS_eflux_list%num
      nfld_group_c( 3) = diff_SGS_list%num
      nfld_group_c( 4) = SGS_Csim_list%num
      nfld_group_c( 5) = filter_fld_list%num
      nfld_group_c( 6) = filter_frc_list%num
      nfld_group_c( 7) = fil_eflux_list%num
      nfld_group_c( 8) = fil_rot_frc_list%num
      nfld_group_c( 9) = fil_div_frc_list%num
      nfld_group_c(10) = fil_grad_fld_list%num
      nfld_group_c(11) = fil_div_fld_list%num
      nfld_group_c(12) = w_fil_fld_list%num
      nfld_group_c(13) = d_fil_fld_list%num
      nfld_group_c(14) = diff_fil_vect_list%num
      nfld_group_c(15) = wide_SGS_term_list%num
      nfld_group_c(16) = force_w_SGS_list%num
      nfld_group_c(17) = true_SGS_list%num
      nfld_group_c(18) = dSGS_work_list%num
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
