!>@file   set_field_address.f90
!!        module set_field_address
!!
!! @author H. Matsui
!! @date   Programmed on July, 2006
!! @n      Modified  on Jan., 2012
!!
!!
!> @brief Set start address for field data
!!
!!@verbatim
!!      subroutine set_field_addresses(istart_adress, num_field,        &
!!     &          field_name, num_component, iphys)
!!      integer(kind = kint), intent(in) :: istart_adress
!!      integer(kind = kint), intent(in) :: num_field
!!      integer(kind = kint), intent(in) :: num_component(num_field)
!!      character(len = kchara), intent(in) :: field_name(num_field)
!!      type(phys_address), intent(inout) :: iphys
!!@endverbatim
!!
!!@n @param istart_adress             start address for field data
!!@n @param num_field                 number of field
!!@n @param num_component(num_field)  number of components of field
!!@n @param field_name(num_field)     list of field names
!!@n @param iphys                     structure of field addresses
!
!
      module set_field_address
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_field_addresses(istart_adress, num_field,          &
     &          field_name, num_component, iphys)
!
      use m_phys_labels
      use t_base_field_labels
      use t_base_force_labels
      use t_diff_vector_labels
      use t_field_product_labels
      use t_diffusion_term_labels
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels
      use m_rot_force_labels
      use m_div_force_labels
      use m_filtered_field_labels
      use m_rot_filtered_force_labels
      use m_div_filtered_force_labels
      use m_diff_filter_vect_labels
      use m_wide_SGS_term_labels
      use m_true_SGS_term_labels
!
      integer(kind = kint), intent(in) :: istart_adress
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: num_component(num_field)
      character(len = kchara), intent(in) :: field_name(num_field)
!
      type(phys_address), intent(inout) :: iphys
!
      integer(kind = kint) :: i, i0
      logical :: flag
!
!
      i0 = istart_adress
      do i = 1, num_field
        call set_base_vector_addresses                                  &
     &     (i0, field_name(i), iphys%base, flag)
        call set_base_scalar_addresses                                  &
     &     (i0, field_name(i), iphys%base, flag)
!
        if ( field_name(i) .eq. div_filtered_velo%name ) then
          iphys%i_div_filter_v =    i0
        else if ( field_name(i) .eq. div_filtered_magne%name ) then
          iphys%i_div_filter_b =    i0
        else if ( field_name(i) .eq. div_filtered_vector_potential%name ) then
          iphys%i_div_filter_a =    i0
        end if
!
        call set_base_force_addresses                                   &
     &     (i0, field_name(i), iphys%forces, flag)
        call set_enegy_fluxes_addresses                                 &
     &     (i0, field_name(i), iphys%ene_flux, flag)
        call set_div_force_addresses                                    &
     &     (i0, field_name(i), iphys%div_forces, flag)
        call set_base_diffusion_addresses                               &
     &     (i0, field_name(i), iphys%diffusion, flag)
        call set_field_product_addresses                                &
     &     (i0, field_name(i), iphys%prod_fld, flag)
!
        call set_filter_field_addresses                                 &
     &     (i0, field_name(i), iphys%filter_fld, flag)
        call set_SGS_term_addresses                                     &
     &     (i0, field_name(i), iphys%SGS_term, flag)
        call set_div_SGS_term_addresses                                 &
     &     (i0, field_name(i), iphys%div_SGS, flag)
        call set_rot_SGS_term_addresses                                 &
     &     (i0, field_name(i), iphys%rot_SGS, flag)
!
        call set_wide_SGS_term_addresses                                &
     &     (i0, field_name(i), iphys%wide_SGS, flag)
        call set_double_SGS_term_addresses                              &
     &     (i0, field_name(i), iphys%dble_SGS, flag)
!
        call set_SGS_model_coef_addresses                               &
     &     (i0, field_name(i), iphys%Csim, flag)
!
        call set_SGS_ene_flux_addresses                                 &
     &     (i0, field_name(i), iphys%SGS_ene_flux, flag)
!
        call set_force_w_SGS_addresses                                  &
     &     (i0, field_name(i), iphys%frc_w_SGS, flag)
!
        if ( field_name(i) .eq. geostrophic_balance%name ) then
          iphys%prod_fld%i_geostrophic =  i0
        end if
!
        call set_true_SGS_term_addresses                                &
     &     (i0, field_name(i), iphys%true_SGS, flag)
        call set_true_div_SGS_term_addresses                            &
     &     (i0, field_name(i), iphys%true_div_SGS, flag)
        call set_true_SGS_ene_flux_addresses                            &
     &     (i0, field_name(i), iphys%true_SGS_eflux, flag)
!
        call set_work_4_poisson_addresses                               &
     &     (i0, field_name(i), iphys%ene_flux, flag)
!
        call set_gradient_field_addresses                               &
     &     (i0, field_name(i), iphys%grad_fld, flag)
        call set_diff_vector_addresses                                  &
     &     (i0, field_name(i), iphys%diff_vector, flag)
!
        call set_grad_filter_field_addresses                            &
     &     (i0, field_name(i), iphys%grad_fil_fld, flag)
        call set_diff_filter_vect_addresses                             &
     &     (i0, field_name(i), iphys%diff_fil_vect, flag)
!
        call set_dynamic_SGS_work_addresses                             &
     &     (i0, field_name(i), iphys%SGS_wk, flag)
!
        call set_wide_fil_vector_addresses                              &
     &     (i0, field_name(i), iphys%wide_filter_fld, flag)
        call set_wide_fil_scaler_addresses                              &
     &     (i0, field_name(i), iphys%wide_filter_fld, flag)
        call set_wide_fil_grad_addresses                                &
     &     (i0, field_name(i), iphys%wide_filter_grad, flag)
!
        call set_dble_fil_vector_addresses                              &
     &     (i0, field_name(i), iphys%dbl_filter_fld, flag)
        call set_dble_fil_scaler_addresses                              &
     &     (i0, field_name(i), iphys%dbl_filter_fld, flag)
        call set_dble_fil_grad_addresses                                &
     &     (i0, field_name(i), iphys%dbl_filter_grad, flag)
!
        if ( field_name(i) .eq. sum_forces%name ) then
          iphys%i_forces =     i0
        else if ( field_name(i) .eq. rot_sum_forces%name ) then
          iphys%i_rot_forces = i0
        else if ( field_name(i) .eq. div_sum_forces%name ) then
          iphys%i_div_forces = i0
        end if
!
        call set_rot_force_addresses(i0, field_name(i),                 &
     &      iphys%rot_forces, flag)
!
        call set_filtered_force_addresses(i0, field_name(i),            &
     &      iphys%rot_frc_by_filter, flag)
        call set_rot_fil_force_addresses(i0, field_name(i),             &
     &      iphys%rot_frc_by_filter, flag)
        call set_div_fil_force_addresses(i0, field_name(i),             &
     &      iphys%div_frc_by_filter, flag)
        call set_filter_ene_flux_addresses(i0, field_name(i),           &
     &      iphys%eflux_by_filter, flag)
!
        if ( field_name(i) .eq. previous_momentum%name ) then
          iphys%i_pre_mom =      i0
        else if ( field_name(i) .eq. previous_induction%name ) then
          iphys%i_pre_uxb =      i0
        else if ( field_name(i) .eq. previous_heat%name ) then
          iphys%i_pre_heat =     i0
        else if ( field_name(i) .eq. previous_composition%name ) then
          iphys%i_pre_composit = i0
        else if ( field_name(i) .eq. previous_pressure%name ) then
          iphys%i_pre_press =    i0
!
        else if ( field_name(i) .eq. check_momentum%name ) then
          iphys%i_chk_mom =       i0
        else if ( field_name(i) .eq. check_induction%name ) then
          iphys%i_chk_uxb =       i0
        else if ( field_name(i) .eq. check_heat%name ) then
          iphys%i_chk_heat =      i0
        else if ( field_name(i) .eq. check_composition%name ) then
          iphys%i_chk_composit =  i0
        else if ( field_name(i) .eq. check_pressure%name ) then
          iphys%i_chk_press =     i0
        else if ( field_name(i) .eq. check_potential%name ) then
          iphys%i_chk_potential = i0
!
        else if ( field_name(i) .eq. check_momentum_2%name ) then
          iphys%check_fld2%i_pre_mom =       i0
        else if ( field_name(i) .eq. check_induction_2%name ) then
          iphys%check_fld2%i_pre_uxb =       i0
        else if ( field_name(i) .eq. check_heat_2%name ) then
          iphys%check_fld2%i_pre_heat =      i0
        else if ( field_name(i) .eq. check_composition_2%name ) then
          iphys%check_fld2%i_pre_composit =  i0
        else if ( field_name(i) .eq. check_pressure_2%name ) then
          iphys%check_fld2%i_pre_press =     i0
        else if ( field_name(i) .eq. check_potential_2%name ) then
          iphys%check_fld2%i_pre_phi = i0
        end if
!
!   Old field label... Should be deleted later!!
        if (field_name(i) .eq. buoyancy_work%name) then
          iphys%ene_flux%i_buo_gen =   i0
        end if
!
        i0 = i0 + num_component(i)
      end do
!
      end subroutine set_field_addresses
!
!  --------------------------------------------------------------------
!
      end module set_field_address
