!>@file   set_mean_square_array.f90
!!        module set_mean_square_array
!!
!! @author H. Matsui
!! @date   Programmed in 2002
!! @n      Modified  on Jan., 2013
!!
!
!> @brief addresses for volume integrated data
!!
!!@verbatim
!!      subroutine set_mean_square_values                               &
!!     &         (nod_fld, i_rms, j_ave, ifld_msq)
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(inout) :: i_rms, j_ave
!!        type(mean_square_address), intent(inout) :: ifld_msq
!!        type(mean_square_values), intent(inout) :: fem_msq
!!@endverbatim
!
      module set_mean_square_array
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
      use t_mean_square_filed_list
      use t_mean_square_values
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_mean_square_values                                 &
     &         (nod_fld, iphys, i_rms, j_ave, ifld_msq, msq_list)
!
      use t_base_field_labels
      use t_energy_flux_labels
      use t_diffusion_term_labels
      use t_diff_vector_labels
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels
      use m_phys_constants
      use m_volume_average_labels
      use m_rot_force_labels
      use m_div_force_labels
      use m_diff_SGS_term_labels
      use m_true_SGS_term_labels
      use m_filtered_field_labels
      use m_filtered_force_labels
      use m_dble_filter_field_labels
      use m_filtered_ene_flux_labels
      use m_diff_filter_vect_labels
!
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys
!
      type(phys_address), intent(inout) :: i_rms, j_ave
      type(mean_square_address), intent(inout) :: ifld_msq
      type(mean_square_list), intent(inout) :: msq_list
!
      integer (kind = kint) :: i, num_comps
      character(len = kchara) :: field_name
!
!
      do i = 1, nod_fld%num_phys
        field_name = nod_fld%phys_name(i)
        num_comps =  nod_fld%num_component(i)
        if (nod_fld%iflag_monitor(i) .eq. 1) then
          if(check_filter_vector(field_name)) then
            if(field_name .eq. filter_velocity%name) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%i_filter_velo,           &
     &            i_rms%i_filter_velo, j_ave%i_filter_velo, msq_list)
!
              i_rms%i_div_filter_v = msq_list%numrms + 1
              j_ave%i_div_filter_v = msq_list%numave + 1
!
              ifld_msq%jr_amom_f = msq_list%numave + 2
!
              msq_list%numrms = msq_list%numrms + 1
              msq_list%numave = msq_list%numave + 4
            else if(field_name .eq. filter_vector_potential%name) then
              call set_rms_address                                      &
     &           (field_name, n_scalar, iphys%i_filter_vecp,            &
     &            i_rms%i_filter_vecp, j_ave%i_filter_vecp, msq_list)
!
              i_rms%i_div_filter_a = msq_list%numrms + 1
              j_ave%i_div_filter_a = msq_list%numave + 1
!
              msq_list%numrms = msq_list%numrms + 1
              msq_list%numave = msq_list%numave + 4
            else if(field_name .eq. filter_magne%name) then
              num_comps = num_comps + n_vector + n_scalar
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%i_filter_magne,          &
     &            i_rms%i_filter_magne, j_ave%i_filter_magne, msq_list)
!
              ifld_msq%ir_me_f_ic =  msq_list%numrms + 1
              ifld_msq%ja_mag_f_ic = msq_list%numave + 1
!
              i_rms%i_div_filter_b = msq_list%numrms + 2
              j_ave%i_div_filter_b = msq_list%numave + 4
!
              msq_list%numrms = msq_list%numrms + 2
              msq_list%numave = msq_list%numave + 6
            else
!            else if(field_name .eq. filter_vorticity%name) then
!            else if(field_name .eq. filter_current%name) then
              call set_rms_address_list(i, nod_fld, msq_list)
            end if
          end if
!
          if(check_base_vector(field_name)) then
            if(field_name .eq. velocity%name) then
              call set_rms_address(field_name, num_comps, iphys%i_velo, &
     &            i_rms%i_velo, j_ave%i_velo, msq_list)
!
              i_rms%grad_fld%i_div_v = msq_list%numrms + 1
              j_ave%grad_fld%i_div_v = msq_list%numave + 1
!
              ifld_msq%ja_amom = msq_list%numave + 2
!
              msq_list%numrms = msq_list%numrms + 1
              msq_list%numave = msq_list%numave + 4
!
            else if(field_name .eq. magnetic_field%name) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%i_magne,                 &
     &            i_rms%i_magne, j_ave%i_magne, msq_list)
!
              ifld_msq%ir_me_ic =  msq_list%numrms + 1
              ifld_msq%ja_mag_ic = msq_list%numave + 1
!
              i_rms%grad_fld%i_div_b = msq_list%numrms + 2
              j_ave%grad_fld%i_div_b = msq_list%numave + 4
!
              msq_list%numrms = msq_list%numrms + 2
              msq_list%numave = msq_list%numave + 4
!
            else if(field_name .eq. vector_potential%name) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%base%i_vecp,             &
     &            i_rms%base%i_vecp, j_ave%base%i_vecp, msq_list)
!
              i_rms%grad_fld%i_div_a =  msq_list%numrms + 1
              j_ave%grad_fld%i_div_a = msq_list%numave + 1
!
              msq_list%numrms = msq_list%numrms + 1
              msq_list%numave = msq_list%numave + 1
!
            else if(field_name .eq. vorticity%name) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%i_vort,                  &
     &            i_rms%i_vort, j_ave%i_vort, msq_list)
!
              ifld_msq%ir_rms_w = msq_list%numrms + 1
              msq_list%numrms = msq_list%numrms + 1
!
            else if(field_name .eq. current_density%name) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%base%i_current,          &
     &            i_rms%base%i_current, j_ave%base%i_current, msq_list)
!
              ifld_msq%ir_sqj_ic = msq_list%numrms + 1
              ifld_msq%ja_j_ic = msq_list%numave + 1
!
              ifld_msq%ir_rms_j =    msq_list%numrms + 2
              ifld_msq%ir_rms_j_ic = msq_list%numrms + 3
!
              msq_list%numrms = msq_list%numrms + 3
              msq_list%numave = msq_list%numave + 3
            end if
          end if
!
          if(check_base_scalar(field_name)) then
            if ( field_name .eq. pressure%name ) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%base%i_press,            &
     &            i_rms%base%i_press, j_ave%base%i_press, msq_list)
            else if ( field_name .eq. magnetic_potential%name ) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%base%i_mag_p,            &
     &            i_rms%base%i_mag_p, j_ave%base%i_mag_p, msq_list)
            else
              call set_rms_address_list(i, nod_fld, msq_list)
            end if
          end if
!
          if(check_force_vectors(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_field_product_vectors(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_field_product_scalars(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if ( field_name .eq. filter_temperature%name ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_filter_temp,             &
     &          i_rms%i_filter_temp, j_ave%i_filter_temp, msq_list)
          else if ( field_name .eq. filter_composition%name ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_filter_comp,             &
     &          i_rms%i_filter_comp, j_ave%i_filter_comp, msq_list)
          else if(check_wide_filter_scalar(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          else if(check_double_filter_scalar(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
!
          if(check_difference_vectors(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_diff_filter_vectors(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_grad_filter_field(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_flux_tensors(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_asym_flux_tensors(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_rot_force(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(     check_div_force(field_name)                           &
     &       .or. check_div_flux_tensor(field_name)                     &
     &       .or. check_div_scalar_flux(field_name)) then
           call set_rms_address_list(i, nod_fld, msq_list)
         end if
!
          if(check_filtered_force(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_vector_diffusion(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_scalar_diffusion(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_SGS_vector_terms(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_SGS_tensor_terms(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_SGS_induction_tensor(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_div_SGS_flux_vector(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_div_SGS_flux_tensor(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_rot_SGS_terms(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_flux_tensor_w_SGS(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_induction_tensor_w_SGS(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(    check_wide_SGS_vector_terms(field_name)                &
     &      .or. check_double_SGS_vector_terms(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_force_w_SGS(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_enegy_fluxes(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_scalar_advection(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_filter_enegy_fluxes(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_SGS_ene_fluxes(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(      check_true_SGS_vector_terms(field_name)              &
     &        .or. check_true_div_SGS_flux_vector(field_name)           &
     &        .or. check_true_div_SGS_flux_tensor(field_name)           &
     &        .or. check_true_SGS_ene_fluxes(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_wide_filter_vector(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          else if(check_wide_filter_scalar(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          else if(check_wide_filter_grad(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
!
          else if(check_double_filter_vector(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          else if(check_double_filter_scalar(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          else if(check_double_filter_grad(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_SGS_moedel_coefs(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
!   Old field label... Should be deleted later!!
          if(field_name .eq. buoyancy_work%name) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
        else
          if ( field_name .eq. velocity%name) then
            call set_rms_address                                        &
     &         (e_hd_div_v, n_scalar, iphys%i_velo,                     &
     &          i_rms%grad_fld%i_div_v, j_ave%grad_fld%i_div_v,         &
     &          msq_list)
          else if ( field_name .eq. magnetic_field%name ) then
            call set_rms_address                                        &
     &         (e_hd_div_b, n_scalar, iphys%i_magne,                    &
     &          i_rms%grad_fld%i_div_b, j_ave%grad_fld%i_div_b,         &
     &          msq_list)
          else if ( field_name .eq. vector_potential%name ) then
            call set_rms_address                                        &
     &         (e_hd_div_a, n_scalar, iphys%base%i_vecp,                &
     &          i_rms%grad_fld%i_div_a, j_ave%grad_fld%i_div_a,         &
     &          msq_list)
          else if ( field_name .eq. filter_velocity%name ) then
            call set_rms_address                                        &
     &         (e_hd_fil_div_v, n_scalar, iphys%i_filter_velo,          &
     &          i_rms%i_div_filter_v, j_ave%i_div_filter_v, msq_list)
          else if ( field_name .eq. filter_magne%name ) then
            call set_rms_address                                        &
     &         (e_hd_fil_div_b, n_scalar, iphys%i_filter_magne,         &
     &          i_rms%i_div_filter_b, j_ave%i_div_filter_b, msq_list)
          else if ( field_name .eq. filter_vector_potential%name ) then
            call set_rms_address                                        &
     &         (e_hd_fil_div_a, n_scalar, iphys%i_div_filter_a,         &
     &          i_rms%i_div_filter_a, j_ave%i_div_filter_a, msq_list)
          else if ( field_name .eq. magnetic_potential%name ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%base%i_mag_p,              &
     &          i_rms%base%i_mag_p, j_ave%base%i_mag_p, msq_list)
          end if

        end if
      end do
!
      ifld_msq%ivol =   msq_list%numrms + 1
      msq_list%numrms = msq_list%numrms + 1
!
      end subroutine set_mean_square_values
!
! ----------------------------------------------------------------------
!
      end module set_mean_square_array
