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
!!     &         (nod_fld, iphys, iphys_LES, i_msq, msq_list)
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(mean_square_address), intent(inout) :: i_msq
!!        type(mean_square_list), intent(inout) :: msq_list
!!@endverbatim
!
      module set_mean_square_array
!
      use m_precision
!
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
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
     &         (nod_fld, iphys, iphys_LES, i_msq, msq_list)
!
      use t_base_field_labels
      use t_energy_flux_labels
      use t_diffusion_term_labels
      use t_diff_vector_labels
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels
!
      use m_phys_constants
      use m_base_field_labels
      use m_wide_filter_field_labels
      use m_grad_filter_field_labels
      use m_wide_SGS_term_labels
      use m_force_w_SGS_labels
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
      use m_div_filtered_force_labels
!
      use set_MHD_field_address
      use set_SGS_MHD_field_address
!
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
!
      type(mean_square_address), intent(inout) :: i_msq
      type(mean_square_list), intent(inout) :: msq_list
!
      integer (kind = kint) :: i, num_comps, i_rms_tmp, j_ave_tmp
      character(len = kchara) :: field_name
!
!
      do i = 1, nod_fld%num_phys
        field_name = nod_fld%phys_name(i)
        num_comps =  nod_fld%num_component(i)
        if(nod_fld%flag_monitor(i)) then
          if(check_filter_vector(field_name)) then
            if(field_name .eq. filter_velocity%name) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys_LES%filter_fld%i_velo,   &
     &            i_msq%imsq_fil_velo, j_ave_tmp, msq_list)
!
              i_msq%imsq_div_fil_v = msq_list%numrms + 1
              i_msq%jave_div_fil_v = msq_list%numave + 1
!
              i_msq%jr_amom_f = msq_list%numave + 2
!
              msq_list%numrms = msq_list%numrms + 1
              msq_list%numave = msq_list%numave + 4
            else if(field_name .eq. filter_vector_potential%name) then
              call set_rms_address                                      &
     &           (field_name, n_scalar, iphys_LES%filter_fld%i_vecp,    &
     &            i_rms_tmp, j_ave_tmp, msq_list)
!
              i_msq%imsq_div_fil_a = msq_list%numrms + 1
              i_msq%jave_div_fil_a = msq_list%numave + 1
!
              msq_list%numrms = msq_list%numrms + 1
              msq_list%numave = msq_list%numave + 4
            else if(field_name .eq. filter_magne%name) then
              num_comps = num_comps + n_vector + n_scalar
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys_LES%filter_fld%i_magne,  &
     &            i_msq%imsq_fil_magne, j_ave_tmp, msq_list)
!
              i_msq%ir_me_f_ic =  msq_list%numrms + 1
              i_msq%ja_mag_f_ic = msq_list%numave + 1
!
              i_msq%imsq_div_fil_b = msq_list%numrms + 2
              i_msq%jave_div_fil_b = msq_list%numave + 4
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
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%base%i_velo,             &
     &            i_msq%imsq_velo, j_ave_tmp, msq_list)
!
              i_msq%imsq_div_v = msq_list%numrms + 1
              i_msq%jave_div_v = msq_list%numave + 1
!
              i_msq%ja_amom = msq_list%numave + 2
!
              msq_list%numrms = msq_list%numrms + 1
              msq_list%numave = msq_list%numave + 4
!
            else if(field_name .eq. magnetic_field%name) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%base%i_magne,            &
     &            i_msq%imsq_magne, j_ave_tmp, msq_list)
!
              i_msq%ir_me_ic =  msq_list%numrms + 1
              i_msq%ja_mag_ic = msq_list%numave + 1
!
              i_msq%imsq_div_b = msq_list%numrms + 2
              i_msq%jave_div_b = msq_list%numave + 4
!
              msq_list%numrms = msq_list%numrms + 2
              msq_list%numave = msq_list%numave + 4
!
            else if(field_name .eq. vector_potential%name) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%base%i_vecp,             &
     &            i_rms_tmp, j_ave_tmp, msq_list)
!
              i_msq%imsq_div_a = msq_list%numrms + 1
              i_msq%jave_div_a = msq_list%numave + 1
!
              msq_list%numrms = msq_list%numrms + 1
              msq_list%numave = msq_list%numave + 1
!
            else if(field_name .eq. vorticity%name) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%base%i_vort,             &
     &            i_msq%imsq_vort, j_ave_tmp, msq_list)
!
              i_msq%ir_rms_w = msq_list%numrms + 1
              msq_list%numrms = msq_list%numrms + 1
!
            else if(field_name .eq. current_density%name) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%base%i_current,          &
     &            i_msq%imsq_current, j_ave_tmp, msq_list)
!
              i_msq%ir_sqj_ic = msq_list%numrms + 1
              i_msq%ja_j_ic = msq_list%numave + 1
!
              i_msq%ir_rms_j =    msq_list%numrms + 2
              i_msq%ir_rms_j_ic = msq_list%numrms + 3
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
     &            i_msq%imsq_press, i_msq%jave_press, msq_list)
            else if ( field_name .eq. magnetic_potential%name ) then
              call set_rms_address                                      &
     &           (field_name, num_comps, iphys%base%i_mag_p,            &
     &            i_msq%imsq_mag_p, i_msq%jave_mag_p, msq_list)
            else
              call set_rms_address_list(i, nod_fld, msq_list)
            end if
          end if
!
          if ( field_name .eq. filter_temperature%name ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys_LES%filter_fld%i_temp,     &
     &          i_rms_tmp, j_ave_tmp, msq_list)
          else if ( field_name .eq. filter_composition%name ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys_LES%filter_fld%i_light,    &
     &          i_rms_tmp, j_ave_tmp, msq_list)
          end if
!
          if(    check_force_vectors(field_name)                        &
     &      .or. check_rot_force(field_name)                            &
     &      .or. check_div_flux_tensor(field_name)                      &
     &      .or. check_vector_diffusion(field_name)                     &
     &      .or. check_difference_vectors(field_name)                   &
     &      .or. check_field_product_vectors(field_name)                &
!
     &      .or. check_enegy_fluxes(field_name)                         &
     &      .or. check_scalar_advection(field_name)                     &
     &      .or. check_div_force(field_name)                            &
     &      .or. check_div_scalar_flux(field_name)                      &
     &      .or. check_scalar_diffusion(field_name)                     &
     &      .or. check_field_product_scalars(field_name)                &
!
     &      .or. check_sym_tensor_fields(field_name)                    &
     &      .or. check_asym_tensor_fields(field_name)                   &

     &      .or. check_SGS_vector_fields(field_name)                    &
     &      .or. check_SGS_scalar_fields(field_name)                    &
     &      .or. check_SGS_asym_tensor_fields(field_name)               &
!
     &      .or. check_SGS_tensor_terms(field_name)                     &
     &      .or. check_flux_tensor_w_SGS(field_name)                    &
     &      .or. check_filtered_flux_tensor(field_name)                 &
     &      .or. check_div_fil_flux_t(field_name)                       &
     &    ) then
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
     &         (e_hd_div_v, n_scalar, iphys%base%i_velo,                &
     &          i_msq%imsq_div_v, i_msq%jave_div_v, msq_list)
          else if ( field_name .eq. magnetic_field%name ) then
            call set_rms_address                                        &
     &         (e_hd_div_b, n_scalar, iphys%base%i_magne,               &
     &          i_msq%imsq_div_b, i_msq%jave_div_b, msq_list)
          else if ( field_name .eq. vector_potential%name ) then
            call set_rms_address                                        &
     &         (e_hd_div_a, n_scalar, iphys%base%i_vecp,                &
     &          i_msq%imsq_div_a, i_msq%jave_div_a, msq_list)
          else if ( field_name .eq. filter_velocity%name ) then
            call set_rms_address                                        &
     &         (e_hd_fil_div_v, n_scalar, iphys_LES%filter_fld%i_velo,  &
     &          i_msq%imsq_div_fil_v, i_msq%jave_div_fil_v, msq_list)
          else if ( field_name .eq. filter_magne%name ) then
            call set_rms_address                                        &
     &         (e_hd_fil_div_b, n_scalar, iphys_LES%filter_fld%i_magne, &
     &          i_msq%imsq_div_fil_b, i_msq%jave_div_fil_b, msq_list)
          else if ( field_name .eq. filter_vector_potential%name ) then
            call set_rms_address(e_hd_fil_div_a,                        &
     &          n_scalar, iphys_LES%grad_fil_fld%i_div_a,               &
     &          i_msq%imsq_div_fil_a, i_msq%jave_div_fil_a, msq_list)
          else if ( field_name .eq. magnetic_potential%name ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%base%i_mag_p,              &
     &          i_msq%imsq_mag_p, i_msq%jave_mag_p, msq_list)
          end if

        end if
      end do
!
      i_msq%ivol =      msq_list%numrms + 1
      msq_list%numrms = msq_list%numrms + 1
!
      end subroutine set_mean_square_values
!
! ----------------------------------------------------------------------
!
      end module set_mean_square_array
