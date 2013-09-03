!
!     module int_vol_temp_monitor
!
!     numerical integration for finite elememt equations of heat
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2002
!     modified by H. Matsui on Aug., 2005
!     modified by H. Matsui on Aug., 2007
!
!      subroutine int_vol_ene_monitor(i_field)
!      subroutine int_vol_ene_monitor_upw(i_field)
!
      module int_vol_temp_monitor
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_parameter
      use m_geometry_data_MHD
      use m_physical_property
      use m_SGS_model_coefs
      use m_SGS_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_ene_monitor(i_field)
!
      use m_node_phys_address
      use m_element_phys_address
      use m_element_phys_data
      use m_SGS_address
!
      use int_vol_inertia_1st
      use int_vol_vect_cst_diff_1st
      use int_vol_SGS_div_flux_1st
!
      integer (kind=kint), intent(in) :: i_field
!
!
      if (i_field .eq. iphys%i_h_advect) then
        call int_vol_scalar_inertia_1st(iele_fl_smp_stack,              &
     &      intg_point_t_evo, iphys%i_temp, d_ele(1,iphys_ele%i_velo),  &
     &      coef_nega_t)
!
      else if (i_field .eq. iphys%i_ph_advect) then
        call int_vol_scalar_inertia_1st(iele_fl_smp_stack,              &
     &      intg_point_t_evo, iphys%i_par_temp,                         &
     &      d_ele(1,iphys_ele%i_velo), coef_nega_t)
!
      else if (i_field .eq. iphys%i_h_flux_div) then
        call int_vol_div_w_const_1st(iele_fl_smp_stack,                 &
     &      intg_point_t_evo, iphys%i_h_flux, coef_nega_t)
!
      else if (i_field .eq. iphys%i_ph_flux_div) then
        call int_vol_div_w_const_1st(iele_fl_smp_stack,                 &
     &      intg_point_t_evo, iphys%i_ph_flux, coef_nega_t)
!
      else if (i_field .eq. iphys%i_SGS_div_h_flux) then
        if(iflag_commute_heat .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_vec_flux(iele_fl_smp_stack,              &
     &        intg_point_t_evo, iphys%i_velo, iphys%i_temp,             &
     &        iphys%i_SGS_h_flux, ifilter_final, iak_diff_hf,           &
     &        coef_nega_t)
        else
          call int_vol_div_w_const_1st(iele_fl_smp_stack,               &
     &        intg_point_t_evo, iphys%i_SGS_h_flux, coef_nega_t)
        end if
      end if
!
      end subroutine int_vol_ene_monitor
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_ene_monitor_upw(i_field)
!
      use m_node_phys_address
      use m_element_phys_address
      use m_element_phys_data
      use m_SGS_address
!
      use int_vol_inertia_1st
      use int_vol_vect_cst_diff_upw_1
      use int_vol_SGS_div_flux_1st
!
      integer (kind = kint), intent(in) :: i_field
!
!
      if (i_field .eq. iphys%i_h_advect)  then
        call int_vol_scalar_inertia_upw_1st(iele_fl_smp_stack,          &
     &      intg_point_t_evo, iphys%i_temp,  d_ele(1,iphys_ele%i_velo), &
     &      d_ele(1,iphys_ele%i_velo), coef_nega_t)
!
      else if (i_field .eq. iphys%i_ph_advect) then
        call int_vol_scalar_inertia_upw_1st(iele_fl_smp_stack,          &
     &      intg_point_t_evo, iphys%i_par_temp,                         &
     &      d_ele(1,iphys_ele%i_velo), d_ele(1,iphys_ele%i_velo),       &
     &      coef_nega_t)
!
      else if (i_field .eq. iphys%i_h_flux_div) then
        call int_vol_div_w_const_upw_1(iele_fl_smp_stack,               &
     &      intg_point_t_evo, iphys_ele%i_velo, iphys%i_h_flux,         &
     &      coef_nega_t)
!
      else if (i_field .eq. iphys%i_ph_flux_div) then
        call int_vol_div_w_const_upw_1(iele_fl_smp_stack,               &
     &      intg_point_t_evo, iphys_ele%i_velo, iphys%i_ph_flux,        &
     &      coef_nega_t)
!
      else if (i_field .eq. iphys%i_SGS_div_h_flux) then
        if(iflag_commute_heat .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_vec_flux_upw(iele_fl_smp_stack,          &
     &        intg_point_t_evo, iphys_ele%i_velo, iphys%i_velo,         &
     &        iphys%i_temp, iphys%i_SGS_h_flux, ifilter_final,          &
     &        iak_diff_hf, coef_nega_t)
        else
          call int_vol_div_w_const_upw_1(iele_fl_smp_stack,             &
              intg_point_t_evo, iphys_ele%i_velo, iphys%i_SGS_h_flux,   &
     &        coef_nega_t)
        end if
      end if
!
      end subroutine int_vol_ene_monitor_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_temp_monitor
