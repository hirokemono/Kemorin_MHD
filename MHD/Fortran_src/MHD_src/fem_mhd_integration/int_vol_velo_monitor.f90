!
!     module int_vol_velo_monitor
!
!     numerical integration for finite elememt equations of momentum
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine int_vol_velo_monitor_pg(i_field)
!      subroutine int_vol_velo_monitor_upw(i_field)
!      subroutine int_vol_velo_monitor_upm(i_field)
!
      module int_vol_velo_monitor
!
      use m_precision
!
      use m_control_parameter
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
      use m_physical_property
      use m_ele_material_property
      use m_node_phys_address
      use m_element_phys_address
      use m_element_phys_data
      use m_SGS_model_coefs
      use m_SGS_address
!
      implicit none
!
      private :: int_vol_velo_monitor_upwind
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_monitor_pg(i_field)
!
      use int_vol_inertia_1st
      use int_vol_vect_cst_diff_1st
      use int_vol_SGS_div_flux_1st
      use int_vol_buoyancy_1st
      use int_vol_coriolis_1st
      use int_vol_Lorentz_1st
!
      integer(kind=kint), intent(in) :: i_field
!
!
      if(i_field .eq. iphys%i_m_advect) then
        if (iflag_4_rotate .eq. id_turn_ON) then
          call int_vol_rot_inertia_1st(iele_fl_smp_stack,               &
     &        intg_point_t_evo, iphys%i_velo,                           &
     &        d_ele(1,iphys_ele%i_vort), coef_nega_v)
        else
          call int_vol_vector_inertia_1st(iele_fl_smp_stack,            &
     &        intg_point_t_evo, iphys%i_velo,                           &
     &        d_ele(1,iphys_ele%i_velo), coef_nega_v)
        end if
!
      else if(i_field .eq. iphys%i_m_flux_div) then
        call int_vol_div_tsr_w_const_1st(iele_fl_smp_stack,             &
     &      intg_point_t_evo, iphys%i_m_flux, coef_nega_v)
!
      end if
!
      if(i_field .eq. iphys%i_coriolis) then
        call int_vol_coriolis_pg(iele_fl_smp_stack, intg_point_t_evo)
      end if
!
      if(i_field .eq. iphys%i_buoyancy) then
        call int_vol_buoyancy_pg(iele_fl_smp_stack,                     &
     &      intg_point_t_evo, iphys%i_temp, ak_buo)
      else if(i_field .eq. iphys%i_comp_buo) then
        call int_vol_buoyancy_pg(iele_fl_smp_stack,                     &
     &      intg_point_t_evo, iphys%i_light, ak_comp_buo)
      else if(i_field .eq. iphys%i_filter_buo) then
        call int_vol_buoyancy_pg(iele_fl_smp_stack,                     &
     &      intg_point_t_evo, iphys%i_filter_temp, ak_buo)
      end if
!
      if(i_field .eq. iphys%i_m_tension) then
        call int_vol_Lorentz_pg(iele_fl_smp_stack,                      &
     &        intg_point_t_evo)
      else if(i_field .eq. iphys%i_lorentz) then
        if (iflag_4_rotate .eq. id_turn_ON) then
          call int_vol_full_rot_Lorentz_pg(iele_fl_smp_stack,           &
     &        intg_point_t_evo)
        else
          call int_vol_full_Lorentz_pg(iele_fl_smp_stack,               &
     &        intg_point_t_evo)
        end if
      end if
!
      if(i_field .eq. iphys%i_maxwell_div) then
        call int_vol_div_tsr_w_const_1st(iele_fl_smp_stack,             &
     &      intg_point_t_evo, iphys%i_maxwell, coef_lor)
!
      else if(i_field .eq. iphys%i_SGS_div_m_flux) then
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux(iele_fl_smp_stack,              &
     &        intg_point_t_evo, iphys%i_velo, iphys%i_SGS_m_flux,       &
     &        ifilter_final, iak_diff_mf, coef_nega_v)
        else
          call int_vol_div_tsr_w_const_1st(iele_fl_smp_stack,           &
     &        intg_point_t_evo, iphys%i_SGS_m_flux, coef_nega_v)
        end if
!
      else if(i_field .eq. iphys%i_SGS_Lorentz) then
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux(iele_fl_smp_stack,              &
     &        intg_point_t_evo, iphys%i_magne, iphys%i_SGS_maxwell,     &
     &        ifilter_final, iak_diff_lor, coef_lor)
        else
          call int_vol_div_tsr_w_const_1st(iele_fl_smp_stack,           &
     &        intg_point_t_evo, iphys%i_SGS_maxwell, coef_lor)
        end if
      end if
!
      end subroutine int_vol_velo_monitor_pg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_monitor_upw(i_field)
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call int_vol_velo_monitor_upwind(i_field, iphys_ele%i_velo)
!
      end subroutine int_vol_velo_monitor_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_monitor_upm(i_field)
!
      integer(kind = kint), intent(in) :: i_field
!
!
      call int_vol_velo_monitor_upwind(i_field, iphys_ele%i_magne)
!
      end subroutine int_vol_velo_monitor_upm
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_monitor_upwind(i_field, iv_upw)
!
      use int_vol_inertia_1st
      use int_vol_vect_cst_diff_upw_1
      use int_vol_SGS_div_flux_1st
      use int_vol_buoyancy_1st
      use int_vol_coriolis_1st
      use int_vol_Lorentz_1st
!
      integer(kind = kint), intent(in) :: i_field, iv_upw
!
!
      if(i_field .eq. iphys%i_m_advect) then
        if (iflag_4_rotate .eq. id_turn_ON) then
          call int_vol_rot_inertia_upw_1st(iele_fl_smp_stack,           &
     &        intg_point_t_evo, iphys%i_velo,                           &
     &        d_ele(1,iphys_ele%i_vort), d_ele(1,iv_upw), coef_nega_v)
        else
          call int_vol_vector_inertia_upw_1st(iele_fl_smp_stack,        &
     &        intg_point_t_evo, iphys%i_velo,                           &
     &        d_ele(1,iphys_ele%i_velo), d_ele(1,iv_upw), coef_nega_v)
        end if
!
      else if(i_field .eq. iphys%i_m_flux_div) then
        call int_vol_div_tsr_w_const_upw_1(iele_fl_smp_stack,           &
     &      intg_point_t_evo, iv_upw, iphys%i_m_flux, coef_nega_v)
      end if
!
      if(i_field .eq. iphys%i_coriolis) then
        call int_vol_coriolis_upw(iele_fl_smp_stack, intg_point_t_evo,  &
     &      iv_upw)
      end if
!
      if(i_field .eq. iphys%i_buoyancy) then
        call int_vol_buoyancy_upw(iele_fl_smp_stack,                    &
     &      intg_point_t_evo, iphys%i_temp, ak_buo, iv_upw)
      else if(i_field .eq. iphys%i_comp_buo) then
        call int_vol_buoyancy_upw(iele_fl_smp_stack,                    &
     &      intg_point_t_evo, iphys%i_light, ak_comp_buo, iv_upw)
      else if(i_field .eq. iphys%i_filter_buo) then
        call int_vol_buoyancy_upw(iele_fl_smp_stack,                    &
     &      intg_point_t_evo, iphys%i_filter_temp, ak_buo, iv_upw)
      end if
!
!
      if(i_field .eq. iphys%i_m_tension) then
        call int_vol_Lorentz_upw(iele_fl_smp_stack,                     &
     &        intg_point_t_evo, iv_upw)
      else if(i_field .eq. iphys%i_lorentz) then
        if (iflag_4_rotate .eq. id_turn_ON) then
          call int_vol_full_rot_Lorentz_pg(iele_fl_smp_stack,           &
     &        intg_point_t_evo)
        else
          call int_vol_full_Lorentz_upw(iele_fl_smp_stack,              &
     &        intg_point_t_evo, iv_upw)
        end if
      end if
!
!
      if(i_field .eq. iphys%i_maxwell_div)  then
        call int_vol_div_tsr_w_const_upw_1(iele_fl_smp_stack,           &
     &      intg_point_t_evo, iv_upw, iphys%i_maxwell, coef_lor)
!
      else if(i_field .eq. iphys%i_SGS_div_m_flux) then 
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux_upw(iele_fl_smp_stack,          &
     &        intg_point_t_evo, iv_upw, iphys%i_velo,                   &
     &        iphys%i_SGS_m_flux, ifilter_final, iak_diff_mf,           &
     &        coef_nega_v)
        else
          call int_vol_div_tsr_w_const_upw_1(iele_fl_smp_stack,         &
     &        intg_point_t_evo, iv_upw, iphys%i_SGS_m_flux,             &
     &        coef_nega_v)
        end if
!
      else if(i_field .eq. iphys%i_SGS_Lorentz) then
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux_upw(iele_fl_smp_stack,          &
     &        intg_point_t_evo, iv_upw, iphys%i_magne,                  &
     &        iphys%i_SGS_maxwell, ifilter_final, iak_diff_lor,         &
     &        coef_lor)
        else
          call int_vol_div_tsr_w_const_upw_1(iele_fl_smp_stack,         &
     &        intg_point_t_evo, iv_upw, iphys%i_SGS_maxwell, coef_lor)
        end if
      end if
!
      end subroutine int_vol_velo_monitor_upwind
!
!-----------------------------------------------------------------------
!
      end module int_vol_velo_monitor
