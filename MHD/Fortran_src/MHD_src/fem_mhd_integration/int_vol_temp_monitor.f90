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
      use m_geometry_data
      use m_geometry_data_MHD
      use m_jacobians
      use m_sorted_node
      use m_physical_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_filter_elength
      use m_finite_element_matrix
      use m_int_vol_data
      use m_node_phys_data
      use m_element_phys_data
!
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
      use int_vol_inertia
      use int_vol_vect_cst_difference
      use int_vol_SGS_div_flux
!
      integer (kind=kint), intent(in) :: i_field
!
!
      if (i_field .eq. iphys%i_h_advect) then
        call int_vol_scalar_inertia                                     &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      iele_fl_smp_stack, intg_point_t_evo, iphys%i_temp,          &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,       &
     &      coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_ph_advect) then
        call int_vol_scalar_inertia                                     &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      iele_fl_smp_stack, intg_point_t_evo, iphys%i_par_temp,      &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,       &
     &      coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_h_flux_div) then
        call int_vol_div_w_const                                        &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      iele_fl_smp_stack, intg_point_t_evo, iphys%i_h_flux,        &
     &      coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_ph_flux_div) then
        call int_vol_div_w_const                                        &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      iele_fl_smp_stack, intg_point_t_evo, iphys%i_ph_flux,       &
     &      coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_SGS_div_h_flux) then
        if(iflag_commute_heat .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_vec_flux                                 &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, nod_fld1,    &
     &        iele_fl_smp_stack, intg_point_t_evo,                      &
     &        iphys%i_velo, iphys%i_temp, iphys%i_SGS_h_flux,           &
     &        ifilter_final, ak_diff(1,iak_diff_hf), coef_nega_t,       &
     &        fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_div_w_const                                      &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        iele_fl_smp_stack, intg_point_t_evo, iphys%i_SGS_h_flux,  &
     &        coef_nega_t, fem1_wk, f1_nl)
        end if
      end if
!
      end subroutine int_vol_ene_monitor
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_ene_monitor_upw(i_field)
!
      use int_vol_inertia
      use int_vol_vect_cst_diff_upw
      use int_vol_SGS_div_flux
!
      integer (kind = kint), intent(in) :: i_field
!
!
      if (i_field .eq. iphys%i_h_advect)  then
        call int_vol_scalar_inertia_upw                                 &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      iele_fl_smp_stack, intg_point_t_evo, iphys%i_temp,          &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, iphys_ele%i_velo,     &
     &      fld_ele1%d_fld, coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_ph_advect) then
        call int_vol_scalar_inertia_upw                                 &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      iele_fl_smp_stack, intg_point_t_evo, iphys%i_par_temp,      &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, iphys_ele%i_velo,     &
     &      fld_ele1%d_fld, coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_h_flux_div) then
        call int_vol_div_w_const_upw                                    &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      iele_fl_smp_stack, intg_point_t_evo, iphys%i_h_flux,        &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,       &
     &      coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_ph_flux_div) then
        call int_vol_div_w_const_upw                                    &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      iele_fl_smp_stack, intg_point_t_evo, iphys%i_ph_flux,       &
     &      fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,       &
     &      coef_nega_t, fem1_wk, f1_nl)
!
      else if (i_field .eq. iphys%i_SGS_div_h_flux) then
        if(iflag_commute_heat .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_vec_flux_upw                             &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, nod_fld1,    &
     &        iele_fl_smp_stack, intg_point_t_evo,                      &
     &        iphys%i_velo, iphys%i_temp, iphys%i_SGS_h_flux,           &
     &        ifilter_final, ak_diff(1,iak_diff_hf),                    &
     &        fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,     &
     &        coef_nega_t, fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_div_w_const_upw                                  &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
              iele_fl_smp_stack, intg_point_t_evo, iphys%i_SGS_h_flux,  &
     &        fld_ele1%ntot_phys, iphys_ele%i_velo, fld_ele1%d_fld,     &
     &        coef_nega_t, fem1_wk, f1_nl)
        end if
      end if
!
      end subroutine int_vol_ene_monitor_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_temp_monitor
