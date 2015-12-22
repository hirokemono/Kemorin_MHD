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
      use m_geometry_data
      use m_geometry_data_MHD
      use m_phys_constants
      use m_physical_property
      use m_ele_material_property
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_SGS_model_coefs
      use m_SGS_address
      use m_filter_elength
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
      use int_vol_inertia
      use int_vol_vect_cst_difference
      use int_vol_SGS_div_flux
      use int_vol_buoyancy
      use int_vol_coriolis
      use int_vol_Lorentz
!
      integer(kind=kint), intent(in) :: i_field
!
!
      if(i_field .eq. iphys%i_m_advect) then
        if (iflag_4_rotate .eq. id_turn_ON) then
          call int_vol_rot_inertia                                      &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_velo, fld_ele1%ntot_phys, iphys_ele%i_vort,       &
     &        fld_ele1%d_fld, coef_nega_v, fem1_wk, f1_nl)
        else
          call int_vol_vector_inertia                                   &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_velo, fld_ele1%ntot_phys, iphys_ele%i_velo,       &
     &        fld_ele1%d_fld, coef_nega_v, fem1_wk, f1_nl)
        end if
!
      else if(i_field .eq. iphys%i_m_flux_div) then
        call int_vol_div_tsr_w_const                                    &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_m_flux, coef_nega_v, fem1_wk, f1_nl)
!
      end if
!
      if(i_field .eq. iphys%i_coriolis) then
        call int_vol_coriolis_pg                                        &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo, iphys%i_velo,  &
     &      fem1_wk, f1_nl)
      end if
!
      if(i_field .eq. iphys%i_buoyancy) then
        call int_vol_buoyancy_pg                                        &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo, iphys%i_temp,  &
     &      ak_buo, fem1_wk, f1_nl)
      else if(i_field .eq. iphys%i_comp_buo) then
        call int_vol_buoyancy_pg                                        &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo, iphys%i_light, &
     &      ak_comp_buo, fem1_wk, f1_nl)
      else if(i_field .eq. iphys%i_filter_buo) then
        call int_vol_buoyancy_pg                                        &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_filter_temp, ak_buo, fem1_wk, f1_nl)
      end if
!
      if(i_field .eq. iphys%i_m_tension) then
        call int_vol_Lorentz_pg                                         &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo, iphys%i_magne, &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, fld_ele1%d_fld,      &
     &      fem1_wk, mhd_fem1_wk, f1_nl)
      else if(i_field .eq. iphys%i_lorentz) then
        if (iflag_4_rotate .eq. id_turn_ON) then
          call int_vol_full_rot_Lorentz_pg                              &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_vecp, fld_ele1%ntot_phys, iphys_ele%i_magne,      &
     &        fld_ele1%d_fld, fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_full_Lorentz_pg                                  &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_magne, fld_ele1%ntot_phys, iphys_ele%i_magne,     &
     &        fld_ele1%d_fld, fem1_wk, f1_nl)
        end if
      end if
!
      if(i_field .eq. iphys%i_maxwell_div) then
        call int_vol_div_tsr_w_const                                    &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_maxwell, coef_lor, fem1_wk, f1_nl)
!
      else if(i_field .eq. iphys%i_SGS_div_m_flux) then
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux                                 &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, nod_fld1,    &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_velo, iphys%i_SGS_m_flux, ifilter_final,          &
     &        ak_diff(1,iak_diff_mf), coef_nega_v,                      &
     &        fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_div_tsr_w_const                                  &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_SGS_m_flux, coef_nega_v, fem1_wk, f1_nl)
        end if
!
      else if(i_field .eq. iphys%i_SGS_Lorentz) then
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux                                 &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, nod_fld1,    &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_magne, iphys%i_SGS_maxwell, ifilter_final,        &
     &        ak_diff(1,iak_diff_lor), coef_lor,                        &
     &        fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_div_tsr_w_const                                  &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_SGS_maxwell, coef_lor, fem1_wk, f1_nl)
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
      use m_element_phys_data
      use int_vol_inertia
      use int_vol_vect_cst_diff_upw
      use int_vol_SGS_div_flux
      use int_vol_buoyancy
      use int_vol_coriolis
      use int_vol_Lorentz
!
      integer(kind = kint), intent(in) :: i_field, iv_upw
!
!
      if(i_field .eq. iphys%i_m_advect) then
        if (iflag_4_rotate .eq. id_turn_ON) then
          call int_vol_rot_inertia_upw                                  &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_velo, fld_ele1%ntot_phys, iphys_ele%i_vort,       &
     &        iv_upw, fld_ele1%d_fld, coef_nega_v, fem1_wk, f1_nl)
        else
          call int_vol_vector_inertia_upw                               &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_velo, fld_ele1%ntot_phys, iphys_ele%i_velo,       &
     &        iv_upw, fld_ele1%d_fld, coef_nega_v, fem1_wk, f1_nl)
        end if
!
      else if(i_field .eq. iphys%i_m_flux_div) then
        call int_vol_div_tsr_w_const_upw                                &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_m_flux, fld_ele1%ntot_phys, iv_upw,                 &
     &      fld_ele1%d_fld, coef_nega_v, fem1_wk, f1_nl)
      end if
!
      if(i_field .eq. iphys%i_coriolis) then
        call int_vol_coriolis_upw                                       &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo, iphys%i_velo,  &
     &      fld_ele1%ntot_phys, iv_upw, fld_ele1%d_fld, fem1_wk, f1_nl)
      end if
!
      if(i_field .eq. iphys%i_buoyancy) then
        call int_vol_buoyancy_upw                                       &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo, iphys%i_temp,  &
     &      ak_buo, fld_ele1%ntot_phys, iv_upw, fld_ele1%d_fld,         &
     &      fem1_wk, f1_nl)
      else if(i_field .eq. iphys%i_comp_buo) then
        call int_vol_buoyancy_upw                                       &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo, iphys%i_light, &
     &      ak_comp_buo, fld_ele1%ntot_phys, iv_upw, fld_ele1%d_fld,    &
     &      fem1_wk, f1_nl)
      else if(i_field .eq. iphys%i_filter_buo) then
        call int_vol_buoyancy_upw                                       &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_filter_temp, ak_buo, fld_ele1%ntot_phys, iv_upw,    &
     &      fld_ele1%d_fld, fem1_wk, f1_nl)
      end if
!
!
      if(i_field .eq. iphys%i_m_tension) then
        call int_vol_Lorentz_upw                                        &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo, iphys%i_magne, &
     &      fld_ele1%ntot_phys, iphys_ele%i_magne, iv_upw,              &
     &      fld_ele1%d_fld, fem1_wk, mhd_fem1_wk, f1_nl)
      else if(i_field .eq. iphys%i_lorentz) then
        if (iflag_4_rotate .eq. id_turn_ON) then
          call int_vol_full_rot_Lorentz_pg                              &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_vecp, fld_ele1%ntot_phys, iphys_ele%i_magne,      &
     &        fld_ele1%d_fld, fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_full_Lorentz_upw                                 &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_magne, fld_ele1%ntot_phys,iphys_ele%i_magne,      &
     &        iv_upw, fld_ele1%d_fld, fem1_wk, f1_nl)
        end if
      end if
!
!
      if(i_field .eq. iphys%i_maxwell_div)  then
        call int_vol_div_tsr_w_const_upw                                &
     &     (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,                 &
     &      fluid1%istack_ele_fld_smp, intg_point_t_evo,                &
     &      iphys%i_maxwell, fld_ele1%ntot_phys, iv_upw,                &
     &      fld_ele1%d_fld, coef_lor, fem1_wk, f1_nl)
!
      else if(i_field .eq. iphys%i_SGS_div_m_flux) then 
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux_upw                             &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, nod_fld1,    &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_velo, iphys%i_SGS_m_flux, ifilter_final,          &
     &        ak_diff(1,iak_diff_mf), fld_ele1%ntot_phys, iv_upw,       &
     &        fld_ele1%d_fld, coef_nega_v, fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_div_tsr_w_const_upw                              &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_SGS_m_flux, fld_ele1%ntot_phys, iv_upw,           &
     &        fld_ele1%d_fld, coef_nega_v, fem1_wk, f1_nl)
        end if
!
      else if(i_field .eq. iphys%i_SGS_Lorentz) then
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux_upw                             &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, FEM1_elen, nod_fld1,    &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_magne, iphys%i_SGS_maxwell, ifilter_final,        &
     &        ak_diff(1,iak_diff_lor), fld_ele1%ntot_phys, iv_upw,      &
     &        fld_ele1%d_fld, coef_lor, fem1_wk, mhd_fem1_wk, f1_nl)
        else
          call int_vol_div_tsr_w_const_upw                              &
     &       (node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1,               &
     &        fluid1%istack_ele_fld_smp, intg_point_t_evo,              &
     &        iphys%i_SGS_maxwell, fld_ele1%ntot_phys, iv_upw,          &
     &        fld_ele1%d_fld, coef_lor, fem1_wk, f1_nl)
        end if
      end if
!
      end subroutine int_vol_velo_monitor_upwind
!
!-----------------------------------------------------------------------
!
      end module int_vol_velo_monitor
