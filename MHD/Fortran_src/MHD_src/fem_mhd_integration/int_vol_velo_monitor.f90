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
!!      subroutine int_vol_velo_monitor_pg                              &
!!     &         (i_field, iak_diff_mf, iak_diff_lor,                   &
!!     &          FEM_prm, SGS_param, cmt_param, node, ele, fluid,      &
!!     &          fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ak_MHD,  &
!!     &          jac_3d, rhs_tbl, FEM_elens, diff_coefs,               &
!!     &          mhd_fem_wk, fem_wk, f_nl, ele_fld)
!!      subroutine int_vol_velo_monitor_upwind                          &
!!     &         (i_field, iak_diff_mf, iak_diff_lor, iv_upw, dt,       &
!!     &          FEM_prm, SGS_param, cmt_param, node, ele, fluid,      &
!!     &          fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ak_MHD,  &
!!     &          jac_3d, rhs_tbl, FEM_elens, diff_coefs,               &
!!     &          mhd_fem_wk, fem_wk, f_nl, ele_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(phys_data), intent(inout) :: ele_fld
!
      module int_vol_velo_monitor
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use m_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_phys_address
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_SGS_model_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_monitor_pg                                &
     &         (i_field, iak_diff_mf, iak_diff_lor,                     &
     &          FEM_prm, SGS_param, cmt_param, node, ele, fluid,        &
     &          fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ak_MHD,    &
     &          jac_3d, rhs_tbl, FEM_elens, diff_coefs,                 &
     &          mhd_fem_wk, fem_wk, f_nl, ele_fld)
!
      use int_vol_inertia
      use int_vol_vect_cst_difference
      use int_vol_SGS_div_flux
      use int_vol_buoyancy
      use int_vol_coriolis
      use int_vol_Lorentz
!
      integer(kind=kint), intent(in) :: i_field
      integer(kind= kint), intent(in) :: iak_diff_mf, iak_diff_lor
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: ele_fld
!
!
      if(i_field .eq. iphys%i_m_advect) then
        if (FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
          call int_vol_rot_inertia                                      &
     &       (node, ele, g_FEM1, jac_3d, rhs_tbl, nod_fld,              &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys%i_velo, ele_fld%ntot_phys, iphys_ele%i_vort,        &
     &        ele_fld%d_fld, fl_prop%coef_nega_v, fem_wk, f_nl)
        else
          call int_vol_vector_inertia                                   &
     &       (node, ele, g_FEM1, jac_3d, rhs_tbl, nod_fld,              &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys%i_velo, ele_fld%ntot_phys, iphys_ele%i_velo,        &
     &        ele_fld%d_fld, fl_prop%coef_nega_v, fem_wk, f_nl)
        end if
!
      else if(i_field .eq. iphys%i_m_flux_div) then
        call int_vol_div_tsr_w_const                                    &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys%i_m_flux, fl_prop%coef_nega_v, fem_wk, f_nl)
!
      end if
!
      if(i_field .eq. iphys%i_coriolis) then
        call int_vol_coriolis_pg                                        &
     &     (node, ele, fl_prop, jac_3d, rhs_tbl, nod_fld,               &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys%i_velo, fem_wk, f_nl)
      end if
!
      if(i_field .eq. iphys%i_buoyancy) then
        call int_vol_buoyancy_pg                                        &
     &     (node, ele, g_FEM1, jac_3d, fl_prop, rhs_tbl, nod_fld,       &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys%i_temp, ak_MHD%ak_buo, fem_wk, f_nl)
      else if(i_field .eq. iphys%i_comp_buo) then
        call int_vol_buoyancy_pg                                        &
     &     (node, ele, g_FEM1, jac_3d, fl_prop, rhs_tbl, nod_fld,       &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys%i_light, ak_MHD%ak_comp_buo, fem_wk, f_nl)
      else if(i_field .eq. iphys%i_filter_buo) then
        call int_vol_buoyancy_pg                                        &
     &     (node, ele, g_FEM1, jac_3d, fl_prop, rhs_tbl, nod_fld,       &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys%i_filter_temp, ak_MHD%ak_buo, fem_wk, f_nl)
      end if
!
      if(i_field .eq. iphys%i_m_tension) then
        call int_vol_Lorentz_pg(node, ele,                              &
     &      fl_prop, cd_prop, g_FEM1, jac_3d, rhs_tbl, nod_fld,         &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys%i_magne, ele_fld%ntot_phys, iphys_ele%i_magne,        &
     &      ele_fld%d_fld, fem_wk, mhd_fem_wk, f_nl)
      else if(i_field .eq. iphys%i_lorentz) then
        if (FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
          call int_vol_full_rot_Lorentz_pg(node, ele,                   &
     &        fl_prop, cd_prop, g_FEM1, jac_3d, rhs_tbl, nod_fld,       &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys%i_vecp, ele_fld%ntot_phys, iphys_ele%i_magne,       &
     &        ele_fld%d_fld, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_full_Lorentz_pg(node, ele,                       &
     &        fl_prop, cd_prop, g_FEM1, jac_3d, rhs_tbl, nod_fld,       &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys%i_magne, ele_fld%ntot_phys, iphys_ele%i_magne,      &
     &        ele_fld%d_fld, fem_wk, f_nl)
        end if
      end if
!
      if(i_field .eq. iphys%i_maxwell_div) then
        call int_vol_div_tsr_w_const                                    &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys%i_maxwell, fl_prop%coef_lor, fem_wk, f_nl)
!
      else if(i_field .eq. iphys%i_SGS_div_m_flux) then
        if (cmt_param%iflag_c_mf .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux(node, ele, nod_fld,             &
     &        jac_3d, rhs_tbl, FEM_elens, diff_coefs,                   &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys%i_velo, iphys%i_SGS_m_flux,                         &
     &        SGS_param%ifilter_final, iak_diff_mf,                     &
     &        fl_prop%coef_nega_v, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_tsr_w_const                                  &
     &       (node, ele, jac_3d, rhs_tbl, nod_fld,                      &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys%i_SGS_m_flux, fl_prop%coef_nega_v, fem_wk, f_nl)
        end if
!
      else if(i_field .eq. iphys%i_SGS_Lorentz) then
        if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux(node, ele, nod_fld,             &
     &        jac_3d, rhs_tbl, FEM_elens, diff_coefs,                   &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys%i_magne, iphys%i_SGS_maxwell,                       &
     &        SGS_param%ifilter_final, iak_diff_lor,                    &
     &        fl_prop%coef_lor, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_tsr_w_const                                  &
     &       (node, ele, jac_3d, rhs_tbl, nod_fld,                      &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys%i_SGS_maxwell, fl_prop%coef_lor, fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_velo_monitor_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_monitor_upwind                            &
     &         (i_field, iak_diff_mf, iak_diff_lor, iv_upw, dt,         &
     &          FEM_prm, SGS_param, cmt_param, node, ele, fluid,        &
     &          fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ak_MHD,    &
     &          jac_3d, rhs_tbl, FEM_elens, diff_coefs,                 &
     &          mhd_fem_wk, fem_wk, f_nl, ele_fld)
!
      use int_vol_inertia
      use int_vol_vect_cst_diff_upw
      use int_vol_SGS_div_flux
      use int_vol_buoyancy
      use int_vol_coriolis
      use int_vol_Lorentz
!
      integer(kind = kint), intent(in) :: i_field, iv_upw
      integer(kind= kint), intent(in) :: iak_diff_mf, iak_diff_lor
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: ele_fld
!
!
      if(i_field .eq. iphys%i_m_advect) then
        if (FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
          call int_vol_rot_inertia_upw                                  &
     &       (node, ele, g_FEM1, jac_3d, rhs_tbl, nod_fld,              &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys%i_velo, ele_fld%ntot_phys, iphys_ele%i_vort,        &
     &        iv_upw, ele_fld%d_fld, fl_prop%coef_nega_v,               &
     &        fem_wk, f_nl)
        else
          call int_vol_vector_inertia_upw                               &
     &       (node, ele, g_FEM1, jac_3d, rhs_tbl, nod_fld,              &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys%i_velo, ele_fld%ntot_phys, iphys_ele%i_velo,        &
     &        iv_upw, ele_fld%d_fld, fl_prop%coef_nega_v,               &
     &        fem_wk, f_nl)
        end if
!
      else if(i_field .eq. iphys%i_m_flux_div) then
        call int_vol_div_tsr_w_const_upw                                &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys%i_m_flux, ele_fld%ntot_phys, iv_upw,                  &
     &      ele_fld%d_fld, fl_prop%coef_nega_v, fem_wk, f_nl)
      end if
!
      if(i_field .eq. iphys%i_coriolis) then
        call int_vol_coriolis_upw                                       &
     &     (node, ele, fl_prop, jac_3d, rhs_tbl, nod_fld,               &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys%i_velo, ele_fld%ntot_phys, iv_upw, ele_fld%d_fld,     &
     &      fem_wk, f_nl)
      end if
!
      if(i_field .eq. iphys%i_buoyancy) then
        call int_vol_buoyancy_upw                                       &
     &     (node, ele, g_FEM1, jac_3d, fl_prop, rhs_tbl, nod_fld,       &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys%i_temp, ak_MHD%ak_buo, ele_fld%ntot_phys,             &
     &      iv_upw, ele_fld%d_fld, fem_wk, f_nl)
      else if(i_field .eq. iphys%i_comp_buo) then
        call int_vol_buoyancy_upw                                       &
     &     (node, ele, g_FEM1, jac_3d, fl_prop, rhs_tbl, nod_fld,       &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys%i_light, ak_MHD%ak_comp_buo, ele_fld%ntot_phys,       &
     &      iv_upw, ele_fld%d_fld, fem_wk, f_nl)
      else if(i_field .eq. iphys%i_filter_buo) then
        call int_vol_buoyancy_upw                                       &
     &     (node, ele, g_FEM1, jac_3d, fl_prop, rhs_tbl, nod_fld,       &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys%i_filter_temp, ak_MHD%ak_buo, ele_fld%ntot_phys,      &
     &      iv_upw, ele_fld%d_fld, fem_wk, f_nl)
      end if
!
!
      if(i_field .eq. iphys%i_m_tension) then
        call int_vol_Lorentz_upw(node, ele,                             &
     &      fl_prop, cd_prop, g_FEM1, jac_3d, rhs_tbl, nod_fld,         &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys%i_magne, ele_fld%ntot_phys, iphys_ele%i_magne,        &
     &      iv_upw, ele_fld%d_fld, fem_wk, mhd_fem_wk, f_nl)
      else if(i_field .eq. iphys%i_lorentz) then
        if (FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
          call int_vol_full_rot_Lorentz_pg(node, ele,                   &
     &        fl_prop, cd_prop, g_FEM1, jac_3d, rhs_tbl, nod_fld,       &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys%i_vecp, ele_fld%ntot_phys, iphys_ele%i_magne,       &
     &        ele_fld%d_fld, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_full_Lorentz_upw(node, ele,                      &
     &        fl_prop, cd_prop, g_FEM1, jac_3d, rhs_tbl, nod_fld,       &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys%i_magne, ele_fld%ntot_phys,iphys_ele%i_magne,       &
     &        iv_upw, ele_fld%d_fld, fem_wk, f_nl)
        end if
      end if
!
!
      if(i_field .eq. iphys%i_maxwell_div)  then
        call int_vol_div_tsr_w_const_upw                                &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys%i_maxwell, ele_fld%ntot_phys, iv_upw,                 &
     &      ele_fld%d_fld, fl_prop%coef_lor, fem_wk, f_nl)
!
      else if(i_field .eq. iphys%i_SGS_div_m_flux) then 
        if (cmt_param%iflag_c_mf .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux_upw(node, ele, nod_fld,         &
     &        jac_3d, rhs_tbl, FEM_elens, diff_coefs,                   &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys%i_velo, iphys%i_SGS_m_flux,                         &
     &        SGS_param%ifilter_final, iak_diff_mf,                     &
     &        ele_fld%ntot_phys, iv_upw, ele_fld%d_fld,                 &
     &        fl_prop%coef_nega_v, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_tsr_w_const_upw                              &
     &       (node, ele, jac_3d, rhs_tbl, nod_fld,                      &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys%i_SGS_m_flux, ele_fld%ntot_phys, iv_upw,            &
     &        ele_fld%d_fld, fl_prop%coef_nega_v, fem_wk, f_nl)
        end if
!
      else if(i_field .eq. iphys%i_SGS_Lorentz) then
        if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux_upw(node, ele, nod_fld,         &
     &        jac_3d, rhs_tbl, FEM_elens, diff_coefs,                   &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys%i_magne, iphys%i_SGS_maxwell,                       &
     &        SGS_param%ifilter_final, iak_diff_lor,                    &
     &        ele_fld%ntot_phys, iv_upw, ele_fld%d_fld,                 &
     &        fl_prop%coef_lor, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_tsr_w_const_upw                              &
     &       (node, ele, jac_3d, rhs_tbl, nod_fld,                      &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys%i_SGS_maxwell, ele_fld%ntot_phys, iv_upw,           &
     &        ele_fld%d_fld, fl_prop%coef_lor, fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_velo_monitor_upwind
!
!-----------------------------------------------------------------------
!
      end module int_vol_velo_monitor
