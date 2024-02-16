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
!!      subroutine int_vol_velo_monitor_pg(i_field, FEM_prm, SGS_param, &
!!     &         cmt_param, node, ele, fluid, fl_prop, cd_prop,         &
!!     &         iphys_base, iphys_frc, iphys_div_frc,                  &
!!     &         iphys_fil, iphys_fil_frc, iphys_SGS, iphys_div_SGS,    &
!!     &         nod_fld, iphys_ele_base, ak_MHD, g_FEM, jac_3d,        &
!!     &         rhs_tbl, FEM_elens, diff_coefs,                        &
!!     &         mhd_fem_wk, fem_wk, f_nl, ele_fld)
!!      subroutine int_vol_velo_monitor_upwind(i_field, iv_upw, dt,     &
!!     &         FEM_prm, SGS_param, cmt_param, node, ele, fluid,       &
!!     &         fl_prop, cd_prop, iphys_base, iphys_frc, iphys_div_frc,&
!!     &         iphys_fil, iphys_fil_frc, iphys_SGS, iphys_div_SGS,    &
!!     &         nod_fld, iphys_ele_base, ak_MHD, g_FEM, jac_3d,        &
!!     &         rhs_tbl, FEM_elens, diff_coefs,                        &
!!     &         mhd_fem_wk, fem_wk, f_nl, ele_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_force_address), intent(in) :: iphys_frc
!!        type(base_force_address), intent(in) :: iphys_div_frc
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(base_force_address), intent(in) :: iphys_fil_frc
!!        type(SGS_term_address), intent(in) :: iphys_SGS_term
!!        type(SGS_term_address), intent(in) :: iphys_div_SGS
!!        type(phys_data), intent(in) :: nod_fld
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
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
      use m_geometry_constants
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_base_field_labels
      use t_base_force_labels
      use t_SGS_term_labels
      use t_fem_gauss_int_coefs
      use t_jacobians
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
      subroutine int_vol_velo_monitor_pg(i_field, FEM_prm, SGS_param,   &
     &          cmt_param, node, ele, fluid, fl_prop, cd_prop,          &
     &          iphys_base, iphys_frc, iphys_div_frc,                   &
     &          iphys_fil, iphys_fil_frc, iphys_SGS, iphys_div_SGS,     &
     &          nod_fld, iphys_ele_base, ak_MHD, g_FEM, jac_3d,         &
     &          rhs_tbl, FEM_elens, diff_coefs             ,            &
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
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_force_address), intent(in) :: iphys_frc
      type(base_force_address), intent(in) :: iphys_div_frc
      type(base_field_address), intent(in) :: iphys_fil
      type(base_force_address), intent(in) :: iphys_fil_frc
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(SGS_term_address), intent(in) :: iphys_div_SGS
!
      type(phys_data), intent(in) :: nod_fld
      type(base_field_address), intent(in) :: iphys_ele_base
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
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
      if(i_field .eq. iphys_frc%i_m_advect) then
        if (FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
          call int_vol_rot_inertia                                      &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys_base%i_velo, ele_fld%ntot_phys,                     &
     &        iphys_ele_base%i_vort, ele_fld%d_fld,                     &
     &        fl_prop%coef_nega_v, fem_wk, f_nl)
        else
          call int_vol_vector_inertia                                   &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys_base%i_velo, ele_fld%ntot_phys,                     &
     &        iphys_ele_base%i_velo, ele_fld%d_fld,                     &
     &        fl_prop%coef_nega_v, fem_wk, f_nl)
        end if
!
      else if(i_field .eq. iphys_div_frc%i_m_flux) then
        call int_vol_div_tsr_w_const                                    &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,                 &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys_frc%i_m_flux, fl_prop%coef_nega_v, fem_wk, f_nl)
      end if
!
      if(i_field .eq. iphys_frc%i_coriolis) then
        call int_vol_coriolis_pg                                        &
     &     (node, ele, fluid, fl_prop, g_FEM, jac_3d, rhs_tbl, nod_fld, &
     &      FEM_prm%npoint_t_evo_int, iphys_base%i_velo, fem_wk, f_nl)
      end if
!
      if(i_field .eq. iphys_frc%i_buoyancy) then
        call int_vol_buoyancy_pg                                        &
     &     (node, ele, g_FEM, jac_3d, fl_prop, rhs_tbl, nod_fld,        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys_base%i_temp, ak_MHD%ak_buo, fem_wk, f_nl)
      else if(i_field .eq. iphys_frc%i_comp_buo) then
        call int_vol_buoyancy_pg                                        &
     &     (node, ele, g_FEM, jac_3d, fl_prop, rhs_tbl, nod_fld,        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys_base%i_light, ak_MHD%ak_comp_buo, fem_wk, f_nl)
      else if(i_field .eq. iphys_fil_frc%i_buoyancy) then
        call int_vol_buoyancy_pg                                        &
     &     (node, ele, g_FEM, jac_3d, fl_prop, rhs_tbl, nod_fld,        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys_fil%i_temp, ak_MHD%ak_buo, fem_wk, f_nl)
      else if(i_field .eq. iphys_fil_frc%i_comp_buo) then
        call int_vol_buoyancy_pg                                        &
     &     (node, ele, g_FEM, jac_3d, fl_prop, rhs_tbl, nod_fld,        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys_fil%i_light, ak_MHD%ak_comp_buo, fem_wk, f_nl)
      end if
!
      if(i_field .eq. iphys_frc%i_m_tension) then
        call int_vol_Lorentz_pg(node, ele,                              &
     &      fl_prop, cd_prop, g_FEM, jac_3d, rhs_tbl, nod_fld,          &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys_base%i_magne, ele_fld%ntot_phys,                      &
     &      iphys_ele_base%i_magne, ele_fld%d_fld,                      &
     &      fem_wk, mhd_fem_wk, f_nl)
      else if(i_field .eq. iphys_frc%i_lorentz) then
        if (FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
          call int_vol_full_rot_Lorentz_pg(node, ele,                   &
     &        fl_prop, cd_prop, g_FEM, jac_3d, rhs_tbl, nod_fld,        &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys_base%i_vecp, ele_fld%ntot_phys,                     &
     &        iphys_ele_base%i_magne, ele_fld%d_fld,                    &
     &        fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_full_Lorentz_pg(node, ele,                       &
     &        fl_prop, cd_prop, g_FEM, jac_3d, rhs_tbl, nod_fld,        &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys_base%i_magne, ele_fld%ntot_phys,                    &
     &        iphys_ele_base%i_magne, ele_fld%d_fld, fem_wk, f_nl)
        end if
      end if
!
      if(i_field .eq. iphys_div_frc%i_maxwell) then
        call int_vol_div_tsr_w_const                                    &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,                 &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,         &
     &      iphys_frc%i_maxwell, fl_prop%coef_lor, fem_wk, f_nl)
!
      else if(i_field .eq. iphys_div_SGS%i_SGS_m_flux) then
        if(SGS_param%SGS_momentum%iflag_commute_flux                    &
     &      .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux(node, ele, nod_fld, g_FEM,      &
     &        jac_3d, rhs_tbl, FEM_elens, diff_coefs%Cdiff_SGS_mf,      &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys_base%i_velo, iphys_SGS%i_SGS_m_flux,                &
     &        SGS_param%ifilter_final, fl_prop%coef_nega_v,             &
     &        fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_tsr_w_const                                  &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys_SGS%i_SGS_m_flux, fl_prop%coef_nega_v,              &
     &        fem_wk, f_nl)
        end if
!
      else if(i_field .eq. iphys_SGS%i_SGS_Lorentz) then
        if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux(node, ele, nod_fld, g_FEM,      &
     &        jac_3d, rhs_tbl, FEM_elens, diff_coefs%Cdiff_SGS_lor,     &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys_base%i_magne, iphys_SGS%i_SGS_maxwell,              &
     &        SGS_param%ifilter_final, fl_prop%coef_lor,                &
     &        fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_tsr_w_const                                  &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys_SGS%i_SGS_maxwell, fl_prop%coef_lor,                &
     &        fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_velo_monitor_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_velo_monitor_upwind(i_field, iv_upw, dt,       &
     &          FEM_prm, SGS_param, cmt_param, node, ele, fluid,        &
     &          fl_prop, cd_prop, iphys_base, iphys_frc, iphys_div_frc, &
     &          iphys_fil, iphys_fil_frc, iphys_SGS, iphys_div_SGS,     &
     &          nod_fld, iphys_ele_base, ak_MHD, g_FEM, jac_3d,         &
     &          rhs_tbl, FEM_elens, diff_coefs, mhd_fem_wk, fem_wk,     &
     &          f_nl, ele_fld)
!
      use int_vol_inertia
      use int_vol_vect_cst_diff_upw
      use int_vol_SGS_div_flux
      use int_vol_buoyancy
      use int_vol_coriolis
      use int_vol_Lorentz
!
      integer(kind = kint), intent(in) :: i_field, iv_upw
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_force_address), intent(in) :: iphys_frc
      type(base_force_address), intent(in) :: iphys_div_frc
      type(base_field_address), intent(in) :: iphys_fil
      type(base_force_address), intent(in) :: iphys_fil_frc
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(SGS_term_address), intent(in) :: iphys_div_SGS
!
      type(phys_data), intent(in) :: nod_fld
      type(base_field_address), intent(in) :: iphys_ele_base
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
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
      if(i_field .eq. iphys_frc%i_m_advect) then
        if (FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
          call int_vol_rot_inertia_upw                                  &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys_base%i_velo, ele_fld%ntot_phys,                     &
     &        iphys_ele_base%i_vort, iv_upw, ele_fld%d_fld,             &
     &        fl_prop%coef_nega_v, fem_wk, f_nl)
        else
          call int_vol_vector_inertia_upw                               &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys_base%i_velo, ele_fld%ntot_phys,                     &
     &        iphys_ele_base%i_velo, iv_upw, ele_fld%d_fld,             &
     &        fl_prop%coef_nega_v, fem_wk, f_nl)
        end if
!
      else if(i_field .eq. iphys_div_frc%i_m_flux) then
        call int_vol_div_tsr_w_const_upw                                &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,                 &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys_frc%i_m_flux, ele_fld%ntot_phys, iv_upw,              &
     &      ele_fld%d_fld, fl_prop%coef_nega_v, fem_wk, f_nl)
      end if
!
      if(i_field .eq. iphys_frc%i_coriolis) then
        call int_vol_coriolis_upw                                       &
     &     (node, ele, fluid, fl_prop, g_FEM, jac_3d, rhs_tbl, nod_fld, &
     &      FEM_prm%npoint_t_evo_int, dt, iphys_base%i_velo,            &
     &      ele_fld%ntot_phys, iv_upw, ele_fld%d_fld, fem_wk, f_nl)
      end if
!
      if(i_field .eq. iphys_frc%i_buoyancy) then
        call int_vol_buoyancy_upw                                       &
     &     (node, ele, g_FEM, jac_3d, fl_prop, rhs_tbl, nod_fld,        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys_base%i_temp, ak_MHD%ak_buo, ele_fld%ntot_phys,        &
     &      iv_upw, ele_fld%d_fld, fem_wk, f_nl)
      else if(i_field .eq. iphys_frc%i_comp_buo) then
        call int_vol_buoyancy_upw                                       &
     &     (node, ele, g_FEM, jac_3d, fl_prop, rhs_tbl, nod_fld,        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys_base%i_light, ak_MHD%ak_comp_buo, ele_fld%ntot_phys,  &
     &      iv_upw, ele_fld%d_fld, fem_wk, f_nl)
      else if(i_field .eq. iphys_fil_frc%i_buoyancy) then
        call int_vol_buoyancy_upw                                       &
     &     (node, ele, g_FEM, jac_3d, fl_prop, rhs_tbl, nod_fld,        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys_fil%i_temp, ak_MHD%ak_buo, ele_fld%ntot_phys,         &
     &      iv_upw, ele_fld%d_fld, fem_wk, f_nl)
      else if(i_field .eq. iphys_fil_frc%i_comp_buo) then
        call int_vol_buoyancy_upw                                       &
     &     (node, ele, g_FEM, jac_3d, fl_prop, rhs_tbl, nod_fld,        &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys_fil%i_light, ak_MHD%ak_comp_buo,                      &
     &      ele_fld%ntot_phys, iv_upw, ele_fld%d_fld, fem_wk, f_nl)
      end if
!
!
      if(i_field .eq. iphys_frc%i_m_tension) then
        call int_vol_Lorentz_upw(node, ele,                             &
     &      fl_prop, cd_prop, g_FEM, jac_3d, rhs_tbl, nod_fld,          &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys_base%i_magne, ele_fld%ntot_phys,                      &
     &      iphys_ele_base%i_magne, iv_upw, ele_fld%d_fld,              &
     &      fem_wk, mhd_fem_wk, f_nl)
      else if(i_field .eq. iphys_frc%i_lorentz) then
        if (FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
          call int_vol_full_rot_Lorentz_pg(node, ele,                   &
     &        fl_prop, cd_prop, g_FEM, jac_3d, rhs_tbl, nod_fld,        &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int,       &
     &        iphys_base%i_vecp, ele_fld%ntot_phys,                     &
     &        iphys_ele_base%i_magne, ele_fld%d_fld,                    &
     &        fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_full_Lorentz_upw(node, ele,                      &
     &        fl_prop, cd_prop, g_FEM, jac_3d, rhs_tbl, nod_fld,        &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys_base%i_magne, ele_fld%ntot_phys,                    &
     &        iphys_ele_base%i_magne, iv_upw, ele_fld%d_fld,            &
     &        fem_wk, f_nl)
        end if
      end if
!
!
      if(i_field .eq. iphys_div_frc%i_maxwell)  then
        call int_vol_div_tsr_w_const_upw                                &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,                 &
     &      fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,     &
     &      iphys_frc%i_maxwell, ele_fld%ntot_phys, iv_upw,             &
     &      ele_fld%d_fld, fl_prop%coef_lor, fem_wk, f_nl)
!
      else if(i_field .eq. iphys_div_SGS%i_SGS_m_flux) then 
        if(SGS_param%SGS_momentum%iflag_commute_flux                    &
     &        .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux_upw(node, ele, nod_fld, g_FEM,  &
     &        jac_3d, rhs_tbl, FEM_elens, diff_coefs%Cdiff_SGS_mf,      &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys_base%i_velo, iphys_SGS%i_SGS_m_flux,                &
     &        SGS_param%ifilter_final,                                  &
     &        ele_fld%ntot_phys, iv_upw, ele_fld%d_fld,                 &
     &        fl_prop%coef_nega_v, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_tsr_w_const_upw                              &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys_SGS%i_SGS_m_flux, ele_fld%ntot_phys, iv_upw,        &
     &        ele_fld%d_fld, fl_prop%coef_nega_v, fem_wk, f_nl)
        end if
!
      else if(i_field .eq. iphys_SGS%i_SGS_Lorentz) then
        if (cmt_param%iflag_c_lorentz .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_tsr_flux_upw(node, ele, nod_fld, g_FEM,  &
     &        jac_3d, rhs_tbl, FEM_elens, diff_coefs%Cdiff_SGS_lor,     &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys_base%i_magne, iphys_SGS%i_SGS_maxwell,              &
     &        SGS_param%ifilter_final,                                  &
     &        ele_fld%ntot_phys, iv_upw, ele_fld%d_fld,                 &
     &        fl_prop%coef_lor, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_tsr_w_const_upw                              &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
     &        fluid%istack_ele_fld_smp, FEM_prm%npoint_t_evo_int, dt,   &
     &        iphys_SGS%i_SGS_maxwell, ele_fld%ntot_phys, iv_upw,       &
     &        ele_fld%d_fld, fl_prop%coef_lor, fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_velo_monitor_upwind
!
!-----------------------------------------------------------------------
!
      end module int_vol_velo_monitor
