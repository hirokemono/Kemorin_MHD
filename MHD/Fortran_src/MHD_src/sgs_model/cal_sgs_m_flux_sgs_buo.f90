!cal_sgs_m_flux_sgs_buo.f90
!      module cal_sgs_m_flux_sgs_buo
!
!      written by H. Matsui on Aug., 2007
!
!!      subroutine cal_sgs_mom_flux_with_sgs_buo(dt, FEM_prm, SGS_par,  &
!!     &          nod_comm, node, ele, surf, fluid, layer_tbl, sf_grp,  &
!!     &          fl_prop, cd_prop, Vsf_bcs, Bsf_bcs, iphys, iphys_LES, &
!!     &          iphys_ele_base, ak_MHD, fem_int, FEM_elens, filtering,&
!!     &          Csim_SGS_tbuo, Csim_SGS_cbuo, diff_coefs, mlump_fl,   &
!!     &          wk_filter, wk_lsq, wk_sgs, mhd_fem_wk, rhs_mat,       &
!!     &          nod_fld, ele_fld, Csim_SGS_mf, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(SGS_model_coefficient), intent(in) :: Csim_SGS_tbuo
!!        type(SGS_model_coefficient), intent(in) :: Csim_SGS_cbuo
!!        type(SGS_commutation_coefs), intent(in) :: diff_coefs
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(dynamic_least_suare_data), intent(inout) :: wk_lsq
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(SGS_model_coefficient), intent(inout) :: Csim_SGS_mf
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_sgs_m_flux_sgs_buo
!
      use m_precision
!
      use m_phys_constants
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_base_field_labels
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_material_property
      use t_SGS_model_coefs
      use t_layering_ele_list
      use t_surface_bc_vector
      use t_surface_bc_velocity
      use t_physical_property
      use t_MHD_finite_element_mat
      use t_work_FEM_integration
      use t_vector_for_solver
      use t_solver_SR
!
      implicit none
!
      private :: select_int_vol_sgs_buoyancy
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_mom_flux_with_sgs_buo(dt, FEM_prm, SGS_par,    &
     &          nod_comm, node, ele, surf, fluid, layer_tbl, sf_grp,    &
     &          fl_prop, cd_prop, Vsf_bcs, Bsf_bcs, iphys, iphys_LES,   &
     &          iphys_ele_base, ak_MHD, fem_int, FEM_elens, filtering,  &
     &          Csim_SGS_tbuo, Csim_SGS_cbuo, diff_coefs, mlump_fl,     &
     &          wk_filter, wk_lsq, wk_sgs, mhd_fem_wk, rhs_mat,         &
     &          nod_fld, ele_fld, Csim_SGS_mf, v_sol, SR_sig, SR_r)
!
      use m_phys_constants
!
      use cal_sgs_fluxes
      use cal_momentum_terms
      use products_nodal_fields_smp
      use cal_sgs_buoyancy_flux
      use merge_dynamic_coefs
      use set_sgs_diff_model_coefs
      use int_rms_ave_ele_grps
      use modify_Csim_by_SGS_buo_ele
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(base_field_address), intent(in) :: iphys_ele_base
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(layering_tbl), intent(in) :: layer_tbl
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_model_coefficient), intent(in) :: Csim_SGS_tbuo
      type(SGS_model_coefficient), intent(in) :: Csim_SGS_cbuo
!
      type(SGS_commutation_coefs), intent(in) :: diff_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(SGS_model_coefficient), intent(inout) :: Csim_SGS_mf
      type(filtering_work_type), intent(inout) :: wk_filter
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint), parameter :: ncomp_sgs_buo= 6
!      integer(kind = kint) :: i, k
!
!
!   lead SGS momentum flux using original model coefficient
!
      call sel_model_coefs_2_ele(ele, layer_tbl%e_grp,                  &
     &    SGS_par%model_p%SGS_momentum%itype_Csym_flux,                 &
     &    wk_sgs, Csim_SGS_mf)
!
      call cal_sgs_momentum_flux(dt, FEM_prm, SGS_par%model_p,          &
     &    SGS_par%filter_p, nod_comm, node, ele, fluid, iphys%base,     &
     &    iphys_LES%filter_fld, iphys_LES%SGS_term, iphys_LES%SGS_wk,   &
     &    iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,        &
     &    FEM_elens, filtering, Csim_SGS_mf, mlump_fl, wk_filter,       &
     &    mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl,        &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!   lead work of Reynolds stress
!
      call cal_terms_4_momentum(iphys_LES%div_SGS%i_SGS_m_flux,         &
     &    dt, FEM_prm, SGS_par%model_p, SGS_par%commute_p,              &
     &    nod_comm, node, ele, surf, sf_grp, fluid, fl_prop, cd_prop,   &
     &    Vsf_bcs, Bsf_bcs, iphys%base, iphys%forces, iphys%div_forces, &
     &    iphys%diffusion, iphys_LES%filter_fld,                        &
     &    iphys_LES%force_by_filter, iphys_LES%SGS_term,                &
     &    iphys_LES%div_SGS, iphys_ele_base, ak_MHD, fem_int,           &
     &    FEM_elens, diff_coefs, mlump_fl, mhd_fem_wk, rhs_mat,         &
     &    nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!
!$omp parallel
      call cal_phys_dot_product                                         &
     &   (iphys%base%i_velo, iphys_LES%div_SGS%i_SGS_m_flux,            &
     &    iphys_LES%SGS_ene_flux%i_reynolds_wk, nod_fld)
!$omp end parallel
!
!   lead SGS buoyancy flux
!
      if(fl_prop%iflag_4_gravity) then
        call cal_SGS_gravity_flux                                       &
     &     (node, fl_prop%i_grav, fl_prop%coef_buo, fl_prop%grav,       &
     &      iphys_LES%SGS_term%i_SGS_h_flux,                            &
     &      iphys_LES%SGS_ene_flux%i_SGS_buo_wk, nod_fld)
      end if
      if(fl_prop%iflag_4_composit_buo) then
        call cal_SGS_gravity_flux                                       &
     &     (node, fl_prop%i_grav, fl_prop%coef_comp_buo, fl_prop%grav,  &
     &      iphys_LES%SGS_term%i_SGS_c_flux,                            &
     &      iphys_LES%SGS_ene_flux%i_SGS_comp_buo_wk, nod_fld)
       end if
!
!   take RMS of SGS buoyancy flux and work of Reynolds stress
      call select_int_vol_sgs_buoyancy                                  &
     &   (FEM_prm%npoint_t_evo_int, node, ele, fl_prop, layer_tbl,      &
     &    iphys_LES%SGS_ene_flux, nod_fld, fem_int%jcs%g_FEM,           &
     &    fem_int%jcs%jac_3d, fem_int%jcs%jac_3d_l,                     &
     &    wk_lsq%nlayer, wk_lsq%slocal)
!
      call sum_lsq_coefs_4_comps(ncomp_sgs_buo, wk_lsq)
!
!   Parameterize model coeffisient including SGS Buoyancy
!
      if(fl_prop%iflag_4_gravity) then
!        call cal_Csim_buo_by_Reynolds_ratio(wk_sgs%nlayer, ifive,      &
!     &      wk_sgs%num_kinds, wk_sgs%ntot_comp,                        &
!     &      Csim_SGS_tbuo%iak_Csim, Csim_SGS_tbuo%icomp_Csim,          &
!     &      wk_lsq%slsq, wk_sgs%comp_coef, wk_sgs%fld_coef)
        call single_Csim_buo_by_mf_ratio(wk_sgs%nlayer, ifive,          &
     &      wk_sgs%num_kinds, wk_sgs%ntot_comp,                         &
     &      Csim_SGS_tbuo%iak_Csim, Csim_SGS_tbuo%icomp_Csim,           &
     &      wk_lsq%slsq, wk_sgs%comp_coef, wk_sgs%fld_coef)
        call clippging_sgs_diff_coefs(SGS_par%iflag_SGS_initial,        &
     &      SGS_par%model_p, ncomp_sgs_buo,                             &
     &      Csim_SGS_tbuo%iak_Csim, Csim_SGS_tbuo%icomp_Csim, wk_sgs)
      end if
      if(fl_prop%iflag_4_composit_buo) then
!        call cal_Csim_buo_by_Reynolds_ratio(wk_sgs%nlayer, isix,       &
!     &      wk_sgs%num_kinds, wk_sgs%ntot_comp,                        &
!     &      Csim_SGS_cbuo%iak_Csim, Csim_SGS_cbuo%icomp_Csim,          &
!     &      wk_lsq%slsq, wk_sgs%comp_coef, wk_sgs%fld_coef)
        call single_Csim_buo_by_mf_ratio(wk_sgs%nlayer, isix,           &
     &      wk_sgs%num_kinds, wk_sgs%ntot_comp,                         &
     &      Csim_SGS_cbuo%iak_Csim, Csim_SGS_cbuo%icomp_Csim,           &
     &      wk_lsq%slsq, wk_sgs%comp_coef, wk_sgs%fld_coef)
        call clippging_sgs_diff_coefs                                   &
     &     (SGS_par%iflag_SGS_initial, SGS_par%model_p, ncomp_sgs_buo,  &
     &      Csim_SGS_cbuo%iak_Csim, Csim_SGS_cbuo%icomp_Csim, wk_sgs)
      end if
!
      call mod_Csim_by_SGS_buoyancy_ele                                 &
     &   (SGS_par%model_p, ele, layer_tbl%e_grp, fl_prop,               &
     &    Csim_SGS_tbuo%icomp_Csim, Csim_SGS_cbuo%icomp_Csim,           &
     &    wk_sgs, Csim_SGS_mf)
!
      end subroutine cal_sgs_mom_flux_with_sgs_buo
!
!  ---------------------------------------------------------------------
!
      subroutine select_int_vol_sgs_buoyancy(num_int, node, ele,        &
     &          fl_prop, layer_tbl, iphys_sef, nod_fld,                 &
     &          g_FEM, jac_3d_q, jac_3d_l, n_layer_d, sgs_l)
!
      use int_rms_ave_ele_grps
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(fluid_property), intent(in) :: fl_prop
      type(SGS_ene_flux_address), intent(in) :: iphys_sef
      type(layering_tbl), intent(in) :: layer_tbl
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(phys_data), intent(in) :: nod_fld
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: n_layer_d
!
      real(kind= kreal), intent(inout) :: sgs_l(n_layer_d,18)
!
!
!
!   take RMS of SGS buoyancy flux and work of Reynolds stress
      if(fl_prop%iflag_4_gravity) then
        call int_vol_2rms_ave_ele_grps(node, ele, layer_tbl%e_grp,      &
     &      g_FEM, jac_3d_q, jac_3d_l, num_int,                         &
     &      nod_fld%ntot_phys, iphys_sef%i_reynolds_wk,                 &
     &      nod_fld%d_fld, nod_fld%ntot_phys, iphys_sef%i_SGS_buo_wk,   &
     &      nod_fld%d_fld, sgs_l(1,1), sgs_l(1,4), sgs_l(1,2),          &
     &      sgs_l(1,5) )
!
        if(fl_prop%iflag_4_composit_buo) then
          call int_vol_rms_ave_ele_grps                                 &
     &       (node, ele, layer_tbl%e_grp, g_FEM, jac_3d_q, jac_3d_l,    &
     &        num_int, nod_fld%ntot_phys, iphys_sef%i_SGS_comp_buo_wk,  &
     &        nod_fld%d_fld, sgs_l(1,3), sgs_l(1,6))
        end if
      else if(fl_prop%iflag_4_composit_buo) then
        call int_vol_2rms_ave_ele_grps(node, ele, layer_tbl%e_grp,      &
     &      g_FEM, jac_3d_q, jac_3d_l, num_int, nod_fld%ntot_phys,      &
     &      iphys_sef%i_reynolds_wk, nod_fld%d_fld, nod_fld%ntot_phys,  &
     &      iphys_sef%i_SGS_comp_buo_wk, nod_fld%d_fld,                 &
     &      sgs_l(1,1), sgs_l(1,4), sgs_l(1,3), sgs_l(1,6))
      end if
!
      end subroutine select_int_vol_sgs_buoyancy
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_m_flux_sgs_buo
