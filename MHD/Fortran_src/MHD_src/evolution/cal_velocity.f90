!
!      module cal_velocity
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!!      subroutine velocity_evolution                                   &
!!     &        (time, dt, FEM_prm, SGS_par, nod_comm, node, ele, surf, &
!!     &         fluid, sf_grp, sf_grp_nod, fl_prop, cd_prop,           &
!!     &         Vnod_bcs, Vsf_bcs, Bsf_bcs, Psf_bcs, iphys,            &
!!     &         iphys_ele, ak_MHD, fem_int, FEM_elens,                 &
!!     &         ifld_sgs, icomp_sgs, ifld_diff, iphys_elediff,         &
!!     &         sgs_coefs_nod, diff_coefs, filtering, layer_tbl,       &
!!     &         mlump_fl, Vmatrix, Pmatrix, MGCG_WK, wk_lsq, wk_sgs,   &
!!     &         wk_filter, mhd_fem_wk, rhs_mat, nod_fld, ele_fld,      &
!!     &         sgs_coefs, fem_sq)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Vsf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: ifld_sgs
!!        type(SGS_terms_address), intent(in) :: icomp_sgs
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_terms_address), intent(in) :: iphys_elediff
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(MHD_MG_matrix), intent(in) :: Vmatrix
!!        type(MHD_MG_matrix), intent(in) :: Pmatrix
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_data), intent(inout) :: ele_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      module cal_velocity
!
      use m_precision
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_layering_ele_list
      use t_bc_data_velo
      use t_surface_bc_data
      use t_material_property
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_solver_djds_MHD
      use t_physical_property
      use t_MGCG_data
      use t_MHD_finite_element_mat
      use t_work_FEM_integration
      use t_FEM_MHD_mean_square
!
      implicit none
!
      real(kind = kreal) :: ave_pr0, rms_pr0
      private :: ave_pr0, rms_pr0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine velocity_evolution                                     &
     &        (time, dt, FEM_prm, SGS_par, nod_comm, node, ele, surf,   &
     &         fluid, sf_grp, sf_grp_nod, fl_prop, cd_prop,             &
     &         Vnod_bcs, Vsf_bcs, Bsf_bcs, Psf_bcs, iphys,              &
     &         iphys_ele, ak_MHD, fem_int, FEM_elens,                   &
     &         ifld_sgs, icomp_sgs, ifld_diff, iphys_elediff,           &
     &         sgs_coefs_nod, diff_coefs, filtering, layer_tbl,         &
     &         mlump_fl, Vmatrix, Pmatrix, MGCG_WK, wk_lsq, wk_sgs,     &
     &         wk_filter, mhd_fem_wk, rhs_mat, nod_fld, ele_fld,        &
     &         sgs_coefs, fem_sq)
!
      use cal_velocity_pre
      use cal_mod_vel_potential
      use cal_sol_pressure_MHD
      use init_4_sol_potentials
      use int_rms_div_MHD
      use int_norm_div_MHD
      use cal_rms_potentials
!
      real(kind = kreal), intent(in) :: time, dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Vsf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(potential_surf_bc_type), intent(in) :: Psf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(filtering_data_type), intent(in) :: filtering
      type(layering_tbl), intent(in) :: layer_tbl
      type(lumped_mass_matrices), intent(in) :: mlump_fl
      type(MHD_MG_matrix), intent(in) :: Vmatrix
      type(MHD_MG_matrix), intent(in) :: Pmatrix
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      integer(kind=kint) :: iloop
      real(kind = kreal) :: rel_correct
!
!
      if (fl_prop%iflag_4_lorentz .eq. id_turn_ON) then
        if (FEM_prm%iflag_rotate_form .eq. id_turn_OFF) then
          call cal_sol_pressure_w_mag_ene                               &
     &      (dt, node%numnod, node%istack_internal_smp,                 &
     &       fl_prop%coef_press, fl_prop%acoef_press, fl_prop%coef_lor, &
     &       nod_fld%ntot_phys, iphys%i_p_phi, iphys%i_magne,           &
     &       iphys%i_press, nod_fld%d_fld)
        else if (cd_prop%iflag_magneto_cv .eq. id_turn_ON               &
     &     .and. FEM_prm%iflag_rotate_form .eq. id_turn_OFF) then
          call cal_sol_pressure_mcv                                     &
     &       (dt, node%numnod, node%istack_internal_smp,                &
     &        fl_prop%coef_press, fl_prop%acoef_press,                  &
     &        fl_prop%coef_lor, cd_prop%ex_magne,                       &
     &        nod_fld%ntot_phys, iphys%i_p_phi, iphys%i_magne,          &
     &        iphys%i_press, nod_fld%d_fld)
        else
          call init_sol_potential(node%numnod, node%istack_nod_smp,     &
     &        dt, fl_prop%coef_press, nod_fld%ntot_phys, iphys%i_p_phi, &
     &        iphys%i_press, nod_fld%d_fld)
        end if
      else
        call init_sol_potential(node%numnod, node%istack_nod_smp,       &
     &      dt, fl_prop%coef_press, nod_fld%ntot_phys, iphys%i_p_phi,   &
     &      iphys%i_press, nod_fld%d_fld)
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1)  write(*,*) 's_cal_velocity_pre'
      call s_cal_velocity_pre(time, dt, FEM_prm, SGS_par,               &
     &    nod_comm, node, ele, surf, fluid, sf_grp, sf_grp_nod,         &
     &    fl_prop, cd_prop, Vnod_bcs, Vsf_bcs, Bsf_bcs, iphys,          &
     &    iphys_ele, ak_MHD, fem_int, FEM_elens,                        &
     &    ifld_sgs, icomp_sgs, ifld_diff, iphys_elediff, sgs_coefs_nod, &
     &    diff_coefs, filtering, layer_tbl, mlump_fl,                   &
     &    Vmatrix, MGCG_WK%MG_vector, wk_lsq, wk_sgs, wk_filter,        &
     &    mhd_fem_wk, rhs_mat, nod_fld, ele_fld, sgs_coefs)
!
!     --------------------- 
!
      iloop = -1
      call int_norm_div_v_monitor(iloop, node, ele, fluid,              &
     &    iphys, nod_fld, fem_int%jcs%jac_3d, fem_sq%j_ave,             &
     &    rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!      call int_rms_div_v_monitor(iloop, node, ele, fluid,              &
!     &    iphys, nod_fld, fem_int%jcs%jac_3d, fem_sqi_rms,             &
!     &    rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!
      do iloop = 0, FEM_prm%maxiter_stokes
        call cal_mod_potential(ifld_diff%i_velo,                        &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      node, ele, surf, fluid, sf_grp, Vnod_bcs, Vsf_bcs, Psf_bcs, &
     &      iphys, fem_int%jcs, fem_int%rhs_tbl,                        &
     &      FEM_elens, diff_coefs, Pmatrix, MGCG_WK%MG_vector,          &
     &      rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, &
     &      nod_fld)
!
        call cal_sol_pressure                                           &
     &     (dt, node%numnod, node%istack_internal_smp,                  &
     &      fl_prop%acoef_press, nod_fld%ntot_phys,                     &
     &      iphys%i_p_phi, iphys%i_press,  nod_fld%d_fld)
!
        call cal_velocity_co(time, dt, FEM_prm, SGS_par,                &
     &      nod_comm, node, ele, surf, fluid, sf_grp, sf_grp_nod,       &
     &      fl_prop, Vnod_bcs, Vsf_bcs, Psf_bcs, iphys, iphys_ele,      &
     &      ele_fld, ak_MHD, fem_int, FEM_elens, ifld_diff, diff_coefs, &
     &      mlump_fl, Vmatrix, MGCG_WK%MG_vector,                       &
     &      mhd_fem_wk, rhs_mat, nod_fld)
!
!
        call cal_rms_scalar_potential                                   &
     &     (iloop, fluid%istack_ele_fld_smp, iphys%i_press,             &
     &      fem_sq%i_rms%i_press, fem_sq%j_ave%i_press, node, ele,      &
     &      nod_fld,  fem_int%jcs%jac_3d, fem_int%jcs%jac_3d_l,         &
     &      rhs_mat%fem_wk, fem_sq%msq, rel_correct, ave_pr0, rms_pr0)
!
!
        if (iflag_debug.eq.1)                                           &
     &         write(12,*) 'average and RMS of presssur correction: ',  &
     &         iloop, ave_pr0, rms_pr0
!
!
        call int_norm_div_v_monitor(iloop, node, ele, fluid,            &
     &      iphys, nod_fld, fem_int%jcs%jac_3d, fem_sq%j_ave,           &
     &      rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!        call int_rms_div_v_monitor(iloop, node, ele, fluid,            &
!     &      iphys, nod_fld, fem_int%jcs%jac_3d, fem_sq%i_rms,          &
!     &      rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!
        if (abs(rel_correct) .lt. FEM_prm%eps_4_stokes) go to 10
!
      end do
 10   continue
!
      if (FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
        call cal_sol_pressure_rotate                                    &
     &     (node%numnod, node%istack_internal_smp, nod_fld%ntot_phys,   &
     &      iphys%i_velo, iphys%i_press, nod_fld%d_fld)
      end if
!
      end subroutine velocity_evolution
!
!-----------------------------------------------------------------------
!
      end module cal_velocity
