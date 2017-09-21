!
!      module cal_velocity
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!!      subroutine velocity_evolution(time, dt, FEM_prm, SGS_par,       &
!!     &          mesh, group, surf, fluid, fl_prop, cd_prop,           &
!!     &          Vnod_bcs, Vsf_bcs, Bsf_bcs, Psf_bcs, iphys,           &
!!     &          iphys_ele, ak_MHD, fem_int, FEM_filters,              &
!!     &          ifld_sgs, icomp_sgs, ifld_diff, iphys_elediff,        &
!!     &          sgs_coefs_nod, diff_coefs, mk_MHD,                    &
!!     &          Vmatrix, Pmatrix, MGCG_WK, FEM_SGS_wk, mhd_fem_wk,    &
!!     &          rhs_mat, nod_fld, ele_fld, sgs_coefs, fem_sq)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(surface_data), intent(in) :: surf
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
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_terms_address), intent(in) :: ifld_sgs
!!        type(SGS_terms_address), intent(in) :: icomp_sgs
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_terms_address), intent(in) :: iphys_elediff
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(MHD_MG_matrix), intent(in) :: Vmatrix
!!        type(MHD_MG_matrix), intent(in) :: Pmatrix
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
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
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_data
      use t_surface_group_connect
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_FEM_MHD_filter_data
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
      use t_MHD_mass_matricxes
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
      use t_FEM_MHD_mean_square
!
      implicit none
!
      real(kind = kreal) :: ave_pr0, rms_pr0
      private :: ave_pr0, rms_pr0
!
      private :: sel_init_sol_potential, sel_cal_sol_pressure_rotate
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine velocity_evolution(time, dt, FEM_prm, SGS_par,         &
     &          mesh, group, surf, fluid, fl_prop, cd_prop,             &
     &          Vnod_bcs, Vsf_bcs, Bsf_bcs, Psf_bcs, iphys,             &
     &          iphys_ele, ak_MHD, fem_int, FEM_filters,                &
     &          ifld_sgs, icomp_sgs, ifld_diff, iphys_elediff,          &
     &          sgs_coefs_nod, diff_coefs, mk_MHD,                      &
     &          Vmatrix, Pmatrix, MGCG_WK, FEM_SGS_wk, mhd_fem_wk,      &
     &          rhs_mat, nod_fld, ele_fld, sgs_coefs, fem_sq)
!
      use cal_velocity_pre
      use cal_mod_vel_potential
      use int_rms_div_MHD
      use int_norm_div_MHD
      use cal_rms_potentials
      use cal_sol_pressure_MHD
!
      real(kind = kreal), intent(in) :: time, dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(surface_data), intent(in) :: surf
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
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
      type(MHD_MG_matrix), intent(in) :: Vmatrix
      type(MHD_MG_matrix), intent(in) :: Pmatrix
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
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
      call sel_init_sol_potential                                       &
     &   (dt, FEM_prm, mesh%node, fl_prop, cd_prop, iphys, nod_fld)
!
!     --------------------- 
!
      if (iflag_debug.eq.1)  write(*,*) 's_cal_velocity_pre'
      call s_cal_velocity_pre(time, dt, FEM_prm, SGS_par,               &
     &    mesh%nod_comm, mesh%node, mesh%ele, surf,                     &
     &    fluid, group%surf_grp, group%surf_nod_grp,                    &
     &    fl_prop, cd_prop, Vnod_bcs, Vsf_bcs, Bsf_bcs, iphys,          &
     &    iphys_ele, ak_MHD, fem_int, FEM_filters%FEM_elens,            &
     &    ifld_sgs, icomp_sgs, ifld_diff, iphys_elediff, sgs_coefs_nod, &
     &    diff_coefs, FEM_filters%filtering, FEM_filters%layer_tbl,     &
     &    mk_MHD%mlump_fl, Vmatrix, MGCG_WK%MG_vector,                  &
     &    FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs, FEM_SGS_wk%wk_filter,   &
     &    mhd_fem_wk, rhs_mat, nod_fld, ele_fld, sgs_coefs)
!
!     --------------------- 
!
      iloop = -1
      call int_norm_div_v_monitor(iloop, mesh%node, mesh%ele, fluid,    &
     &    iphys, nod_fld, fem_int%jcs, fem_sq%j_ave, rhs_mat%fem_wk,    &
     &    fem_sq%msq, rel_correct)
!      call int_rms_div_v_monitor(iloop, mesh%node, mesh%ele, fluid,    &
!     &    iphys, nod_fld, fem_int%jcs, fem_sqi_rms, rhs_mat%fem_wk,    &
!     &    fem_sq%msq, rel_correct)
!
      do iloop = 0, FEM_prm%maxiter_stokes
        call cal_mod_potential(ifld_diff%i_velo,                        &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      mesh%node, mesh%ele, surf, fluid,                           &
     &      group%surf_grp, Vnod_bcs, Vsf_bcs, Psf_bcs,                 &
     &      iphys, fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens, &
     &      diff_coefs, Pmatrix, MGCG_WK%MG_vector, rhs_mat%fem_wk,     &
     &      rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
        call cal_sol_pressure                                           &
     &     (dt, mesh%node%numnod, mesh%node%istack_internal_smp,        &
     &      fl_prop%acoef_press, nod_fld%ntot_phys,                     &
     &      iphys%i_p_phi, iphys%i_press,  nod_fld%d_fld)
!
        call cal_velocity_co(time, dt, FEM_prm, SGS_par,                &
     &      mesh%nod_comm, mesh%node, mesh%ele,                         &
     &      surf, fluid, group%surf_grp, group%surf_nod_grp,            &
     &      fl_prop, Vnod_bcs, Vsf_bcs, Psf_bcs, iphys, iphys_ele,      &
     &      ele_fld, ak_MHD, fem_int, FEM_filters%FEM_elens, ifld_diff, &
     &      diff_coefs, mk_MHD%mlump_fl, Vmatrix, MGCG_WK%MG_vector,    &
     &      mhd_fem_wk, rhs_mat, nod_fld)
!
!
        call cal_rms_scalar_potential                                   &
     &     (iloop, fluid%istack_ele_fld_smp, iphys%i_press,             &
     &      fem_sq%i_rms%i_press, fem_sq%j_ave%i_press,                 &
     &      mesh, nod_fld, fem_int%jcs, rhs_mat%fem_wk,                 &
     &      fem_sq%msq, rel_correct, ave_pr0, rms_pr0)
!
!
        if (iflag_debug.eq.1)                                           &
     &         write(12,*) 'average and RMS of presssur correction: ',  &
     &         iloop, ave_pr0, rms_pr0
!
!
        call int_norm_div_v_monitor(iloop, mesh%node, mesh%ele, fluid,  &
     &      iphys, nod_fld, fem_int%jcs, fem_sq%j_ave, rhs_mat%fem_wk,  &
     &      fem_sq%msq, rel_correct)
!        call int_rms_div_v_monitor(iloop, mesh%node, mesh%ele, fluid,  &
!     &      iphys, nod_fld, fem_int%jcs, fem_sq%i_rms, rhs_mat%fem_wk, &
!     &      fem_sq%msq, rel_correct)
!
        if (abs(rel_correct) .lt. FEM_prm%eps_4_stokes) go to 10
!
      end do
 10   continue
!
      call sel_cal_sol_pressure_rotate                                 &
     &   (FEM_prm, mesh%node, iphys, nod_fld)
!
      end subroutine velocity_evolution
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_init_sol_potential                                 &
     &         (dt, FEM_prm, node, fl_prop, cd_prop, iphys, nod_fld)
!
      use cal_sol_pressure_MHD
      use init_4_sol_potentials
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
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
      end subroutine sel_init_sol_potential
!
!-----------------------------------------------------------------------
!
      subroutine sel_cal_sol_pressure_rotate                            &
     &         (FEM_prm, node, iphys, nod_fld)
!
      use cal_sol_pressure_MHD
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (FEM_prm%iflag_rotate_form .eq. id_turn_ON) then
        call cal_sol_pressure_rotate                                    &
     &     (node%numnod, node%istack_internal_smp, nod_fld%ntot_phys,   &
     &      iphys%i_velo, iphys%i_press, nod_fld%d_fld)
      end if
!
      end subroutine sel_cal_sol_pressure_rotate
!
!-----------------------------------------------------------------------
!
      end module cal_velocity
