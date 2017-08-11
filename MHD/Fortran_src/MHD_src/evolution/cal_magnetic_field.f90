!
!      module cal_magnetic_field
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!!      subroutine cal_vector_potential                                 &
!!     &         (dt, FEM_prm, SGS_par, mesh, group, surf, conduct,     &
!!     &          cd_prop, Bnod_bcs, Asf_bcs, Fsf_bcs, iphys, iphys_ele,&
!!     &          ele_fld, jacobians, rhs_tbl, icomp_sgs,               &
!!     &          ifld_diff, iphys_elediff, sgs_coefs, diff_coefs,      &
!!     &          FEM_filters, m_lump, mlump_cd, Bmatrix, Fmatrix,      &
!!     &          ak_d_magne, MGCG_WK, FEM_SGS_wk, mhd_fem_wk,          &
!!     &          rhs_mat, fem_sq, nod_fld)
!!      subroutine s_cal_magnetic_field(dt, FEM_prm, SGS_par,           &
!!     &          mesh, group, surf, conduct, cd_prop, Bnod_bcs,        &
!!     &          Asf_bcs, Bsf_bcs, Fsf_bcs, iphys, iphys_ele, ele_fld, &
!!     &          jacobians, rhs_tbl, icomp_sgs, ifld_diff,             &
!!     &          iphys_elediff, sgs_coefs, sgs_coefs_nod, diff_coefs,  &
!!     &          FEM_filters, m_lump, mlump_cd, Bmatrix, Fmatrix,      &
!!     &          ak_d_magne, MGCG_WK, FEM_SGS_wk, mhd_fem_wk,          &
!!     &          rhs_mat, fem_sq, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(surface_data), intent(in) :: surf
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(SGS_terms_address), intent(in) :: icomp_sgs
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_terms_address), intent(in) :: iphys_elediff
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(lumped_mass_matrices), intent(in) :: mlump_cd
!!        type(MHD_MG_matrix), intent(in) :: Bmatrix
!!        type(MHD_MG_matrix), intent(in) :: Fmatrix
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      module cal_magnetic_field
!
      use m_precision
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_data
      use t_phys_data
      use t_phys_address
      use t_table_FEM_const
      use t_MHD_finite_element_mat
      use t_FEM_MHD_filter_data
      use t_bc_data_magne
      use t_surface_bc_data
      use t_material_property
      use t_SGS_model_coefs
      use t_solver_djds_MHD
      use t_MGCG_data
      use t_FEM_MHD_mean_square
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
!
      implicit none
!
      real(kind = kreal) :: ave_mp0, rms_mp0
      private :: ave_mp0, rms_mp0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_vector_potential                                   &
     &         (dt, FEM_prm, SGS_par, mesh, group, surf, conduct,       &
     &          cd_prop, Bnod_bcs, Asf_bcs, Fsf_bcs, iphys, iphys_ele,  &
     &          ele_fld, jacobians, rhs_tbl, icomp_sgs,                 &
     &          ifld_diff, iphys_elediff, sgs_coefs, diff_coefs,        &
     &          FEM_filters, m_lump, mlump_cd, Bmatrix, Fmatrix,        &
     &          ak_d_magne, MGCG_WK, FEM_SGS_wk, mhd_fem_wk,            &
     &          rhs_mat, fem_sq, nod_fld)
!
      use cal_vector_potential_pre
      use cal_mod_vel_potential
      use cal_sol_pressure_MHD
      use init_4_sol_potentials
      use int_rms_div_MHD
      use int_norm_div_MHD
      use cal_rms_potentials
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_matrices), intent(in) :: mlump_cd
      type(MHD_MG_matrix), intent(in) :: Bmatrix
      type(MHD_MG_matrix), intent(in) :: Fmatrix
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: ak_d_magne(mesh%ele%numele)
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      integer(kind=kint ) :: iloop
      real(kind = kreal) :: rel_correct
!
!
      call init_sol_potential                                           &
     &   (mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    dt, cd_prop%coef_mag_p, nod_fld%ntot_phys,                    &
     &    iphys%i_m_phi, iphys%i_mag_p, nod_fld%d_fld)
!
!     --------------------- 
!
      if (iflag_debug .gt. 0)  write(*,*) 'vector_p_pre'
      call cal_vector_p_pre(ifld_diff%i_magne, icomp_sgs%i_induction,   &
     &    iphys_elediff%i_velo, ak_d_magne, dt, FEM_prm,                &
     &    SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,         &
     &    mesh%nod_comm, mesh%node, mesh%ele, surf, conduct,            &
     &    group%surf_grp, cd_prop, Bnod_bcs, Asf_bcs, iphys,            &
     &    iphys_ele, ele_fld, jacobians, rhs_tbl,                       &
     &    FEM_filters%FEM_elens, sgs_coefs, diff_coefs,                 &
     &    FEM_filters%filtering, mlump_cd, Bmatrix, MGCG_WK%MG_vector,  &
     &    FEM_SGS_wk%wk_filter, mhd_fem_wk, rhs_mat%fem_wk,             &
     &    rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
!     --------------------- 
!
      iloop = -1
      call int_norm_div_a_monitor(iloop, mesh%node, mesh%ele,           &
     &    iphys, nod_fld, jacobians%jac_3d, fem_sq%j_ave,               &
     &    rhs_mat%fem_wk,fem_sq%msq, rel_correct)
!      call int_rms_div_a_monitor(iloop, mesh%node, mesh%ele,           &
!     &    iphys, nod_fld, jacobians%jac_3d, fem_sq%i_rms,              &
!     &    rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!
      call init_sol_potential                                           &
     &   (mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    dt, cd_prop%coef_mag_p, nod_fld%ntot_phys,                    &
     &    iphys%i_m_phi, iphys%i_mag_p,nod_fld%d_fld)
!
      do iloop = 0, FEM_prm%maxiter_coulomb
!
        if (iflag_debug.gt.0) write(*,*) 'cal_electric_potential'
        call cal_electric_potential(ifld_diff%i_magne,                  &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      mesh%node, mesh%ele, surf, group%surf_grp,                  &
     &      Bnod_bcs, Asf_bcs, Fsf_bcs, iphys, jacobians, rhs_tbl,      &
     &      FEM_filters%FEM_elens, diff_coefs, Fmatrix,                 &
     &      MGCG_WK%MG_vector, rhs_mat%fem_wk, rhs_mat%surf_wk,         &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
        if (iflag_debug.gt.0) write(*,*) 'cal_sol_m_potential', iloop
        call cal_sol_m_potential                                        &
     &     (mesh%node%numnod, mesh%node%istack_internal_smp,            &
     &      nod_fld%ntot_phys, iphys%i_m_phi, iphys%i_mag_p,            &
     &      nod_fld%d_fld)
!
        if (iflag_debug.gt.0) write(*,*) 'vector_potential_correct'
        call cal_vector_p_co(ifld_diff%i_magne, ak_d_magne, dt,         &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      mesh%nod_comm, mesh%node, mesh%ele, surf, conduct,          &
     &      group%surf_grp, cd_prop, Bnod_bcs, Fsf_bcs, iphys,          &
     &      iphys_ele, ele_fld, jacobians, rhs_tbl,                     &
     &      FEM_filters%FEM_elens, diff_coefs, m_lump, Bmatrix,         &
     &      MGCG_WK%MG_vector, mhd_fem_wk, rhs_mat%fem_wk,              &
     &      rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld) 
!
!
        if (iflag_debug.gt.0) write(*,*) 'cal_rms_scalar_potential'
        call cal_rms_scalar_potential(iloop, mesh%ele%istack_ele_smp,   &
     &      iphys%i_mag_p, fem_sq%i_rms%i_mag_p, fem_sq%j_ave%i_mag_p,  &
     &      mesh%node, mesh%ele, nod_fld,                               &
     &      jacobians%jac_3d, jacobians%jac_3d_l, rhs_mat%fem_wk,       &
     &      fem_sq%msq, rel_correct, ave_mp0, rms_mp0)
!
        if (iflag_debug.eq.1)                                           &
     &         write(12,*) 'average and RMS of potential correction: ', &
     &         iloop, ave_mp0, rms_mp0
!
        if (iflag_debug.gt.0) write(*,*) 'int_norm_div_a_monitor'
        call int_norm_div_a_monitor(iloop, mesh%node, mesh%ele,         &
     &      iphys, nod_fld, jacobians%jac_3d, fem_sq%j_ave,             &
     &      rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!        call int_rms_div_a_monitor(iloop, mesh%node, mesh%ele,         &
!     &      iphys, nod_fld, jacobians%jac_3d, fem_sq%i_rms,            &
!     &      rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!
        if(abs(rel_correct) .lt. FEM_prm%eps_4_coulomb) exit
      end do
!
      end subroutine cal_vector_potential
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_magnetic_field(dt, FEM_prm, SGS_par,             &
     &          mesh, group, surf, conduct, cd_prop, Bnod_bcs,          &
     &          Asf_bcs, Bsf_bcs, Fsf_bcs, iphys, iphys_ele, ele_fld,   &
     &          jacobians, rhs_tbl, icomp_sgs, ifld_diff,               &
     &          iphys_elediff, sgs_coefs, sgs_coefs_nod, diff_coefs,    &
     &          FEM_filters, m_lump, mlump_cd, Bmatrix, Fmatrix,        &
     &          ak_d_magne, MGCG_WK, FEM_SGS_wk, mhd_fem_wk,            &
     &          rhs_mat, fem_sq, nod_fld)
!
      use cal_magnetic_pre
      use cal_sol_pressure_MHD
      use init_4_sol_potentials
      use int_rms_div_MHD
      use int_norm_div_MHD
      use cal_mod_vel_potential
      use cal_rms_potentials
      use skip_comment_f
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_matrices), intent(in) :: mlump_cd
      type(MHD_MG_matrix), intent(in) :: Bmatrix
      type(MHD_MG_matrix), intent(in) :: Fmatrix
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: ak_d_magne(mesh%ele%numele)
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
      integer(kind=kint) :: iloop, maxiter_insulater
      real(kind = kreal) :: rel_correct
!
!
      if (cmp_no_case(FEM_prm%condutive_group%group_name(1), 'all'))    &
     & then
        maxiter_insulater = 0
      else
        maxiter_insulater = 1
      end if
!
      call init_sol_potential                                           &
     &   (mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    dt, cd_prop%coef_mag_p, nod_fld%ntot_phys,                    &
     &    iphys%i_m_phi, iphys%i_mag_p, nod_fld%d_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_magnetic_field_pre'
      call cal_magnetic_field_pre                                       &
     &   (icomp_sgs%i_induction, ifld_diff%i_magne,                     &
     &    ifld_diff%i_induction, iphys_elediff%i_velo,                  &
     &    iphys_elediff%i_magne, ak_d_magne, dt, FEM_prm,               &
     &    SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,         &
     &    mesh%nod_comm, mesh%node, mesh%ele, surf, conduct,            &
     &    group%surf_grp, cd_prop, Bnod_bcs, Asf_bcs, Bsf_bcs,          &
     &    iphys, iphys_ele, ele_fld, jacobians, rhs_tbl,                &
     &    FEM_filters%FEM_elens, sgs_coefs, sgs_coefs_nod, diff_coefs,  &
     &    FEM_filters%filtering, mlump_cd, Bmatrix, MGCG_WK%MG_vector,  &
     &    FEM_SGS_wk%wk_filter, mhd_fem_wk, rhs_mat%fem_wk,             &
     &    rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
!----  set magnetic field in insulate layer
!
      iloop = -1
      call int_norm_div_b_monitor(iloop, mesh%node, mesh%ele,           &
     &    iphys, nod_fld, jacobians%jac_3d, fem_sq%j_ave,               &
     &    rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!
!
      do iloop = 0, FEM_prm%maxiter_coulomb
        call cal_mag_potential(ifld_diff%i_magne,                       &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      mesh%node, mesh%ele, surf, group%surf_grp,                  &
     &      Bnod_bcs, Bsf_bcs, Fsf_bcs, iphys, jacobians, rhs_tbl,      &
     &      FEM_filters%FEM_elens, diff_coefs, Fmatrix,                 &
     &      MGCG_WK%MG_vector, rhs_mat%fem_wk, rhs_mat%surf_wk,         &
     &      rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
        call cal_sol_m_potential                                        &
     &     (mesh%node%numnod, mesh%node%istack_internal_smp,            &
     &      nod_fld%ntot_phys, iphys%i_m_phi, iphys%i_mag_p,            &
     &      nod_fld%d_fld)
!
!
      if (iflag_debug.eq.1) write(*,*) 'magnetic_correction'
        call cal_magnetic_co(ifld_diff%i_magne, ak_d_magne, dt,         &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      mesh%nod_comm, mesh%node, mesh%ele, surf, conduct,          &
     &      group%surf_grp, cd_prop, Bnod_bcs, Fsf_bcs, iphys,          &
     &      iphys_ele, ele_fld, jacobians, rhs_tbl,                     &
     &      FEM_filters%FEM_elens, diff_coefs, m_lump, Bmatrix,         &
     &      MGCG_WK%MG_vector, mhd_fem_wk, rhs_mat%fem_wk,              &
     &      rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
        call cal_rms_scalar_potential(iloop, mesh%ele%istack_ele_smp,   &
     &      iphys%i_mag_p, fem_sq%i_rms%i_mag_p, fem_sq%j_ave%i_mag_p,  &
     &      mesh%node, mesh%ele, nod_fld,                               &
     &      jacobians%jac_3d, jacobians%jac_3d_l,                       &
     &      rhs_mat%fem_wk, fem_sq%msq, rel_correct, ave_mp0, rms_mp0)
!
      if (iflag_debug.eq.1)                                             &
     &         write(12,*) 'average and RMS of potential correction: ', &
     &         iloop, ave_mp0, rms_mp0
!
!
        call int_norm_div_b_monitor(iloop, mesh%node, mesh%ele,         &
     &      iphys, nod_fld, jacobians%jac_3d, fem_sq%j_ave,             &
     &      rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!        call int_rms_div_b_monitor(iloop, mesh%node, mesh%ele,         &
!     &      iphys, nod_fld, jacobians%jac_3d, fem_sq%i_rms,            &
!     &      rhs_mat%fem_wk,fem_sq%msq, rel_correct)
!
        if (abs(rel_correct) .lt. FEM_prm%eps_4_coulomb) exit
      end do
!
      end subroutine s_cal_magnetic_field
!
!-----------------------------------------------------------------------
!
      end module cal_magnetic_field
