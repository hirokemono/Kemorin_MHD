!
!      module cal_magnetic_field
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!!      subroutine cal_vector_potential                                 &
!!     &         (dt, FEM_prm, SGS_par, mesh, group, conduct,           &
!!     &          cd_prop, Bnod_bcs, Asf_bcs, Fsf_bcs,                  &
!!     &          iphys, iphys_LES, iphys_ele_base, ele_fld, fem_int,   &
!!     &          Csims_FEM_MHD, FEM_filters, mk_MHD, Bmatrix, Fmatrix, &
!!     &          ak_d_magne, MGCG_WK, FEM_SGS_wk, mhd_fem_wk, rhs_mat, &
!!     &          fem_sq, nod_fld, m_SR)
!!      subroutine s_cal_magnetic_field                                 &
!!     &         (dt, FEM_prm, SGS_par, mesh, group, conduct,           &
!!     &          cd_prop, Bnod_bcs, Asf_bcs, Bsf_bcs, Fsf_bcs,         &
!!     &          iphys, iphys_LES, iphys_ele_base, ele_fld, fem_int,   &
!!     &          Csims_FEM_MHD, FEM_filters, mk_MHD, Bmatrix, Fmatrix, &
!!     &          ak_d_magne, MGCG_WK, FEM_SGS_wk, mhd_fem_wk, rhs_mat, &
!!     &          fem_sq, nod_fld, m_SR)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_coefficients_data), intent(in) :: Csims_FEM_MHD
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(MHD_MG_matrix), intent(in) :: Bmatrix
!!        type(MHD_MG_matrix), intent(in) :: Fmatrix
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!        type(mesh_SR), intent(inout) :: m_SR
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
      use t_base_field_labels
      use t_SGS_model_addresses
      use t_table_FEM_const
      use t_jacobians
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_FEM_MHD_filter_data
      use t_bc_data_magne
      use t_surface_bc_scalar
      use t_surface_bc_vector
      use t_surface_bc_velocity
      use t_material_property
      use t_SGS_model_coefs
      use t_solver_djds_MHD
      use t_MGCG_data
      use t_FEM_MHD_mean_square
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
      use t_FEM_SGS_model_coefs
      use t_mesh_SR
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
     &         (dt, FEM_prm, SGS_par, mesh, group, conduct,             &
     &          cd_prop, Bnod_bcs, Asf_bcs, Fsf_bcs,                    &
     &          iphys, iphys_LES, iphys_ele_base, ele_fld, fem_int,     &
     &          Csims_FEM_MHD, FEM_filters, mk_MHD, Bmatrix, Fmatrix,   &
     &          ak_d_magne, MGCG_WK, FEM_SGS_wk, mhd_fem_wk, rhs_mat,   &
     &          fem_sq, nod_fld, m_SR)
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
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(SGS_coefficients_data), intent(in) :: Csims_FEM_MHD
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
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
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind=kint ) :: iloop
      real(kind = kreal) :: rel_correct
!
!
      call init_sol_potential                                           &
     &   (mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    dt, cd_prop%coef_mag_p, nod_fld%ntot_phys,                    &
     &    iphys%exp_work%i_m_phi, iphys%base%i_mag_p, nod_fld%d_fld)
!
!     --------------------- 
!
      if (iflag_debug .gt. 0)  write(*,*) 'vector_p_pre'
      call cal_vector_p_pre(ak_d_magne, dt, FEM_prm,                    &
     &    SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,         &
     &    mesh, conduct, group, cd_prop, Bnod_bcs, Asf_bcs,             &
     &    iphys, iphys_LES, iphys_ele_base, ele_fld,                    &
     &    fem_int%jcs, fem_int%rhs_tbl, Csims_FEM_MHD, FEM_filters,     &
     &    mk_MHD%mlump_cd, Bmatrix, MGCG_WK%MG_vector,                  &
     &    FEM_SGS_wk%wk_filter, mhd_fem_wk, rhs_mat, nod_fld,           &
     &    m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
!     --------------------- 
!
      iloop = -1
      call int_norm_div_a_monitor(iloop, mesh%node, mesh%ele,           &
     &    iphys, nod_fld, fem_int%jcs, fem_sq%i_msq,                    &
     &    rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!      call int_rms_div_a_monitor(iloop, mesh%node, mesh%ele,           &
!     &    iphys, nod_fld, fem_int%jcs, fem_sq%i_msq,                   &
!     &    rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!
      call init_sol_potential                                           &
     &   (mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    dt, cd_prop%coef_mag_p, nod_fld%ntot_phys,                    &
     &    iphys%exp_work%i_m_phi, iphys%base%i_mag_p, nod_fld%d_fld)
!
      do iloop = 0, FEM_prm%maxiter_coulomb
!
        if (iflag_debug.gt.0) write(*,*) 'cal_electric_potential'
        call cal_electric_potential                                     &
     &    (FEM_prm, SGS_par%model_p, SGS_par%commute_p,                 &
     &     mesh, group, Bnod_bcs, Asf_bcs, Fsf_bcs, iphys,              &
     &     fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens,         &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%diff_coefs,       &
     &     Fmatrix, MGCG_WK%MG_vector, rhs_mat%fem_wk, rhs_mat%surf_wk, &
     &     rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                          &
     &     m_SR%v_sol, m_SR%SR_sig, m_SR%SR_rr)
!
        if (iflag_debug.gt.0) write(*,*) 'cal_sol_m_potential', iloop
        call cal_sol_m_potential                                        &
     &     (mesh%node%numnod, mesh%node%istack_internal_smp,            &
     &      nod_fld%ntot_phys, iphys%exp_work%i_m_phi,                  &
     &      iphys%base%i_mag_p, nod_fld%d_fld)
!
        if (iflag_debug.gt.0) write(*,*) 'vector_potential_correct'
        call cal_vector_p_co(Csims_FEM_MHD%iak_diff_base, ak_d_magne,   &
     &      dt, FEM_prm, SGS_par%model_p, SGS_par%commute_p,            &
     &      mesh, conduct, group, cd_prop, Bnod_bcs, Fsf_bcs,           &
     &      iphys%base, iphys%exp_work, iphys_ele_base, ele_fld,        &
     &      fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens,        &
     &      Csims_FEM_MHD%diff_coefs, fem_int%m_lump, Bmatrix,          &
     &      MGCG_WK%MG_vector, mhd_fem_wk, rhs_mat, nod_fld,            &
     &      m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
!
        if (iflag_debug.gt.0) write(*,*) 'cal_rms_scalar_potential'
        call cal_rms_scalar_potential                                   &
     &     (iloop, mesh%ele%istack_ele_smp, iphys%base%i_mag_p,         &
     &      fem_sq%i_msq%imsq_mag_p, fem_sq%i_msq%jave_mag_p,           &
     &      mesh, nod_fld, fem_int%jcs, rhs_mat%fem_wk,                 &
     &      fem_sq%msq, rel_correct, ave_mp0, rms_mp0)
!
        if (iflag_debug.eq.1)                                           &
     &         write(12,*) 'average and RMS of potential correction: ', &
     &         iloop, ave_mp0, rms_mp0
!
        if (iflag_debug.gt.0) write(*,*) 'int_norm_div_a_monitor'
        call int_norm_div_a_monitor(iloop, mesh%node, mesh%ele,         &
     &      iphys, nod_fld, fem_int%jcs, fem_sq%i_msq,                  &
     &      rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!        call int_rms_div_a_monitor(iloop, mesh%node, mesh%ele,         &
!     &      iphys, nod_fld, fem_int%jcs, fem_sq%i_msq,                 &
!     &      rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!
        if(abs(rel_correct) .lt. FEM_prm%eps_4_coulomb) exit
      end do
!
      end subroutine cal_vector_potential
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_magnetic_field                                   &
     &         (dt, FEM_prm, SGS_par, mesh, group, conduct,             &
     &          cd_prop, Bnod_bcs, Asf_bcs, Bsf_bcs, Fsf_bcs,           &
     &          iphys, iphys_LES, iphys_ele_base, ele_fld, fem_int,     &
     &          Csims_FEM_MHD, FEM_filters, mk_MHD, Bmatrix, Fmatrix,   &
     &          ak_d_magne, MGCG_WK, FEM_SGS_wk, mhd_fem_wk, rhs_mat,   &
     &          fem_sq, nod_fld, m_SR)
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
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(SGS_coefficients_data), intent(in) :: Csims_FEM_MHD
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
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
      type(mesh_SR), intent(inout) :: m_SR
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
     &    iphys%exp_work%i_m_phi, iphys%base%i_mag_p, nod_fld%d_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_magnetic_field_pre'
      call cal_magnetic_field_pre(ak_d_magne, dt, FEM_prm,              &
     &    SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p,         &
     &    mesh, conduct, group, cd_prop, Bnod_bcs, Asf_bcs, Bsf_bcs,    &
     &    iphys, iphys_LES, iphys_ele_base, ele_fld,                    &
     &    fem_int%jcs, fem_int%rhs_tbl, Csims_FEM_MHD, FEM_filters,     &
     &    mk_MHD%mlump_cd, Bmatrix, MGCG_WK%MG_vector,                  &
     &    FEM_SGS_wk%wk_filter, mhd_fem_wk, rhs_mat, nod_fld,           &
     &    m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
!----  set magnetic field in insulate layer
!
      iloop = -1
      call int_norm_div_b_monitor(iloop, mesh%node, mesh%ele,           &
     &    iphys, nod_fld, fem_int%jcs, fem_sq%i_msq,                    &
     &    rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!
!
      do iloop = 0, FEM_prm%maxiter_coulomb
        call cal_mag_potential                                          &
     &    (FEM_prm, SGS_par%model_p, SGS_par%commute_p,                 &
     &     mesh%node, mesh%ele, mesh%surf, group%surf_grp,              &
     &     Bnod_bcs, Bsf_bcs, Fsf_bcs, iphys,                           &
     &     fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens,         &
     &     Csims_FEM_MHD%iak_diff_base, Csims_FEM_MHD%diff_coefs,       &
     &     Fmatrix, MGCG_WK%MG_vector, rhs_mat%fem_wk, rhs_mat%surf_wk, &
     &     rhs_mat%f_l, rhs_mat%f_nl, nod_fld,                          &
     &     m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
        call cal_sol_m_potential                                        &
     &     (mesh%node%numnod, mesh%node%istack_internal_smp,            &
     &      nod_fld%ntot_phys, iphys%exp_work%i_m_phi,                  &
     &      iphys%base%i_mag_p, nod_fld%d_fld)
!
!
      if (iflag_debug.eq.1) write(*,*) 'magnetic_correction'
        call cal_magnetic_co(ak_d_magne, dt, FEM_prm,                   &
     &      SGS_par%model_p, SGS_par%commute_p,                         &
     &      mesh, conduct, group, cd_prop, Bnod_bcs, Fsf_bcs,           &
     &      iphys, iphys_ele_base, ele_fld,                             &
     &      fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens,        &
     &      Csims_FEM_MHD, fem_int%m_lump, Bmatrix, MGCG_WK%MG_vector,  &
     &      mhd_fem_wk, rhs_mat,nod_fld,                                &
     &      m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
        call cal_rms_scalar_potential                                   &
     &     (iloop, mesh%ele%istack_ele_smp, iphys%base%i_mag_p,         &
     &      fem_sq%i_msq%imsq_mag_p, fem_sq%i_msq%jave_mag_p,           &
     &      mesh, nod_fld, fem_int%jcs, rhs_mat%fem_wk,                 &
     &      fem_sq%msq, rel_correct, ave_mp0, rms_mp0)
!
      if (iflag_debug.eq.1)                                             &
     &         write(12,*) 'average and RMS of potential correction: ', &
     &         iloop, ave_mp0, rms_mp0
!
!
        call int_norm_div_b_monitor(iloop, mesh%node, mesh%ele,         &
     &      iphys, nod_fld, fem_int%jcs, fem_sq%i_msq,                  &
     &      rhs_mat%fem_wk, fem_sq%msq, rel_correct)
!        call int_rms_div_b_monitor(iloop, mesh%node, mesh%ele,         &
!     &      iphys, nod_fld, fem_int%jcs, fem_sq%i_msq,                 &
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
