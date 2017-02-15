!
!      module cal_magnetic_field
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H.Matsui on July, 2006
!
!!      subroutine cal_vector_potential                                 &
!!     &         (FEM_prm, SGS_par, nod_comm, node, ele, surf, conduct, &
!!     &          sf_grp, cd_prop, Bnod_bcs, Asf_bcs, Fsf_bcs,          &
!!     &          iphys, iphys_ele, ele_fld,                            &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,       &
!!     &          rhs_tbl, FEM_elens, icomp_sgs, ifld_diff,             &
!!     &          iphys_elediff, sgs_coefs, diff_coefs, filtering,      &
!!     &          m_lump, Bmatrix, Fmatrix, ak_d_magne, wk_filter,      &
!!     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!!      subroutine s_cal_magnetic_field                                 &
!!     &         (FEM_prm, SGS_par, nod_comm, node, ele, surf, conduct, &
!!     &          sf_grp, cd_prop, Bnod_bcs, Asf_bcs, Bsf_bcs, Fsf_bcs, &
!!     &          iphys, iphys_ele, ele_fld,                            &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,       &
!!     &          rhs_tbl, FEM_elens, icomp_sgs, ifld_diff,             &
!!     &          iphys_elediff, sgs_coefs, sgs_coefs_nod,              &
!!     &          diff_coefs, filtering, m_lump, Bmatrix, Fmatrix,      &
!!     &          ak_d_magne, wk_filter, mhd_fem_wk, fem_wk, surf_wk,   &
!!     &          f_l, f_nl, nod_fld)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(potential_surf_bc_type), intent(in) :: Fsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: icomp_sgs
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_terms_address), intent(in) :: iphys_elediff
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(MHD_MG_matrix), intent(in) :: Bmatrix
!!        type(MHD_MG_matrix), intent(in) :: Fmatrix
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_magnetic_field
!
      use m_precision
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_bc_data_magne
      use t_surface_bc_data
      use t_material_property
      use t_SGS_model_coefs
      use t_solver_djds_MHD
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
     &         (FEM_prm, SGS_par, nod_comm, node, ele, surf, conduct,   &
     &          sf_grp, cd_prop, Bnod_bcs, Asf_bcs, Fsf_bcs,            &
     &          iphys, iphys_ele, ele_fld,                              &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,         &
     &          rhs_tbl, FEM_elens, icomp_sgs, ifld_diff,               &
     &          iphys_elediff, sgs_coefs, diff_coefs, filtering,        &
     &          m_lump, Bmatrix, Fmatrix, ak_d_magne, wk_filter,        &
     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
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
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(filtering_data_type), intent(in) :: filtering
      type(MHD_MG_matrix), intent(in) :: Bmatrix
      type(MHD_MG_matrix), intent(in) :: Fmatrix
!
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind=kint ) :: iloop
      real(kind = kreal) :: rel_correct
!
!
      call init_sol_potential(node%numnod, node%istack_nod_smp,         &
     &    cd_prop%coef_mag_p, nod_fld%ntot_phys,                        &
     &    iphys%i_m_phi, iphys%i_mag_p, nod_fld%d_fld)
!
!     --------------------- 
!
      if (iflag_debug .gt. 0)  write(*,*) 'vector_p_pre'
      call cal_vector_p_pre(ifld_diff%i_magne,                          &
     &   icomp_sgs%i_induction, iphys_elediff%i_velo, ak_d_magne,       &
     &   FEM_prm, SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p, &
     &   nod_comm, node, ele, surf, conduct,                            &
     &   sf_grp, cd_prop, Bnod_bcs, Asf_bcs, iphys, iphys_ele,          &
     &   ele_fld, jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens,           &
     &   sgs_coefs, diff_coefs, filtering, Bmatrix, MG_vector,          &
     &   wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
!     --------------------- 
!
      iloop = -1
      call int_norm_div_a_monitor(iloop, node, ele,                     &
     &    iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!      call int_rms_div_a_monitor(iloop, node, ele,                     &
!     &    iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!
      call init_sol_potential(node%numnod, node%istack_nod_smp,         &
     &    cd_prop%coef_mag_p, nod_fld%ntot_phys,                        &
     &    iphys%i_m_phi, iphys%i_mag_p,nod_fld%d_fld)
!
      do iloop = 0, FEM_prm%maxiter_coulomb
!
        if (iflag_debug.gt.0) write(*,*) 'cal_electric_potential'
        call cal_electric_potential(ifld_diff%i_magne,                  &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      node, ele, surf, sf_grp, Bnod_bcs, Asf_bcs, Fsf_bcs,        &
     &      iphys, jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl,           &
     &      FEM_elens, diff_coefs, Fmatrix, MG_vector,                  &
     &      fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
        if (iflag_debug.gt.0) write(*,*) 'cal_sol_m_potential', iloop
        call cal_sol_m_potential                                        &
     &     (node%numnod, node%istack_internal_smp, nod_fld%ntot_phys,   &
     &      iphys%i_m_phi, iphys%i_mag_p, nod_fld%d_fld)
!
        if (iflag_debug.gt.0) write(*,*) 'vector_potential_correct'
        call cal_vector_p_co(ifld_diff%i_magne, ak_d_magne,             &
     &      evo_vect_p, FEM_prm, SGS_par%model_p, SGS_par%commute_p,    &
     &      nod_comm, node, ele, surf, conduct, sf_grp, cd_prop,        &
     &      Bnod_bcs, Fsf_bcs, iphys, iphys_ele, ele_fld,               &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,    &
     &      FEM_elens, diff_coefs, m_lump, Bmatrix, MG_vector,          &
     &      mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
!
        if (iflag_debug.gt.0) write(*,*) 'cal_rms_scalar_potential'
        call cal_rms_scalar_potential(iloop, ele%istack_ele_smp,        &
     &      iphys%i_mag_p, i_rms%i_mag_p, j_ave%i_mag_p,                &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rel_correct, ave_mp0, rms_mp0)
!
        if (iflag_debug.eq.1)                                           &
     &         write(12,*) 'average and RMS of potential correction: ', &
     &         iloop, ave_mp0, rms_mp0
!
        if (iflag_debug.gt.0) write(*,*) 'int_norm_div_a_monitor'
        call int_norm_div_a_monitor(iloop, node, ele,                   &
     &      iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!        call int_rms_div_a_monitor(iloop, node, ele,                   &
!     &      iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!
        if(abs(rel_correct) .lt. FEM_prm%eps_4_coulomb) exit
      end do
!
      end subroutine cal_vector_potential
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_magnetic_field                                   &
     &         (FEM_prm, SGS_par, nod_comm, node, ele, surf, conduct,   &
     &          sf_grp, cd_prop, Bnod_bcs, Asf_bcs, Bsf_bcs, Fsf_bcs,   &
     &          iphys, iphys_ele, ele_fld,                              &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l,         &
     &          rhs_tbl, FEM_elens, icomp_sgs, ifld_diff,               &
     &          iphys_elediff, sgs_coefs, sgs_coefs_nod,                &
     &          diff_coefs, filtering, m_lump, Bmatrix, Fmatrix,        &
     &          ak_d_magne, wk_filter, mhd_fem_wk, fem_wk, surf_wk,     &
     &          f_l, f_nl, nod_fld)
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
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(nodal_bcs_4_induction_type), intent(in) :: Bnod_bcs
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(potential_surf_bc_type), intent(in) :: Fsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q, jac_sf_grp_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: icomp_sgs
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_terms_address), intent(in) :: iphys_elediff
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(filtering_data_type), intent(in) :: filtering
      type(MHD_MG_matrix), intent(in) :: Bmatrix
      type(MHD_MG_matrix), intent(in) :: Fmatrix
!
      real(kind = kreal), intent(in) :: ak_d_magne(ele%numele)
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
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
      call init_sol_potential(node%numnod, node%istack_nod_smp,         &
     &    cd_prop%coef_mag_p, nod_fld%ntot_phys,                        &
     &    iphys%i_m_phi, iphys%i_mag_p, nod_fld%d_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_magnetic_field_pre'
      call cal_magnetic_field_pre(icomp_sgs%i_induction,                &
     &   ifld_diff%i_magne, ifld_diff%i_induction,                      &
     &   iphys_elediff%i_velo, iphys_elediff%i_magne, ak_d_magne,       &
     &   FEM_prm, SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p, &
     &   nod_comm, node, ele, surf, conduct, sf_grp, cd_prop,           &
     &   Bnod_bcs, Asf_bcs, Bsf_bcs, iphys, iphys_ele, ele_fld,         &
     &   jac_3d_q, jac_sf_grp_q, rhs_tbl, FEM_elens,                    &
     &   sgs_coefs, sgs_coefs_nod, diff_coefs, filtering,               &
     &   Bmatrix, MG_vector, wk_filter, mhd_fem_wk, fem_wk, surf_wk,    &
     &   f_l, f_nl, nod_fld)
!
!----  set magnetic field in insulate layer
!
      iloop = -1
      call int_norm_div_b_monitor(iloop, node, ele,                     &
     &    iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!
!
      do iloop = 0, FEM_prm%maxiter_coulomb
        call cal_mag_potential(ifld_diff%i_magne,                       &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p,                &
     &      node, ele, surf, sf_grp, Bnod_bcs, Bsf_bcs, Fsf_bcs,        &
     &      iphys, jac_3d_q, jac_3d_l, jac_sf_grp_l, rhs_tbl,           &
     &      FEM_elens, diff_coefs, Fmatrix, MG_vector,                  &
     &      fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
        call cal_sol_m_potential                                        &
     &     (node%numnod, node%istack_internal_smp, nod_fld%ntot_phys,   &
     &      iphys%i_m_phi, iphys%i_mag_p, nod_fld%d_fld)
!
!
      if (iflag_debug.eq.1) write(*,*) 'magnetic_correction'
        call cal_magnetic_co(ifld_diff%i_magne, ak_d_magne,             &
     &      evo_magne, FEM_prm, SGS_par%model_p, SGS_par%commute_p,     &
     &      nod_comm, node, ele, surf, conduct, sf_grp, cd_prop,        &
     &      Bnod_bcs, Fsf_bcs, iphys, iphys_ele, ele_fld,               &
     &      jac_3d_q, jac_3d_l, jac_sf_grp_q, jac_sf_grp_l, rhs_tbl,    &
     &      FEM_elens, diff_coefs, m_lump, Bmatrix, MG_vector,          &
     &      mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
        call cal_rms_scalar_potential(iloop, ele%istack_ele_smp,        &
     &      iphys%i_mag_p, i_rms%i_mag_p, j_ave%i_mag_p,                &
     &      node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk,             &
     &      rel_correct, ave_mp0, rms_mp0)
!
      if (iflag_debug.eq.1)                                             &
     &         write(12,*) 'average and RMS of potential correction: ', &
     &         iloop, ave_mp0, rms_mp0
!
!
        call int_norm_div_b_monitor(iloop, node, ele,                   &
     &      iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!        call int_rms_div_b_monitor(iloop, node, ele,                   &
!     &      iphys, nod_fld, jac_3d_q, fem_wk, rel_correct)
!
        if (abs(rel_correct) .lt. FEM_prm%eps_4_coulomb) exit
      end do
!
      end subroutine s_cal_magnetic_field
!
!-----------------------------------------------------------------------
!
      end module cal_magnetic_field
