!
!     module construct_matrices
!
!     Written by H. Matsui on June, 2005
!
!!      subroutine set_data_4_const_matrices                            &
!!     &         (fem, MHD_mesh, MHD_prop, fem_int,                     &
!!     &          MGCG_WK, MHD_mat_tbls, MHD_mat, s_package)
!!      subroutine update_matrices(time_d, FEM_prm, SGS_par, fem,       &
!!     &          MHD_mesh, FEM_MHD_BCs, MHD_prop, fem_int, FEM_elens,  &
!!     &          Csims_FEM_MHD, flex_p, mk_MHD, rhs_mat, MHD_CG)
!!      subroutine set_aiccg_matrices(dt, FEM_prm, SGS_par,             &
!!     &          fem, MHD_mesh, FEM_MHD_BCs, MHD_prop, fem_int,        &
!!     &          FEM_elens, Csims_FEM_MHD, mk_MHD, rhs_mat, MHD_CG)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) ::   fem
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(FEM_MHD_BC_data), intent(in) :: FEM_MHD_BCs
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(MGCG_data), intent(in) :: MGCG_WK
!!        type(SGS_coefficients_data), intent(in) :: Csims_FEM_MHD
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!
      module construct_matrices
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
!
      use t_control_parameter
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_mass_matrices
      use t_work_FEM_integration
      use t_filter_elength
      use t_material_property
      use t_SGS_model_coefs
      use t_sorted_node_MHD
      use t_FEM_MHD_boundary_data
      use t_solver_djds_MHD
      use t_MHD_matrices_pack
      use t_MGCG_data
      use t_physical_property
      use t_FEM_SGS_model_coefs
      use t_FEM_MHD_solvers
!
      implicit none
!
      private :: const_MGCG_MHD_matrices, const_MHD_aiccg_matrices
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine set_data_4_const_matrices                              &
     &         (fem, MHD_mesh, MHD_prop, fem_int,                       &
     &          MGCG_WK, MHD_mat_tbls, MHD_mat, s_package)
!
      use calypso_mpi
!
      use t_solver_djds
      use set_MHD_idx_4_mat_type
!
!
      type(mesh_data), intent(in) ::   fem
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(finite_element_integration), intent(in) :: fem_int
      type(MGCG_data), intent(in) :: MGCG_WK
!
      type(tables_MHD_mat_const), intent(inout) :: MHD_mat_tbls
      type(MHD_MG_matrices), intent(inout) :: MHD_mat
      type(MHD_matrices_pack), intent(inout) :: s_package
!
!
      call s_set_MHD_idx_4_mat_type(fem%mesh, MHD_mesh,                 &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, fem_int%rhs_tbl,          &
     &    MHD_mat%MG_DJDS_table(0), MHD_mat%MG_DJDS_fluid(0),           &
     &    MHD_mat%MG_DJDS_linear(0),                                    &
     &    MHD_mat%MG_DJDS_lin_fl(0),                                    &
     &    MHD_mat_tbls%base, MHD_mat_tbls%fluid_q,                      &
     &    MHD_mat_tbls%full_conduct_q, MHD_mat_tbls%linear,             &
     &    MHD_mat_tbls%fluid_l)
!
      call link_MG_DJDS_MHD_structures                                  &
     &   (MGCG_WK%num_MG_level, MHD_mat, s_package)
!
      end subroutine set_data_4_const_matrices
!
! ----------------------------------------------------------------------
!
      subroutine update_matrices(time_d, FEM_prm, SGS_par, fem,         &
     &          MHD_mesh, FEM_MHD_BCs, MHD_prop, fem_int, FEM_elens,    &
     &          Csims_FEM_MHD, flex_p, mk_MHD, rhs_mat, MHD_CG)
!
      use t_time_data
      use t_flex_delta_t_data
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) ::   fem
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(FEM_MHD_BC_data), intent(in) :: FEM_MHD_BCs
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(finite_element_integration), intent(in) :: fem_int

      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_data), intent(in) :: Csims_FEM_MHD
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!
      integer (kind = kint) :: iflag
!
      if (   SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF      &
     & .and. SGS_par%commute_p%iflag_c_linear .gt. id_SGS_commute_OFF   &
     & .and. mod(time_d%i_time_step,SGS_par%i_step_sgs_coefs) .eq. 0)   &
     &   then
        iflag = id_turn_ON
      else
        iflag = id_turn_OFF
      end if
      iflag = iflag + flex_p%iflag_flex_step_changed
!
      if (iflag .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'matrix assemble again'
        call set_aiccg_matrices(time_d%dt, FEM_prm, SGS_par, fem,       &
     &      MHD_mesh, FEM_MHD_BCs, MHD_prop, fem_int, FEM_elens,        &
     &      Csims_FEM_MHD, mk_MHD, rhs_mat, MHD_CG)
        flex_p%iflag_flex_step_changed = 0
      end if
!
      end subroutine update_matrices
!
!  ----------------------------------------------------------------------
!
      subroutine set_aiccg_matrices(dt, FEM_prm, SGS_par,               &
     &          fem, MHD_mesh, FEM_MHD_BCs, MHD_prop, fem_int,          &
     &          FEM_elens, Csims_FEM_MHD, mk_MHD, rhs_mat, MHD_CG)
!
      use set_aiccg_matrices_type
      use precond_djds_MHD
      use initialize_4_MHD_AMG
      use skip_comment_f
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_data), intent(in) ::   fem
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(FEM_MHD_BC_data), intent(in) :: FEM_MHD_BCs
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_data), intent(in) :: Csims_FEM_MHD
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
!
!
      call const_MHD_aiccg_matrices(izero, dt, FEM_prm,                 &
     &    SGS_par%model_p, SGS_par%commute_p, fem,                      &
     &    MHD_mesh, FEM_MHD_BCs%nod_bcs, FEM_MHD_BCs%surf_bcs,          &
     &    MHD_prop, MHD_CG%ak_MHD, fem_int, FEM_elens, Csims_FEM_MHD,   &
     &    MHD_CG%MHD_mat_tbls, mk_MHD, rhs_mat, MHD_CG%MHD_mat)
!
!     set marrix for the Multigrid
!
      if(cmp_no_case(FEM_PRM%CG11_param%METHOD, 'MGCG')) then
        call const_MGCG_MHD_matrices(MHD_prop%iflag_all_scheme, dt,     &
     &      FEM_prm, SGS_par%model_p, SGS_par%commute_p, Csims_FEM_MHD, &
     &      MHD_prop, MHD_CG%MGCG_WK, MHD_CG%MGCG_FEM,                  &
     &      MHD_CG%MGCG_MHD_FEM, MHD_CG%MHD_mat)
      end if
!
      end subroutine set_aiccg_matrices
!
! ----------------------------------------------------------------------
! ---------------------------------------------------------------------
!
      subroutine const_MGCG_MHD_matrices(iflag_scheme, dt, FEM_prm,     &
     &          SGS_param, cmt_param, Csims_FEM_MHD, MHD_prop,          &
     &          MGCG_WK, MGCG_FEM, MGCG_MHD_FEM, MHD_mat)
!
      use t_MGCG_data
      use t_MGCG_data_4_MHD
!
      integer(kind = kint), intent(in) :: iflag_scheme
      real(kind = kreal), intent(in) :: dt
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(SGS_coefficients_data), intent(in) :: Csims_FEM_MHD
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MGCG_data), intent(in) :: MGCG_WK
!
      type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
      type(MGCG_MHD_data), intent(inout) :: MGCG_MHD_FEM
      type(MHD_MG_matrices), intent(inout) :: MHD_mat
!
      integer(kind = kint) :: i_lev
!
!
      do i_lev = 1, MGCG_WK%num_MG_level
        if(my_rank .lt. MGCG_WK%MG_mpi(i_lev)%nprocs) then
          if (iflag_debug.eq.1) write(*,*) 'set MG matrices', i_lev
          call const_MHD_aiccg_matrices(i_lev, dt, FEM_prm,             &
     &        SGS_param, cmt_param, MGCG_FEM%MG_mesh(i_lev),            &
     &        MGCG_MHD_FEM%MG_MHD_mesh(i_lev),                          &
     &        MGCG_MHD_FEM%MG_node_bc(i_lev),                           &
     &        MGCG_MHD_FEM%MG_surf_bc(i_lev), MHD_prop,                 &
     &        MGCG_MHD_FEM%ak_MHD_AMG(i_lev),                           &
     &        MGCG_FEM%MG_FEM_int(i_lev),                               &
     &        MGCG_MHD_FEM%MG_filter_MHD(i_lev), Csims_FEM_MHD,         &
     &        MHD_mat%MG_mat_tbls(i_lev),                               &
     &        MGCG_MHD_FEM%MG_mk_MHD(i_lev),                            &
     &        MGCG_FEM%MG_FEM_mat(i_lev), MHD_mat)
        end if
      end do
!
      end subroutine const_MGCG_MHD_matrices
!
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
      subroutine const_MHD_aiccg_matrices                               &
     &         (i_lev, dt, FEM_prm, SGS_param, cmt_param,               &
     &          fem, MHD_mesh, nod_bcs, surf_bcs, MHD_prop, ak_MHD,     &
     &          fem_int, FEM_elens, Csims_FEM_MHD, MHD_mat_tbls,        &
     &          mk_MHD, rhs_mat, MHD_mat)
!
      use set_aiccg_matrices_type
      use precond_djds_MHD
      use initialize_4_MHD_AMG
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: i_lev
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(mesh_data), intent(in) ::   fem
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in)  :: surf_bcs
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_MHD_mat_const), intent(in) :: MHD_mat_tbls
      type(SGS_coefficients_data), intent(in) :: Csims_FEM_MHD
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(MHD_MG_matrices), intent(inout) :: MHD_mat
!
!
      call s_set_aiccg_matrices                                         &
     &   (MHD_prop%iflag_all_scheme, dt, FEM_prm, SGS_param, cmt_param, &
     &    fem%mesh, fem%group, MHD_mesh, nod_bcs, surf_bcs,             &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    ak_MHD, fem_int%jcs, FEM_elens, Csims_FEM_MHD%ifld_diff,      &
     &    Csims_FEM_MHD%diff_coefs, fem_int%rhs_tbl,                    &
     &    MHD_mat%MG_DJDS_table(i_lev),  MHD_mat%MG_DJDS_fluid(i_lev),  &
     &    MHD_mat%MG_DJDS_linear(i_lev), MHD_mat%MG_DJDS_lin_fl(i_lev), &
     &    MHD_mat_tbls%base, MHD_mat_tbls%fluid_q,                      &
     &    MHD_mat_tbls%full_conduct_q,                                  &
     &    MHD_mat_tbls%linear, MHD_mat_tbls%fluid_l,                    &
     &    mk_MHD%mlump_fl, mk_MHD%mlump_cd,                             &
     &    rhs_mat%surf_wk, rhs_mat%fem_wk,                              &
     &    MHD_mat%Vmat_MG_DJDS(i_lev), MHD_mat%Bmat_MG_DJDS(i_lev),     &
     &    MHD_mat%Tmat_MG_DJDS(i_lev), MHD_mat%Cmat_MG_DJDS(i_lev),     &
     &    MHD_mat%Pmat_MG_DJDS(i_lev), MHD_mat%Fmat_MG_DJDS(i_lev))
!
      if (iflag_debug.eq.1) write(*,*) 'preconditioning'
      call matrix_precondition                                          &
     &   (FEM_PRM%CG11_param%PRECOND, FEM_PRM%precond_33,               &
     &    FEM_prm%CG11_param%sigma_diag,                                &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    MHD_mat%MG_DJDS_table(i_lev),  MHD_mat%MG_DJDS_fluid(i_lev),  &
     &    MHD_mat%MG_DJDS_linear(i_lev), MHD_mat%MG_DJDS_lin_fl(i_lev), &
     &    MHD_mat%Vmat_MG_DJDS(i_lev), MHD_mat%Bmat_MG_DJDS(i_lev),     &
     &    MHD_mat%Tmat_MG_DJDS(i_lev), MHD_mat%Cmat_MG_DJDS(i_lev),     &
     &    MHD_mat%Pmat_MG_DJDS(i_lev), MHD_mat%Fmat_MG_DJDS(i_lev))
!
      end subroutine const_MHD_aiccg_matrices
!
! ----------------------------------------------------------------------
!
      end module construct_matrices
