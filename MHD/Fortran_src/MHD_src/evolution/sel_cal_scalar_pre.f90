!>@file   sel_cal_scalar_pre.f90
!!@brief  module sel_cal_scalar_pre
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2005
!
!>@brief  Time integration routine select for scalar
!!
!!@verbatim
!!      subroutine s_sel_cal_scalar_pre(iflag_supg, i_field, dt,        &
!!     &          iflag_commute_field, i_pre_advect, eps_4_crank,       &
!!     &          FEM_prm, SGS_param, mesh, fluid, property, nod_bcs,   &
!!     &          iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,    &
!!     &          ak_diff, mlump_fl, Smatrix, ak_diffuse, MGCG_WK,      &
!!     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld, m_SR)
!!        integer(kind = kint), intent(in) :: i_field
!!        real(kind = kreal), intent(in) :: dt
!!        integer(kind = kint), intent(in) :: iflag_supg
!!        integer(kind = kint), intent(in) :: iflag_commute_field
!!        integer(kind = kint), intent(in) :: i_pre_advect
!!        real(kind = kreal), intent(in) ::   eps_4_crank
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(scalar_property), intent(in) :: property
!!        type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(MHD_MG_matrix), intent(in) :: Smatrix
!!        real(kind = kreal), intent(in) :: ak_diffuse(mesh%ele%numele)
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module sel_cal_scalar_pre
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_phys_constants
!
      use t_reference_scalar_param
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_base_field_labels
      use t_grad_field_labels
      use t_explicit_term_labels
      use t_SGS_term_labels
      use t_jacobians
      use t_table_FEM_const
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_layering_ele_list
      use t_filter_elength
      use t_filtering_data
      use t_bc_data_temp
      use t_surface_bc_scalar
      use t_material_property
      use t_SGS_model_coefs
      use t_solver_djds_MHD
      use t_MGCG_data
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
      use t_mesh_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_sel_cal_scalar_pre(iflag_supg, i_field, dt,          &
     &          iflag_commute_field, i_pre_advect, eps_4_crank,         &
     &          FEM_prm, SGS_param, mesh, fluid, property, nod_bcs,     &
     &          iphys_ele_base, ele_fld, jacs, rhs_tbl, FEM_elens,      &
     &          ak_diff, mlump_fl, Smatrix, ak_diffuse, MGCG_WK,        &
     &          mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld, m_SR)
!
      use evolve_by_1st_euler
      use evolve_by_adams_bashforth
      use evolve_by_lumped_crank
      use evolve_by_consist_crank
!
      integer(kind = kint), intent(in) :: i_field
      real(kind = kreal), intent(in) :: dt
!
      integer(kind = kint), intent(in) :: iflag_supg
      integer(kind = kint), intent(in) :: iflag_commute_field
      integer(kind = kint), intent(in) :: i_pre_advect
      real(kind = kreal), intent(in) ::   eps_4_crank
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(lumped_mass_matrices), intent(in) :: mlump_fl
      type(MHD_MG_matrix), intent(in) :: Smatrix
!
      real(kind = kreal), intent(in) :: ak_diffuse(mesh%ele%numele)
      real(kind = kreal), intent(in) :: ak_diff(mesh%ele%numele)
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(property%iflag_scheme .eq. id_explicit_euler) then
        call cal_scalar_pre_euler(iflag_supg, i_field, dt,              &
     &      FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,         &
     &      iphys_ele_base, ele_fld, jacs%g_FEM, jacs%jac_3d, rhs_tbl,  &
     &      mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,           &
     &      m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      else if(property%iflag_scheme .eq. id_explicit_adams2) then
        call cal_scalar_pre_adams(iflag_supg, i_field, i_pre_advect,    &
     &      dt, FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,     &
     &      iphys_ele_base, ele_fld, jacs%g_FEM, jacs%jac_3d, rhs_tbl,  &
     &      mlump_fl, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld,           &
     &      m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      else if(property%iflag_scheme .eq. id_Crank_nicolson) then
        call cal_temp_pre_lumped_crank(iflag_supg,                      &
     &      iflag_commute_field, SGS_param%ifilter_final,               &
     &      i_field, i_pre_advect, ak_diffuse, eps_4_crank,             &
     &      dt, FEM_prm, mesh%nod_comm, mesh%node, mesh%ele,            &
     &      fluid, property, nod_bcs, iphys_ele_base, ele_fld,          &
     &      jacs%g_FEM, jacs%jac_3d, rhs_tbl, FEM_elens, ak_diff,       &
     &      mlump_fl, Smatrix, MGCG_WK%MG_vector, mhd_fem_wk, fem_wk,   &
     &      f_l, f_nl, nod_fld, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      else if(property%iflag_scheme .eq. id_Crank_nicolson_cmass) then
        call cal_temp_pre_consist_crank                                 &
     &    (iflag_commute_field, SGS_param%ifilter_final,                &
     &     i_field, i_pre_advect, ak_diffuse, eps_4_crank, dt, FEM_prm, &
     &     mesh%node, mesh%ele, fluid, property, nod_bcs,               &
     &     jacs%g_FEM, jacs%jac_3d, rhs_tbl, FEM_elens, ak_diff,        &
     &     Smatrix, MGCG_WK%MG_vector, mhd_fem_wk, fem_wk, f_l, f_nl,   &
     &     nod_fld, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
      end if
!
      end subroutine s_sel_cal_scalar_pre
!
! ----------------------------------------------------------------------
!
      end module sel_cal_scalar_pre
