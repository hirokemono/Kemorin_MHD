!>@file   update_after_evolution.f90
!!        module update_after_evolution
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine FEM_fields_evolution                                 &
!!     &         (time_d, FEM_prm, SGS_par, geofem, MHD_mesh,           &
!!     &          MHD_prop, FEM_MHD_BCs, iphys, iphys_LES, ak_MHD,      &
!!     &          s_package, MGCG_WK, SGS_MHD_wk, nod_fld,              &
!!     &          Csims_FEM_MHD, fem_sq, v_sol)
!!      subroutine update_FEM_fields                                    &
!!     &         (time_d, FEM_prm, SGS_par, geofem, MHD_mesh,           &
!!     &          FEM_MHD_BCs, iphys, iphys_LES, FEM_filters,           &
!!     &          SGS_MHD_wk, nod_fld, Csims_FEM_MHD, v_sol)
!!
!!      subroutine cal_FEM_model_coefficients(time_d, FEM_prm, SGS_par, &
!!     &          geofem, MHD_mesh, MHD_prop, FEM_MHD_BCs,              &
!!     &          iphys, iphys_LES, FEM_filters, SGS_MHD_wk,            &
!!     &          nod_fld, Csims_FEM_MHD, v_sol)
!!
!!      subroutine fields_evolution_4_FEM_SPH                           &
!!     &         (time_d, FEM_prm, SGS_par, geofem, fluid, MHD_prop,    &
!!     &          FEM_MHD_BCs, iphys, iphys_LES, ak_MHD, FEM_filters,   &
!!     &          s_package, MGCG_WK, SGS_MHD_wk, nod_fld,              &
!!     &          Csims_FEM_MHD, fem_sq, v_sol)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: geofem
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(FEM_MHD_BC_data), intent(in) :: FEM_MHD_BCs
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(MHD_matrices_pack), intent(in) :: s_package
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!@endverbatim
!
      module update_after_evolution
!
      use m_precision
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_control_parameter
      use t_physical_property
      use t_reference_scalar_param
      use t_time_data
      use t_mesh_data
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_table_FEM_const
      use t_material_property
      use t_FEM_MHD_filter_data
      use t_surface_bc_data_MHD
      use t_bc_data_MHD
      use t_MHD_matrices_pack
      use t_MGCG_data
      use t_MHD_mass_matrices
      use t_FEM_SGS_model_coefs
      use t_FEM_MHD_mean_square
      use t_FEM_MHD_boundary_data
      use t_work_FEM_SGS_MHD
      use t_FEM_MHD_solvers
      use t_FEM_SGS_structure
      use t_vector_for_solver
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_fields_evolution(time_d, FEM_prm,                  &
     &          geofem, MHD_mesh, MHD_prop, FEM_MHD_BCs, iphys,         &
     &          MHD_CG, SGS_MHD_wk, nod_fld, FEM_SGS, fem_sq, v_sol)
!
      use FEM_MHD_evolution
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(FEM_MHD_BC_data), intent(in) :: FEM_MHD_BCs
      type(phys_address), intent(in) :: iphys
!
      type(FEM_MHD_solvers), intent(inout) :: MHD_CG
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_SGS_structure), intent(inout) :: FEM_SGS
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call fields_evolution                                             &
     &   (time_d, FEM_prm, FEM_SGS%SGS_par, geofem, MHD_mesh, MHD_prop, &
     &    FEM_MHD_BCs%nod_bcs, FEM_MHD_BCs%surf_bcs, iphys,             &
     &    FEM_SGS%iphys_LES, MHD_CG%ak_MHD, FEM_SGS%FEM_filters,        &
     &    MHD_CG%solver_pack, MHD_CG%MGCG_WK, SGS_MHD_wk, nod_fld,      &
     &    FEM_SGS%Csims, fem_sq, v_sol)
!
      end subroutine FEM_fields_evolution
!
!-----------------------------------------------------------------------
!
      subroutine update_FEM_fields                                      &
     &         (time_d, FEM_prm, SGS_par, geofem, MHD_mesh,             &
     &          FEM_MHD_BCs, iphys, iphys_LES, FEM_filters,             &
     &          SGS_MHD_wk, nod_fld, Csims_FEM_MHD, v_sol)
!
      use FEM_MHD_evolution
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(FEM_MHD_BC_data), intent(in) :: FEM_MHD_BCs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(filters_on_FEM), intent(in) :: FEM_filters
!
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call update_fields                                                &
     &   (time_d, FEM_prm, SGS_par, geofem, MHD_mesh,                   &
     &    FEM_MHD_BCs%nod_bcs, FEM_MHD_BCs%surf_bcs, iphys, iphys_LES,  &
     &    FEM_filters, SGS_MHD_wk, nod_fld, Csims_FEM_MHD, v_sol)
!
      end subroutine update_FEM_fields
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_FEM_model_coefficients(time_d, FEM_prm, SGS_par,   &
     &          geofem, MHD_mesh, MHD_prop, FEM_MHD_BCs,                &
     &          iphys, iphys_LES, FEM_filters, SGS_MHD_wk,              &
     &          nod_fld, Csims_FEM_MHD, v_sol)
!
      use cal_model_coefficients
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) ::   geofem
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(FEM_MHD_BC_data), intent(in) :: FEM_MHD_BCs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(filters_on_FEM), intent(in) :: FEM_filters
!
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      if(iflag_debug.eq.1) write(*,*) 's_cal_model_coefficients'
      call s_cal_model_coefficients                                     &
     &   (time_d, FEM_prm, SGS_par, geofem, MHD_mesh, MHD_prop,         &
     &    FEM_MHD_BCs%nod_bcs, FEM_MHD_BCs%surf_bcs, iphys, iphys_LES,  &
     &    Csims_FEM_MHD%iak_sgs_term, Csims_FEM_MHD%icomp_sgs_term,     &
     &    Csims_FEM_MHD%iak_diff_sgs, Csims_FEM_MHD%icomp_diff_sgs,     &
     &    Csims_FEM_MHD%iphys_elediff_vec,                              &
     &    Csims_FEM_MHD%iphys_elediff_fil,                              &
     &    SGS_MHD_wk%fem_int, FEM_filters,                              &
     &    SGS_MHD_wk, nod_fld, Csims_FEM_MHD%sgs_coefs,                 &
     &    Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs, v_sol)
!
      end subroutine cal_FEM_model_coefficients
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fields_evolution_4_FEM_SPH                             &
     &         (time_d, FEM_prm, SGS_par, geofem, fluid, MHD_prop,      &
     &          FEM_MHD_BCs, iphys, iphys_LES, ak_MHD, FEM_filters,     &
     &          s_package, MGCG_WK, SGS_MHD_wk, nod_fld,                &
     &          Csims_FEM_MHD, fem_sq, v_sol)
!
      use FEM_MHD_evolution
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: geofem
      type(field_geometry_data), intent(in) :: fluid
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(FEM_MHD_BC_data), intent(in) :: FEM_MHD_BCs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(MHD_matrices_pack), intent(in) :: s_package
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call fields_evo_for_FEM_SPH                                       &
     &   (time_d, FEM_prm, SGS_par, geofem, fluid, MHD_prop,            &
     &    FEM_MHD_BCs%nod_bcs, FEM_MHD_BCs%surf_bcs, iphys, iphys_LES,  &
     &    ak_MHD, FEM_filters, s_package, MGCG_WK, SGS_MHD_wk,          &
     &    nod_fld, Csims_FEM_MHD, fem_sq, v_sol)
!
      end subroutine fields_evolution_4_FEM_SPH
!
!-----------------------------------------------------------------------
!
      end module update_after_evolution
