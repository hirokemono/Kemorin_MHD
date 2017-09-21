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
!!     &         (time_d, FEM_prm, SGS_par, femmesh, ele_mesh, MHD_mesh,&
!!     &          MHD_prop, nod_bcs, surf_bcs, iphys, ak_MHD, fem_int,  &
!!     &          FEM_filters, mk_MHD, s_package, MGCG_WK, SGS_MHD_wk,  &
!!     &          nod_fld, Csims_FEM_MHD, fem_sq)
!!      subroutine update_FEM_fields(time_d, FEM_prm, SGS_par,          &
!!     &          femmesh, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,       &
!!     &          iphys, fem_int, FEM_filters, mk_MHD, SGS_MHD_wk,      &
!!     &          nod_fld, Csims_FEM_MHD)
!!
!!      subroutine cal_FEM_model_coefficients(time_d, FEM_prm, SGS_par, &
!!     &          femmesh, ele_mesh, MHD_mesh, MHD_prop,                &
!!     &          nod_bcs, surf_bcs, iphys, fem_int, FEM_filters,       &
!!     &          mk_MHD, SGS_MHD_wk, nod_fld, Csims_FEM_MHD)
!!
!!      subroutine fields_evolution_4_FEM_SPH                           &
!!     &         (time_d, FEM_prm, SGS_par, femmesh, ele_mesh, fluid,   &
!!     &          MHD_prop, nod_bcs, surf_bcs, iphys, ak_MHD,           &
!!     &          fem_int, FEM_filters, mk_MHD, s_package, MGCG_WK,     &
!!     &          SGS_MHD_wk, nod_fld, Csims_FEM_MHD, fem_sq)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) :: femmesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(MHD_matrices_pack), intent(in) :: s_package
!!        type(MGCG_data), intent(inout) :: MGCG_WK
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!        type(FEM_MHD_mean_square), intent(inout) :: fem_sq
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
      use t_table_FEM_const
      use t_material_property
      use t_FEM_MHD_filter_data
      use t_surface_bc_data_MHD
      use t_bc_data_MHD
      use t_MHD_matrices_pack
      use t_MGCG_data
      use t_MHD_mass_matricxes
      use t_FEM_SGS_model_coefs
      use t_FEM_MHD_mean_square
      use t_work_FEM_SGS_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_fields_evolution                                   &
     &         (time_d, FEM_prm, SGS_par, femmesh, ele_mesh, MHD_mesh,  &
     &          MHD_prop, nod_bcs, surf_bcs, iphys, ak_MHD, fem_int,    &
     &          FEM_filters, mk_MHD, s_package, MGCG_WK, SGS_MHD_wk,    &
     &          nod_fld, Csims_FEM_MHD, fem_sq)
!
      use FEM_MHD_evolution
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
      type(MHD_matrices_pack), intent(in) :: s_package
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
!
      call fields_evolution(time_d, FEM_prm, SGS_par, femmesh,          &
     &    ele_mesh, MHD_mesh, MHD_prop, nod_bcs, surf_bcs, iphys,       &
     &    Csims_FEM_MHD%ifld_sgs, Csims_FEM_MHD%icomp_sgs,              &
     &    Csims_FEM_MHD%ifld_diff, Csims_FEM_MHD%icomp_diff,            &
     &    Csims_FEM_MHD%iphys_elediff, ak_MHD, fem_int, FEM_filters,    &
     &    mk_MHD, s_package, MGCG_WK, SGS_MHD_wk, nod_fld,              &
     &    Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod,         &
     &    Csims_FEM_MHD%diff_coefs, fem_sq)
!
      end subroutine FEM_fields_evolution
!
!-----------------------------------------------------------------------
!
      subroutine update_FEM_fields(time_d, FEM_prm, SGS_par,            &
     &          femmesh, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,         &
     &          iphys, fem_int, FEM_filters, mk_MHD, SGS_MHD_wk,        &
     &          nod_fld, Csims_FEM_MHD)
!
      use FEM_MHD_evolution
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!
!
      call update_fields(time_d, FEM_prm, SGS_par,                      &
     &    femmesh, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,               &
     &    iphys, Csims_FEM_MHD%ifld_diff, Csims_FEM_MHD%icomp_diff,     &
     &    Csims_FEM_MHD%iphys_elediff, fem_int, FEM_filters, mk_MHD,    &
     &    SGS_MHD_wk, nod_fld, Csims_FEM_MHD%diff_coefs)
!
      end subroutine update_FEM_fields
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_FEM_model_coefficients(time_d, FEM_prm, SGS_par,   &
     &          femmesh, ele_mesh, MHD_mesh, MHD_prop,                  &
     &          nod_bcs, surf_bcs, iphys, fem_int, FEM_filters,         &
     &          mk_MHD, SGS_MHD_wk, nod_fld, Csims_FEM_MHD)
!
      use cal_model_coefficients
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) ::   femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      if(iflag_debug.eq.1) write(*,*) 's_cal_model_coefficients'
      call s_cal_model_coefficients(time_d, FEM_prm, SGS_par,           &
     &    femmesh, ele_mesh, MHD_mesh, MHD_prop,                        &
     &    nod_bcs, surf_bcs, iphys, Csims_FEM_MHD%ifld_sgs,             &
     &    Csims_FEM_MHD%icomp_sgs, Csims_FEM_MHD%ifld_diff,             &
     &    Csims_FEM_MHD%icomp_diff, Csims_FEM_MHD%iphys_elediff,        &
     &    fem_int, FEM_filters, mk_MHD, SGS_MHD_wk, nod_fld,            &
     &    Csims_FEM_MHD%sgs_coefs, Csims_FEM_MHD%sgs_coefs_nod,         &
     &    Csims_FEM_MHD%diff_coefs)
!
      end subroutine cal_FEM_model_coefficients
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fields_evolution_4_FEM_SPH                             &
     &         (time_d, FEM_prm, SGS_par, femmesh, ele_mesh, fluid,     &
     &          MHD_prop, nod_bcs, surf_bcs, iphys, ak_MHD,             &
     &          fem_int, FEM_filters, mk_MHD, s_package, MGCG_WK,       &
     &          SGS_MHD_wk, nod_fld, Csims_FEM_MHD, fem_sq)
!
      use FEM_MHD_evolution
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(mesh_data), intent(in) :: femmesh
      type(element_geometry), intent(in) :: ele_mesh
      type(field_geometry_data), intent(in) :: fluid
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
      type(MHD_matrices_pack), intent(in) :: s_package
!
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(FEM_MHD_mean_square), intent(inout) :: fem_sq
!
!
      call fields_evo_for_FEM_SPH                                       &
     &   (time_d, FEM_prm, SGS_par, femmesh, ele_mesh, fluid, MHD_prop, &
     &    nod_bcs, surf_bcs, iphys, Csims_FEM_MHD%ifld_sgs,             &
     &    Csims_FEM_MHD%icomp_sgs, Csims_FEM_MHD%ifld_diff,             &
     &    Csims_FEM_MHD%icomp_diff, Csims_FEM_MHD%iphys_elediff,        &
     &    ak_MHD, fem_int, FEM_filters, mk_MHD, s_package, MGCG_WK,     &
     &    SGS_MHD_wk, nod_fld, Csims_FEM_MHD%sgs_coefs,                 &
     &    Csims_FEM_MHD%sgs_coefs_nod, Csims_FEM_MHD%diff_coefs,        &
     &    fem_sq)
!
      end subroutine fields_evolution_4_FEM_SPH
!
!-----------------------------------------------------------------------
!
      end module update_after_evolution
