!>@file   lead_physical_values.f90
!!        module lead_physical_values
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate many kind of field data
!!
!!@verbatim
!!      subroutine lead_fields_by_FEM(mesh, group, ele_mesh, MHD_mesh,  &
!!     &          nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld,         &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp, rhs_tbl, FEM_elens,   &
!!     &          filtering, wide_filtering, layer_tbl, m_lump,         &
!!     &          wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(nodal_boundarty_conditions), intent(in) :: nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_data_type), intent(in) :: wide_filtering
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module lead_physical_values
!
      use m_precision
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_filtering_data
      use t_layering_ele_list
      use t_bc_data_MHD
      use t_MHD_boundary_data
!
      implicit none
!
      private :: cal_energy_fluxes
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine lead_fields_by_FEM(mesh, group, ele_mesh, MHD_mesh,    &
     &          nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld,           &
     &          jac_3d_q, jac_3d_l, jac_sf_grp, rhs_tbl, FEM_elens,     &
     &          filtering, wide_filtering, layer_tbl, m_lump,&
     &          wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      use m_machine_parameter
      use m_t_step_parameter
!
      use update_after_evolution
      use itp_potential_on_edge
      use MHD_field_by_rotation
      use cal_helicities
      use output_viz_file_control
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(filtering_data_type), intent(in) :: wide_filtering
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
!
      if ( iflag.eq.0 ) then
        if (iflag_debug.gt.0) write(*,*) 'cal_potential_on_edge'
        call cal_potential_on_edge                                      &
     &     (mesh%node, mesh%ele, ele_mesh%edge, iphys, nod_fld)
!
        if (iflag_debug.gt.0) write(*,*) 'update_fields'
        call update_fields(mesh, group, ele_mesh, MHD_mesh,             &
     &      nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld,               &
     &      jac_3d_q, jac_3d_l, jac_sf_grp, rhs_tbl, FEM_elens,         &
     &      filtering, wide_filtering, layer_tbl, m_lump,               &
     &      wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
        call cal_field_by_rotation                                      &
     &     (mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,          &
     &      MHD_mesh%fluid, MHD_mesh%conduct, group%surf_grp,           &
     &      nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld,               &
     &      jac_3d_q, jac_sf_grp, rhs_tbl, FEM_elens, m_lump,           &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
        if (iflag_debug.gt.0) write(*,*) 'cal_helicity'
        call cal_helicity(mesh%node, iphys, nod_fld)
!
        if (iflag_debug.gt.0) write(*,*) 'cal_energy_fluxes'
        call cal_energy_fluxes                                          &
     &     (mesh, group, ele_mesh, MHD_mesh, nod_bcs, surf_bcs, iphys,  &
     &      iphys_ele, ele_fld, jac_3d_q, jac_sf_grp, rhs_tbl,          &
     &      FEM_elens, filtering, m_lump, wk_filter,                    &
     &      mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      end if
!
      end subroutine lead_fields_by_FEM
!
! ----------------------------------------------------------------------
!
      subroutine cal_energy_fluxes(mesh, group, ele_mesh, MHD_mesh,     &
     &          nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld,           &
     &          jac_3d_q, jac_sf_grp, rhs_tbl, FEM_elens, filtering,    &
     &          m_lump, wk_filter, mhd_fem_wk, fem_wk,                  &
     &          f_l, f_nl, nod_fld)
!
      use m_machine_parameter
      use m_physical_property
!
      use cal_MHD_forces_4_monitor
      use cal_sgs_4_monitor
      use cal_true_sgs_terms
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(nodal_boundarty_conditions), intent(in) :: nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      call cal_true_sgs_terms_pre                                       &
     &   (mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,            &
     &    group%surf_grp, MHD_mesh%fluid, MHD_mesh%conduct,             &
     &    nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld,                 &
     &    jac_3d_q, jac_sf_grp, rhs_tbl, FEM_elens, mhd_fem_wk,         &
     &    fem_wk, f_l, f_nl, nod_fld)
!
      call cal_sgs_terms_4_monitor(mesh%nod_comm, mesh%node, mesh%ele,  &
     &    MHD_mesh%fluid, MHD_mesh%conduct, iphys, iphys_ele, ele_fld,  &
     &    jac_3d_q, rhs_tbl, FEM_elens, filtering,                      &
     &    wk_filter, mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
!
      call cal_fluxes_4_monitor(mesh%node, iphys, nod_fld)
!
      call cal_forces_4_monitor                                         &
     &   (mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,            &
     &    MHD_mesh%fluid, MHD_mesh%conduct, group%surf_grp,             &
     &    nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld,                 &
     &    jac_3d_q, jac_sf_grp, rhs_tbl, FEM_elens, m_lump,             &
     &    mhd_fem_wk, fem_wk, f_l, f_nl, nod_fld)
      call cal_diff_of_sgs_terms                                        &
     &   (mesh%nod_comm, mesh%node, mesh%ele, ele_mesh%surf,            &
     &    MHD_mesh%fluid, MHD_mesh%conduct, group%surf_grp,             &
     &    nod_bcs, surf_bcs, iphys, iphys_ele, ele_fld,                 &
     &    jac_3d_q, jac_sf_grp, rhs_tbl, FEM_elens, mhd_fem_wk, fem_wk, &
     &    f_l, f_nl, nod_fld)
!
      call cal_true_sgs_terms_post(mesh%nod_comm, mesh%node, iphys,     &
     &    filtering, wk_filter, nod_fld)
!
      call cal_work_4_forces(mesh%nod_comm, mesh%node, mesh%ele,        &
     &    iphys, jac_3d_q, rhs_tbl, mhd_fem_wk, fem_wk, f_nl, nod_fld)
!
      call cal_work_4_sgs_terms                                         &
     &   (mesh%nod_comm, mesh%node, mesh%ele, MHD_mesh%conduct,         &
     &    iphys, jac_3d_q, rhs_tbl, mhd_fem_wk, fem_wk, f_nl, nod_fld)
! 
      end subroutine cal_energy_fluxes
!
!  ---------------------------------------------------------------------
!
      end module lead_physical_values
