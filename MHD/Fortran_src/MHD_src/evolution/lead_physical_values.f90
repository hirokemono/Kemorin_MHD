!>@file   lead_physical_values.f90
!!        module lead_physical_values
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate many kind of field data
!!
!!@verbatim
!!      subroutine lead_fields_by_FEM(mesh, surf1, edge1, MHD_mesh,     &
!!     &          sf_grp1, iphys, iphys_ele, fld_ele1,                  &
!!     &          jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,     &
!!     &          FEM1_elen, layer_tbl, m1_lump, mhd_fem1_wk, fem1_wk,  &
!!     &          f1_l, f1_nl, nod_fld1)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(surface_data), intent(in) :: surf1
!!        type(edge_data), intent(in) :: edge1
!!        type(surface_group_data), intent(in) :: sf_grp1
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: fld_ele1
!!        type(jacobians_3d), intent(in) :: jac1_3d_q, jac1_3d_l
!!        type(jacobians_2d), intent(in) :: jac1_sf_grp_2d_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
!!        type(lumped_mass_matrices), intent(in) :: m1_lump
!!        type(gradient_model_data_type), intent(in) :: FEM1_elen
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
!!        type(work_finite_element_mat), intent(inout) :: fem1_wk
!!        type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
!!        type(phys_data), intent(inout) :: nod_fld1
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
      use t_layering_ele_list
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
      subroutine lead_fields_by_FEM(mesh, surf1, edge1, MHD_mesh,       &
     &          sf_grp1, iphys, iphys_ele, fld_ele1,                    &
     &          jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,       &
     &          FEM1_elen, layer_tbl, m1_lump, mhd_fem1_wk, fem1_wk,    &
     &          f1_l, f1_nl, nod_fld1)
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
      type(surface_data), intent(in) :: surf1
      type(edge_data), intent(in) :: edge1
      type(surface_group_data), intent(in) :: sf_grp1
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: fld_ele1
      type(jacobians_3d), intent(in) :: jac1_3d_q, jac1_3d_l
      type(jacobians_2d), intent(in) :: jac1_sf_grp_2d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
      type(lumped_mass_matrices), intent(in) :: m1_lump
      type(gradient_model_data_type), intent(in) :: FEM1_elen
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
      type(phys_data), intent(inout) :: nod_fld1
!
      integer (kind =kint) :: iflag
!
!
      call set_lead_physical_values_flag(iflag)
!
      if ( iflag.eq.0 ) then
        if (iflag_debug.gt.0) write(*,*) 'cal_potential_on_edge'
        call cal_potential_on_edge                                      &
     &     (mesh%node, mesh%ele, edge1, iphys, nod_fld1)
!
        if (iflag_debug.gt.0) write(*,*) 'update_fields'
        call update_fields                                              &
     &     (mesh, surf1, MHD_mesh, sf_grp1, iphys, iphys_ele, fld_ele1, &
     &      jac1_3d_q, jac1_3d_l, jac1_sf_grp_2d_q, rhs_tbl1,           &
     &      FEM1_elen, layer_tbl, m1_lump, mhd_fem1_wk, fem1_wk,        &
     &      f1_l, f1_nl, nod_fld1)
!
        call cal_field_by_rotation                                      &
     &     (mesh%nod_comm, mesh%node, mesh%ele, surf1,                  &
     &      sf_grp1, MHD_mesh%fluid, MHD_mesh%conduct,                  &
     &      iphys, iphys_ele, fld_ele1, jac1_3d_q, jac1_sf_grp_2d_q,    &
     &      rhs_tbl1, FEM1_elen, m1_lump, mhd_fem1_wk, fem1_wk,         &
     &      f1_l, f1_nl, nod_fld1)
!
        if (iflag_debug.gt.0) write(*,*) 'cal_helicity'
        call cal_helicity(mesh%node, iphys, nod_fld1)
!
        if (iflag_debug.gt.0) write(*,*) 'cal_energy_fluxes'
        call cal_energy_fluxes                                          &
     &     (mesh, surf1, MHD_mesh, sf_grp1, iphys, iphys_ele, fld_ele1, &
     &      jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,           &
     &      m1_lump, mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      end if
!
      end subroutine lead_fields_by_FEM
!
! ----------------------------------------------------------------------
!
      subroutine cal_energy_fluxes(mesh, surf1, MHD_mesh,               &
     &          sf_grp1, iphys, iphys_ele, fld_ele1,                    &
     &          jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,       &
     &          m1_lump, mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
!
      use m_machine_parameter
      use m_physical_property
!
      use cal_MHD_forces_4_monitor
      use cal_sgs_4_monitor
      use cal_true_sgs_terms
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf1
      type(surface_group_data), intent(in) :: sf_grp1
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: fld_ele1
      type(jacobians_3d), intent(in) :: jac1_3d_q
      type(jacobians_2d), intent(in) :: jac1_sf_grp_2d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
      type(lumped_mass_matrices), intent(in) :: m1_lump
      type(gradient_model_data_type), intent(in) :: FEM1_elen
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
      type(phys_data), intent(inout) :: nod_fld1
!
!
      call cal_true_sgs_terms_pre                                       &
     &   (mesh%nod_comm, mesh%node, mesh%ele, surf1, sf_grp1,           &
     &    MHD_mesh%fluid, MHD_mesh%conduct, iphys, iphys_ele, fld_ele1, &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
!
      call cal_sgs_terms_4_monitor(mesh%nod_comm, mesh%node, mesh%ele,  &
     &    MHD_mesh%fluid, MHD_mesh%conduct, iphys, iphys_ele, fld_ele1, &
     &    jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk,         &
     &    f1_l, f1_nl, nod_fld1)
!
      call cal_fluxes_4_monitor(mesh%node, iphys, nod_fld1)
!
      call cal_forces_4_monitor                                         &
     &   (mesh%nod_comm, mesh%node, mesh%ele, surf1, sf_grp1,           &
     &    MHD_mesh%fluid, MHD_mesh%conduct, iphys, iphys_ele, fld_ele1, &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    m1_lump, mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
      call cal_diff_of_sgs_terms                                        &
     &   (mesh%nod_comm, mesh%node, mesh%ele, surf1, sf_grp1,           &
     &    MHD_mesh%fluid, MHD_mesh%conduct, iphys, iphys_ele, fld_ele1, &
     &    jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,             &
     &    mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
!
      call cal_true_sgs_terms_post                                      &
     &   (mesh%nod_comm, mesh%node, iphys, nod_fld1)
!
      call cal_work_4_forces(mesh%nod_comm, mesh%node, mesh%ele,        &
     &    iphys, jac1_3d_q, rhs_tbl1, mhd_fem1_wk, fem1_wk,             &
     &    f1_nl, nod_fld1)
!
      call cal_work_4_sgs_terms                                         &
     &   (mesh%nod_comm, mesh%node, mesh%ele, MHD_mesh%conduct,         &
     &    iphys, jac1_3d_q, rhs_tbl1, mhd_fem1_wk, fem1_wk,             &
     &    f1_nl, nod_fld1)
! 
      end subroutine cal_energy_fluxes
!
!  ---------------------------------------------------------------------
!
      end module lead_physical_values
