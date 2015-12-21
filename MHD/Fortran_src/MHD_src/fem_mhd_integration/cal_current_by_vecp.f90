!cal_current_by_vecp.f90
!      module cal_current_by_vecp
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!
!!      subroutine int_current_diffuse
!!
!!      subroutine int_surf_temp_diffuse(node, ele, surf, sf_grp,       &
!!     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, fem_wk, f_l)
!!
!!      subroutine int_surf_velo_diffuse(node, ele, surf, sf_grp,       &
!!     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, fem_wk, f_l)
!!      subroutine int_surf_vector_p_diffuse(node, ele, surf, sf_grp,   &
!!     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, fem_wk, f_l)
!!      subroutine int_surf_magne_diffuse(node, ele, surf, sf_grp,      &
!!     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, fem_wk, f_l)
!!      subroutine int_surf_composit_diffuse(node, ele, surf, sf_grp,   &
!!     &         iphys, nod_fld, jac_sf_grp, rhs_tbl, fem_wk, f_l)
!
      module cal_current_by_vecp
!
      use m_precision
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_address
      use t_phys_data
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
!
      use m_control_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_current_diffuse
!
      use m_machine_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_geometry_data_MHD
      use m_finite_element_matrix
      use m_jacobian_sf_grp
      use m_int_vol_data
      use m_surf_data_vector_p
      use m_node_phys_data
!
      use cal_multi_pass
      use cal_for_ffs
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
      use int_vol_current_by_vecp
      use int_surf_diffuse_terms
!
!
!  Volume integration
!
      call int_vol_current_diffuse
!
!  for boundary conditions
!
      call int_surf_current_diffuse(node1, ele1, surf1, sf_grp1,        &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, sf_bc1_lead_a,          &
     &    intg_point_t_evo, iphys%i_vecp, fem1_wk, f1_l)
!
      call cal_multi_pass_4_vector_ff
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_l%ff, m1_lump%ml, nod_fld1%ntot_phys,                      &
     &    iphys%i_current, nod_fld1%d_fld)
!
!    communication
!
      call vector_send_recv                                             &
     &   (iphys%i_current, node1, nod_comm, nod_fld1)
!
      end subroutine int_current_diffuse
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_surf_temp_diffuse(node, ele, surf, sf_grp,         &
     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, fem_wk, f_l)
!
      use m_ele_material_property
      use m_surf_data_temp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_diffuse_term                                        &
     &   (node, ele, surf, sf_grp, nod_fld, jac_sf_grp, rhs_tbl,        &
     &    sf_bc1_lead_gd_t, intg_point_t_evo,                           &
     &    ak_d_temp, iphys%i_temp, fem_wk, f_l)
!
      end subroutine int_surf_temp_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_velo_diffuse(node, ele, surf, sf_grp,         &
     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, fem_wk, f_l)
!
      use m_ele_material_property
      use m_surf_data_torque
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_vect_diffuse_term                                   &
     &   (node, ele, surf, sf_grp, jac_sf_grp, nod_fld, rhs_tbl,        &
     &    sf_bc1_lead_tq, intg_point_t_evo,                             &
     &    ak_d_velo, iphys%i_velo, fem_wk, f_l)
!
      end subroutine int_surf_velo_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_vector_p_diffuse(node, ele, surf, sf_grp,     &
     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, fem_wk, f_l)
!
      use m_ele_material_property
      use m_surf_data_vector_p
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_vect_diffuse_term                                   &
     &   (node, ele, surf, sf_grp, jac_sf_grp, nod_fld, rhs_tbl,        &
     &    sf_bc1_lead_a, intg_point_t_evo,                              &
     &    ak_d_magne, iphys%i_vecp, fem_wk, f_l)
!
      end subroutine int_surf_vector_p_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_magne_diffuse(node, ele, surf, sf_grp,        &
     &          iphys, nod_fld, jac_sf_grp, rhs_tbl, fem_wk, f_l)
!
      use m_ele_material_property
      use m_surf_data_magne
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_vect_diffuse_term                                   &
     &   (node, ele, surf, sf_grp, jac_sf_grp, nod_fld, rhs_tbl,        &
     &    sf_bc1_lead_b, intg_point_t_evo,                              &
     &    ak_d_magne, iphys%i_magne, fem_wk, f_l)
!
      end subroutine int_surf_magne_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_composit_diffuse(node, ele, surf, sf_grp,     &
     &         iphys, nod_fld, jac_sf_grp, rhs_tbl, fem_wk, f_l)
!
      use m_ele_material_property
      use m_surf_data_composition
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_diffuse_term                                        &
     &   (node, ele, surf, sf_grp, nod_fld, jac_sf_grp, rhs_tbl,        &
     &    sf_bc1_lead_gd_c, intg_point_t_evo,                           &
     &    ak_d_composit, iphys%i_light, fem_wk, f_l)
!
      end subroutine int_surf_composit_diffuse
!
!-----------------------------------------------------------------------
!
      end module cal_current_by_vecp
