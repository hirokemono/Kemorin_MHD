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
      private :: int_vol_current_diffuse
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
      use m_jacobians
      use m_element_id_4_node
      use m_int_vol_data
      use m_surf_data_vector_p
      use m_node_phys_data
!
      use cal_multi_pass
      use cal_for_ffs
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
      use int_surf_diffuse_terms
!
!
!  Volume integration
!
      call int_vol_current_diffuse(node1, ele1, nod_fld1, iphys%i_vecp)
!
!  for boundary conditions
!
      call int_surf_current_diffuse(node1, ele1, surf1, sf_grp1,        &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, sf_bc1_lead_a,          &
     &    intg_point_t_evo, iphys%i_vecp, fem1_wk, f1_l)
!
      call cal_multi_pass_4_vector_ff(ele1%istack_ele_smp, m1_lump,     &
     &    nod_comm, node1, ele1, jac1_3d_q, rhs_tbl1,                   &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
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
! ----------------------------------------------------------------------
!
      subroutine int_vol_current_diffuse(node, ele, nod_fld, i_vecp)
!
      use m_phys_constants
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
!
      use nodal_fld_2_each_element
      use fem_skv_vector_diff_type
      use cal_skv_to_ff_smp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: i_vecp
!
      integer (kind = kint) :: k2
!
!
      call reset_ff_smp(node%max_nod_smp, f1_nl)
      call reset_sk6(n_vector, ele, fem1_wk%sk6)
!
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element                                 &
     &     (node, ele, nod_fld, k2, i_vecp, fem1_wk%vector_1)
        call fem_skv_rot_rot_by_laplace(ele%istack_ele_smp,             &
     &      intg_point_poisson, k2, ele, jac1_3d_q, fem1_wk%vector_1,   &
     &      fem1_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl1,                   &
     &    fem1_wk%sk6, f1_nl%ff_smp)
!
      end subroutine int_vol_current_diffuse
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
