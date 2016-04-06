!cal_current_by_vecp.f90
!      module cal_current_by_vecp
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!
!!      subroutine int_current_diffuse(nod_comm, node, ele, surf,       &
!!     &          sf_grp, Asf_bcs, iphys, jac_3d, jac_sf_grp, rhs_tbl,  &
!!     &          m_lump, mhd_fem_wk, fem_wk, surf_wk,                  &
!!     &          f_l, f_nl, nod_fld)
!!
!!      subroutine int_surf_temp_diffuse(node, ele, surf, sf_grp,       &
!!     &          Tsf_bcs, iphys, nod_fld, jac_sf_grp, rhs_tbl,         &
!!     &          fem_wk, surf_wk, f_l)
!!      subroutine int_surf_velo_diffuse(node, ele, surf, sf_grp,       &
!!     &          Vsf_bcs, iphys, nod_fld, jac_sf_grp, rhs_tbl,         &
!!     &          fem_wk, surf_wk, f_l)
!!      subroutine int_surf_vector_p_diffuse(node, ele, surf, sf_grp,   &
!!     &          Asf_bcs, iphys, nod_fld, jac_sf_grp, rhs_tbl,         &
!!     &          fem_wk, surf_wk, f_l)
!!      subroutine int_surf_magne_diffuse(node, ele, surf, sf_grp,      &
!!     &          Bsf_bcs, iphys, nod_fld, jac_sf_grp, rhs_tbl,         &
!!     &          fem_wk, surf_wk, f_l)
!!      subroutine int_surf_composit_diffuse(node, ele, surf, sf_grp,   &
!!     &          Csf_bcs, iphys, nod_fld, jac_sf_grp, rhs_tbl,         &
!!     &          fem_wk, surf_wk, f_l)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
!!        type(velocity_surf_bc_type), intent(in) :: Asf_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
!!        type(scaler_surf_bc_type), intent(in) :: Csf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data),    intent(inout) :: nod_fld
!
      module cal_current_by_vecp
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_address
      use t_phys_data
      use t_jacobians
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_surface_bc_data
!
      use m_machine_parameter
      use m_phys_constants
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
      subroutine int_current_diffuse(nod_comm, node, ele, surf,         &
     &          sf_grp, Asf_bcs, iphys, jac_3d, jac_sf_grp, rhs_tbl,    &
     &          m_lump, mhd_fem_wk, fem_wk, surf_wk,                    &
     &          f_l, f_nl, nod_fld)
!
      use cal_multi_pass
      use cal_for_ffs
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
      use int_surf_diffuse_terms
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data),    intent(inout) :: nod_fld
!
!
!  Volume integration
!
      call int_vol_current_diffuse(node, ele, nod_fld, iphys%i_vecp,    &
     &    jac_3d, rhs_tbl, fem_wk, surf_wk, f_nl)
!
!  for boundary conditions
!
      call int_surf_current_diffuse(node, ele, surf, sf_grp,            &
     &    nod_fld, jac_sf_grp, rhs_tbl, Asf_bcs%torque_lead,            &
     &    intg_point_t_evo, iphys%i_vecp, fem_wk, surf_wk, f_l)
!
      call cal_multi_pass_4_vector_ff(ele%istack_ele_smp, m_lump,       &
     &    nod_comm, node, ele, jac_3d, rhs_tbl,                         &
     &    mhd_fem_wk%ff_m_smp, fem_wk, f_l, f_nl)
      call cal_ff_2_vector(node%numnod, node%istack_nod_smp,            &
     &    f_l%ff, m_lump%ml, nod_fld%ntot_phys,                         &
     &    iphys%i_current, nod_fld%d_fld)
!
!    communication
!
      call vector_send_recv                                             &
     &   (iphys%i_current, node, nod_comm, nod_fld)
!
      end subroutine int_current_diffuse
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_current_diffuse(node, ele, nod_fld, i_vecp,    &
     &          jac_3d, rhs_tbl, fem_wk, surf_wk, f_nl)
!
      use nodal_fld_2_each_element
      use fem_skv_vector_diff_type
      use cal_skv_to_ff_smp
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      integer(kind = kint), intent(in) :: i_vecp
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer (kind = kint) :: k2
!
!
      call reset_ff_smp(node%max_nod_smp, f_nl)
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element                                 &
     &     (node, ele, nod_fld, k2, i_vecp, fem_wk%vector_1)
        call fem_skv_rot_rot_by_laplace(ele%istack_ele_smp,             &
     &      intg_point_poisson, k2, ele, jac_3d, fem_wk%vector_1,       &
     &      fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_current_diffuse
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_surf_temp_diffuse(node, ele, surf, sf_grp,         &
     &          Tsf_bcs, iphys, nod_fld, jac_sf_grp, rhs_tbl,           &
     &          fem_wk, surf_wk, f_l)
!
      use m_ele_material_property
      use t_surface_bc_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type), intent(in) :: Tsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_diffuse_term                                        &
     &   (node, ele, surf, sf_grp, nod_fld, jac_sf_grp, rhs_tbl,        &
     &    Tsf_bcs%flux_lead, intg_point_t_evo,                          &
     &    ak_d_temp, iphys%i_temp, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_temp_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_velo_diffuse(node, ele, surf, sf_grp,         &
     &          Vsf_bcs, iphys, nod_fld, jac_sf_grp, rhs_tbl,           &
     &          fem_wk, surf_wk, f_l)
!
      use m_ele_material_property
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_vect_diffuse_term                                   &
     &   (node, ele, surf, sf_grp, jac_sf_grp, nod_fld, rhs_tbl,        &
     &    Vsf_bcs%torque_lead, intg_point_t_evo,                        &
     &    ak_d_velo, iphys%i_velo, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_velo_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_vector_p_diffuse(node, ele, surf, sf_grp,     &
     &          Asf_bcs, iphys, nod_fld, jac_sf_grp, rhs_tbl,           &
     &          fem_wk, surf_wk, f_l)
!
      use m_ele_material_property
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(velocity_surf_bc_type), intent(in) :: Asf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_vect_diffuse_term                                   &
     &   (node, ele, surf, sf_grp, jac_sf_grp, nod_fld, rhs_tbl,        &
     &    Asf_bcs%torque_lead, intg_point_t_evo, ak_d_magne,            &
     &    iphys%i_vecp, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_vector_p_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_magne_diffuse(node, ele, surf, sf_grp,        &
     &          Bsf_bcs, iphys, nod_fld, jac_sf_grp, rhs_tbl,           &
     &          fem_wk, surf_wk, f_l)
!
      use m_ele_material_property
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_vect_diffuse_term                                   &
     &   (node, ele, surf, sf_grp, jac_sf_grp, nod_fld, rhs_tbl,        &
     &    Bsf_bcs%torque_lead, intg_point_t_evo,                        &
     &    ak_d_magne, iphys%i_magne, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_magne_diffuse
!
!-----------------------------------------------------------------------
!
      subroutine int_surf_composit_diffuse(node, ele, surf, sf_grp,     &
     &          Csf_bcs, iphys, nod_fld, jac_sf_grp, rhs_tbl,           &
     &          fem_wk, surf_wk, f_l)
!
      use m_ele_material_property
      use t_surface_bc_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_type), intent(in) :: Csf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_data),    intent(in) :: nod_fld
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_surf_diffuse_term                                        &
     &   (node, ele, surf, sf_grp, nod_fld, jac_sf_grp, rhs_tbl,        &
     &    Csf_bcs%flux_lead, intg_point_t_evo,                          &
     &    ak_d_composit, iphys%i_light, fem_wk, surf_wk, f_l)
!
      end subroutine int_surf_composit_diffuse
!
!-----------------------------------------------------------------------
!
      end module cal_current_by_vecp
