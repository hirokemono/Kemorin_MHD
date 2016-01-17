!
!     module cal_momentum_terms
!
!     Written by H. Matsui on June, 2005
!
!      subroutine cal_terms_4_momentum(i_field)
!      subroutine cal_viscous_diffusion
!
      module cal_momentum_terms
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
!      use m_nod_comm_table
!      use m_geometry_data
!      use m_group_data
!      use m_geometry_data_MHD
!      use m_node_phys_data
!      use m_element_phys_data
!      use m_jacobians
!      use m_jacobian_sf_grp
!      use m_element_id_4_node
!      use m_finite_element_matrix
!      use m_int_vol_data
!      use m_filter_elength
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_filter_elength
!
      use cal_multi_pass
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
      use set_nodal_bc_id_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_momentum                                   &
     &         (i_field, nod_comm, node1, ele1, surf1, fluid1, sf_grp1, &
     &          iphys, iphys_ele, fld_ele1, jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1,        &
     &          FEM1_elen, mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
!
      use int_vol_velo_monitor
      use int_surf_velo_pre
!
      integer (kind=kint), intent(in) :: i_field
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node1
      type(element_data), intent(in) :: ele1
      type(surface_data), intent(in) :: surf1
      type(surface_group_data), intent(in) :: sf_grp1
      type(field_geometry_data), intent(in) :: fluid1
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: fld_ele1
      type(jacobians_3d), intent(in) :: jac1_3d_q
      type(jacobians_2d), intent(in) :: jac1_sf_grp_2d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
      type(gradient_model_data_type), intent(in) :: FEM1_elen
!
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem1_wk
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
      type(phys_data), intent(inout) :: nod_fld1
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      if (iflag_velo_supg .eq. id_turn_ON) then
        call int_vol_velo_monitor_upwind(i_field, node1, ele1, fluid1,  &
     &     iphys, nod_fld1, iphys_ele, fld_ele1, iphys_ele%i_velo,      &
     &     jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk, f1_nl)
      else if (iflag_velo_supg .eq. id_magnetic_SUPG) then
        call int_vol_velo_monitor_upwind(i_field, node1, ele1, fluid1,  &
     &     iphys, nod_fld1, iphys_ele, fld_ele1, iphys_ele%i_magne,     &
     &     jac1_3d_q, rhs_tbl1, FEM1_elen, mhd_fem1_wk, fem1_wk, f1_nl)
      else
       call int_vol_velo_monitor_pg(i_field, node1, ele1, fluid1,       &
     &     iphys, nod_fld1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,   &
     &     FEM1_elen, mhd_fem1_wk, fem1_wk, f1_nl)
      end if
!
      call int_surf_velo_monitor(i_field, node1, ele1, surf1, sf_grp1,  &
     &    iphys, nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,       &
     &    fem1_wk, f1_l, f1_nl)
!
      call cal_t_evo_4_vector(iflag_velo_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!       call set_boundary_velo_4_rhs(node1, f1_l, f1_nl)
!
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%mlump_fl%ml,                            &
     &    nod_fld1%ntot_phys, i_field, nod_fld1%d_fld)
      call vector_send_recv(i_field, node1, nod_comm, nod_fld1)
!
      end subroutine cal_terms_4_momentum
!
!-----------------------------------------------------------------------
!
      subroutine cal_viscous_diffusion(nod_comm, node1, ele1, surf1,    &
     &          fluid1, sf_grp1, iphys, jac1_3d_q, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, &
     &          mhd_fem1_wk, fem1_wk, f1_l, f1_nl, nod_fld1)
!
      use int_vol_diffusion_ele
      use int_surf_velo_pre
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node1
      type(element_data), intent(in) :: ele1
      type(surface_data), intent(in) :: surf1
      type(surface_group_data), intent(in) :: sf_grp1
      type(field_geometry_data), intent(in) :: fluid1
      type(phys_address), intent(in) :: iphys
      type(jacobians_3d), intent(in) :: jac1_3d_q
      type(jacobians_2d), intent(in) :: jac1_sf_grp_2d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl1
      type(gradient_model_data_type), intent(in) :: FEM1_elen
      type(work_MHD_fe_mat), intent(in) :: mhd_fem1_wk
!
      type(work_finite_element_mat), intent(inout) :: fem1_wk
      type(finite_ele_mat_node), intent(inout) :: f1_l, f1_nl
      type(phys_data), intent(inout) :: nod_fld1
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_vector_diffuse_ele(fluid1%istack_ele_fld_smp,        &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,        &
     &    iak_diff_v, one, ak_d_velo, iphys%i_velo, fem1_wk, f1_l)
!
      call int_surf_velo_monitor                                        &
     &   (iphys%i_v_diffuse, node1, ele1, surf1, sf_grp1,               &
     &    iphys, nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,       &
     &    fem1_wk, f1_l, f1_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      call set_boundary_velo_4_rhs(node1, f1_l, f1_nl)
!
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_l%ff, mhd_fem1_wk%mlump_fl%ml,                             &
     &    nod_fld1%ntot_phys, iphys%i_v_diffuse, nod_fld1%d_fld)
!
      call vector_send_recv                                             &
     &   (iphys%i_v_diffuse, node1, nod_comm, nod_fld1)
!
      end subroutine cal_viscous_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_momentum_terms
