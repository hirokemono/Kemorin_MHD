!
!     module cal_magnetic_terms
!
!     Written by H. Matsui on June, 2005
!
!      subroutine cal_terms_4_magnetic(i_field)
!      subroutine cal_magnetic_diffusion
!
      module cal_magnetic_terms
!
      use m_precision
!
      use m_control_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_element_phys_data
      use m_geometry_data_MHD
      use m_jacobians
      use m_jacobian_sf_grp
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use cal_multi_pass
      use nod_phys_send_recv
      use int_surf_magne_pre
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_magnetic(i_field)
!
      use m_bc_data_magne
!
      use int_vol_magne_monitor
      use set_boundary_scalars
!
      integer (kind=kint), intent(in) :: i_field
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      if (iflag_mag_supg .gt. id_turn_OFF) then
        call int_vol_magne_monitor_upm(i_field, node1, ele1, conduct1,  &
     &      iphys, nod_fld1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,  &
     &      FEM1_elen, mhd_fem1_wk, fem1_wk, f1_nl)
      else
        call int_vol_magne_monitor_pg(i_field, node1, ele1, conduct1,   &
     &      iphys, nod_fld1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,  &
     &      FEM1_elen, mhd_fem1_wk, fem1_wk, f1_nl)
      end if
!
      call int_surf_magne_monitor(i_field, node1, ele1, surf1, sf_grp1, &
     &    iphys, nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,       &
     &    fem1_wk, f1_l, f1_nl)
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg,                        &
     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,            &
     &    nod_comm, node1, ele1, iphys_ele, fld_ele1, jac1_3d_q,        &
     &    rhs_tbl1, mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%mlump_cd%ml, nod_fld1%ntot_phys,        &
     &    i_field, nod_fld1%d_fld)
      call vector_send_recv(i_field, node1, nod_comm, nod_fld1)
!
      end subroutine cal_terms_4_magnetic
!
!-----------------------------------------------------------------------
!
      subroutine cal_magnetic_diffusion
!
      use m_geometry_data
      use m_group_data
      use m_phys_constants
      use m_node_phys_data
      use m_bc_data_magne
!
      use int_vol_diffusion_ele
      use set_boundary_scalars
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_vector_diffuse_ele(conduct1%istack_ele_fld_smp,      &
     &    node1, ele1, nod_fld1, jac1_3d_q, rhs_tbl1, FEM1_elen,        &
     &    iak_diff_b, one, ak_d_magne, iphys%i_magne, fem1_wk, f1_l)
!
      call int_surf_magne_monitor                                       &
     &   (iphys%i_b_diffuse, node1, ele1, surf1, sf_grp1,               &
     &    iphys, nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen,       &
     &    fem1_wk, f1_l, f1_nl)
!
      call set_ff_nl_smp_2_ff(n_vector, node1, rhs_tbl1, f1_l, f1_nl)
!
      call delete_vector_ffs_on_bc(node1, nod_bc1_b, f1_l, f1_nl)
!
      call cal_ff_2_vector                                              &
     &   (node1%numnod, node1%istack_nod_smp, f1_l%ff, m1_lump%ml,      &
     &    nod_fld1%ntot_phys, iphys%i_b_diffuse, nod_fld1%d_fld)
      call vector_send_recv                                             &
     &   (iphys%i_b_diffuse, node1, nod_comm, nod_fld1)
!
      end subroutine cal_magnetic_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_magnetic_terms
