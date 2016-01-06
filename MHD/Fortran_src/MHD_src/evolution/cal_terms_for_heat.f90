!
!     module cal_terms_for_heat
!
!     Written by H. Matsui on June, 2005
!
!      subroutine cal_terms_4_heat(i_field)
!      subroutine cal_viscous_diffusion
!
      module cal_terms_for_heat
!
      use m_precision
!
      use m_control_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_phys_constants
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_element_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_int_vol_data
      use m_finite_element_matrix
      use m_filter_elength
!
      use m_bc_data_ene
!
      use cal_multi_pass
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
      use set_boundary_scalars
      use int_surf_temp
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_terms_4_heat(i_field)
!
      use int_vol_temp_monitor
!
      integer (kind = kint), intent(in) :: i_field
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      if (iflag_temp_supg .gt. id_turn_OFF) then
       call int_vol_ene_monitor_upw(i_field, node1, ele1, fluid1,       &
     &     iphys, nod_fld1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,   &
     &      FEM1_elen, mhd_fem1_wk, fem1_wk, f1_nl)
      else
       call int_vol_ene_monitor(i_field, node1, ele1, fluid1,           &
     &     iphys, nod_fld1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,   &
     &     FEM1_elen, mhd_fem1_wk, fem1_wk, f1_nl)
      end if
!
      call int_surf_temp_monitor(node1, ele1, surf1, sf_grp1, i_field)
!
      call cal_t_evo_4_scalar(iflag_temp_supg,                          &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl, nod_comm,    &
     &    node1, ele1, iphys_ele, fld_ele1, jac1_3d_q, rhs_tbl1,        &
     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!
      call set_boundary_rhs_scalar(node1, nod_bc1_t, f1_l, f1_nl)
!
!       call check_ff(my_rank, n_scalar, node1%numnod, f1_nl)
!
      call cal_ff_2_scalar(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%mlump_fl%ml,                            &
     &    nod_fld1%ntot_phys, i_field, nod_fld1%d_fld)
!
!   communication
!
      call scalar_send_recv(i_field, node1, nod_comm, nod_fld1)
!
      end subroutine cal_terms_4_heat
!
!-----------------------------------------------------------------------
!
      subroutine cal_thermal_diffusion
!
      use int_vol_diffusion_ele
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_t_diffuse_ele_monitor
!
      call int_surf_temp_monitor(node1, ele1, surf1, sf_grp1,           &
     &                           iphys%i_t_diffuse)
!
      call set_ff_nl_smp_2_ff(n_scalar, node1, rhs_tbl1, f1_l, f1_nl)
!
      call set_boundary_rhs_scalar(node1, nod_bc1_t, f1_l, f1_nl)
!
      call cal_ff_2_scalar(node1%numnod, node1%istack_nod_smp,          &
     &    f1_l%ff, mhd_fem1_wk%mlump_fl%ml,                             &
     &    nod_fld1%ntot_phys, iphys%i_t_diffuse, nod_fld1%d_fld)
!
!   communication
!
      call scalar_send_recv                                             &
     &   (iphys%i_t_diffuse, node1, nod_comm, nod_fld1)
!
      end subroutine cal_thermal_diffusion
!
!-----------------------------------------------------------------------
!
      end module cal_terms_for_heat
