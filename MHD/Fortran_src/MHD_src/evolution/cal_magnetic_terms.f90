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
      use m_finite_element_matrix
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
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_int_vol_data
      use m_finite_element_matrix
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
       call int_vol_magne_monitor_upm(i_field)
      else
       call int_vol_magne_monitor_pg(i_field)
      end if
!
      call int_surf_magne_monitor(i_field)
!
      call cal_t_evo_4_vector_cd(iflag_mag_supg)
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
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_address
      use m_phys_constants
      use m_node_phys_data
      use m_finite_element_matrix
      use m_bc_data_magne
!
      use int_vol_diffusion_ele
      use set_boundary_scalars
!
!
      call reset_ff_smps(node1%max_nod_smp, f1_l, f1_nl)
!
      call int_vol_m_diffuse_ele_monitor
!
      call int_surf_magne_monitor(iphys%i_b_diffuse)
!
      call set_ff_nl_smp_2_ff(node1, rhs_tbl1, n_vector)
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
