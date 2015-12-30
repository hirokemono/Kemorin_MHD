!
!     module cal_rotation_fields
!
!     Written by H. Matsui
!
!      subroutine cal_vorticity
!      subroutine cal_current_density
!      subroutine cal_magnetic_f_by_vect_p
!
      module cal_rotation_fields
!
      use m_precision
!
      use m_control_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_SGS_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_vorticity
!
      use m_bc_data_velo
      use m_surf_data_torque
!
      use cal_rotation_sgs
!
!
      call choose_cal_rotation_sgs(iflag_commute_velo, iflag_velo_supg, &
     &    fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    node1, ele1, surf1, sf_grp1, nod_bc1_w, sf_sgs1_grad_v,       &
     &    iak_diff_v, iphys%i_velo, iphys%i_vort)
!
      end subroutine cal_vorticity
!
!-----------------------------------------------------------------------
!
      subroutine cal_current_density
!
      use m_bc_data_magne
      use m_surf_data_magne
      use m_surf_data_current
!
      use cal_rotation_sgs
!
!
      call choose_cal_rotation_sgs(iflag_commute_magne, iflag_mag_supg, &
     &    ele1%istack_ele_smp, m1_lump, node1, ele1, surf1, sf_grp1,    &
     &    nod_bc1_j, sf_sgs1_grad_b, iak_diff_b,                        &
     &    iphys%i_magne, iphys%i_current)
!      call choose_cal_rotation_sgs(iflag_commute_magne, iflag_mag_supg,&
!     &    conduct1%istack_ele_fld_smp, mhd_fem1_wk%mlump_cd,           &
!     &    node1, ele1, surf1, sf_grp1, nod_bc1_j, sf_sgs1_grad_b,      &
!     &     iak_diff_b, iphys%i_magne, iphys%i_current)
!
      end subroutine cal_current_density
!
!-----------------------------------------------------------------------
!
      subroutine cal_magnetic_f_by_vect_p
!
      use m_surf_data_vector_p
      use m_bc_data_magne
!
      use cal_rotation_sgs
!
      call choose_cal_rotation_sgs(iflag_commute_magne, iflag_mag_supg, &
     &    ele1%istack_ele_smp, m1_lump, node1, ele1, surf1, sf_grp1,    &
     &    nod_bc1_b, sf_sgs1_grad_a, iak_diff_b,                        &
     &    iphys%i_vecp, iphys%i_magne)
!
      end subroutine cal_magnetic_f_by_vect_p
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module cal_rotation_fields
