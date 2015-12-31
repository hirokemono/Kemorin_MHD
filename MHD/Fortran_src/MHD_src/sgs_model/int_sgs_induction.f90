!
!     module int_sgs_induction
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on AUg., 2007
!
!      subroutine int_vol_sgs_induction
!
      module int_sgs_induction
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_sgs_induction
!
      use m_control_parameter
      use m_nod_comm_table
      use m_geometry_data_MHD
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
!
      use int_vol_vect_differences
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
!
      call reset_ff_smp(node1%max_nod_smp, f1_nl)
!
      call int_vol_rotation(node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1, &
     &    conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    iphys%i_SGS_vp_induct, fem1_wk, f1_nl)
!
!      call cal_multi_pass_4_vector_ff(ele1%istack_ele_smp, m1_lump,    &
!     &    nod_comm, node1, ele1, jac1_3d_q, rhs_tbl1,                  &
!     &    mhd_fem1_wk%ff_m_smp, fem1_wk, f1_l, f1_nl)
!      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,         &
!     &    f1_l%ff, mhd_fem1_wk%mlump_cd%ml, nod_fld1%ntot_phys,        &
!     &    iphys%i_magne, nod_fld1%d_fld)
       call cal_ff_smp_2_vector                                         &
     &    (node1, rhs_tbl1, f1_nl%ff_smp, mhd_fem1_wk%mlump_cd%ml,      &
     &     nod_fld1%ntot_phys, iphys%i_SGS_induction, nod_fld1%d_fld)
!
       call vector_send_recv                                            &
     &    (iphys%i_SGS_induction, node1, nod_comm, nod_fld1)
!
      end subroutine int_vol_sgs_induction
!
!-----------------------------------------------------------------------
!
      end module int_sgs_induction
