!
!      module int_magne_induction
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine s_int_magne_induction
!
      module int_magne_induction
!
      use m_precision
      use m_phys_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_int_magne_induction
!
      use m_control_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_jacobians
      use m_sorted_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_node_phys_data
!
      use int_vol_vect_differences
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
!
      call reset_ff_smp(node1%max_nod_smp, f1_nl)
!
      call int_vol_rotation(node1, ele1, jac1_3d_q, rhs_tbl1, nod_fld1, &
     &    ele1%istack_ele_smp, intg_point_poisson, iphys%i_vp_induct,   &
     &    fem1_wk, f1_nl)
!
!      call cal_multi_pass_4_vector_ff
!      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,         &
!     &   f1_l%ff, mhd_fem1_wk%mlump_cd%ml, nod_fld1%ntot_phys,         &
!     &   iphys%i_magne, nod_fld1%d_fld)
      call cal_ff_smp_2_vector(node1, rhs_tbl1,                         &
     &    f1_nl%ff_smp, mhd_fem1_wk%mlump_cd%ml,                        &
     &    nod_fld1%ntot_phys, iphys%i_induction, nod_fld1%d_fld)
!
!    communication
!
      call vector_send_recv                                             &
     &   (iphys%i_induction, node1, nod_comm, nod_fld1)
!
      end subroutine s_int_magne_induction
!
! ----------------------------------------------------------------------
!
      end module int_magne_induction
