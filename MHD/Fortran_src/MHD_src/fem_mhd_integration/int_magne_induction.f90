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
!
      implicit none
!
      integer(kind = kint), parameter, private :: n_vector = 3
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
      use m_geometry_data
      use m_finite_element_matrix
      use m_int_vol_data
      use m_node_phys_address
      use m_node_phys_data
!
      use int_vol_vect_diff_1st
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
!
      ff_nl_smp = 0.0d0
!
      call int_vol_rot_1st(ele1%istack_ele_smp, intg_point_poisson,     &
          iphys%i_vp_induct)
!
!      call cal_multi_pass_4_vector_ff
!      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,         &
!     &    ff, ml_cd, nod_fld1%ntot_phys, iphys%i_magne, d_nod)
      call cal_ff_smp_2_vector(ff_nl_smp, ml_cd,                        &
     &    nod_fld1%ntot_phys, iphys%i_induction, d_nod)
!
!    communication
!
      call vector_send_recv                                             &
     &   (nod_fld1%ntot_phys, iphys%i_induction, d_nod)
!
      end subroutine s_int_magne_induction
!
! ----------------------------------------------------------------------
!
      end module int_magne_induction
