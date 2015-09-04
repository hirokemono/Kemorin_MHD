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
      use m_geometry_data_MHD
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
      use m_finite_element_matrix
!
      use int_vol_vect_diff_1st
      use cal_ff_smp_to_ffs
      use nod_phys_send_recv
!
!
      ff_nl_smp = 0.0d0
!
      call int_vol_rot_1st(iele_cd_smp_stack, intg_point_t_evo,         &
          iphys%i_SGS_vp_induct)
!
!      call cal_multi_pass_4_vector_ff
!      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,         &
!    &     ff, ml_cd, num_tot_nod_phys, iphys%i_magne, d_nod)
       call cal_ff_smp_2_vector(d_nod(1,iphys%i_SGS_induction),         &
     &    ff_nl_smp, ml_cd)
!
       call vector_send_recv                                            &
     &    (num_tot_nod_phys, iphys%i_SGS_induction, d_nod)
!
      end subroutine int_vol_sgs_induction
!
!-----------------------------------------------------------------------
!
      end module int_sgs_induction
