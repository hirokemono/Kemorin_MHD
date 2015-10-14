!
!      module monitor_sgs_terms
!
!      Written by H. Matsui
!
!      subroutine cal_sgs_uxb_2_monitor
!
      module monitor_sgs_terms
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_2_monitor
!
      use calypso_mpi
      use m_geometry_data
      use m_sorted_node
      use m_node_phys_address
      use m_node_phys_data
!
      use m_control_parameter
      use m_phys_constants
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_sgs_fluxes
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
!
!
      call reset_ff_smps
      call cal_sgs_uxb_2_evo
!
      call set_ff_nl_smp_2_ff(node1, rhs_tbl1, n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%ml_cd, nod_fld1%ntot_phys,              &
     &    iphys%i_SGS_vp_induct, nod_fld1%d_fld)
      call vector_send_recv                                             &
     &   (nod_fld1%ntot_phys, iphys%i_SGS_vp_induct, nod_fld1%d_fld)
!
      end subroutine cal_sgs_uxb_2_monitor
!
!-----------------------------------------------------------------------
!
      end module monitor_sgs_terms
