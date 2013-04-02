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
      use m_control_parameter
      use m_phys_constants
      use m_finite_element_matrix
!
      use cal_sgs_fluxes
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
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
      use m_parallel_var_dof
      use m_node_phys_address
      use m_node_phys_data
!
      call reset_ff_smps
      call cal_sgs_uxb_2_evo
!
      call set_ff_nl_smp_2_ff(n_vector)
      call cal_ff_2_vector(d_nod(1,iphys%i_SGS_vp_induct),              &
     &    ff_nl, ml_cd)
      call vector_send_recv(iphys%i_SGS_vp_induct)
!
      end subroutine cal_sgs_uxb_2_monitor
!
!-----------------------------------------------------------------------
!
      end module monitor_sgs_terms
