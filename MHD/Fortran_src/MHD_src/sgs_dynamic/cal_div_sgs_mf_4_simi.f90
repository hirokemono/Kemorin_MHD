!
!     module cal_div_sgs_mf_4_simi
!
!     Written by H. Matsui
!
!       subroutine cal_div_sgs_m_flux_simi
!       subroutine cal_div_sgs_maxwell_simi
!       subroutine cal_div_sgs_filter_mf_simi
!       subroutine cal_div_sgs_filter_mxwl_simi
!
      module cal_div_sgs_mf_4_simi
!
      use m_precision
!
      implicit none
!
      private :: cal_div_sgs_mf_simi
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine cal_div_sgs_m_flux_simi
!
       use m_node_phys_address
!
       call cal_div_sgs_mf_simi(iphys%i_sgs_grad, iphys%i_SGS_m_flux,   &
     &     iphys%i_velo)
!
       end subroutine cal_div_sgs_m_flux_simi
!
!-----------------------------------------------------------------------
!
       subroutine cal_div_sgs_maxwell_simi
!
       use m_node_phys_address
!
       call cal_div_sgs_mf_simi(iphys%i_sgs_grad, iphys%i_SGS_maxwell,  &
      &    iphys%i_magne)
!
       end subroutine cal_div_sgs_maxwell_simi
!
!-----------------------------------------------------------------------
!
       subroutine cal_div_sgs_filter_mf_simi
!
       use m_node_phys_address
!
       call cal_div_sgs_mf_simi(iphys%i_sgs_simi, iphys%i_sgs_grad_f,   &
     &     iphys%i_filter_velo)
!
       end subroutine cal_div_sgs_filter_mf_simi
!
!-----------------------------------------------------------------------
!
       subroutine cal_div_sgs_filter_mxwl_simi
!
       use m_node_phys_address
!
       call cal_div_sgs_mf_simi(iphys%i_sgs_simi, iphys%i_sgs_grad_f,   &
     &     iphys%i_filter_magne)
!
       end subroutine cal_div_sgs_filter_mxwl_simi
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_mf_simi(i_sgs, i_flux, i_vect)
!
      use m_control_parameter
      use m_geometry_data
      use m_finite_element_matrix
      use m_int_vol_data
      use m_sorted_node
      use m_phys_constants
      use m_node_phys_data
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
      use int_div_sgs_mf_simi
!
      integer(kind = kint), intent(in) :: i_flux, i_vect
      integer(kind = kint), intent(in) :: i_sgs
!
!
      call reset_ff_smps
!
      if (iflag_velo_supg .eq. id_magnetic_SUPG) then
        call int_div_sgs_mf_simi_upm(i_flux, i_vect)
      else if (iflag_velo_supg .eq. id_turn_ON) then
        call int_div_sgs_mf_simi_upw(i_flux, i_vect)
      else
        call int_div_sgs_mf_simi_pg(i_flux, i_vect)
      end if
!
      call set_ff_nl_smp_2_ff(node1, rhs_tbl1, n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    f1_nl%ff, mhd_fem1_wk%ml_fl, nod_fld1%ntot_phys,              &
     &    i_sgs, nod_fld1%d_fld)
!
! ----------   communications
!
      call vector_send_recv(nod_fld1%ntot_phys, i_sgs, nod_fld1%d_fld)
!
      end subroutine cal_div_sgs_mf_simi
!
!-----------------------------------------------------------------------
!
      end module cal_div_sgs_mf_4_simi
