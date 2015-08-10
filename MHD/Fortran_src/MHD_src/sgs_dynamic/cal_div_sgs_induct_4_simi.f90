!
!     module cal_div_sgs_induct_4_simi
!
!     Written by H. Matsui
!
!      subroutine cal_div_sgs_induct_simi
!      subroutine cal_div_sgs_filter_idct_simi
!
      module cal_div_sgs_induct_4_simi
!
      use m_precision
!
      implicit none
!
      private :: cal_div_sgs_idct_simi
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_induct_simi
!
      use m_node_phys_address
!
       call cal_div_sgs_idct_simi(iphys%i_sgs_grad,                     &
     &     iphys%i_SGS_induct_t, iphys%i_velo, iphys%i_magne)
!
      end subroutine cal_div_sgs_induct_simi
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_filter_idct_simi
!
      use m_node_phys_address
!
       call cal_div_sgs_idct_simi(iphys%i_sgs_simi, iphys%i_sgs_grad_f, &
     &     iphys%i_filter_velo, iphys%i_filter_magne)
!
      end subroutine cal_div_sgs_filter_idct_simi
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_idct_simi(i_sgs, i_flux, i_v, i_b)
!
      use m_control_parameter
      use m_geometry_data
      use m_phys_constants
      use m_finite_element_matrix
      use m_int_vol_data
      use m_node_phys_data
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
      use int_div_sgs_idct_simi
!
      integer(kind = kint), intent(in) :: i_flux, i_v, i_b
      integer(kind = kint), intent(in) :: i_sgs
!
!
      call reset_ff_smps
!
      if ( iflag_mag_supg .gt. id_turn_OFF) then
        call int_div_sgs_idct_simi_upw(i_flux, i_v, i_b)
      else
        call int_div_sgs_idct_simi_pg(i_flux, i_v, i_b)
      end if
!
      call set_ff_nl_smp_2_ff(n_vector)
      call cal_ff_2_vector(node1%numnod, inod_smp_stack,                &
     &    d_nod(1,i_sgs), ff_nl, ml_fl)
!
! ----------   communications
!
      call vector_send_recv(i_sgs)
!
      end subroutine cal_div_sgs_idct_simi
!
!-----------------------------------------------------------------------
!
      end module cal_div_sgs_induct_4_simi
