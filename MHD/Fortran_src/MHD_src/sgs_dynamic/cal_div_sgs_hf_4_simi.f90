!
!     module cal_div_sgs_hf_4_simi
!
!     Written by H. Matsui
!
!       subroutine cal_div_sgs_h_flux_simi
!       subroutine cal_div_sgs_filter_hf_simi
!
      module cal_div_sgs_hf_4_simi
!
      use m_precision
!
      use m_control_parameter
!
      implicit none
!
      private :: cal_div_sgs_hf_simi
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_hf_simi(i_sgs, i_flux, i_vect, i_scalar)
!
      use m_finite_element_matrix
      use m_int_vol_data
      use m_node_phys_data
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use int_div_sgs_hf_simi
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: i_flux, i_vect, i_scalar
      integer(kind = kint), intent(in) :: i_sgs
!
      integer (kind=kint), parameter :: numdir_d = 1
!
!
       call reset_ff_smps
!
        if ( iflag_temp_supg .gt. id_turn_OFF) then
          call int_div_sgs_hf_simi_upw(i_flux, i_vect, i_scalar)
        else
          call int_div_sgs_hf_simi_pg(i_flux, i_vect, i_scalar)
        end if
!
       call set_ff_nl_smp_2_ff(numdir_d)
       call cal_ff_2_scalar(d_nod(1,i_sgs), ff_nl, ml_fl)
!
! ----------   communications
!
      call scalar_send_recv(i_sgs)
!
      end subroutine cal_div_sgs_hf_simi
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_sgs_h_flux_simi
!
      use m_node_phys_address
!
      call cal_div_sgs_hf_simi(iphys%i_sgs_grad, iphys%i_SGS_h_flux,    &
     &    iphys%i_velo, iphys%i_sgs_temp)
!
      end subroutine cal_div_sgs_h_flux_simi
!
!-----------------------------------------------------------------------
!
       subroutine cal_div_sgs_filter_hf_simi
!
       use m_node_phys_address
!
       call cal_div_sgs_hf_simi(iphys%i_sgs_simi, iphys%i_sgs_grad_f,   &
     &     iphys%i_filter_velo, iphys%i_filter_temp)
!
       end subroutine cal_div_sgs_filter_hf_simi
!
!-----------------------------------------------------------------------
!
       end module cal_div_sgs_hf_4_simi
