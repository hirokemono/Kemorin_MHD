!
!     module cal_divergence
!
!     Written by H. Matsui
!
!      subroutine cal_divergence_whole(iflag_4_supg, i_res, i_vector)
!      subroutine cal_divergence_in_fluid(iflag_4_supg, i_res, i_vector)
!      subroutine cal_divergence_in_conduct(iflag_4_supg,               &
!     &          i_res, i_vector)
!
      module cal_divergence
!
      use m_precision
!
      use m_phys_constants
      use m_int_vol_data
      use m_finite_element_matrix
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
!
      implicit none
!
      private :: choose_int_vol_divs
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_divergence_whole(iflag_4_supg, i_res, i_vector)
!
      use m_geometry_parameter
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
!
!
       call reset_ff_smps
!
       call choose_int_vol_divs(iflag_4_supg,                           &
     &          iele_smp_stack, i_vector)
!
       call set_ff_nl_smp_2_ff(n_scalar)
       call cal_ff_2_scalar(d_nod(1,i_res), ff_nl, ml)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_divergence_whole
!
!-----------------------------------------------------------------------
!
      subroutine cal_divergence_in_fluid(iflag_4_supg, i_res, i_vector)
!
      use m_geometry_data_MHD
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
!
!
       call reset_ff_smps
!
       call choose_int_vol_divs(iflag_4_supg,                           &
     &          iele_fl_smp_stack, i_vector)
!
       call set_ff_nl_smp_2_ff(n_scalar)
       call cal_ff_2_scalar(d_nod(1,i_res), ff_nl, ml_fl)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_divergence_in_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_divergence_in_conduct(iflag_4_supg,                &
     &          i_res, i_vector)
!
      use m_geometry_data_MHD
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_vector, i_res
!
!
       call reset_ff_smps
!
       call choose_int_vol_divs(iflag_4_supg,                           &
     &          iele_cd_smp_stack, i_vector)
!
       call set_ff_nl_smp_2_ff(n_scalar)
       call cal_ff_2_scalar(d_nod(1,i_res), ff_nl, ml_cd)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_divergence_in_conduct
!
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_divs(iflag_4_supg,                      &
     &          iele_fsmp_stack, i_vector)
!
      use m_control_parameter
      use m_element_phys_address
!
      use int_vol_vect_diff_1st
      use int_vol_vect_diff_upw_1st
!
      integer(kind = kint), intent(in) :: iflag_4_supg, i_vector
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_vol_div_upw_1st(iele_fsmp_stack, intg_point_t_evo,     &
     &      i_vector, iphys_ele%i_magne)
      else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_vol_div_upw_1st(iele_fsmp_stack, intg_point_t_evo,     &
     &      i_vector, iphys_ele%i_velo)
      else
        call int_vol_div_1st(iele_fsmp_stack, intg_point_t_evo,         &
     &      i_vector)
      end if
!
      end subroutine choose_int_vol_divs
!
!-----------------------------------------------------------------------
!
      end module cal_divergence
