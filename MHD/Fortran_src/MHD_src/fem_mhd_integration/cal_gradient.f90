!
!     module cal_gradient
!
!     Written by H. Matsui
!
!      subroutine cal_gradent_whole(i_res, i_scalar)
!      subroutine cal_gradent_in_fluid(i_res, i_scalar)
!      subroutine cal_gradent_in_conduct(i_res, i_scalar)
!
      module cal_gradient
!
      use m_precision
!
      use m_phys_constants
      use m_finite_element_matrix
      use m_int_vol_data
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
!
      implicit none
!
      private :: choose_int_vol_grads
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_gradent_whole(i_res, i_scalar)
!
      use m_geometry_parameter
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: i_scalar
      integer(kind = kint), intent(in) :: i_res
!
!
!
       call reset_ff_smps
!
       call choose_int_vol_grads(iele_smp_stack, i_scalar)
!
       call set_ff_nl_smp_2_ff(n_vector)
       call cal_ff_2_vector(d_nod(1,i_res), ff_nl, ml)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_gradent_whole
!
!-----------------------------------------------------------------------
!
      subroutine cal_gradent_in_fluid(i_res, i_scalar)
!
      use m_geometry_data_MHD
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: i_scalar
      integer(kind = kint), intent(in) :: i_res
!
!
       call reset_ff_smps
!
       call choose_int_vol_grads(iele_fl_smp_stack, i_scalar)
!
       call set_ff_nl_smp_2_ff(n_vector)
       call cal_ff_2_vector(d_nod(1,i_res), ff_nl, ml_fl)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_gradent_in_fluid
!
!-----------------------------------------------------------------------
!
      subroutine cal_gradent_in_conduct(i_res, i_scalar)
!
      use m_geometry_data_MHD
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: i_scalar
      integer(kind = kint), intent(in) :: i_res
!
!
       call reset_ff_smps
!
       call choose_int_vol_grads(iele_cd_smp_stack, i_scalar)
!
       call set_ff_nl_smp_2_ff(n_vector)
       call cal_ff_2_vector(d_nod(1,i_res), ff_nl, ml_cd)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_gradent_in_conduct
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_grads(iele_fsmp_stack, i_scalar)
!
      use m_control_parameter
      use m_element_phys_address
!
      use int_vol_vect_diff_1st
      use int_vol_vect_diff_upw_1st
!
      integer(kind = kint), intent(in) :: i_scalar
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      if ( iflag_4_supg .eq. 2) then
        call int_vol_grad_upw_1st(iele_fsmp_stack, intg_point_t_evo,    &
     &      i_scalar, iphys_ele%i_magne)
      else if ( iflag_4_supg .ge. 1) then
        call int_vol_grad_upw_1st(iele_fsmp_stack, intg_point_t_evo,    &
     &      i_scalar, iphys_ele%i_velo)
      else
         call int_vol_grad_1st(iele_fsmp_stack, intg_point_t_evo,       &
     &      i_scalar)
      end if
!
      end subroutine choose_int_vol_grads
!
!-----------------------------------------------------------------------
!
      end module cal_gradient
