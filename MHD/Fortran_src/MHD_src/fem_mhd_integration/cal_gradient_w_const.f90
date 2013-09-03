!cal_gradient_w_const.f90
!     module cal_gradient_w_const
!
!     Written by H. Matsui
!
!      subroutine cal_gradent(i_res, i_scalar)
!      subroutine cal_gradent_in_fluid(i_res, i_scalar)
!      subroutine cal_gradent_in_conduct(i_res, i_scalar)
!
!      subroutine cal_gradent_w_const(i_res, i_scalar, const)
!      subroutine cal_gradent_in_fluid_w_const(i_res, i_scalar, const)
!      subroutine cal_gradent_in_conduct_w_const(i_res, i_scalar, const)
!
      module cal_gradient_w_const
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
      private :: choose_int_vol_grads_w_const
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_gradent_w_const(i_res, i_scalar, const)
!
      use m_geometry_parameter
      use m_node_phys_data
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: i_scalar
      integer(kind = kint), intent(in) :: i_res
!
!
!
       call reset_ff_smps
!
       call choose_int_vol_grads_w_const(iele_smp_stack, const,         &
      &    i_scalar)
!
       call set_ff_nl_smp_2_ff(n_vector)
       call cal_ff_2_vector(d_nod(1,i_res), ff_nl, ml)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_gradent_w_const
!
!-----------------------------------------------------------------------
!
      subroutine cal_gradent_in_fluid_w_const(i_res, i_scalar, const)
!
      use m_geometry_data_MHD
      use m_geometry_parameter
      use m_node_phys_data
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: i_scalar
      integer(kind = kint), intent(in) :: i_res
!
!
       call reset_ff_smps
!
       call choose_int_vol_grads_w_const(iele_fl_smp_stack, const,      &
     &     i_scalar)
!
       call set_ff_nl_smp_2_ff(n_vector)
       call cal_ff_2_vector(d_nod(1,i_res), ff_nl, ml_fl)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_gradent_in_fluid_w_const
!
!-----------------------------------------------------------------------
!
      subroutine cal_gradent_in_conduct_w_const(i_res, i_scalar, const)
!
      use m_geometry_data_MHD
      use m_geometry_parameter
      use m_node_phys_data
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: i_scalar
      integer(kind = kint), intent(in) :: i_res
!
!
       call reset_ff_smps
!
       call choose_int_vol_grads_w_const(iele_cd_smp_stack, const,      &
     &     i_scalar)
!
       call set_ff_nl_smp_2_ff(n_vector)
       call cal_ff_2_vector(d_nod(1,i_res), ff_nl, ml_cd)
!
! ----------   communications
!
      call vector_send_recv(i_res)
!
      end subroutine cal_gradent_in_conduct_w_const
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_grads_w_const(iele_fsmp_stack, const,   &
     &          i_scalar)
!
      use m_control_parameter
      use m_element_phys_address
      use int_vol_vect_cst_diff_1st
      use int_vol_vect_cst_diff_upw_1
!
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: i_scalar
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_vol_grad_w_const_upw_1(iele_fsmp_stack,                &
     &     intg_point_t_evo, i_scalar, const, iphys_ele%i_magne)
      else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_vol_grad_w_const_upw_1(iele_fsmp_stack,                &
     &     intg_point_t_evo, i_scalar, const, iphys_ele%i_velo)
      else
         call int_vol_grad_w_const_1st(iele_fsmp_stack,                 &
     &     intg_point_t_evo, i_scalar, const)
      end if
!
      end subroutine choose_int_vol_grads_w_const
!
!-----------------------------------------------------------------------
!
      end module cal_gradient_w_const
