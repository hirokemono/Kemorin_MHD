!cal_gradient_w_const.f90
!     module cal_gradient_w_const
!
!     Written by H. Matsui
!
!      subroutine cal_gradent_w_const(iflag_4_supg,                     &
!     &          i_res, i_scalar, const)
!      subroutine cal_gradent_in_fluid_w_const(iflag_4_supg,            &
!     &          i_res, i_scalar, const)
!      subroutine cal_gradent_in_conduct_w_const(iflag_4_supg,          &
!     &          i_res, i_scalar, const)
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
      subroutine cal_gradent_w_const(iflag_4_supg,                      &
     &          i_res, i_scalar, const)
!
      use m_geometry_data
      use m_node_phys_data
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_scalar, i_res
!
!
!
      call reset_ff_smps
!
      call choose_int_vol_grads_w_const(iflag_4_supg,                   &
     &    ele1%istack_ele_smp, const, i_scalar)
!
      call set_ff_nl_smp_2_ff(n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    ff_nl, ml, nod_fld1%ntot_phys, i_res, d_nod)
!
! ----------   communications
!
      call vector_send_recv(nod_fld1%ntot_phys, i_res, d_nod)
!
      end subroutine cal_gradent_w_const
!
!-----------------------------------------------------------------------
!
      subroutine cal_gradent_in_fluid_w_const(iflag_4_supg,             &
     &          i_res, i_scalar, const)
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_scalar, i_res
!
!
      call reset_ff_smps
!
      call choose_int_vol_grads_w_const(iflag_4_supg,                   &
     &     iele_fl_smp_stack, const, i_scalar)
!
      call set_ff_nl_smp_2_ff(n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    ff_nl, ml_fl, nod_fld1%ntot_phys, i_res, d_nod)
!
! ----------   communications
!
      call vector_send_recv(nod_fld1%ntot_phys, i_res, d_nod)
!
      end subroutine cal_gradent_in_fluid_w_const
!
!-----------------------------------------------------------------------
!
      subroutine cal_gradent_in_conduct_w_const(iflag_4_supg,           &
     &          i_res, i_scalar, const)
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: i_scalar, i_res
!
!
      call reset_ff_smps
!
      call choose_int_vol_grads_w_const(iflag_4_supg,                   &
     &     iele_cd_smp_stack, const, i_scalar)
!
      call set_ff_nl_smp_2_ff(n_vector)
      call cal_ff_2_vector(node1%numnod, node1%istack_nod_smp,          &
     &    ff_nl, ml_cd, nod_fld1%ntot_phys, i_res, d_nod)
!
! ----------   communications
!
      call vector_send_recv(nod_fld1%ntot_phys, i_res, d_nod)
!
      end subroutine cal_gradent_in_conduct_w_const
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_grads_w_const(iflag_4_supg,             &
     &          iele_fsmp_stack, const, i_scalar)
!
      use m_control_parameter
      use m_element_phys_data
      use int_vol_vect_cst_diff_1st
      use int_vol_vect_cst_diff_upw_1
!
!
      real(kind = kreal), intent(in) :: const
      integer(kind = kint), intent(in) :: iflag_4_supg, i_scalar
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_vol_grad_w_const_upw_1(iele_fsmp_stack,                &
     &      intg_point_t_evo, i_scalar, fld_ele1%ntot_phys,             &
     &      iphys_ele%i_magne, fld_ele1%d_fld, const)
      else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_vol_grad_w_const_upw_1(iele_fsmp_stack,                &
     &      intg_point_t_evo, i_scalar, fld_ele1%ntot_phys,             &
     &      iphys_ele%i_velo, fld_ele1%d_fld, const)
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
