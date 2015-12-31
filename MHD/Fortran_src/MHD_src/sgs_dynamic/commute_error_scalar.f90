!commute_error_scalar.f90
!     module commute_error_scalar
!
!     Written by H. Matsui
!
!
!       subroutine cal_commute_error_temp(i_filter, i_sgs)
!       subroutine cal_commute_error_f_temp(i_filter, i_sgs)
!       subroutine cal_commute_error_press(i_filter, i_sgs)
!       subroutine cal_commute_error_f_press(i_filter, i_sgs)
!       subroutine cal_commute_error_magne_p(i_filter, i_sgs)
!       subroutine cal_commute_error_f_magne_p(i_filter, i_sgs)
!
      module commute_error_scalar
!
      use m_precision
!
      use commute_error_gradient
      use m_geometry_data_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_temp(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_temp
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
!
      call cal_grad_commute                                             &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    sf_sgs1_grad_t, i_filter, i_sgs, iphys%i_sgs_temp)
!
      end subroutine cal_commute_error_temp
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_temp(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_temp
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_grad_commute                                             &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    sf_sgs1_grad_t, i_filter, i_sgs, iphys%i_filter_temp)
!
      end subroutine cal_commute_error_f_temp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_press(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_press
!
       integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_grad_commute                                             &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    sf_sgs1_grad_p, i_filter, i_sgs, iphys%i_press)
!
      end subroutine cal_commute_error_press
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_press(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_press
!
       integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_grad_commute                                             &
     &   (fluid1%istack_ele_fld_smp, mhd_fem1_wk%mlump_fl,              &
     &    sf_sgs1_grad_p, i_filter, i_sgs, i_sgs)
!
      end subroutine cal_commute_error_f_press
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_magne_p(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_magne_p
!
       integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_grad_commute(ele1%istack_ele_smp, m1_lump,               &
     &    sf_sgs1_grad_f, i_filter, i_sgs, iphys%i_mag_p)
!
      end subroutine cal_commute_error_magne_p
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_magne_p(i_filter, i_sgs)
!
      use m_node_phys_data
      use m_surf_data_magne_p
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
      call cal_grad_commute(ele1%istack_ele_smp, m1_lump,               &
     &    sf_sgs1_grad_f, i_filter, i_sgs, i_sgs)
!
      end subroutine cal_commute_error_f_magne_p
!
!-----------------------------------------------------------------------
!
      end module commute_error_scalar
