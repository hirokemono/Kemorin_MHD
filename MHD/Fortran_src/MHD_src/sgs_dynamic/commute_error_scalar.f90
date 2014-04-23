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
      use m_node_phys_address
      use m_node_phys_address
      use m_surf_data_temp
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_grad_commute_fluid(ngrp_sf_sgs_temp,                    &
     &     id_grp_sf_sgs_temp, i_filter, i_sgs, iphys%i_sgs_temp)
!
      end subroutine cal_commute_error_temp
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_temp(i_filter, i_sgs)
!
      use m_node_phys_address
      use m_node_phys_address
      use m_surf_data_temp
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_grad_commute_fluid(ngrp_sf_sgs_temp,                    &
     &     id_grp_sf_sgs_temp,  i_filter, i_sgs, iphys%i_filter_temp)
!
      end subroutine cal_commute_error_f_temp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_press(i_filter, i_sgs)
!
      use m_node_phys_address
      use m_surf_data_press
!
       integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_grad_commute_fluid(ngrp_sf_sgs_p, id_grp_sf_sgs_p,      &
     &     i_filter, i_sgs, iphys%i_press)
!
      end subroutine cal_commute_error_press
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_press(i_filter, i_sgs)
!
      use m_node_phys_address
      use m_surf_data_press
!
       integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_grad_commute_fluid(ngrp_sf_sgs_p, id_grp_sf_sgs_p,      &
     &     i_filter, i_sgs, i_sgs)
!
      end subroutine cal_commute_error_f_press
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_magne_p(i_filter, i_sgs)
!
      use m_node_phys_address
      use m_surf_data_magne_p
!
       integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_grad_commute(ngrp_sf_sgs_magp, id_grp_sf_sgs_magp,      &
     &     i_filter, i_sgs, iphys%i_mag_p)
!
      end subroutine cal_commute_error_magne_p
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_magne_p(i_filter, i_sgs)
!
      use m_node_phys_address
      use m_surf_data_magne_p
!
       integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_grad_commute(ngrp_sf_sgs_magp, id_grp_sf_sgs_magp,      &
     &     i_filter, i_sgs, i_sgs)
!
      end subroutine cal_commute_error_f_magne_p
!
!-----------------------------------------------------------------------
!
      end module commute_error_scalar
