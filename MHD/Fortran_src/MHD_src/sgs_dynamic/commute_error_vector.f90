!commute_error_vector.f90
!     module commute_error_vector
!
!     Written by H. Matsui
!
!       subroutine cal_commute_error_velo(i_filter, i_sgs)
!       subroutine cal_commute_error_f_velo(i_filter, i_sgs)
!       subroutine cal_commute_error_magne(i_filter, i_sgs)
!       subroutine cal_commute_error_f_magne(i_filter, i_sgs)
!       subroutine cal_commute_error_vector_p(i_filter, i_sgs)
!       subroutine cal_commute_error_f_vector_p(i_filter, i_sgs)
!         i_filter: ID for filter function
!
      module commute_error_vector
!
      use m_precision
!
      use commute_error_rotation
!
      implicit none
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_velo(i_filter, i_sgs)
!
      use m_node_phys_address
      use m_surf_data_torque
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_rotation_commute_fluid(sf_sgs1_grad_v,                  &
     &     i_filter, i_sgs, iphys%i_velo)
!
      end subroutine cal_commute_error_velo
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_velo(i_filter, i_sgs)
!
      use m_surf_data_torque
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_rotation_commute_fluid(sf_sgs1_grad_v,                  &
     &     i_filter, i_sgs, i_sgs)
!
      end subroutine cal_commute_error_f_velo
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_magne(i_filter, i_sgs)
!
      use m_node_phys_address
      use m_surf_data_magne
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_rotation_commute(sf_sgs1_grad_b,                        &
     &     i_filter, i_sgs, iphys%i_magne)
!
      end subroutine cal_commute_error_magne
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_magne(i_filter, i_sgs)
!
      use m_node_phys_address
      use m_surf_data_magne
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_rotation_commute(sf_sgs1_grad_b,                        &
     &     i_filter, i_sgs, i_sgs)
!
      end subroutine cal_commute_error_f_magne
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_vector_p(i_filter, i_sgs)
!
      use m_node_phys_address
      use m_surf_data_vector_p
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_rotation_commute(sf_sgs1_grad_a,                        &
     &     i_filter, i_sgs, iphys%i_vecp)
!
      end subroutine cal_commute_error_vector_p
!
!-----------------------------------------------------------------------
!
      subroutine cal_commute_error_f_vector_p(i_filter, i_sgs)
!
      use m_node_phys_address
      use m_surf_data_vector_p
!
      integer(kind = kint), intent(in) :: i_filter, i_sgs
!
       call cal_rotation_commute(sf_sgs1_grad_a,                        &
     &     i_filter, i_sgs, i_sgs)
!
       end subroutine cal_commute_error_f_vector_p
!
!-----------------------------------------------------------------------
!
      end module commute_error_vector
