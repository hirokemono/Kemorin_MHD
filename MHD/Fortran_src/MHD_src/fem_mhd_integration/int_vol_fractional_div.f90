!
!     module int_vol_fractional_div
!
!      Written by H. Matsui on june, 2005
!
!      subroutine int_vol_divergence_velo
!      subroutine int_vol_divergence_vect_p
!      subroutine int_vol_divergence_magne
!      subroutine int_vol_divergence_magne_ins
!
      module int_vol_fractional_div
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_SGS_address
!
      implicit none
!
      private :: int_vol_fractional_div_ele
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_divergence_velo
!
!
      call int_vol_fractional_div_ele(iele_fl_smp_stack,                &
     &    iphys%i_velo, iak_diff_v)
!
      end subroutine int_vol_divergence_velo
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_divergence_vect_p
!
!
      call int_vol_fractional_div_ele(iele_smp_stack,                   &
     &    iphys%i_vecp, iak_diff_b)
!
      end subroutine int_vol_divergence_vect_p
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_divergence_magne
!
!
      call int_vol_fractional_div_ele(iele_smp_stack,                   &
     &    iphys%i_magne, iak_diff_b)
!
      end subroutine int_vol_divergence_magne
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_divergence_magne_ins
!
!
      call int_vol_fractional_div_ele(iele_ins_smp_stack,               &
     &    iphys%i_magne, iak_diff_b)
!
      end subroutine int_vol_divergence_magne_ins
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_fractional_div_ele(iele_fsmp_stack,            &
     &          i_vector, iak_diff)
!
      use m_control_parameter
!
      use int_vol_fractional_1st
      use int_vol_sgs_fractional_1st
!
      integer(kind = kint), intent(in) :: i_vector, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      if (iak_diff .gt. 0) then
        call int_vol_sgs_div_v_linear_1st(iele_fsmp_stack,              &
     &      intg_point_poisson, i_vector, ifilter_final, iak_diff)
      else
        call int_vol_div_vect_linear_1st(iele_fsmp_stack,               &
     &      intg_point_poisson, i_vector)
      end if
!
      end subroutine int_vol_fractional_div_ele
!
! ----------------------------------------------------------------------
!
      end module int_vol_fractional_div
