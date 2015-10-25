!int_vol_solenoid_correct.f90
!     module int_vol_solenoid_correct
!
!      Written by H. Matsui on june, 2005
!
!      subroutine int_vol_velo_co
!
      module int_vol_solenoid_correct
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_node_phys_data
      use m_jacobians
      use m_sorted_node
      use m_finite_element_matrix
      use m_SGS_address
      use m_SGS_model_coefs
      use m_filter_elength
!
      implicit none
!
      private :: int_vol_solenoid_co
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_velo_co
!
!
      call int_vol_solenoid_co(iele_fl_smp_stack, iphys%i_p_phi,        &
     &    iak_diff_v)
!
      end subroutine int_vol_velo_co
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_magne_co
!
!
      call int_vol_solenoid_co(ele1%istack_ele_smp, iphys%i_m_phi,      &
     &    iak_diff_b)
!
      end subroutine int_vol_magne_co
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_magne_ins_co
!
!
      call int_vol_solenoid_co(iele_ins_smp_stack, iphys%i_mag_p,       &
     &    iak_diff_b)
!
      end subroutine int_vol_magne_ins_co
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_solenoid_co(iele_fsmp_stack,                   &
     &          i_scalar, iak_diff)
!
      use m_control_parameter
!
      use int_vol_fractional
      use int_vol_sgs_fractional
!
      integer(kind=kint), intent(in) :: i_scalar, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      if (iak_diff .gt. 0) then
        call int_vol_sgs_solenoidal_co(node1, ele1,                     &
     &      jac1_3d_q, jac1_3d_l, rhs_tbl1, FEM1_elen, nod_fld1,        &
     &      iele_fsmp_stack, intg_point_poisson, i_scalar,              &
     &      ifilter_final, ak_diff(1,iak_diff), fem1_wk, f1_nl)
      else
        call int_vol_solenoidal_co                                      &
     &     (node1, ele1, jac1_3d_q, jac1_3d_l, rhs_tbl1, nod_fld1,      &
     &      iele_fsmp_stack, intg_point_poisson, i_scalar,              &
     &      fem1_wk, f1_nl)
      end if
!
      end subroutine int_vol_solenoid_co
!
! ----------------------------------------------------------------------
!
      end module int_vol_solenoid_correct
