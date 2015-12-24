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
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_SGS_address
      use m_filter_elength
      use m_SGS_model_coefs
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
      call int_vol_fractional_div_ele(fluid1%istack_ele_fld_smp,        &
     &    iphys%i_velo, iak_diff_v)
!
      end subroutine int_vol_divergence_velo
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_divergence_vect_p
!
!
      call int_vol_fractional_div_ele(ele1%istack_ele_smp,              &
     &    iphys%i_vecp, iak_diff_b)
!
      end subroutine int_vol_divergence_vect_p
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_divergence_magne
!
!
      call int_vol_fractional_div_ele(ele1%istack_ele_smp,              &
     &    iphys%i_magne, iak_diff_b)
!
      end subroutine int_vol_divergence_magne
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_divergence_magne_ins
!
!
      call int_vol_fractional_div_ele(insulate1%istack_ele_fld_smp,     &
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
      use int_vol_fractional
      use int_vol_sgs_fractional
!
      integer(kind = kint), intent(in) :: i_vector, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      if (iak_diff .gt. 0) then
        call int_vol_sgs_div_v_linear(node1, ele1,                      &
     &      jac1_3d_q, jac1_3d_l, rhs_tbl1, FEM1_elen, nod_fld1,        &
     &      iele_fsmp_stack, intg_point_poisson, i_vector,              &
     &      ifilter_final, ak_diff(1,iak_diff), fem1_wk, f1_l)
      else
        call int_vol_div_vect_linear                                    &
     &     (node1, ele1, jac1_3d_q, jac1_3d_l, rhs_tbl1, nod_fld1,      &
     &      iele_fsmp_stack, intg_point_poisson, i_vector,              &
     &      fem1_wk, f1_l)
      end if
!
      end subroutine int_vol_fractional_div_ele
!
! ----------------------------------------------------------------------
!
      end module int_vol_fractional_div
