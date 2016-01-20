!
!     module int_vol_fractional_div
!
!      Written by H. Matsui on june, 2005
!
!!      subroutine int_vol_fractional_div_ele                           &
!!     &         (iele_fsmp_stack, i_vector, iak_diff,                  &
!!     &          node, ele, nod_fld, jac_3d_q, jac_3d_l,               &
!!     &          rhs_tbl, FEM_elen, fem_wk, f_l)
!
      module int_vol_fractional_div
!
      use m_precision
!
      use m_machine_parameter
      use m_SGS_address
      use m_SGS_model_coefs
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_fractional_div_ele                             &
     &         (iele_fsmp_stack, i_vector, iak_diff,                    &
     &          node, ele, nod_fld, jac_3d_q, jac_3d_l,                 &
     &          rhs_tbl, FEM_elen, fem_wk, f_l)
!
      use m_control_parameter
!
      use int_vol_fractional
      use int_vol_sgs_fractional
!
      integer(kind = kint), intent(in) :: i_vector, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iak_diff .gt. 0) then
        call int_vol_sgs_div_v_linear(node, ele,                        &
     &      jac_3d_q, jac_3d_l, rhs_tbl, FEM_elen, nod_fld,             &
     &      iele_fsmp_stack, intg_point_poisson, i_vector,              &
     &      ifilter_final, ak_diff(1,iak_diff), fem_wk, f_l)
      else
        call int_vol_div_vect_linear                                    &
     &     (node, ele, jac_3d_q, jac_3d_l, rhs_tbl, nod_fld,            &
     &      iele_fsmp_stack, intg_point_poisson, i_vector,              &
     &      fem_wk, f_l)
      end if
!
      end subroutine int_vol_fractional_div_ele
!
! ----------------------------------------------------------------------
!
      end module int_vol_fractional_div
