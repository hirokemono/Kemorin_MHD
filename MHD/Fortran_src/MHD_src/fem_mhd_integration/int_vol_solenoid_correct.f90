!int_vol_solenoid_correct.f90
!     module int_vol_solenoid_correct
!
!      Written by H. Matsui on june, 2005
!
!!      subroutine int_vol_solenoid_co(num_int, ifilter_final,          &
!!     &          iele_fsmp_stack, i_scalar, iak_diff,                  &
!!     &          node, ele, nod_fld, jac_3d_q, jac_3d_l,               &
!!     &          rhs_tbl, FEM_elen, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_vol_solenoid_correct
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_SGS_model_coefs
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_solenoid_co(num_int, ifilter_final,            &
     &          iele_fsmp_stack, i_scalar, iak_diff,                    &
     &          node, ele, nod_fld, jac_3d_q, jac_3d_l,                 &
     &          rhs_tbl, FEM_elen, diff_coefs, fem_wk, f_nl)
!
      use int_vol_fractional
      use int_vol_sgs_fractional
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: ifilter_final
      integer(kind=kint), intent(in) :: i_scalar, iak_diff
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      if (iak_diff .gt. 0) then
        call int_vol_sgs_solenoidal_co(node, ele, jac_3d_q, jac_3d_l,   &
     &     rhs_tbl, FEM_elen, nod_fld, iele_fsmp_stack,                 &
     &     num_int, i_scalar, ifilter_final,                            &
     &     diff_coefs%num_field, iak_diff, diff_coefs%ak, fem_wk, f_nl)
      else
        call int_vol_solenoidal_co                                      &
     &     (node, ele, jac_3d_q, jac_3d_l, rhs_tbl, nod_fld,            &
     &      iele_fsmp_stack, num_int, i_scalar, fem_wk, f_nl)
      end if
!
      end subroutine int_vol_solenoid_co
!
! ----------------------------------------------------------------------
!
      end module int_vol_solenoid_correct
