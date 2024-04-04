!
!     module int_vol_fractional_div
!
!      Written by H. Matsui on june, 2005
!
!!      subroutine int_vol_fractional_div_ele(ifilter_final,            &
!!     &          iele_fsmp_stack, num_int, i_vector,                   &
!!     &          node, ele, nod_fld, g_FEM, jac_3d_q, jac_3d_l,        &
!!     &          rhs_tbl, FEM_elen, Cdiff, fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(SGS_model_coefficient), intent(in) :: Cdiff
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module int_vol_fractional_div
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_FEM_SGS_model_coefs
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_fractional_div_ele(ifilter_final,              &
     &          iele_fsmp_stack, num_int, i_vector,                     &
     &          node, ele, nod_fld, g_FEM, jac_3d_q, jac_3d_l,          &
     &          rhs_tbl, FEM_elen, Cdiff, fem_wk, f_l)
!
      use int_vol_fractional
      use int_vol_sgs_fractional
!
      integer(kind = kint), intent(in) :: ifilter_final, num_int
      integer(kind = kint), intent(in) :: i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(SGS_model_coefficient), intent(in) :: Cdiff
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if(Cdiff%num_comp .gt. 0) then
        call int_vol_sgs_div_v_linear                                   &
     &     (node, ele, g_FEM, jac_3d_q, jac_3d_l, rhs_tbl, FEM_elen,    &
     &      nod_fld, iele_fsmp_stack, num_int, i_vector, ifilter_final, &
     &      Cdiff%coef(1,1), fem_wk, f_l)
      else
        call int_vol_div_vect_linear                                    &
     &     (node, ele, g_FEM, jac_3d_q, jac_3d_l, rhs_tbl, nod_fld,     &
     &      iele_fsmp_stack, num_int, i_vector, fem_wk, f_l)
      end if
!
      end subroutine int_vol_fractional_div_ele
!
! ----------------------------------------------------------------------
!
      end module int_vol_fractional_div
