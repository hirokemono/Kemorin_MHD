!
!      module int_vol_diffusion_ele
!
! numerical integration for diffusion terms (Lapracians)
!      Written by H. Matsui on July, 2005
!
!!      subroutine int_vol_scalar_diffuse_ele(iele_fsmp_stack,          &
!!     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          iak_diff, coef_crank, ak_d, i_scalar, fem_wk, f_l)
!!      subroutine int_vol_vector_diffuse_ele(iele_fsmp_stack,          &
!!     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          iak_diff, coef_crank, ak_d, i_vector, fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module int_vol_diffusion_ele
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
      use m_phys_constants
      use m_control_parameter
      use m_SGS_model_coefs
      use m_SGS_address
!
      use t_geometry_data
      use t_phys_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_scalar_diffuse_ele(iele_fsmp_stack,            &
     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          iak_diff, coef_crank, ak_d, i_scalar, fem_wk, f_l)
!
      use int_vol_fractional
      use int_vol_sgs_fractional
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: i_scalar, iak_diff
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iak_diff .gt. 0) then
        call int_vol_scalar_sgs_diffuse                                 &
     &     (node, ele, jac_3d, rhs_tbl, FEM_elens, nod_fld,             &
     &      iele_fsmp_stack, intg_point_t_evo, coef_crank, ak_d,        &
     &      i_scalar, ifilter_final, ak_diff(1,iak_diff),               &
     &      fem_wk, f_l)
      else
        call int_vol_scalar_diffuse                                     &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      iele_fsmp_stack, intg_point_t_evo, coef_crank,              &
     &      ak_d, i_scalar, fem_wk, f_l)
      end if
!
      end subroutine int_vol_scalar_diffuse_ele
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_vector_diffuse_ele(iele_fsmp_stack,            &
     &          node, ele, nod_fld, jac_3d, rhs_tbl, FEM_elens,  &
     &          iak_diff, coef_crank, ak_d, i_vector, fem_wk, f_l)
!
      use int_vol_fractional
      use int_vol_sgs_fractional
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: i_vector, iak_diff
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if (iak_diff .gt. 0) then
        call int_vol_vector_sgs_diffuse                                 &
     &     (node, ele, jac_3d, rhs_tbl, FEM_elens, nod_fld,             &
     &      iele_fsmp_stack, intg_point_t_evo, coef_crank, ak_d,        &
     &      i_vector, ifilter_final, ak_diff(1,iak_diff),               &
     &      fem_wk, f_l)
      else
        call int_vol_vector_diffuse                                     &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      iele_fsmp_stack, intg_point_t_evo, coef_crank,              &
     &      ak_d, i_vector, fem_wk, f_l)
      end if
!
      end subroutine int_vol_vector_diffuse_ele
!
!  ---------------------------------------------------------------------
!
      end module int_vol_diffusion_ele
