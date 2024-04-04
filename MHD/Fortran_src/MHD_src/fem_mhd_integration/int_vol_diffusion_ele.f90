!>@file   int_vol_diffusion_ele.f90
!!        module int_vol_diffusion_ele
!!
!>@author H. Matsui
!>@date Written by H. Matsui in July, 2005
!!
!>@brief Evaluate field data for time integration for FEM dynamo model
!!
!!@verbatim
!!      subroutine int_vol_scalar_diffuse_ele                           &
!!     &         (ifilter_final, iele_fsmp_stack, num_int,              &
!!     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,&
!!     &          Cdiff, coef_crank, ak_d, i_scalar, fem_wk, f_l)
!!      subroutine int_vol_vector_diffuse_ele                           &
!!     &         (ifilter_final, iele_fsmp_stack, num_int,              &
!!     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,&
!!     &          Cdiff, coef_crank, ak_d, i_vector, fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_model_coefficient), intent(in) :: Cdiff
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!@endverbatim
!
      module int_vol_diffusion_ele
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_FEM_SGS_model_coefs
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_scalar_diffuse_ele                             &
     &         (ifilter_final, iele_fsmp_stack, num_int,                &
     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,  &
     &          Cdiff, coef_crank, ak_d, i_scalar, fem_wk, f_l)
!
      use int_vol_fractional
      use int_vol_sgs_fractional
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_model_coefficient), intent(in) :: Cdiff
!
      integer(kind = kint), intent(in) :: ifilter_final, num_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: i_scalar
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if(Cdiff%num_comp .gt. 0) then
        call int_vol_scalar_sgs_diffuse                                 &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, FEM_elens, nod_fld,      &
     &      iele_fsmp_stack, num_int, coef_crank, ak_d, i_scalar,       &
     &      ifilter_final, Cdiff%coef(1,1), fem_wk, f_l)
      else
        call int_vol_scalar_diffuse                                     &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,                 &
     &      iele_fsmp_stack, num_int, coef_crank, ak_d, i_scalar,       &
     &      fem_wk, f_l)
      end if
!
      end subroutine int_vol_scalar_diffuse_ele
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_vector_diffuse_ele                             &
     &         (ifilter_final, iele_fsmp_stack, num_int,                &
     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl, FEM_elens,  &
     &          Cdiff, coef_crank, ak_d, i_vector, fem_wk, f_l)
!
      use int_vol_fractional
      use int_vol_sgs_fractional
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_model_coefficient), intent(in) :: Cdiff
!
      integer(kind = kint), intent(in) :: ifilter_final, num_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: i_vector
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      if(Cdiff%num_comp .gt. 0) then
        call int_vol_vector_sgs_diffuse                                 &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, FEM_elens, nod_fld,      &
     &      iele_fsmp_stack, num_int, coef_crank, ak_d, i_vector,       &
     &      ifilter_final, Cdiff%coef(1,1), fem_wk, f_l)
      else
        call int_vol_vector_diffuse                                     &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,                 &
     &      iele_fsmp_stack, num_int, coef_crank, ak_d, i_vector,       &
     &      fem_wk, f_l)
      end if
!
      end subroutine int_vol_vector_diffuse_ele
!
!  ---------------------------------------------------------------------
!
      end module int_vol_diffusion_ele
