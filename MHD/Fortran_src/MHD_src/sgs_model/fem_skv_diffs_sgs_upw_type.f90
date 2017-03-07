!fem_skv_diffs_sgs_upw_type.f90
!     module fem_skv_diffs_sgs_upw_type
!
!        programmed by H.Matsui on July, 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine fem_skv_grad_sgs_upwind(iele_fsmp_stack, n_int, k2,   &
!     &          i_filter, ak_diff, ele, jac_3d, FEM_elens,             &
!     &          vxe, scalar_1, sk_v)
!      subroutine fem_skv_div_sgs_upwind(iele_fsmp_stack, n_int, k2,    &
!     &          i_filter, ak_diff, ele, jac_3d, FEM_elens,             &
!     &          vxe, vector_1, sk_v)
!      subroutine fem_skv_rot_sgs_upwind(iele_fsmp_stack, n_int, k2,    &
!     &          i_filter, ak_diff, ele, jac_3d, FEM_elens,             &
!     &          vxe, vector_1, sk_v)
!      subroutine fem_skv_div_tsr_sgs_upwind(iele_fsmp_stack, n_int,    &
!     &          k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,         &
!     &          vxe, tensor_1, sk_v)
!      subroutine fem_skv_div_as_tsr_sgs_upwind(iele_fsmp_stack,        &
!     &          n_int, k2, i_filter, ak_diff, ele, jac_3d,             &
!     &          FEM_elens, vxe, as_tsr_1, sk_v)
!
      module fem_skv_diffs_sgs_upw_type
!
      use m_precision
!
      use t_geometry_data
      use t_filter_elength
      use t_finite_element_mat
      use t_jacobians
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
      use m_fem_gauss_int_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_grad_sgs_upwind(iele_fsmp_stack, n_int, k2,    &
     &          i_filter, ak_diff, ele, jac_3d, FEM_elens,              &
     &          vxe, scalar_1, sk_v)
!
      use m_t_step_parameter
      use fem_skv_gradient_sgs_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
      real(kind=kreal), intent(in) :: scalar_1(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_grad_sgs_upw                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,           &
     &    iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,              &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, vxe, scalar_1, sk_v)
!
      end subroutine fem_skv_grad_sgs_upwind
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_upwind(iele_fsmp_stack, n_int, k2,     &
     &          i_filter, ak_diff, ele, jac_3d, FEM_elens,              &
     &          vxe, vector_1, sk_v)
!
      use m_t_step_parameter
      use fem_skv_divergence_sgs_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
      real(kind=kreal), intent(in) :: vector_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_div_sgs_upw                                          &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,           &
     &    iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,              &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, vxe, vector_1, sk_v)
!
      end subroutine fem_skv_div_sgs_upwind
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_sgs_upwind(iele_fsmp_stack, n_int, k2,     &
     &          i_filter, ak_diff, ele, jac_3d, FEM_elens,              &
     &          vxe, vector_1, sk_v)
!
      use m_t_step_parameter
      use fem_skv_rotation_sgs_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
      real(kind=kreal), intent(in) :: vector_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_rot_sgs_upw                                          &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,           &
     &    iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,              &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, vxe, vector_1, sk_v)
!
      end subroutine fem_skv_rot_sgs_upwind
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_tsr_sgs_upwind(iele_fsmp_stack, n_int,     &
     &          k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,          &
     &          vxe, tensor_1, sk_v)
!
      use m_t_step_parameter
      use fem_skv_div_tensor_sgs_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
      real(kind=kreal), intent(in) :: tensor_1(ele%numele,6)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_div_tsr_sgs_upw                                      &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,           &
     &    iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,              &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, vxe, tensor_1, sk_v)
!
      end subroutine fem_skv_div_tsr_sgs_upwind
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_as_tsr_sgs_upwind(iele_fsmp_stack,         &
     &          n_int, k2, i_filter, ak_diff, ele, jac_3d,              &
     &          FEM_elens, vxe, as_tsr_1, sk_v)
!
      use m_t_step_parameter
      use fem_skv_div_as_tsr_sgs_upw
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
      real(kind=kreal), intent(in) :: as_tsr_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_div_ast_sgs_upw                                      &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,           &
     &    iele_fsmp_stack, n_int, k2, dt, jac_3d%ntot_int,              &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, vxe, as_tsr_1, sk_v)
!
      end subroutine fem_skv_div_as_tsr_sgs_upwind
!
!-----------------------------------------------------------------------
!
      end module fem_skv_diffs_sgs_upw_type
