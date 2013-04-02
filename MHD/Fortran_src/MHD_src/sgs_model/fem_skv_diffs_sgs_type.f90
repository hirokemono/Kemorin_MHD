!fem_skv_diffs_sgs_type.f90
!     module fem_skv_diffs_sgs_type
!
!        programmed by H.Matsui on July, 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine fem_skv_grad_sgs_pg_type(iele_fsmp_stack, n_int, k2,  &
!     &          i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
!      subroutine fem_skv_div_sgs_pg_type(iele_fsmp_stack, n_int, k2,   &
!     &          i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
!      subroutine fem_skv_rot_sgs_pg_type(iele_fsmp_stack, n_int, k2,   &
!     &          i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
!      subroutine fem_skv_div_tsr_sgs_pg_type(iele_fsmp_stack, n_int,   &
!     &          k2, i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
!      subroutine fem_skv_div_as_tsr_sgs_pg_type(iele_fsmp_stack, n_int,&
!     &          k2, i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
!
!      subroutine fem_skv_linear_grad_sgs_type(iele_fsmp_stack, n_int,  &
!     &          k2, i_filter, ak_diff, ele, jac_3d, jac_3d_l,          &
!     &          FEM_elens, fem_wk)
!      subroutine fem_skv_div_2l_sgs_type(iele_fsmp_stack, n_int, k2,   &
!     &          i_filter, ak_diff, ele, jac_3d, jac_3d_l, FEM_elens,   &
!     &          fem_wk)
!
      module fem_skv_diffs_sgs_type
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
      subroutine fem_skv_grad_sgs_pg_type(iele_fsmp_stack, n_int, k2,   &
     &          i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
!
      use fem_skv_gradient_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grad_sgs_pg                                          &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_grad_sgs_pg_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_pg_type(iele_fsmp_stack, n_int, k2,    &
     &          i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
!
      use fem_skv_divergence_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_div_sgs_pg                                           &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,           &
     &    iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,                  &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_sgs_pg_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_sgs_pg_type(iele_fsmp_stack, n_int, k2,    &
     &          i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
!
      use fem_skv_rotation_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_rot_sgs_pg                                           &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,           &
     &    iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,                  &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_rot_sgs_pg_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_tsr_sgs_pg_type(iele_fsmp_stack, n_int,    &
     &          k2, i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
!
      use fem_skv_div_tensor_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_div_tsr_sgs_pg                                       &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,           &
     &    iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,                  &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%tensor_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_tsr_sgs_pg_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_as_tsr_sgs_pg_type(iele_fsmp_stack, n_int, &
     &          k2, i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
!
      use fem_skv_div_as_tsr_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_div_ast_sgs_pg                                       &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,           &
     &    iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,                  &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_as_tsr_sgs_pg_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_linear_grad_sgs_type(iele_fsmp_stack, n_int,   &
     &          k2, i_filter, ak_diff, ele, jac_3d, jac_3d_l,           &
     &          FEM_elens, fem_wk)
!
      use fem_skv_gradient_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_grad_sgs_pg                                          &
     &   (ele%numele, ele%nnod_4_ele, num_t_linear,                     &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d_l%dnx,             &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%scalar_1, fem_wk%sk6)
!
      end subroutine fem_skv_linear_grad_sgs_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_2l_sgs_type(iele_fsmp_stack, n_int, k2,    &
     &          i_filter, ak_diff, ele, jac_3d, jac_3d_l, FEM_elens,    &
     &          fem_wk)
!
      use fem_skv_divergence_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_skv_div_sgs_pg                                           &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, np_smp,           &
     &    iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,                  &
     &    jac_3d%xjac, jac_3d_l%an, jac_3d_l%dnx, jac_3d%dnx,           &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, fem_wk%vector_1, fem_wk%sk6)
!
      end subroutine fem_skv_div_2l_sgs_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_diffs_sgs_type
