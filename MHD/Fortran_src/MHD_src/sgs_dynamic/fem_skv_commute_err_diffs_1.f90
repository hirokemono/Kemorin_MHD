!fem_skv_commute_err_diffs_1.f90
!     module fem_skv_commute_err_diffs_1
!
!        programmed by H.Matsui on July, 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine fem_skv_commute_err_grad_1(iele_fsmp_stack, n_int, k2,&
!     &          i_filter, scalar_1, sk_v)
!      subroutine fem_skv_commute_err_div_1(iele_fsmp_stack, n_int, k2, &
!     &          i_filter, vector_1, sk_v)
!      subroutine fem_skv_commute_err_rot_1(iele_fsmp_stack, n_int, k2, &
!     &          i_filter, vector_1, sk_v)
!      subroutine fem_skv_commute_err_div_tsr_1(iele_fsmp_stack,        &
!     &          n_int, k2, i_filter, tensor_1, sk_v)
!      subroutine fem_skv_commute_err_div_ast_1(iele_fsmp_stack,        &
!     &          n_int, k2, i_filter, as_tsr_1, sk_v)
!
      module fem_skv_commute_err_diffs_1
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_phys_constants
      use m_fem_gauss_int_coefs
      use m_geometry_constants
      use m_geometry_parameter
      use m_jacobians
      use m_filter_elength
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_commute_err_grad_1(iele_fsmp_stack, n_int, k2, &
     &          i_filter, scalar_1, sk_v)
!
      use m_t_int_parameter
      use fem_skv_commute_err_grad
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: scalar_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_commute_error_grad(numele, nnod_4_ele, nnod_4_ele,   &
     &    np_smp, iele_fsmp_stack, n_int, k2,                           &
     &    ntot_int_3d, xjac, dwx, dwx,                                  &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom,        &
     &    diff1_1%df_x2,  diff1_1%df_y2,  diff1_1%df_z2,                &
     &    diff1_1%df_xy,  diff1_1%df_yz,  diff1_1%df_zx,                &
     &    scalar_1, sk_v)
!
      end subroutine fem_skv_commute_err_grad_1
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_commute_err_div_1(iele_fsmp_stack, n_int, k2,  &
     &          i_filter, vector_1, sk_v)
!
      use m_t_int_parameter
      use fem_skv_commute_err_div
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vector_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_commute_error_div(numele, nnod_4_ele, nnod_4_ele,    &
     &    np_smp, iele_fsmp_stack, n_int, k2,              &
     &    ntot_int_3d, xjac, dwx, dwx,         &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom,     &
     &    diff1_1%df_x2,  diff1_1%df_y2,  diff1_1%df_z2,          &
     &    diff1_1%df_xy,  diff1_1%df_yz,  diff1_1%df_zx,          &
     &    vector_1, sk_v)
!
      end subroutine fem_skv_commute_err_div_1
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_commute_err_rot_1(iele_fsmp_stack, n_int, k2,  &
     &          i_filter, vector_1, sk_v)
!
      use m_t_int_parameter
      use fem_skv_commute_err_rot
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vector_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_commute_error_rot(numele, nnod_4_ele, nnod_4_ele,    &
     &    np_smp, iele_fsmp_stack, n_int, k2,              &
     &    ntot_int_3d, xjac, dwx, dwx,  &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom,     &
     &    diff1_1%df_x2,  diff1_1%df_y2,  diff1_1%df_z2,          &
     &    diff1_1%df_xy,  diff1_1%df_yz,  diff1_1%df_zx,          &
     &    vector_1, sk_v)
!
      end subroutine fem_skv_commute_err_rot_1
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_commute_err_div_tsr_1(iele_fsmp_stack,         &
     &          n_int, k2, i_filter, tensor_1, sk_v)
!
      use m_t_int_parameter
      use fem_skv_commute_err_div_tsr
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: tensor_1(numele,6)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_commute_error_div_tsr                                &
     &   (numele, nnod_4_ele, nnod_4_ele,                               &
     &    np_smp, iele_fsmp_stack, n_int, k2,              &
     &    ntot_int_3d, xjac, dwx, dwx,    &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom,     &
     &    diff1_1%df_x2,  diff1_1%df_y2,  diff1_1%df_z2,          &
     &    diff1_1%df_xy,  diff1_1%df_yz,  diff1_1%df_zx,          &
     &    tensor_1, sk_v)
!
      end subroutine fem_skv_commute_err_div_tsr_1
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_commute_err_div_ast_1(iele_fsmp_stack,         &
     &          n_int, k2, i_filter, as_tsr_1, sk_v)
!
      use m_t_int_parameter
      use fem_skv_commute_err_div_ast
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: as_tsr_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_commute_error_div_ast                                &
     &   (numele, nnod_4_ele, nnod_4_ele,                               &
     &    np_smp, iele_fsmp_stack, n_int, k2,              &
     &    ntot_int_3d, xjac, dwx, dwx,   &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom,     &
     &    diff1_1%df_x2,  diff1_1%df_y2,  diff1_1%df_z2,          &
     &    diff1_1%df_xy,  diff1_1%df_yz,  diff1_1%df_zx,          &
     &    as_tsr_1, sk_v)
!
      end subroutine fem_skv_commute_err_div_ast_1
!
!-----------------------------------------------------------------------
!
      end module fem_skv_commute_err_diffs_1
