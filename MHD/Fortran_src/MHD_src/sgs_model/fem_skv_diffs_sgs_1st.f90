!fem_skv_diffs_sgs_1st.f90
!     module fem_skv_diffs_sgs_1st
!
!        programmed by H.Matsui on July, 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine fem_skv_grad_sgs_pg_1st(iele_fsmp_stack, n_int, k2,   &
!     &          i_filter, ak_diff, scalar_1, sk_v)
!      subroutine fem_skv_div_sgs_pg_1st(iele_fsmp_stack, n_int, k2,    &
!     &          i_filter, ak_diff, vector_1, sk_v)
!      subroutine fem_skv_rot_sgs_pg_1st(iele_fsmp_stack, n_int, k2,    &
!     &          i_filter, ak_diff, vector_1, sk_v)
!      subroutine fem_skv_div_tsr_sgs_pg_1st(iele_fsmp_stack, n_int, k2,&
!     &          i_filter, ak_diff, tensor_1, sk_v)
!      subroutine fem_skv_div_as_tsr_sgs_pg_1st(iele_fsmp_stack,        &
!     &          n_int, k2, i_filter, ak_diff, as_tsr_1, sk_v)
!
!      subroutine fem_skv_linear_grad_sgs_1st(iele_fsmp_stack, n_int,   &
!     &          k2, i_filter, ak_diff, scalar_1, sk_v)
!      subroutine fem_skv_div_2l_sgs_1st(iele_fsmp_stack, n_int, k2,    &
!     &          i_filter, ak_diff, vector_1, sk_v)
!
      module fem_skv_diffs_sgs_1st
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
      subroutine fem_skv_grad_sgs_pg_1st(iele_fsmp_stack, n_int, k2,    &
     &          i_filter, ak_diff, scalar_1, sk_v)
!
      use fem_skv_gradient_sgs
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: scalar_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_grad_sgs_pg(numele, nnod_4_ele, nnod_4_ele,          &
     &    np_smp, iele_fsmp_stack, n_int, k2,                           &
     &    ntot_int_3d, xjac, aw, dwx, dwx, &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom, &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, scalar_1, sk_v)
!
      end subroutine fem_skv_grad_sgs_pg_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_pg_1st(iele_fsmp_stack, n_int, k2,     &
     &          i_filter, ak_diff, vector_1, sk_v)
!
      use fem_skv_divergence_sgs
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: vector_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_div_sgs_pg(numele, nnod_4_ele, nnod_4_ele, np_smp,   &
     &    iele_fsmp_stack, n_int, k2,                                   &
     &    ntot_int_3d, xjac, aw, dwx, dwx, &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom, &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, vector_1, sk_v)
!
      end subroutine fem_skv_div_sgs_pg_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_sgs_pg_1st(iele_fsmp_stack, n_int, k2,     &
     &          i_filter, ak_diff, vector_1, sk_v)
!
      use fem_skv_rotation_sgs
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: vector_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_rot_sgs_pg(numele, nnod_4_ele, nnod_4_ele, np_smp,   &
     &    iele_fsmp_stack, n_int, k2,                      &
     &    ntot_int_3d, xjac, aw, dwx, dwx, &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom, &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, vector_1, sk_v)
!
      end subroutine fem_skv_rot_sgs_pg_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_tsr_sgs_pg_1st(iele_fsmp_stack, n_int, k2, &
     &          i_filter, ak_diff, tensor_1, sk_v)
!
      use fem_skv_div_tensor_sgs
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: tensor_1(numele,6)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_div_tsr_sgs_pg(numele, nnod_4_ele, nnod_4_ele,       &
     &    np_smp, iele_fsmp_stack, n_int, k2,              &
     &    ntot_int_3d, xjac, aw, dwx, dwx, &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom, &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, tensor_1, sk_v)
!
      end subroutine fem_skv_div_tsr_sgs_pg_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_as_tsr_sgs_pg_1st(iele_fsmp_stack,         &
     &          n_int, k2, i_filter, ak_diff, as_tsr_1, sk_v)
!
      use fem_skv_div_as_tsr_sgs
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: as_tsr_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_div_ast_sgs_pg(numele, nnod_4_ele, nnod_4_ele,       &
     &    np_smp, iele_fsmp_stack, n_int, k2,              &
     &    ntot_int_3d, xjac, aw, dwx, dwx, &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom, &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, as_tsr_1, sk_v)
!
      end subroutine fem_skv_div_as_tsr_sgs_pg_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_linear_grad_sgs_1st(iele_fsmp_stack, n_int,    &
     &          k2, i_filter, ak_diff, scalar_1, sk_v)
!
!
      use fem_skv_gradient_sgs
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: scalar_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_grad_sgs_pg(numele, nnod_4_ele, num_t_linear,        &
     &    np_smp, iele_fsmp_stack, n_int, k2,              &
     &    ntot_int_3d, xjac, aw, dwx, dnx, &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom, &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, scalar_1, sk_v)
!
      end subroutine fem_skv_linear_grad_sgs_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_2l_sgs_1st(iele_fsmp_stack, n_int, k2,     &
     &          i_filter, ak_diff, vector_1, sk_v)
!
      use fem_skv_divergence_sgs
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: vector_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_div_sgs_pg(numele, num_t_linear, nnod_4_ele, np_smp, &
     &    iele_fsmp_stack, n_int, k2,                      &
     &    ntot_int_3d, xjac, an, dnx, dwx, &
     &    filter_conf1%xmom_1d_org(i_filter,2), nele_filter_mom, &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, vector_1, sk_v)
!
      end subroutine fem_skv_div_2l_sgs_1st
!
!-----------------------------------------------------------------------
!
      end module fem_skv_diffs_sgs_1st
