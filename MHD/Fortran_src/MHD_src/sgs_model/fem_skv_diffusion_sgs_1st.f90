!fem_skv_diffusion_sgs_1st.f90
!      module fem_skv_diffusion_sgs_1st
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine fem_skv_scalar_diffuse_1st(iele_fsmp_stack,           &
!     &          n_int, k2, ak_d, scalar_1, sk_v)
!      subroutine fem_skv_vector_diffuse_sgs_1st(iele_fsmp_stack,       &
!     &          n_int, k2, i_filter, ak_diff, ak_d, vect_1, sk_v)
!
!      subroutine fem_skv_poisson_sgs_1st(iele_fsmp_stack,              &
!     &          n_int, k2, i_filter, ak_diff, sk_v)
!      subroutine fem_skv_poisson_linear_sgs_1st(iele_fsmp_stack,       &
!     &          n_int, k2, i_filter, ak_diff, sk_v)
!
!
      module fem_skv_diffusion_sgs_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_fem_gauss_int_coefs
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
      subroutine fem_skv_scalar_diffuse_sgs_1st(iele_fsmp_stack,        &
     &          n_int, k2, i_filter, ak_diff, ak_d, scalar_1, sk_v)
!
      use fem_skv_diffusion_sgs
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_d(numele)
      real (kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal),   intent(in) :: scalar_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_scalar_diffuse_sgs(numele, nnod_4_ele, nnod_4_ele,   &
     &    np_smp, iele_fsmp_stack, n_int, k2,                           &
     &    ntot_int_3d, xjac, dwx, dwx,                                  &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    elen1%diff2%df_x2, elen1%diff2%df_y2, elen1%diff2%df_z2,      &
     &    elen1%diff2%df_xy, elen1%diff2%df_yz, elen1%diff2%df_zx,      &
     &    ak_diff, ak_d, scalar_1, sk_v)
!
      end subroutine fem_skv_scalar_diffuse_sgs_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_diffuse_sgs_1st(iele_fsmp_stack,        &
     &          n_int, k2, i_filter, ak_diff, ak_d, vect_1, sk_v)
!
      use fem_skv_diffusion_sgs
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_d(numele)
      real (kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: vect_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_vector_diffuse_sgs(numele, nnod_4_ele, nnod_4_ele,   &
     &    np_smp, iele_fsmp_stack, n_int, k2,                           &
     &    ntot_int_3d, xjac, dwx, dwx,                                  &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    elen1%diff2%df_x2, elen1%diff2%df_y2, elen1%diff2%df_z2,      &
     &    elen1%diff2%df_xy, elen1%diff2%df_yz, elen1%diff2%df_zx,      &
     &    ak_diff, ak_d, vect_1, sk_v)
!
      end subroutine fem_skv_vector_diffuse_sgs_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_sgs_1st(iele_fsmp_stack,               &
     &          n_int, k2, i_filter, ak_diff, sk_v)
!
      use fem_skv_poisson_sgs
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_poisson_sgs_pg(numele, nnod_4_ele, nnod_4_ele,       &
     &    np_smp, iele_fsmp_stack, n_int, k2,                           &
     &    ntot_int_3d, xjac, dwx, dwx,                                  &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    elen1%diff2%df_x2, elen1%diff2%df_y2, elen1%diff2%df_z2,      &
     &    elen1%diff2%df_xy, elen1%diff2%df_yz, elen1%diff2%df_zx,      &
     &    ak_diff, sk_v)
!
      end subroutine fem_skv_poisson_sgs_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_linear_sgs_1st(iele_fsmp_stack,        &
     &          n_int, k2, i_filter, ak_diff, sk_v)
!
      use fem_skv_poisson_sgs
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_poisson_sgs_pg(numele, num_t_linear, num_t_linear,   &
     &    np_smp, iele_fsmp_stack, n_int, k2,                           &
     &    ntot_int_3d, xjac, dnx, dnx,                                  &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    elen1%diff2%df_x2, elen1%diff2%df_y2, elen1%diff2%df_z2,      &
     &    elen1%diff2%df_xy, elen1%diff2%df_yz, elen1%diff2%df_zx,      &
     &    ak_diff, sk_v)
!
      end subroutine fem_skv_poisson_linear_sgs_1st
!
!-----------------------------------------------------------------------
!
      end module fem_skv_diffusion_sgs_1st
