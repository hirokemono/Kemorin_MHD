!fem_skv_diffs_sgs_upw_1st.f90
!     module fem_skv_diffs_sgs_upw_1st
!
!        programmed by H.Matsui on July, 2005
!        modified by H. Matsui on Aug., 2007
!
!      subroutine fem_skv_grad_sgs_upw_1st(iele_fsmp_stack, n_int, k2,  &
!     &          i_filter, ak_diff, vxe, scalar_1, sk_v)
!      subroutine fem_skv_div_sgs_upw_1st(iele_fsmp_stack, n_int, k2,   &
!     &          i_filter, ak_diff, vxe, vector_1, sk_v)
!      subroutine fem_skv_rot_sgs_upw_1st(iele_fsmp_stack, n_int, k2,   &
!     &          i_filter, ak_diff, vxe, vector_1, sk_v)
!      subroutine fem_skv_div_tsr_sgs_upw_1st(iele_fsmp_stack,          &
!     &          n_int, k2, i_filter, ak_diff, vxe, tensor_1, sk_v)
!      subroutine fem_skv_div_as_tsr_sgs_upw_1st(iele_fsmp_stack,       &
!     &          n_int, k2, i_filter, ak_diff, vxe, as_tsr_1, sk_v)
!
      module fem_skv_diffs_sgs_upw_1st
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
      subroutine fem_skv_grad_sgs_upw_1st(iele_fsmp_stack, n_int, k2,   &
     &          i_filter, ak_diff, vxe, scalar_1, sk_v)
!
      use m_t_int_parameter
      use fem_skv_gradient_sgs_upw
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: scalar_1(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_grad_sgs_upw(numele, nnod_4_ele, nnod_4_ele,         &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt,                       &
     &    ntot_int_3d, xjac, aw, dwx, dwx,                              &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, vxe, scalar_1, sk_v)
!
      end subroutine fem_skv_grad_sgs_upw_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_upw_1st(iele_fsmp_stack, n_int, k2,    &
     &          i_filter, ak_diff, vxe, vector_1, sk_v)
!
      use m_t_int_parameter
      use fem_skv_divergence_sgs_upw
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: vector_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_div_sgs_upw(numele, nnod_4_ele, nnod_4_ele, np_smp,  &
     &    iele_fsmp_stack, n_int, k2, dt,                               &
     &    ntot_int_3d, xjac, aw, dwx, dwx,                              &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, vxe, vector_1, sk_v)
!
      end subroutine fem_skv_div_sgs_upw_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_rot_sgs_upw_1st(iele_fsmp_stack, n_int, k2,    &
     &          i_filter, ak_diff, vxe, vector_1, sk_v)
!
      use m_t_int_parameter
      use fem_skv_rotation_sgs_upw
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: vector_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_rot_sgs_upw(numele, nnod_4_ele, nnod_4_ele, np_smp,  &
     &    iele_fsmp_stack, n_int, k2, dt,                               &
     &    ntot_int_3d, xjac, aw, dwx, dwx,                              &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, vxe, vector_1, sk_v)
!
      end subroutine fem_skv_rot_sgs_upw_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_tsr_sgs_upw_1st(iele_fsmp_stack,           &
     &          n_int, k2, i_filter, ak_diff, vxe, tensor_1, sk_v)
!
      use m_t_int_parameter
      use fem_skv_div_tensor_sgs_upw
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: tensor_1(numele,6)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_div_tsr_sgs_upw(numele, nnod_4_ele, nnod_4_ele,      &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt,                       &
     &    ntot_int_3d, xjac, aw, dwx, dwx,                              &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, vxe, tensor_1, sk_v)
!
      end subroutine fem_skv_div_tsr_sgs_upw_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_div_as_tsr_sgs_upw_1st(iele_fsmp_stack,        &
     &          n_int, k2, i_filter, ak_diff, vxe, as_tsr_1, sk_v)
!
      use m_t_int_parameter
      use fem_skv_div_as_tsr_sgs_upw
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: vxe(numele,3)
      real(kind=kreal), intent(in) :: ak_diff(numele)
      real(kind=kreal), intent(in) :: as_tsr_1(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_div_ast_sgs_upw(numele, nnod_4_ele, nnod_4_ele,      &
     &    np_smp, iele_fsmp_stack, n_int, k2, dt,                       &
     &    ntot_int_3d, xjac, aw, dwx, dwx,                              &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    elen1%diff%df_x2,  elen1%diff%df_y2,  elen1%diff%df_z2,       &
     &    elen1%diff%df_xy,  elen1%diff%df_yz,  elen1%diff%df_zx,       &
     &    ak_diff, vxe, as_tsr_1, sk_v)
!
      end subroutine fem_skv_div_as_tsr_sgs_upw_1st
!
!-----------------------------------------------------------------------
!
      end module fem_skv_diffs_sgs_upw_1st
