!fem_skv_diffusion_sgs_type.f90
!      module fem_skv_diffusion_sgs_type
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine fem_skv_scalar_diffuse_sgs_type(iele_fsmp_stack,      &
!     &          n_int, k2, i_filter, ak_diff, ak_d,                    &
!     &          ele, jac_3d, FEM_elens, fem_wk)
!      subroutine fem_skv_vector_diffuse_sgs_type(iele_fsmp_stack,      &
!     &          n_int, k2, i_filter, ak_diff, ak_d, vect_1, sk_v)
!
!      subroutine fem_skv_poisson_sgs_type(iele_fsmp_stack, n_int, k2,  &
!     &          i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
!      subroutine fem_skv_poisson_linear_sgs_type(iele_fsmp_stack,      &
!     &          n_int, k2, i_filter, ak_diff, ele, jac_3d_l, FEM_elens,&
!     &          fem_wk)
!
      module fem_skv_diffusion_sgs_type
!
      use m_precision
!
      use t_geometry_data
      use t_filter_elength
      use t_jacobians
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
      subroutine fem_skv_scalar_diffuse_sgs_type(iele_fsmp_stack,       &
     &          n_int, k2, i_filter, ak_diff, ak_d,                     &
     &          ele, jac_3d, FEM_elens, scalar_1, sk_v)
!
      use fem_skv_diffusion_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
      real(kind=kreal), intent(in) :: scalar_1(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_scalar_diffuse_sgs(ele%numele, ele%nnod_4_ele,       &
     &  ele%nnod_4_ele, np_smp, iele_fsmp_stack,                        &
     &  max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,     &
     &  jac_3d%ntot_int, jac_3d%xjac, jac_3d%dnx, jac_3d%dnx,           &
     &  FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM_elens%nele_filter_mom,                                      &
     &  FEM_elens%elen_ele%diff2%df_x2, FEM_elens%elen_ele%diff2%df_y2, &
     &  FEM_elens%elen_ele%diff2%df_z2, FEM_elens%elen_ele%diff2%df_xy, &
     &  FEM_elens%elen_ele%diff2%df_yz, FEM_elens%elen_ele%diff2%df_zx, &
     &  ak_diff, ak_d, scalar_1, sk_v)
!
      end subroutine fem_skv_scalar_diffuse_sgs_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vector_diffuse_sgs_type(iele_fsmp_stack,       &
     &          n_int, k2, i_filter, ak_diff, ak_d,                     &
     &          ele, jac_3d, FEM_elens, vector_1, sk_v)
!
      use fem_skv_diffusion_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
      real(kind=kreal), intent(in) :: vector_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_diffuse_sgs(ele%numele, ele%nnod_4_ele,       &
     &  ele%nnod_4_ele, np_smp, iele_fsmp_stack,                        &
     &  max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,     &
     &  jac_3d%ntot_int, jac_3d%xjac, jac_3d%dnx, jac_3d%dnx,           &
     &  FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM_elens%nele_filter_mom,                                      &
     &  FEM_elens%elen_ele%diff2%df_x2, FEM_elens%elen_ele%diff2%df_y2, &
     &  FEM_elens%elen_ele%diff2%df_z2, FEM_elens%elen_ele%diff2%df_xy, &
     &  FEM_elens%elen_ele%diff2%df_yz, FEM_elens%elen_ele%diff2%df_zx, &
     &  ak_diff, ak_d, vector_1, sk_v)
!
      end subroutine fem_skv_vector_diffuse_sgs_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_sgs_type(iele_fsmp_stack, n_int, k2,   &
     &          i_filter, ak_diff, ele, jac_3d, FEM_elens, sk_v)
!
      use fem_skv_poisson_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_poisson_sgs_pg(ele%numele, ele%nnod_4_ele,           &
     &  ele%nnod_4_ele, np_smp, iele_fsmp_stack,                        &
     &  max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,     &
     &  jac_3d%ntot_int, jac_3d%xjac, jac_3d%dnx, jac_3d%dnx,           &
     &  FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM_elens%nele_filter_mom,                                      &
     &  FEM_elens%elen_ele%diff2%df_x2, FEM_elens%elen_ele%diff2%df_y2, &
     &  FEM_elens%elen_ele%diff2%df_z2, FEM_elens%elen_ele%diff2%df_xy, &
     &  FEM_elens%elen_ele%diff2%df_yz, FEM_elens%elen_ele%diff2%df_zx, &
     &  ak_diff, sk_v)
!
      end subroutine fem_skv_poisson_sgs_type
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_poisson_linear_sgs_type(iele_fsmp_stack,       &
     &          n_int, k2, i_filter, ak_diff, ele, jac_3d_l, FEM_elens, &
     &          sk_v)
!
      use fem_skv_poisson_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_poisson_sgs_pg(ele%numele, num_t_linear,             &
     &  num_t_linear, np_smp, iele_fsmp_stack,                          &
     &  max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,     &
     &  jac_3d_l%ntot_int, jac_3d_l%xjac, jac_3d_l%dnx, jac_3d_l%dnx,   &
     &  FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM_elens%nele_filter_mom,                                      &
     &  FEM_elens%elen_ele%diff2%df_x2, FEM_elens%elen_ele%diff2%df_y2, &
     &  FEM_elens%elen_ele%diff2%df_z2, FEM_elens%elen_ele%diff2%df_xy, &
     &  FEM_elens%elen_ele%diff2%df_yz, FEM_elens%elen_ele%diff2%df_zx, &
     &  ak_diff, sk_v)
!
      end subroutine fem_skv_poisson_linear_sgs_type
!
!-----------------------------------------------------------------------
!
      end module fem_skv_diffusion_sgs_type
