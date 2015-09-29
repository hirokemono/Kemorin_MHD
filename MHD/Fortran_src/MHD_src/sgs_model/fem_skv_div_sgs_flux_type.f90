!fem_skv_div_sgs_flux_type.f90
!      module fem_skv_div_sgs_flux_type
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on Aug., 2006
!
!      subroutine fem_skv_div_sgs_vector(iele_fsmp_stack,               &
!     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,  &
!     &          sgs_1, flux_1, sk_v)
!      subroutine fem_skv_div_sgs_tensor(iele_fsmp_stack,               &
!     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,  &
!     &          sgs_1, flux_1, sk_v)
!      subroutine fem_skv_div_sgs_asym_tsr(iele_fsmp_stack,             &
!     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,  &
!     &          sgs_1, flux_1, sk_v)
!
!      subroutine fem_skv_scl_inertia_sgs_pg(iele_fsmp_stack,           &
!     &          n_int, k2, ele, jac_3d, scalar_e, sgs_e, vxe, sk_v)
!      subroutine fem_skv_vec_inertia_sgs_pg(iele_fsmp_stack,           &
!     &           n_int, k2, ele, jac_3d, vector_e, sgs_e, vxe, sk_v)
!      subroutine fem_skv_inertia_rot_sgs_pg(iele_fsmp_stack,           &
!     &          n_int, k2, ele, jac_3d, vector_e, sgs_e, wxe, sk_v)
!
!      subroutine fem_skv_scl_inertia_modsgs_pg(iele_fsmp_stack,        &
!     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,  &
!     &          scalar_e, sgs_e, flux_e, vxe, sk_v)
!      subroutine fem_skv_vec_inertia_modsgs_pg(iele_fsmp_stack,        &
!     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,  &
!     &          vector_e, sgs_e, flux_e, vxe, sk_v)
!
      module fem_skv_div_sgs_flux_type
!
      use m_precision
!
      use t_geometry_data
      use t_filter_elength
      use t_jacobians
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
      use m_fem_gauss_int_coefs
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_vector(iele_fsmp_stack,                &
     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,   &
     &          sgs_1, flux_1, sk_v)
!
      use fem_skv_div_vect_w_sgs
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
      real(kind=kreal), intent(in) :: sgs_1(ele%numele,n_vector)
      real(kind=kreal), intent(in) :: flux_1(ele%numele,n_vector)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_div_sgs_vector_pg                                    &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, sgs_1, flux_1, sk_v)
!
      end subroutine fem_skv_div_sgs_vector
!
!   --------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_tensor(iele_fsmp_stack,                &
     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,   &
     &          sgs_1, flux_1, sk_v)
!
      use fem_skv_div_tsr_w_sgs
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
      real(kind=kreal), intent(in) :: sgs_1(ele%numele,n_sym_tensor)
      real(kind=kreal), intent(in) :: flux_1(ele%numele,n_sym_tensor)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_div_tensor_w_sgs_pg                                  &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, sgs_1, flux_1, sk_v)
!
      end subroutine fem_skv_div_sgs_tensor
!
!   --------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_asym_tsr(iele_fsmp_stack,              &
     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,   &
     &          sgs_1, flux_1, sk_v)
!
      use fem_skv_div_ast_w_sgs
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
      real(kind=kreal), intent(in) :: sgs_1(ele%numele,3)
      real(kind=kreal), intent(in) :: flux_1(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_div_sgs_asym_t_pg                                    &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, sgs_1, flux_1, sk_v)
!
      end subroutine fem_skv_div_sgs_asym_tsr
!
!   --------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scl_inertia_sgs_pg(iele_fsmp_stack,            &
     &          n_int, k2, ele, jac_3d, scalar_e, sgs_e, vxe, sk_v)
!
      use fem_skv_inertia_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: sgs_e(ele%numele,3)
      real (kind=kreal), intent(in) :: scalar_e(ele%numele)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_scalar_inertia_sgs                                   &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx,                           &
     &    scalar_e, sgs_e, vxe, sk_v)
!
      end subroutine fem_skv_scl_inertia_sgs_pg
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vec_inertia_sgs_pg(iele_fsmp_stack,            &
     &           n_int, k2, ele, jac_3d, vector_e, sgs_e, vxe, sk_v)
!
      use fem_skv_inertia_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: sgs_e(ele%numele,6)
      real (kind=kreal), intent(in) :: vector_e(ele%numele,3)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_inertia_sgs                                   &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx,                           &
     &    vector_e, sgs_e, vxe, sk_v)
!
      end subroutine fem_skv_vec_inertia_sgs_pg
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_inertia_rot_sgs_pg(iele_fsmp_stack,            &
     &          n_int, k2, ele, jac_3d, vector_e, sgs_e, wxe, sk_v)
!
      use fem_skv_inertia_sgs
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vector_e(ele%numele,3)
      real (kind=kreal), intent(in) :: sgs_e(ele%numele,6)
      real (kind=kreal), intent(in) :: wxe(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_inertia_rot_sgs                                      &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%an, jac_3d%dnx,                &
     &    vector_e, sgs_e, wxe, sk_v)
!
      end subroutine fem_skv_inertia_rot_sgs_pg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scl_inertia_modsgs_pg(iele_fsmp_stack,         &
     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,   &
     &          scalar_e, sgs_e, flux_e, vxe, sk_v)
!
      use fem_skv_inertia1_sgsmod
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
      real (kind=kreal), intent(in) :: flux_e(ele%numele,3)
      real (kind=kreal), intent(in) :: sgs_e(ele%numele,3)
      real (kind=kreal), intent(in) :: scalar_e(ele%numele)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_scalar_inertia_sgsmod                                &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, scalar_e, sgs_e, flux_e, vxe, sk_v)
!
      end subroutine fem_skv_scl_inertia_modsgs_pg
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vec_inertia_modsgs_pg(iele_fsmp_stack,         &
     &          n_int, k2, i_filter, ak_diff, ele, jac_3d, FEM_elens,   &
     &          vector_e, sgs_e, flux_e, vxe, sk_v)
!
      use fem_skv_inertia3_sgsmod
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
      real (kind=kreal), intent(in) :: flux_e(ele%numele,6)
      real (kind=kreal), intent(in) :: sgs_e(ele%numele,6)
      real (kind=kreal), intent(in) :: vector_e(ele%numele,3)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_vector_inertia_sgsmod                                &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                   &
     &    np_smp, iele_fsmp_stack, n_int, k2, jac_3d%ntot_int,          &
     &    jac_3d%xjac, jac_3d%an, jac_3d%dnx, jac_3d%dnx,               &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    ak_diff, vector_e, sgs_e, flux_e, vxe, sk_v)
!
      end subroutine fem_skv_vec_inertia_modsgs_pg
!
!-----------------------------------------------------------------------
!
      end module fem_skv_div_sgs_flux_type
