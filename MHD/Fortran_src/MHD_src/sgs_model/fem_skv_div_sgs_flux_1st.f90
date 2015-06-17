!fem_skv_div_sgs_flux_1st.f90
!      module fem_skv_div_sgs_flux_1st
!
!        programmed by H.Matsui on July, 2005
!        modified by H.Matsui on Aug., 2006
!
!      subroutine fem_skv_div_sgs_vector_1st(iele_fsmp_stack, n_int,    &
!     &          k2, i_filter, ak_diff, sgs_1, flux_1, sk_v)
!      subroutine fem_skv_div_sgs_tensor_1st(iele_fsmp_stack, n_int,    &
!     &          k2, i_filter, ak_diff, sgs_1, flux_1, sk_v)
!      subroutine fem_skv_div_sgs_asym_t_1st(iele_fsmp_stack, n_int,    &
!     &          k2, i_filter, ak_diff, sgs_1, flux_1, sk_v)
!
!
!      subroutine fem_skv_scl_inertia_sgs_1st(iele_fsmp_stack,          &
!     &          n_int, k2, scalar_e, sgs_e, vxe, sk_v)
!      subroutine fem_skv_vec_inertia_sgs_1st(iele_fsmp_stack,          &
!     &          n_int, k2, vector_e, sgs_e, vxe, sk_v)
!      subroutine fem_skv_inertia_rot_sgs_1st(iele_fsmp_stack,          &
!     &          n_int, k2, vector_e, sgs_e, wxe, sk_v)
!
!      subroutine fem_skv_scl_inertia_modsgs_1st(iele_fsmp_stack,       &
!     &          n_int, k2, i_filter, ak_diff, scalar_e, sgs_e, flux_e, &
!     &          vxe, sk_v)
!      subroutine fem_skv_vec_inertia_modsgs_1st(iele_fsmp_stack,       &
!     &          n_int, k2, i_filter, ak_diff, vector_e, sgs_e, flux_e, &
!     &          vxe, sk_v)
!
      module fem_skv_div_sgs_flux_1st
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
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_vector_1st(iele_fsmp_stack, n_int,     &
     &          k2, i_filter, ak_diff, sgs_1, flux_1, sk_v)
!
      use fem_skv_div_vect_w_sgs
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: sgs_1(numele,n_vector)
      real(kind=kreal), intent(in) :: flux_1(numele,n_vector)
      real(kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_div_sgs_vector_pg(numele, nnod_4_ele, nnod_4_ele,    &
     &    np_smp, iele_fsmp_stack, n_int, k2,                           &
     &    ntot_int_3d, xjac, aw, dwx, dwx,                              &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    ak_diff, sgs_1, flux_1, sk_v)
!
      end subroutine fem_skv_div_sgs_vector_1st
!
!   --------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_tensor_1st(iele_fsmp_stack, n_int,     &
     &          k2, i_filter, ak_diff, sgs_1, flux_1, sk_v)
!
      use fem_skv_div_tsr_w_sgs
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: sgs_1(numele,n_sym_tensor)
      real(kind=kreal), intent(in) :: flux_1(numele,n_sym_tensor)
      real(kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_div_tensor_w_sgs_pg(numele, nnod_4_ele, nnod_4_ele,  &
     &    np_smp, iele_fsmp_stack, n_int, k2,                           &
     &    ntot_int_3d, xjac, aw, dwx, dwx,                              &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    ak_diff, sgs_1, flux_1, sk_v)
!
      end subroutine fem_skv_div_sgs_tensor_1st
!
!   --------------------------------------------------------------------
!
      subroutine fem_skv_div_sgs_asym_t_1st(iele_fsmp_stack, n_int,     &
     &          k2, i_filter, ak_diff, sgs_1, flux_1, sk_v)
!
      use fem_skv_div_ast_w_sgs
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int
      integer(kind=kint), intent(in) :: k2, i_filter
!
      real(kind=kreal), intent(in) :: sgs_1(numele,3)
      real(kind=kreal), intent(in) :: flux_1(numele,3)
      real(kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_div_sgs_asym_t_pg(numele, nnod_4_ele, nnod_4_ele,    &
     &    np_smp, iele_fsmp_stack, n_int, k2,                           &
     &    ntot_int_3d, xjac, aw, dwx, dwx,                              &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    ak_diff, sgs_1, flux_1, sk_v)
!
      end subroutine fem_skv_div_sgs_asym_t_1st
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine fem_skv_scl_inertia_sgs_1st(iele_fsmp_stack,           &
     &          n_int, k2, scalar_e, sgs_e, vxe, sk_v)
!
      use fem_skv_inertia_sgs
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: sgs_e(numele,3)
      real (kind=kreal), intent(in) :: scalar_e(numele)
      real (kind=kreal), intent(in) :: vxe(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_scalar_inertia_sgs                                   &
     &   (numele, nnod_4_ele, nnod_4_ele, np_smp, iele_fsmp_stack,      &
     &    n_int, k2, ntot_int_3d, xjac, aw, dwx,                        &
     &    scalar_e, sgs_e, vxe, sk_v)
!
      end subroutine fem_skv_scl_inertia_sgs_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vec_inertia_sgs_1st(iele_fsmp_stack,           &
     &          n_int, k2, vector_e, sgs_e, vxe, sk_v)
!
      use fem_skv_inertia_sgs
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: sgs_e(numele,6)
      real (kind=kreal), intent(in) :: vector_e(numele,3)
      real (kind=kreal), intent(in) :: vxe(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_vector_inertia_sgs                                   &
     &   (numele, nnod_4_ele, nnod_4_ele, np_smp, iele_fsmp_stack,      &
     &    n_int, k2, ntot_int_3d, xjac, aw, dwx,                        &
     &    vector_e, sgs_e, vxe, sk_v)
!
      end subroutine fem_skv_vec_inertia_sgs_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_inertia_rot_sgs_1st(iele_fsmp_stack,           &
     &          n_int, k2, vector_e, sgs_e, wxe, sk_v)
!
      use fem_skv_inertia_sgs
!
      integer(kind=kint), intent(in) :: n_int, k2
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vector_e(numele,3)
      real (kind=kreal), intent(in) :: sgs_e(numele,6)
      real (kind=kreal), intent(in) :: wxe(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_inertia_rot_sgs                                      &
     &   (numele, nnod_4_ele, nnod_4_ele, np_smp, iele_fsmp_stack,      &
     &    n_int, k2, ntot_int_3d, xjac, aw, aw, dwx,                    &
     &    vector_e, sgs_e, wxe, sk_v)
!
      end subroutine fem_skv_inertia_rot_sgs_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_scl_inertia_modsgs_1st(iele_fsmp_stack,        &
     &          n_int, k2, i_filter, ak_diff, scalar_e, sgs_e, flux_e,  &
     &          vxe, sk_v)
!
      use m_filter_elength
      use fem_skv_inertia1_sgsmod
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_diff(numele)
      real (kind=kreal), intent(in) :: flux_e(numele,3)
      real (kind=kreal), intent(in) :: sgs_e(numele,3)
      real (kind=kreal), intent(in) :: scalar_e(numele)
      real (kind=kreal), intent(in) :: vxe(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      call fem_skv_scalar_inertia_sgsmod                                &
     &   (numele, nnod_4_ele, nnod_4_ele, np_smp,                       &
     &    iele_fsmp_stack,n_int, k2,                                    &
     &    ntot_int_3d, xjac, aw, dwx, dwx,                              &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    ak_diff, scalar_e, sgs_e, flux_e, vxe, sk_v)
!
      end subroutine fem_skv_scl_inertia_modsgs_1st
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_vec_inertia_modsgs_1st(iele_fsmp_stack,        &
     &          n_int, k2, i_filter, ak_diff, vector_e, sgs_e, flux_e,  &
     &          vxe, sk_v)
!
      use m_filter_elength
      use fem_skv_inertia3_sgsmod
!
      integer(kind=kint), intent(in) :: n_int, k2, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: ak_diff(numele)
      real (kind=kreal), intent(in) :: flux_e(numele,6)
      real (kind=kreal), intent(in) :: sgs_e(numele,6)
      real (kind=kreal), intent(in) :: vector_e(numele,3)
      real (kind=kreal), intent(in) :: vxe(numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_vector_inertia_sgsmod                                &
     &   (numele, nnod_4_ele, nnod_4_ele, np_smp,                       &
     &    iele_fsmp_stack, n_int, k2,                                   &
     &    ntot_int_3d, xjac, aw, dwx, dwx,                              &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    ak_diff, vector_e, sgs_e, flux_e, vxe, sk_v)
!
      end subroutine fem_skv_vec_inertia_modsgs_1st
!
!-----------------------------------------------------------------------
!
      end module fem_skv_div_sgs_flux_1st
