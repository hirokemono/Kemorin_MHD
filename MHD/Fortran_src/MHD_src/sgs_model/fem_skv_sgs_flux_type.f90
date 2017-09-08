!fem_skv_sgs_flux_type.f90
!      module fem_skv_sgs_flux_type
!
!     programmed by H. Matsui in April, 2012
!
!      subroutine fem_skv_sgs_flux_galerkin(iele_fsmp_stack, n_int, k2, &
!     &          i_filter, nd_t, ele, jac_3d, FEM_elens,                &
!     &          scalar_1, dvx, sk_v)
!      subroutine fem_skv_sgs_flux_upwind(iele_fsmp_stack, n_int, k2,   &
!     &          i_filter, dt, nd_t, ele, jac_3d, FEM_elens,            &
!     &          scalar_1, vxe, dvx, sk_v)
!
!      subroutine fem_skv_sgs_uxb_galerkin(iele_fsmp_stack, n_int, k2,  &
!     &          i_filter, nd, ele, jac_3d, FEM_elens,                  &
!     &          vector_1, dvx, sk_v)
!      subroutine fem_skv_sgs_uxb_upwind(iele_fsmp_stack, n_int, k2,    &
!     &          i_filter, dt, nd, ele, jac_3d, FEM_elens,              &
!     &          vect_1, vxe, dvx, sk_v)
!
!      subroutine fem_skv_sgs_induct_t_galerkin(iele_fsmp_stack,        &
!     &          n_int, k2, i_filter, nd, ele, jac_3d, FEM_elens,       &
!     &          vect_sgs, dvx, dbx, sk_v)
!      subroutine fem_skv_sgs_induct_t_upwind(iele_fsmp_stack,          &
!     &          n_int, k2, i_filter, dt, nd, ele, jac_3d, FEM_elens,   &
!     &          vect_sgs, vxe, dvx, dbx, sk_v)
!
      module fem_skv_sgs_flux_type
!
      use m_precision
!
      use m_fem_gauss_int_coefs
      use t_geometry_data
      use t_finite_element_mat
      use t_jacobians
      use t_filter_elength
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_flux_galerkin(iele_fsmp_stack, n_int, k2,  &
     &          i_filter, nd_t, ele, jac_3d, FEM_elens,                 &
     &          scalar_1, dvx, sk_v)
!
      use fem_skv_sgs_flux
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_filter, nd_t
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: scalar_1(ele%numele)
      real (kind=kreal), intent(in) :: dvx(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_sgs_flux_pg                                          &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    scalar_1, dvx, nd_t, sk_v)
!
      end subroutine fem_skv_sgs_flux_galerkin
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_flux_upwind(iele_fsmp_stack, n_int, k2,    &
     &          i_filter, dt, nd_t, ele, jac_3d, FEM_elens,             &
     &          scalar_1, vxe, dvx, sk_v)
!
      use fem_skv_sgs_flux
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, nd_t, k2
      integer (kind=kint), intent(in) :: i_filter
!
      real(kind=kreal), intent(in) :: dt
      real (kind=kreal), intent(in) :: scalar_1(ele%numele)
      real (kind=kreal), intent(in) :: vxe(ele%numele,3)
      real (kind=kreal), intent(in) :: dvx(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_sgs_flux_upw                                         &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx, dt,      &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    scalar_1, vxe, dvx, nd_t, sk_v)
!
      end subroutine fem_skv_sgs_flux_upwind
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_uxb_galerkin(iele_fsmp_stack, n_int, k2,   &
     &          i_filter, nd, ele, jac_3d, FEM_elens,                   &
     &          vector_1, dvx, sk_v)
!
      use fem_skv_sgs_uxb
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vector_1(ele%numele,3)
      real (kind=kreal), intent(in) :: dvx(ele%numele,9)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_sgs_uxb_pg                                           &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d, n_int,       &
     &    k2, jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,      &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    vector_1, dvx, nd, sk_v)
!
      end subroutine fem_skv_sgs_uxb_galerkin
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_uxb_upwind(iele_fsmp_stack, n_int, k2,     &
     &          i_filter, dt, nd, ele, jac_3d, FEM_elens,               &
     &          vect_1, vxe, dvx, sk_v)
!
      use fem_skv_sgs_uxb
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: vect_1(ele%numele,3)
      real(kind=kreal), intent(in) :: dvx(ele%numele,9)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_sgs_uxb_upw                                          &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx, dt,      &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    vect_1, vxe, dvx, nd, sk_v)
!
      end subroutine fem_skv_sgs_uxb_upwind
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_induct_t_galerkin(iele_fsmp_stack,         &
     &          n_int, k2, i_filter, nd, ele, jac_3d, FEM_elens,        &
     &          vect_sgs, dvx, dbx, sk_v)
!
      use fem_skv_sgs_induct_t
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
!
      real(kind=kreal), intent(in) :: vect_sgs(ele%numele,3)
      real(kind=kreal), intent(in) :: dvx(ele%numele,3)
      real(kind=kreal), intent(in) :: dbx(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_sgs_induct_t_pg                                      &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx,          &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    vect_sgs, dvx, dbx, nd, sk_v)
!
      end subroutine fem_skv_sgs_induct_t_galerkin
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_induct_t_upwind(iele_fsmp_stack,           &
     &          n_int, k2, i_filter, dt, nd, ele, jac_3d, FEM_elens,    &
     &          vect_sgs, vxe, dvx, dbx, sk_v)
!
      use fem_skv_sgs_induct_t
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
!
      real(kind=kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: vect_sgs(ele%numele,3)
      real(kind=kreal), intent(in) :: vxe(ele%numele,3)
      real(kind=kreal), intent(in) :: dvx(ele%numele,3)
      real(kind=kreal), intent(in) :: dbx(ele%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_skv_sgs_induct_t_upw                                     &
     &   (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele, iele_fsmp_stack,  &
     &    max_int_point, maxtot_int_3d, int_start3, owe3d, n_int, k2,   &
     &    jac_3d%ntot_int, jac_3d%xjac, jac_3d%an, jac_3d%dnx, dt,      &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%moms%f_x2, FEM_elens%elen_ele%moms%f_y2,   &
     &    FEM_elens%elen_ele%moms%f_z2, FEM_elens%elen_ele%moms%f_xy,   &
     &    FEM_elens%elen_ele%moms%f_yz, FEM_elens%elen_ele%moms%f_zx,   &
     &    vect_sgs, vxe, dvx, dbx, nd, sk_v)
!
      end subroutine fem_skv_sgs_induct_t_upwind
!
!-----------------------------------------------------------------------
!
      end module fem_skv_sgs_flux_type
