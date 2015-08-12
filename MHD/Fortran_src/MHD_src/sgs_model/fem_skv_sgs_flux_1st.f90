!fem_skv_sgs_flux_1st.f90
!      module fem_skv_sgs_flux_1st
!
!     programmed by H. Matsui in April, 2012
!
!      subroutine fem_skv_sgs_flux_pg_1(iele_fsmp_stack, n_int, k2,     &
!     &          scalar_1, dvx, i_filter, nd_t, sk_v)
!      subroutine fem_skv_sgs_flux_upw_1(iele_fsmp_stack, n_int, k2,    &
!     &          scalar_1, vxe, dvx, i_filter, nd_t, sk_v)
!
!      subroutine fem_skv_sgs_uxb_pg_1(iele_fsmp_stack, n_int, k2,      &
!     &          vect_1, dvx, i_filter, nd, sk_v)
!      subroutine fem_skv_sgs_uxb_upw_1(iele_fsmp_stack, n_int, k2,     &
!     &          vect_1, vxe, dvx, i_filter, nd, sk_v)
!
!      subroutine fem_skv_sgs_induct_t_pg_1(iele_fsmp_stack, n_int, k2, &
!     &          vect_sgs, dvx, dbx, i_filter, nd, sk_v)
!      subroutine fem_skv_sgs_induct_t_pg_1(iele_fsmp_stack, n_int, k2, &
!     &          vect_sgs, dvx, dbx, i_filter, nd, sk_v)
!
      module fem_skv_sgs_flux_1st
!
      use m_precision
!
      use m_geometry_data
      use m_phys_constants
      use m_jacobians
      use m_filter_elength
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_flux_pg_1(iele_fsmp_stack, n_int, k2,      &
     &          scalar_1, dvx, i_filter, nd_t, sk_v)
!
      use fem_skv_sgs_flux
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: i_filter, nd_t
      integer (kind=kint), intent(in) :: n_int, k2
!
      real (kind=kreal), intent(in) :: scalar_1(ele1%numele)
      real (kind=kreal), intent(in) :: dvx(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_sgs_flux_pg(ele1%numele, nnod_4_ele, nnod_4_ele,     &
     &    iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, aw, dwx,       &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%moms%f_x2, FEM1_elen%elen_ele%moms%f_y2,   &
     &    FEM1_elen%elen_ele%moms%f_z2, FEM1_elen%elen_ele%moms%f_xy,   &
     &    FEM1_elen%elen_ele%moms%f_yz, FEM1_elen%elen_ele%moms%f_zx,   &
     &    scalar_1, dvx, nd_t, sk_v)
!
      end subroutine fem_skv_sgs_flux_pg_1
!
! ----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_flux_upw_1(iele_fsmp_stack, n_int, k2,     &
     &          scalar_1, vxe, dvx, i_filter, nd_t, sk_v)
!
      use m_t_int_parameter
      use fem_skv_sgs_flux
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int, nd_t, k2
      integer (kind=kint), intent(in) :: i_filter
!
      real (kind=kreal), intent(in) :: scalar_1(ele1%numele)
      real (kind=kreal), intent(in) :: vxe(ele1%numele,3)
      real (kind=kreal), intent(in) :: dvx(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_sgs_flux_upw(ele1%numele, nnod_4_ele, nnod_4_ele,    &
     &    iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, aw, dwx,       &
     &    dt, FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),            &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%moms%f_x2, FEM1_elen%elen_ele%moms%f_y2,   &
     &    FEM1_elen%elen_ele%moms%f_z2, FEM1_elen%elen_ele%moms%f_xy,   &
     &    FEM1_elen%elen_ele%moms%f_yz, FEM1_elen%elen_ele%moms%f_zx,   &
     &    scalar_1, vxe, dvx, nd_t, sk_v)
!
      end subroutine fem_skv_sgs_flux_upw_1
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_uxb_pg_1(iele_fsmp_stack, n_int, k2,       &
     &          vect_1, dvx, i_filter, nd, sk_v)
!
      use fem_skv_sgs_uxb
!
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vect_1(ele1%numele,3)
      real (kind=kreal), intent(in) :: dvx(ele1%numele,9)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_sgs_uxb_pg(ele1%numele, nnod_4_ele, nnod_4_ele,      &
     &    iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, aw, dwx,       &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%moms%f_x2, FEM1_elen%elen_ele%moms%f_y2,   &
     &    FEM1_elen%elen_ele%moms%f_z2, FEM1_elen%elen_ele%moms%f_xy,   &
     &    FEM1_elen%elen_ele%moms%f_yz, FEM1_elen%elen_ele%moms%f_zx,   &
     &    vect_1, dvx, nd, sk_v)
!
      end subroutine fem_skv_sgs_uxb_pg_1
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_uxb_upw_1(iele_fsmp_stack, n_int, k2,      &
     &          vect_1, vxe, dvx, i_filter, nd, sk_v)
!
      use m_t_int_parameter
      use fem_skv_sgs_uxb
!
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real (kind=kreal), intent(in) :: vxe(ele1%numele,3)
      real (kind=kreal), intent(in) :: vect_1(ele1%numele,3)
      real (kind=kreal), intent(in) :: dvx(ele1%numele,9)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_sgs_uxb_upw(ele1%numele, nnod_4_ele, nnod_4_ele,     &
     &    iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, aw, dwx,       &
     &    dt, FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),            &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%moms%f_x2, FEM1_elen%elen_ele%moms%f_y2,   &
     &    FEM1_elen%elen_ele%moms%f_z2, FEM1_elen%elen_ele%moms%f_xy,   &
     &    FEM1_elen%elen_ele%moms%f_yz, FEM1_elen%elen_ele%moms%f_zx,   &
     &    vect_1, vxe, dvx, nd, sk_v)
!
      end subroutine fem_skv_sgs_uxb_upw_1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_induct_t_pg_1(iele_fsmp_stack, n_int, k2,  &
     &          vect_sgs, dvx, dbx, i_filter, nd, sk_v)
!
      use fem_skv_sgs_induct_t
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
!
      real(kind=kreal), intent(in) :: vect_sgs(ele1%numele,3)
      real(kind=kreal), intent(in) :: dvx(ele1%numele,3)
      real(kind=kreal), intent(in) :: dbx(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_sgs_induct_t_pg(ele1%numele, nnod_4_ele, nnod_4_ele, &
     &    iele_fsmp_stack, n_int, k2, ntot_int_3d, xjac, aw, dwx,       &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%moms%f_x2, FEM1_elen%elen_ele%moms%f_y2,   &
     &    FEM1_elen%elen_ele%moms%f_z2, FEM1_elen%elen_ele%moms%f_xy,   &
     &    FEM1_elen%elen_ele%moms%f_yz, FEM1_elen%elen_ele%moms%f_zx,   &
     &    vect_sgs, dvx, dbx, nd, sk_v)
!
      end subroutine fem_skv_sgs_induct_t_pg_1
!
!-----------------------------------------------------------------------
!
      subroutine fem_skv_sgs_induct_t_upw_1(iele_fsmp_stack, n_int, k2, &
     &          vect_sgs, vxe, dvx, dbx, i_filter, nd, sk_v)
!
      use m_t_int_parameter
      use fem_skv_sgs_induct_t
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: nd, n_int, i_filter, k2
!
      real(kind=kreal), intent(in) :: vect_sgs(ele1%numele,3)
      real(kind=kreal), intent(in) :: vxe(ele1%numele,3)
      real(kind=kreal), intent(in) :: dvx(ele1%numele,3)
      real(kind=kreal), intent(in) :: dbx(ele1%numele,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                   :: sk_v(ele1%numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_skv_sgs_induct_t_upw                                     &
     &   (ele1%numele, nnod_4_ele, nnod_4_ele,                          &
     &    iele_fsmp_stack, n_int, ntot_int_3d, k2, xjac, aw, dwx,       &
     &    dt, FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),            &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%moms%f_x2, FEM1_elen%elen_ele%moms%f_y2,   &
     &    FEM1_elen%elen_ele%moms%f_z2, FEM1_elen%elen_ele%moms%f_xy,   &
     &    FEM1_elen%elen_ele%moms%f_yz, FEM1_elen%elen_ele%moms%f_zx,   &
     &    vect_sgs, vxe, dvx, dbx, nd, sk_v)
!
      end subroutine fem_skv_sgs_induct_t_upw_1
!
!-----------------------------------------------------------------------
!
      end module fem_skv_sgs_flux_1st
