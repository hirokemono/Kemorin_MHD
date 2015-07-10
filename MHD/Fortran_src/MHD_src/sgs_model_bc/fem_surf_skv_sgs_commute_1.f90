!fem_surf_skv_sgs_commute_1.f90
!      module fem_surf_skv_sgs_commute_1
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine fem_sf_skv_sgs_commute_err_p1(igrp, k2, nd, n_int,    &
!     &          i_filter, n_diff, dxe_sf, scalar_sf, sk_v)
!      subroutine fem_sf_skv_grad_commute_p1(igrp, k2, n_int, i_filter, &
!     &          dxe_sf, scalar_sf, sk_v)
!      subroutine fem_sf_skv_div_flux_commute_p1(igrp, k2, nd, n_int,   &
!     &          i_filter, dxe_sf, vect_sf, sk_v)
!
!      subroutine fem_sf_skv_sgs_vect_diff_p1(igrp, k2, nd, n_int,      &
!     &          i_filter, n_diff, dxe_sf, scalar_sf,                   &
!     &          ak_diff, coef, sk_v)
!      subroutine fem_sf_skv_sgs_grad_p1(igrp, k2, n_int, i_filter,     &
!     &          dxe_sf, scalar_sf, ak_diff, coef, sk_v)
!      subroutine fem_sf_skv_sgs_div_flux_p1(igrp, k2, nd, n_int,       &
!     &          i_filter, dxe_sf, vect_sf, ak_diff, coef, sk_v)
!
!      subroutine fem_sf_skv_sgs_div_linear_p1(igrp, k2, n_diff, n_int, &
!     &          i_filter, dxe_sf, scalar_sf, ak_diff, sk_v)
!      subroutine fem_sf_skv_sgs_velo_co_p1(igrp, k2, n_int, i_filter,  &
!     &          dxe_sf, scalar_sf, ak_diff, sk_v)
!
!      subroutine fem_surf_skv_poisson_sgs_1(igrp, k2, n_int,           &
!     &          i_filter, nd, phi_sf, ak_diff, sk_v)
!      subroutine fem_surf_skv_diffusion_sgs_1(igrp, k2, n_int,         &
!     &          i_filter, nd, vect_sf, ak_diff, ak_d, nd_v, sk_v)
!
      module fem_surf_skv_sgs_commute_1
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_surface_group
      use m_phys_constants
!
      use m_filter_elength
      use m_jacobians
      use m_jacobian_sf_grp
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_commute_err_p1(igrp, k2, nd, n_int,     &
     &          i_filter, n_diff, dxe_sf, scalar_sf, sk_v)
!
      use fem_surf_skv_sgs_commute
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, n_diff, i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp1%num_item)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_sf_skv_sgs_commute_err_p                                 &
     &   (np_smp, numele, nnod_4_ele, nnod_4_surf, nnod_4_surf,         &
     &    node_on_sf, sf_grp1%num_item, surf_item, num_surf_smp,        &
     &    isurf_grp_smp_stack, jac1_sf_grp_2d_q%ntot_int,               &
     &    jac1_sf_grp_2d_q%xsf_sf, jac1_sf_grp_2d_q%axj_sf,             &
     &    jac1_sf_grp_2d_q%an_sf, jac1_sf_grp_2d_q%an_sf,               &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, nd, n_diff, dxe_sf, scalar_sf, sk_v)
!
      end subroutine fem_sf_skv_sgs_commute_err_p1
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_grad_commute_p1(igrp, k2, n_int, i_filter,  &
     &          dxe_sf, scalar_sf, sk_v)
!
      use fem_surf_skv_sgs_grad
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp1%num_item)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_sf_skv_grad_commute_posi                                 &
     &   (np_smp, numele, nnod_4_ele, nnod_4_surf, nnod_4_surf,         &
     &    node_on_sf, sf_grp1%num_item, surf_item, num_surf_smp,        &
     &    isurf_grp_smp_stack, jac1_sf_grp_2d_q%ntot_int,               &
     &    jac1_sf_grp_2d_q%xsf_sf, jac1_sf_grp_2d_q%axj_sf,             &
     &    jac1_sf_grp_2d_q%an_sf, jac1_sf_grp_2d_q%an_sf,               &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, dxe_sf, scalar_sf, sk_v)
!
      end subroutine fem_sf_skv_grad_commute_p1
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_div_flux_commute_p1(igrp, k2, nd, n_int,    &
     &          i_filter, dxe_sf, vect_sf, sk_v)
!
      use fem_surf_skv_sgs_div
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, i_filter
!
      real(kind=kreal), intent(in)                                      &
     &                 :: dxe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real(kind=kreal), intent(in) :: vect_sf(sf_grp1%num_item,3)
!
      real(kind=kreal), intent(inout)                                   &
     &                 :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_sf_skv_div_flux_commute_p                                &
     &   (np_smp, numele, nnod_4_ele, nnod_4_surf, nnod_4_surf,         &
     &    node_on_sf, sf_grp1%num_item, surf_item, num_surf_smp,        &
     &    isurf_grp_smp_stack, jac1_sf_grp_2d_q%ntot_int,               &
     &    jac1_sf_grp_2d_q%xsf_sf, jac1_sf_grp_2d_q%axj_sf,             &
     &    jac1_sf_grp_2d_q%an_sf, jac1_sf_grp_2d_q%an_sf,               &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    igrp, k2, nd, n_int, dxe_sf, vect_sf, sk_v)
!
      end subroutine fem_sf_skv_div_flux_commute_p1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_vect_diff_p1(igrp, k2, nd, n_int,       &
     &          i_filter, n_diff, dxe_sf, scalar_sf,                    &
     &          ak_diff, coef, sk_v)
!
      use fem_surf_skv_sgs_commute
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, n_diff, i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp1%num_item)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_sf_skv_sgs_vect_diff_p                                   &
     &   (np_smp, numele, nnod_4_ele, nnod_4_surf, nnod_4_surf,         &
     &    node_on_sf, sf_grp1%num_item, surf_item, num_surf_smp,        &
     &    isurf_grp_smp_stack, jac1_sf_grp_2d_q%ntot_int,               &
     &    jac1_sf_grp_2d_q%xsf_sf, jac1_sf_grp_2d_q%axj_sf,             &
     &    jac1_sf_grp_2d_q%an_sf, jac1_sf_grp_2d_q%an_sf,               &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, nd, n_diff, dxe_sf, scalar_sf,               &
     &    ak_diff, coef, sk_v)
!
      end subroutine fem_sf_skv_sgs_vect_diff_p1
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_grad_p1(igrp, k2, n_int, i_filter,      &
     &          dxe_sf, scalar_sf, ak_diff, coef, sk_v)
!
      use fem_surf_skv_sgs_grad
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp1%num_item)
      real (kind=kreal), intent(in) :: ak_diff(numele), coef
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_sf_skv_sgs_grad_posi                                     &
     &   (np_smp, numele, nnod_4_ele, nnod_4_surf, nnod_4_surf,         &
     &    node_on_sf, sf_grp1%num_item, surf_item, num_surf_smp,        &
     &    isurf_grp_smp_stack, jac1_sf_grp_2d_q%ntot_int,               &
     &    jac1_sf_grp_2d_q%xsf_sf, jac1_sf_grp_2d_q%axj_sf,             &
     &    jac1_sf_grp_2d_q%an_sf, jac1_sf_grp_2d_q%an_sf,               &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, dxe_sf, scalar_sf, ak_diff, coef, sk_v)
!
      end subroutine fem_sf_skv_sgs_grad_p1
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_div_flux_p1(igrp, k2, nd, n_int,        &
     &          i_filter, dxe_sf, vect_sf, ak_diff, coef, sk_v)
!
      use fem_surf_skv_sgs_div
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real (kind=kreal), intent(in) :: vect_sf(sf_grp1%num_item,3)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_sf_skv_sgs_div_flux_posi                                 &
     &   (np_smp, numele, nnod_4_ele, nnod_4_surf, nnod_4_surf,         &
     &    node_on_sf, sf_grp1%num_item, surf_item, num_surf_smp,        &
     &    isurf_grp_smp_stack, jac1_sf_grp_2d_q%ntot_int,               &
     &    jac1_sf_grp_2d_q%xsf_sf, jac1_sf_grp_2d_q%axj_sf,             &
     &    jac1_sf_grp_2d_q%an_sf, jac1_sf_grp_2d_q%an_sf,               &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    igrp, k2, nd, n_int, dxe_sf, vect_sf, ak_diff, coef, sk_v)
!
      end subroutine fem_sf_skv_sgs_div_flux_p1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_div_linear_p1(igrp, k2, n_diff, n_int,  &
     &          i_filter, dxe_sf, scalar_sf, ak_diff, sk_v)
!
      use fem_surf_skv_sgs_commute
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: n_diff, i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp1%num_item)
      real (kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_sf_skv_sgs_vect_diff_p                                   &
     &   (np_smp, numele, num_t_linear, num_linear_sf, nnod_4_surf,     &
     &    node_on_sf, sf_grp1%num_item, surf_item, num_surf_smp,        &
     &    isurf_grp_smp_stack, jac1_sf_grp_2d_q%ntot_int,               &
     &    jac1_sf_grp_2d_q%xsf_sf, jac1_sf_grp_2d_q%axj_sf,             &
     &    jac1_sf_grp_2d_l%an_sf, jac1_sf_grp_2d_q%an_sf,               &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, ione, n_diff, dxe_sf, scalar_sf,             &
     &    ak_diff, one, sk_v)
!
      end subroutine fem_sf_skv_sgs_div_linear_p1
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_velo_co_p1(igrp, k2, n_int, i_filter,   &
     &          dxe_sf, scalar_sf, ak_diff, sk_v)
!
      use fem_surf_skv_sgs_grad
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp1%num_item)
      real (kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_sf_skv_sgs_grad_posi                                     &
     &   (np_smp, numele, nnod_4_ele, nnod_4_surf, num_linear_sf,       &
     &    node_on_sf, sf_grp1%num_item, surf_item, num_surf_smp,        &
     &    isurf_grp_smp_stack, jac1_sf_grp_2d_q%ntot_int,               &
     &    jac1_sf_grp_2d_q%xsf_sf, jac1_sf_grp_2d_q%axj_sf,             &
     &    jac1_sf_grp_2d_q%an_sf, jac1_sf_grp_2d_l%an_sf,               &
     &    FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM1_elen%nele_filter_mom,                                    &
     &    FEM1_elen%elen_ele%diff%df_x2, FEM1_elen%elen_ele%diff%df_y2, &
     &    FEM1_elen%elen_ele%diff%df_z2, FEM1_elen%elen_ele%diff%df_xy, &
     &    FEM1_elen%elen_ele%diff%df_yz, FEM1_elen%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, dxe_sf, scalar_sf, ak_diff, one, sk_v)
!
      end subroutine fem_sf_skv_sgs_velo_co_p1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_poisson_sgs_1(igrp, k2, n_int,            &
     &          i_filter, phi_sf, ak_diff, sk_v)
!
      use fem_surf_skv_diffuse_sgs
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
!
      real (kind=kreal), intent(in) :: phi_sf(sf_grp1%num_item)
      real (kind=kreal), intent(in) :: ak_diff(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_surf_skv_poisson_sgs(np_smp, numele, num_t_linear,       &
     &  num_t_linear, num_linear_sf, node_on_sf, sf_grp1%num_item,      &
     &  surf_item, num_surf_smp, isurf_grp_smp_stack,                   &
     &  ntot_int_3d, xjac, dnx, dnx,                                    &
     &  FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM1_elen%nele_filter_mom,                                      &
     &  FEM1_elen%elen_ele%diff2%df_x2, FEM1_elen%elen_ele%diff2%df_y2, &
     &  FEM1_elen%elen_ele%diff2%df_z2, FEM1_elen%elen_ele%diff2%df_xy, &
     &  FEM1_elen%elen_ele%diff2%df_yz, FEM1_elen%elen_ele%diff2%df_zx, &
     &  igrp, k2, n_int, ak_diff, phi_sf, sk_v)
!
      end subroutine fem_surf_skv_poisson_sgs_1
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_diffusion_sgs_1(igrp, k2, n_int,          &
     &          i_filter, vect_sf, ak_diff, ak_d, nd_v, sk_v)
!
      use fem_surf_skv_diffuse_sgs
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter, nd_v
!
      real (kind=kreal), intent(in) :: vect_sf(sf_grp1%num_item,3)
      real (kind=kreal), intent(in) :: ak_diff(numele)
      real (kind=kreal), intent(in) :: ak_d(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_surf_skv_diffusion_sgs(np_smp, numele, nnod_4_ele,       &
     &  nnod_4_ele, nnod_4_surf, node_on_sf,                            &
     &  sf_grp1%num_item, surf_item,                                    &
     &  num_surf_smp, isurf_grp_smp_stack,                              &
     &  ntot_int_3d, xjac, dwx, dwx,                                    &
     &  FEM1_elen%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM1_elen%nele_filter_mom,                                      &
     &  FEM1_elen%elen_ele%diff2%df_x2, FEM1_elen%elen_ele%diff2%df_y2, &
     &  FEM1_elen%elen_ele%diff2%df_z2, FEM1_elen%elen_ele%diff2%df_xy, &
     &  FEM1_elen%elen_ele%diff2%df_yz, FEM1_elen%elen_ele%diff2%df_zx, &
     &  igrp, k2, n_int, ak_diff, vect_sf, ak_d, nd_v, sk_v)
!
      end subroutine fem_surf_skv_diffusion_sgs_1
!
!-----------------------------------------------------------------------
!
      end module fem_surf_skv_sgs_commute_1
