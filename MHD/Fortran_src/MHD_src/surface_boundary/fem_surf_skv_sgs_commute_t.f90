!fem_surf_skv_sgs_commute_t.f90
!      module fem_surf_skv_sgs_commute_t
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine fem_sf_grp_skv_commute_err_p                         &
!!     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,             &
!!     &          igrp, k2, nd, n_int, i_filter, n_diff,                &
!!     &          dxe_sf, scalar_sf, sk_v)
!!      subroutine fem_sf_grp_skv_grad_commute_p                        &
!!     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,             &
!!     &          igrp, k2, n_int, i_filter, dxe_sf, scalar_sf, sk_v)
!!      subroutine fem_sf_grp_skv_div_f_commute_p                       &
!!     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,             &
!!     &          igrp, k2, nd, n_int, i_filter, dxe_sf, vect_sf, sk_v)
!!
!!      subroutine fem_sf_grp_skv_sgs_vect_diff_p                       &
!!     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,             &
!!     &          igrp, k2, nd, n_int, i_filter, n_diff,                &
!!     &          dxe_sf, scalar_sf, ak_diff, coef, sk_v)
!!      subroutine fem_sf_grp_skv_sgs_grad_p                            &
!!     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,             &
!!     &         igrp, k2, n_int, i_filter, dxe_sf, scalar_sf,          &
!!     &         ak_diff, coef, sk_v)
!!      subroutine fem_sf_grp_skv_sgs_div_flux_p                        &
!!     &        (ele, surf, sf_grp, jac_sf_grp, FEM_elens,              &
!!     &         igrp, k2, nd, n_int, i_filter, dxe_sf, vect_sf,        &
!!     &         ak_diff, coef, sk_v)
!!
!!      subroutine fem_sf_grp_skv_sgs_div_lin_p                         &
!!     &        (ele, surf, sf_grp, jac_sf_grp, jac_sf_grp_l, FEM_elens,&
!!     &         igrp, k2, n_diff, n_int, i_filter,                     &
!!     &         dxe_sf, scalar_sf, ak_diff, sk_v)
!!      subroutine fem_sf_grp_skv_sgs_velo_co_p                         &
!!     &        (ele, surf, sf_grp, jac_sf_grp, jac_sf_grp_l, FEM_elens,&
!!     &         igrp, k2, n_int, i_filter, dxe_sf, scalar_sf, ak_diff, &
!!     &         sk_v)
!!
!!      subroutine fem_sf_grp_skv_poisson_sgs                           &
!!     &         (ele, surf, sf_grp, jac_3d_l, FEM_elens, igrp,         &
!!     &          k2, n_int, i_filter, phi_sf, ak_diff, sk_v)
!!      subroutine fem_sf_grp_skv_diffusion_sgs                         &
!!     &         (ele, surf, sf_grp, jac_3d, FEM_elens, igrp, k2, n_int,&
!!     &          i_filter, vect_sf, ak_diff, ak_d, nd_v, sk_v)
!
      module fem_surf_skv_sgs_commute_t
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use m_fem_gauss_int_coefs
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobians
      use t_jacobian_2d
      use t_filter_elength
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_grp_skv_commute_err_p                           &
     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,               &
     &          igrp, k2, nd, n_int, i_filter, n_diff,                  &
     &          dxe_sf, scalar_sf, sk_v)
!
      use fem_surf_skv_sgs_commute
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, n_diff, i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp%num_item)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_sf_skv_sgs_commute_err_p                                 &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    surf%nnod_4_surf, surf%nnod_4_surf, surf%node_on_sf,          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    max_int_point, maxtot_int_2d, int_start2, owe2d,              &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, jac_sf_grp%axj_sf,    &
     &    jac_sf_grp%an_sf, jac_sf_grp%an_sf,                           &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, nd, n_diff, dxe_sf, scalar_sf, sk_v)
!
      end subroutine fem_sf_grp_skv_commute_err_p
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_grp_skv_grad_commute_p                          &
     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,               &
     &          igrp, k2, n_int, i_filter, dxe_sf, scalar_sf, sk_v)
!
      use fem_surf_skv_sgs_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp%num_item)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_sf_skv_grad_commute_posi                                 &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    surf%nnod_4_surf, surf%nnod_4_surf, surf%node_on_sf,          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    max_int_point, maxtot_int_2d, int_start2, owe2d,              &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, jac_sf_grp%axj_sf,    &
     &    jac_sf_grp%an_sf, jac_sf_grp%an_sf,                           &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, dxe_sf, scalar_sf, sk_v)
!
      end subroutine fem_sf_grp_skv_grad_commute_p
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_grp_skv_div_f_commute_p                         &
     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,               &
     &          igrp, k2, nd, n_int, i_filter, dxe_sf, vect_sf, sk_v)
!
      use fem_surf_skv_sgs_div
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, i_filter
!
      real(kind=kreal), intent(in)                                      &
     &                 :: dxe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real(kind=kreal), intent(in) :: vect_sf(sf_grp%num_item,3)
!
      real(kind=kreal), intent(inout)                                   &
     &           :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_sf_skv_div_flux_commute_p                                &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    surf%nnod_4_surf, surf%nnod_4_surf, surf%node_on_sf,          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    max_int_point, maxtot_int_2d, int_start2, owe2d,              &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, jac_sf_grp%axj_sf,    &
     &    jac_sf_grp%an_sf, jac_sf_grp%an_sf,                           &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, nd, n_int, dxe_sf, vect_sf, sk_v)
!
      end subroutine fem_sf_grp_skv_div_f_commute_p
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_sf_grp_skv_sgs_vect_diff_p                         &
     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,               &
     &          igrp, k2, nd, n_int, i_filter, n_diff,                  &
     &          dxe_sf, scalar_sf, ak_diff, coef, sk_v)
!
      use fem_surf_skv_sgs_commute
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, n_diff, i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp%num_item)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_sf_skv_sgs_vect_diff_p                                   &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    surf%nnod_4_surf, surf%nnod_4_surf, surf%node_on_sf,          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    max_int_point, maxtot_int_2d, int_start2, owe2d,              &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, jac_sf_grp%axj_sf,    &
     &    jac_sf_grp%an_sf, jac_sf_grp%an_sf,                           &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, nd, n_diff, dxe_sf, scalar_sf,               &
     &    ak_diff, coef, sk_v)
!
      end subroutine fem_sf_grp_skv_sgs_vect_diff_p
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_grp_skv_sgs_grad_p                              &
     &         (ele, surf, sf_grp, jac_sf_grp, FEM_elens,               &
     &          igrp, k2, n_int, i_filter, dxe_sf, scalar_sf,           &
     &          ak_diff, coef, sk_v)
!
      use fem_surf_skv_sgs_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp%num_item)
      real (kind=kreal), intent(in) :: ak_diff(ele%numele), coef
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_sf_skv_sgs_grad_posi(np_smp, ele%numele, ele%nnod_4_ele, &
     &    surf%nnod_4_surf, surf%nnod_4_surf, surf%node_on_sf,          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    max_int_point, maxtot_int_2d, int_start2, owe2d,              &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, jac_sf_grp%axj_sf,    &
     &    jac_sf_grp%an_sf, jac_sf_grp%an_sf,                           &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, dxe_sf, scalar_sf, ak_diff, coef, sk_v)
!
      end subroutine fem_sf_grp_skv_sgs_grad_p
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_grp_skv_sgs_div_flux_p                          &
     &        (ele, surf, sf_grp, jac_sf_grp, FEM_elens,                &
     &         igrp, k2, nd, n_int, i_filter, dxe_sf, vect_sf,          &
     &         ak_diff, coef, sk_v)
!
      use fem_surf_skv_sgs_div
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real (kind=kreal), intent(in) :: vect_sf(sf_grp%num_item,3)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_sf_skv_sgs_div_flux_posi                                 &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    surf%nnod_4_surf, surf%nnod_4_surf, surf%node_on_sf,          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    max_int_point, maxtot_int_2d, int_start2, owe2d,              &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, jac_sf_grp%axj_sf,    &
     &    jac_sf_grp%an_sf, jac_sf_grp%an_sf,                           &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, nd, n_int, dxe_sf, vect_sf, ak_diff, coef, sk_v)
!
      end subroutine fem_sf_grp_skv_sgs_div_flux_p
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_sf_grp_skv_sgs_div_lin_p                           &
     &        (ele, surf, sf_grp, jac_sf_grp, jac_sf_grp_l, FEM_elens,  &
     &         igrp, k2, n_diff, n_int, i_filter,                       &
     &         dxe_sf, scalar_sf, ak_diff, sk_v)
!
      use fem_surf_skv_sgs_commute
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: n_diff, i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp%num_item)
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_sf_skv_sgs_vect_diff_p(np_smp, ele%numele, num_t_linear, &
     &    num_linear_sf, surf%nnod_4_surf, surf%node_on_sf,             &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    max_int_point, maxtot_int_2d, int_start2, owe2d,              &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, jac_sf_grp%axj_sf,    &
     &    jac_sf_grp_l%an_sf, jac_sf_grp%an_sf,                         &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, ione, n_diff, dxe_sf, scalar_sf,             &
     &    ak_diff, one, sk_v)
!
      end subroutine fem_sf_grp_skv_sgs_div_lin_p
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_grp_skv_sgs_velo_co_p                           &
     &        (ele, surf, sf_grp, jac_sf_grp, jac_sf_grp_l, FEM_elens,  &
     &         igrp, k2, n_int, i_filter, dxe_sf, scalar_sf, ak_diff,   &
     &         sk_v)
!
      use fem_surf_skv_sgs_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp%num_item)
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_sf_skv_sgs_grad_posi(np_smp, ele%numele, ele%nnod_4_ele, &
     &    surf%nnod_4_surf, num_linear_sf, surf%node_on_sf,             &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    max_int_point, maxtot_int_2d, int_start2, owe2d,              &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, jac_sf_grp%axj_sf,    &
     &    jac_sf_grp%an_sf, jac_sf_grp_l%an_sf,                         &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, dxe_sf, scalar_sf, ak_diff, one, sk_v)
!
      end subroutine fem_sf_grp_skv_sgs_velo_co_p
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_sf_grp_skv_poisson_sgs                             &
     &         (ele, surf, sf_grp, jac_3d_l, FEM_elens, igrp,           &
     &          k2, n_int, i_filter, phi_sf, ak_diff, sk_v)
!
      use fem_surf_skv_diffuse_sgs
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
!
      real (kind=kreal), intent(in) :: phi_sf(sf_grp%num_item)
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_surf_skv_poisson_sgs(np_smp, ele%numele, num_t_linear,   &
     &  num_t_linear, num_linear_sf, surf%node_on_sf,                   &
     &  sf_grp%num_item, sf_grp%item_sf_grp,                            &
     &  sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                      &
     &  max_int_point, maxtot_int_3d, int_start3, owe3d,                &
     &  jac_3d_l%ntot_int, jac_3d_l%xjac, jac_3d_l%dnx, jac_3d_l%dnx,   &
     &  FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM_elens%nele_filter_mom,                                      &
     &  FEM_elens%elen_ele%diff2%df_x2, FEM_elens%elen_ele%diff2%df_y2, &
     &  FEM_elens%elen_ele%diff2%df_z2, FEM_elens%elen_ele%diff2%df_xy, &
     &  FEM_elens%elen_ele%diff2%df_yz, FEM_elens%elen_ele%diff2%df_zx, &
     &  igrp, k2, n_int, ak_diff, phi_sf, sk_v)
!
      end subroutine fem_sf_grp_skv_poisson_sgs
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_grp_skv_diffusion_sgs                           &
     &         (ele, surf, sf_grp, jac_3d, FEM_elens, igrp, k2, n_int,  &
     &          i_filter, vect_sf, ak_diff, ak_d, nd_v, sk_v)
!
      use fem_surf_skv_diffuse_sgs
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter, nd_v
!
      real (kind=kreal), intent(in) :: vect_sf(sf_grp%num_item,3)
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_surf_skv_diffusion_sgs                                   &
     & (np_smp, ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,             &
     &  surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,             &
     &  sf_grp%item_sf_grp, sf_grp%num_grp_smp, sf_grp%istack_grp_smp,  &
     &  max_int_point, maxtot_int_3d, int_start3, owe3d,                &
     &  jac_3d%ntot_int, jac_3d%xjac, jac_3d%dnx, jac_3d%dnx,           &
     &  FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM_elens%nele_filter_mom,                                      &
     &  FEM_elens%elen_ele%diff2%df_x2, FEM_elens%elen_ele%diff2%df_y2, &
     &  FEM_elens%elen_ele%diff2%df_z2, FEM_elens%elen_ele%diff2%df_xy, &
     &  FEM_elens%elen_ele%diff2%df_yz, FEM_elens%elen_ele%diff2%df_zx, &
     &  igrp, k2, n_int, ak_diff, vect_sf, ak_d, nd_v, sk_v)
!
      end subroutine fem_sf_grp_skv_diffusion_sgs
!
!-----------------------------------------------------------------------
!
      end module fem_surf_skv_sgs_commute_t
