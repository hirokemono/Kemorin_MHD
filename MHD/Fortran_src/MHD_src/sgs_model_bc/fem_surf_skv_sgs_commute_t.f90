!fem_surf_skv_sgs_commute_t.f90
!      module fem_surf_skv_sgs_commute_t
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine fem_sf_skv_sgs_commute_err_pt(igrp, k2, nd, n_int,    &
!     &          i_filter, n_diff, ele, surf, sf_grp, jac_sf_grp,       &
!     &          FEM_elens, fem_sf_wk, fem_wk)
!      subroutine fem_sf_skv_grad_commute_pt(igrp, k2, n_int, i_filter, &
!     &          ak_diff, ele, surf, sf_grp, jac_sf_grp,                &
!     &          FEM_elens, fem_sf_wk, fem_wk)
!      subroutine fem_sf_skv_div_flux_commute_pt(igrp, k2, nd, n_int,   &
!     &          i_filter, ele, surf, sf_grp, jac_sf_grp, FEM_elens,    &
!     &          fem_sf_wk, fem_wk)
!
!      subroutine fem_sf_skv_sgs_vect_diff_pt(igrp, k2, nd, n_int,      &
!     &          i_filter, n_diff, ak_diff, coef, ele, surf,            &
!     &          sf_grp, jac_sf_grp, FEM_elens, fem_sf_wk, fem_wk)
!      subroutine fem_sf_skv_sgs_grad_pt(igrp, k2, n_int, i_filter,     &
!     &          ak_diff, coef, ele, surf, sf_grp, jac_sf_grp,          &
!     &          FEM_elens, fem_sf_wk, fem_wk)
!      subroutine fem_sf_skv_sgs_div_pt(igrp, k2, nd, n_int, i_filter,  &
!     &          ak_diff, coef, ele, surf, sf_grp, jac_sf_grp,          &
!     &          FEM_elens, fem_sf_wk, fem_wk)
!
!      subroutine fem_sf_skv_sgs_div_linear_pt(igrp, k2, n_diff, n_int, &
!     &          i_filter, ak_diff, ele, surf, sf_grp, jac_sf_grp,      &
!     &          jac_sf_grp_l, FEM_elens, fem_sf_wk, fem_wk)
!      subroutine fem_sf_skv_sgs_velo_co_pt(igrp, k2, n_int, i_filter,  &
!     &          ak_diff, ele, surf, sf_grp, jac_sf_grp,                &
!     &          jac_sf_grp_l, FEM_elens, fem_sf_wk, fem_wk)
!
!      subroutine fem_surf_skv_poisson_sgs_t(igrp, k2, n_int, i_filter, &
!     &          ak_diff, ele, surf, sf_grp, jac_3d_l, FEM_elens,       &
!     &          fem_sf_wk, fem_wk)
!      subroutine fem_surf_skv_diffusion_sgs_t(igrp, k2, n_int,         &
!     &          i_filter, ak_diff, ak_d, nd_v, ele, surf, sf_grp,      &
!     &          jac_3d, FEM_elens, fem_sf_wk, fem_wk)
!
      module fem_surf_skv_sgs_commute_t
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobians
      use t_jacobian_2d
      use t_filter_elength
      use t_finite_surface_mat
      use t_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_commute_err_pt(igrp, k2, nd, n_int,     &
     &          i_filter, n_diff, ele, surf, sf_grp, jac_sf_grp,        &
     &          FEM_elens, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_sgs_commute
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, n_diff, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_sf_skv_sgs_commute_err_p                                 &
     &   (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,         &
     &    surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,           &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp,                        &
     &   sf_grp%istack_grp_smp, jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, &
     &   jac_sf_grp%axj_sf, jac_sf_grp%an_sf, jac_sf_grp%an_sf,         &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, nd, n_diff, fem_sf_wk%dxe_sf,                &
     &    fem_sf_wk%scalar_sf, fem_wk%sk6)
!
      end subroutine fem_sf_skv_sgs_commute_err_pt
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_grad_commute_pt(igrp, k2, n_int, i_filter,  &
     &          ele, surf, sf_grp, jac_sf_grp, FEM_elens,               &
     &          fem_sf_wk, fem_wk)
!
      use fem_surf_skv_sgs_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_sf_skv_grad_commute_posi                                 &
     &   (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,         &
     &    surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,           &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp,                        &
     &   sf_grp%istack_grp_smp, jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, &
     &   jac_sf_grp%axj_sf, jac_sf_grp%an_sf,  jac_sf_grp%an_sf,        &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, fem_sf_wk%dxe_sf, fem_sf_wk%scalar_sf,       &
     &    fem_wk%sk6)
!
      end subroutine fem_sf_skv_grad_commute_pt
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_div_flux_commute_pt(igrp, k2, nd, n_int,    &
     &          i_filter, ele, surf, sf_grp, jac_sf_grp, FEM_elens,     &
     &          fem_sf_wk, fem_wk)
!
      use fem_surf_skv_sgs_div
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_sf_skv_div_flux_commute_p                                &
     &   (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,         &
     &    surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,           &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp,                        &
     &   sf_grp%istack_grp_smp, jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, &
     &   jac_sf_grp%axj_sf, jac_sf_grp%an_sf, jac_sf_grp%an_sf,         &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, nd, n_int, fem_sf_wk%dxe_sf, fem_sf_wk%vector_sf,   &
     &    fem_wk%sk6)
!
      end subroutine fem_sf_skv_div_flux_commute_pt
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_vect_diff_pt(igrp, k2, nd, n_int,       &
     &          i_filter, n_diff, ak_diff, coef, ele, surf,             &
     &          sf_grp, jac_sf_grp, FEM_elens, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_sgs_commute
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, n_diff, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_sf_skv_sgs_vect_diff_p                                   &
     &  (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,          &
     &   surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,            &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp,                        &
     &   sf_grp%istack_grp_smp, jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, &
     &   jac_sf_grp%axj_sf, jac_sf_grp%an_sf, jac_sf_grp%an_sf,         &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, nd, n_diff, fem_sf_wk%dxe_sf,                &
     &    fem_sf_wk%scalar_sf, ak_diff, coef, fem_wk%sk6)
!
      end subroutine fem_sf_skv_sgs_vect_diff_pt
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_grad_pt(igrp, k2, n_int, i_filter,      &
     &          ak_diff, coef, ele, surf, sf_grp, jac_sf_grp,           &
     &          FEM_elens, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_sgs_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_sf_skv_sgs_grad_posi                                     &
     &   (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,         &
     &    surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,           &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp,                        &
     &   sf_grp%istack_grp_smp, jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, &
     &   jac_sf_grp%axj_sf, jac_sf_grp%an_sf, jac_sf_grp%an_sf,         &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, fem_sf_wk%dxe_sf, fem_sf_wk%scalar_sf,       &
     &    ak_diff, coef, fem_wk%sk6)
!
      end subroutine fem_sf_skv_sgs_grad_pt
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_div_pt(igrp, k2, nd, n_int, i_filter,   &
     &          ak_diff, coef, ele, surf, sf_grp, jac_sf_grp,           &
     &          FEM_elens, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_sgs_div
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: nd, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_sf_skv_sgs_div_flux_posi                                 &
     &   (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,         &
     &    surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,           &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp,                        &
     &   sf_grp%istack_grp_smp, jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, &
     &   jac_sf_grp%axj_sf, jac_sf_grp%an_sf, jac_sf_grp%an_sf,         &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, nd, n_int, fem_sf_wk%dxe_sf, fem_sf_wk%vector_sf,   &
     &    ak_diff, coef, fem_wk%sk6)
!
      end subroutine fem_sf_skv_sgs_div_pt
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_div_linear_pt(igrp, k2, n_diff, n_int,  &
     &          i_filter, ak_diff, ele, surf, sf_grp, jac_sf_grp,       &
     &          jac_sf_grp_l, FEM_elens, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_sgs_commute
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: n_diff, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_sf_skv_sgs_vect_diff_p                                   &
     &   (np_smp, ele%numele, num_t_linear, num_linear_sf,              &
     &    surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,           &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp,                        &
     &   sf_grp%istack_grp_smp, jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, &
     &   jac_sf_grp%axj_sf, jac_sf_grp_l%an_sf, jac_sf_grp%an_sf,       &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, ione, n_diff, fem_sf_wk%dxe_sf,              &
     &    fem_sf_wk%scalar_sf, ak_diff, one, fem_wk%sk6)
!
      end subroutine fem_sf_skv_sgs_div_linear_pt
!
!-----------------------------------------------------------------------
!
      subroutine fem_sf_skv_sgs_velo_co_pt(igrp, k2, n_int, i_filter,   &
     &          ak_diff, ele, surf, sf_grp, jac_sf_grp,                 &
     &          jac_sf_grp_l, FEM_elens, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_sgs_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp
      type(jacobians_surf_grp), intent(in) :: jac_sf_grp_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_sf_skv_sgs_grad_posi                                     &
     &   (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,         &
     &    num_linear_sf, surf%node_on_sf, sf_grp%num_item,              &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp,                        &
     &   sf_grp%istack_grp_smp, jac_sf_grp%ntot_int, jac_sf_grp%xsf_sf, &
     &   jac_sf_grp%axj_sf, jac_sf_grp%an_sf, jac_sf_grp_l%an_sf,       &
     &    FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                &
     &    FEM_elens%nele_filter_mom,                                    &
     &    FEM_elens%elen_ele%diff%df_x2, FEM_elens%elen_ele%diff%df_y2, &
     &    FEM_elens%elen_ele%diff%df_z2, FEM_elens%elen_ele%diff%df_xy, &
     &    FEM_elens%elen_ele%diff%df_yz, FEM_elens%elen_ele%diff%df_zx, &
     &    igrp, k2, n_int, fem_sf_wk%dxe_sf, fem_sf_wk%scalar_sf,       &
     &    ak_diff, one, fem_wk%sk6)
!
      end subroutine fem_sf_skv_sgs_velo_co_pt
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_poisson_sgs_t(igrp, k2, n_int, i_filter,  &
     &          ak_diff, ele, surf, sf_grp, jac_3d_l, FEM_elens,        &
     &          fem_sf_wk, fem_wk)
!
      use fem_surf_skv_diffuse_sgs
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_surf_skv_poisson_sgs                                     &
     &   (np_smp, ele%numele, num_t_linear, num_t_linear,               &
     &    num_linear_sf, surf%node_on_sf, sf_grp%num_item,              &
     &  sf_grp%item_sf_grp, sf_grp%num_grp_smp, sf_grp%istack_grp_smp,  &
     &  jac_3d_l%ntot_int, jac_3d_l%xjac, jac_3d_l%dnx, jac_3d_l%dnx,   &
     &  FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM_elens%nele_filter_mom,                                      &
     &  FEM_elens%elen_ele%diff2%df_x2, FEM_elens%elen_ele%diff2%df_y2, &
     &  FEM_elens%elen_ele%diff2%df_z2, FEM_elens%elen_ele%diff2%df_xy, &
     &  FEM_elens%elen_ele%diff2%df_yz, FEM_elens%elen_ele%diff2%df_zx, &
     &  igrp, k2, n_int, ak_diff,  fem_sf_wk%vector_sf, fem_wk%sk6)
!
      end subroutine fem_surf_skv_poisson_sgs_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_diffusion_sgs_t(igrp, k2, n_int,          &
     &          i_filter, ak_diff, ak_d, nd_v, ele, surf, sf_grp,       &
     &          jac_3d, FEM_elens, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_diffuse_sgs
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      integer(kind = kint), intent(in) :: igrp, k2, n_int
      integer(kind = kint), intent(in) :: i_filter, nd_v
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_surf_skv_diffusion_sgs                                   &
     &   (np_smp, ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,           &
     &    surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,           &
     &  sf_grp%item_sf_grp, sf_grp%num_grp_smp, sf_grp%istack_grp_smp,  &
     &  jac_3d%ntot_int, jac_3d%xjac,  jac_3d%dnx, jac_3d%dnx,          &
     &  FEM_elens%filter_conf%xmom_1d_org(i_filter,2),                  &
     &  FEM_elens%nele_filter_mom,                                      &
     &  FEM_elens%elen_ele%diff2%df_x2, FEM_elens%elen_ele%diff2%df_y2, &
     &  FEM_elens%elen_ele%diff2%df_z2, FEM_elens%elen_ele%diff2%df_xy, &
     &  FEM_elens%elen_ele%diff2%df_yz, FEM_elens%elen_ele%diff2%df_zx, &
     &  igrp, k2, n_int, ak_diff, fem_sf_wk%vector_sf, ak_d,            &
     &  nd_v, fem_wk%sk6)
!
      end subroutine fem_surf_skv_diffusion_sgs_t
!
!-----------------------------------------------------------------------
!
      end module fem_surf_skv_sgs_commute_t
