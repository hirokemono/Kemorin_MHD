!fem_surf_skv_poisson_type.f90
!      module fem_surf_skv_poisson_type
!
!      Written by H. Matsui on Sep., 2005
!
!
!!      subroutine fem_surf_skv_poisson_wall                            &
!!     &         (ele, surf, sf_grp, jac_sf_grp_l, igrp, k2, n_int,     &
!!     &          vect_sf, sk_v)
!!      subroutine fem_surf_skv_poisson_sph_out                         &
!!     &         (ele, surf, sf_grp, jac_sf_grp_l, igrp, k2, n_int,     &
!!     &          xe_sf, vect_sf, sk_v)
!!
!!      subroutine fem_surf_skv_current_by_vecp                         &
!!     &          (ele, surf, sf_grp, jac_sf_grp, igrp, k2, nd, n_int,  &
!!     &           dxe_sf, scalar_sf, sk_v)
!!      subroutine fem_surf_skv_diffuse_galerkin                        &
!!     &         (ele, surf, sf_grp, jac_sf_grp, igrp, k2, nd, n_int,   &
!!     &          dxe_sf, scalar_sf, ak_d, sk_v)
!!
!!      subroutine fem_surf_skv_trq_sph_out                             &
!!     &         (ele, surf, sf_grp, jac_sf_grp, igrp, k2, n_int,       &
!!     &          ak_d_velo, xe_sf, vect_sf, sk_v)
!!
!!      subroutine fem_surf_skv_norm_grad_galerkin                      &
!!     &         (ele, surf, sf_grp, jac_sf_grp, grad_sf,               &
!!     &          n_int, nd, ak_d, sk_v)
!!      subroutine fem_surf_skv_norm_poisson_pg                         &
!!     &         (ele, surf, sf_grp, jac_sf_grp, grad_sf, n_int, sk_v)
!
      module fem_surf_skv_poisson_type
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_jacobian_2d
      use t_surface_bc_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_poisson_wall                              &
     &         (ele, surf, sf_grp, jac_sf_grp_l, igrp, k2, n_int,       &
     &          vect_sf, sk_v)
!
      use fem_surf_skv_poisson
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      integer (kind = kint), intent(in) :: n_int, k2, igrp
      real (kind=kreal), intent(in) :: vect_sf(sf_grp%num_item,3)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_surf_skv_poisson_wall_pg                                 &
     &   (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,         &
     &    surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,           &
     &    sf_grp%item_sf_grp, sf_grp%num_grp_smp,                       &
     &    sf_grp%istack_grp_smp, igrp, k2, n_int,                       &
     &    jac_sf_grp_l%ntot_int, jac_sf_grp_l%xsf_sf,                   &
     &    jac_sf_grp_l%an_sf, jac_sf_grp_l%an_sf, vect_sf, sk_v)
!
      end subroutine fem_surf_skv_poisson_wall
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_poisson_sph_out                           &
     &         (ele, surf, sf_grp, jac_sf_grp_l, igrp, k2, n_int,       &
     &          xe_sf, vect_sf, sk_v)
!
      use fem_surf_skv_poisson
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp_l
      integer (kind = kint), intent(in) :: n_int, k2, igrp
!
      real (kind=kreal), intent(in)                                     &
     &                  :: xe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real (kind=kreal), intent(in) :: vect_sf(sf_grp%num_item,3)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_surf_skv_poisson_sph_out_pg                              &
     &   (np_smp, ele%numele,  ele%nnod_4_ele, surf%nnod_4_surf,        &
     &    surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,           &
     &    sf_grp%item_sf_grp, sf_grp%num_grp_smp,                       &
     &    sf_grp%istack_grp_smp, igrp, k2, n_int,                       &
     &    jac_sf_grp_l%ntot_int, jac_sf_grp_l%xj_sf,                    &
     &    jac_sf_grp_l%an_sf, jac_sf_grp_l%an_sf, xe_sf, vect_sf, sk_v)
!
      end subroutine fem_surf_skv_poisson_sph_out
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_current_by_vecp                           &
     &          (ele, surf, sf_grp, jac_sf_grp, igrp, k2, nd, n_int,    &
     &           dxe_sf, scalar_sf, sk_v)
!
      use fem_surf_skv_diffuse
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      integer (kind = kint), intent(in) :: igrp, n_int, k2, nd
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp%num_item)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_surf_skv_current_diffuse                                 &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    surf%nnod_4_surf, surf%nnod_4_surf, surf%node_on_sf,          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, nd, n_int, jac_sf_grp%ntot_int, jac_sf_grp%xj_sf,   &
     &    jac_sf_grp%an_sf, jac_sf_grp%an_sf, dxe_sf, scalar_sf, sk_v)
!
      end subroutine fem_surf_skv_current_by_vecp
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_diffuse_galerkin                          &
     &         (ele, surf, sf_grp, jac_sf_grp, igrp, k2, nd, n_int,     &
     &          dxe_sf, scalar_sf, ak_d, sk_v)
!
      use fem_surf_skv_diffuse
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      integer (kind = kint), intent(in) :: igrp, n_int, k2, nd
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp%num_item)
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_surf_skv_diffuse_term                                    &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    surf%nnod_4_surf, surf%nnod_4_surf, surf%node_on_sf,          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, nd, n_int, jac_sf_grp%ntot_int, jac_sf_grp%xj_sf,   &
     &    jac_sf_grp%an_sf, jac_sf_grp%an_sf, ak_d, dxe_sf, scalar_sf,  &
     &    sk_v)
!
      end subroutine fem_surf_skv_diffuse_galerkin
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_trq_sph_out                               &
     &         (ele, surf, sf_grp, jac_sf_grp, igrp, k2, n_int,         &
     &          ak_d_velo, xe_sf, vect_sf, sk_v)
!
      use fem_surf_skv_norm_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      integer (kind = kint), intent(in) :: igrp, k2, n_int
!
      real (kind=kreal), intent(in)                                     &
     &                  :: xe_sf(sf_grp%num_item,4,surf%nnod_4_surf)
      real (kind=kreal), intent(in) :: vect_sf(sf_grp%num_item,3)
      real (kind=kreal), intent(in) :: ak_d_velo(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_surf_skv_trq_sph_out_pg                                  &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    surf%nnod_4_surf, surf%nnod_4_surf, surf%node_on_sf,          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, n_int, jac_sf_grp%ntot_int, jac_sf_grp%xj_sf,       &
     &    jac_sf_grp%an_sf, jac_sf_grp%an_sf,                           &
     &    ak_d_velo, xe_sf, vect_sf, sk_v)
!
!
      end subroutine fem_surf_skv_trq_sph_out
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_surf_skv_norm_grad_galerkin                        &
     &         (ele, surf, sf_grp, jac_sf_grp, grad_sf,                 &
     &          n_int, nd, ak_d, sk_v)
!
      use fem_surf_skv_norm_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(scaler_surf_flux_bc_type), intent(in) :: grad_sf
!
      integer (kind = kint), intent(in) :: n_int, nd
!
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_surf_skv_norm_grad_pg(np_smp, ele%numele,                &
     &   ele%nnod_4_ele, surf%nnod_4_surf, surf%node_on_sf,             &
     &   sf_grp%num_grp, sf_grp%num_item, sf_grp%istack_grp,            &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp, sf_grp%istack_grp_smp, &
     &   grad_sf%ngrp_sf_fix_fx, grad_sf%nitem_sf_fix_fx,               &
     &   grad_sf%ngrp_sf_fix_fx, grad_sf%id_grp_sf_fix_fx,              &
     &   grad_sf%ist_ele_sf_fix_fx, grad_sf%sf_apt_fix_fx, n_int, nd,   &
     &   jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%an_sf,       &
     &   ak_d, sk_v)
!
      end subroutine fem_surf_skv_norm_grad_galerkin
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_norm_poisson_pg                           &
     &         (ele, surf, sf_grp, jac_sf_grp, grad_sf, n_int, sk_v)
!
      use fem_surf_skv_norm_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(scaler_surf_flux_bc_type), intent(in) :: grad_sf
!
      integer (kind = kint), intent(in) :: n_int
!
      real (kind=kreal), intent(inout)                                  &
     &            :: sk_v(ele%numele,n_sym_tensor,ele%nnod_4_ele)
!
!
      call fem_surf_skv_norm_poisson(np_smp, ele%numele,                &
     &   ele%nnod_4_ele, surf%nnod_4_surf, surf%node_on_sf,             &
     &   sf_grp%num_grp, sf_grp%num_item, sf_grp%istack_grp,            &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp, sf_grp%istack_grp_smp, &
     &   grad_sf%ngrp_sf_fix_fx, grad_sf%nitem_sf_fix_fx,               &
     &   grad_sf%ngrp_sf_fix_fx, grad_sf%id_grp_sf_fix_fx,              &
     &   grad_sf%ist_ele_sf_fix_fx, grad_sf%sf_apt_fix_fx, n_int,       &
     &   jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%an_sf, sk_v)
!
      end subroutine fem_surf_skv_norm_poisson_pg
!
!-----------------------------------------------------------------------
!
      end module fem_surf_skv_poisson_type
