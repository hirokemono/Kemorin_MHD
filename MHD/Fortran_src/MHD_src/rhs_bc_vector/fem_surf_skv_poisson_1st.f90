!fem_surf_skv_poisson_1st.f90
!      module fem_surf_skv_poisson_1st
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine fem_surf_skv_poisson_wall_1(igrp, k2, n_int,          &
!     &          vect_sf, sk_v)
!      subroutine fem_surf_skv_poisson_sph_out_1(igrp, k2, n_int,       &
!     &          xe_sf, vect_sf, sk_v)
!
!      subroutine fem_surf_skv_current_diffuse_1(igrp, k2, nd, n_int,   &
!     &          dxe_sf, scalar_sf, sk_v)
!      subroutine fem_surf_skv_diffuse_1st(igrp, k2, nd, n_int,         &
!     &          dxe_sf, scalar_sf, ak_d, sk_v)
!
!      subroutine fem_surf_skv_trq_sph_out_1(igrp, k2, n_int,           &
!     &          ak_d_velo, xe_sf, vect_sf, sk_v)
!
!      subroutine fem_surf_skv_norm_grad_1(nmax_surf, nmax_ele_surf,    &
!     &          ngrp_sf, id_grp_sf, ist_surf, sf_apt, n_int, nd,       &
!     &          ak_d, sk_v)
!      subroutine fem_surf_skv_norm_poisson_1(nmax_surf, nmax_ele_surf, &
!     &          ngrp_sf, id_grp_sf, ist_surf, sf_apt, n_int, sk_v)
!
      module fem_surf_skv_poisson_1st
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
      use m_geometry_parameter
      use m_surface_group
      use m_jacobian_sf_grp
      use m_phys_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_poisson_wall_1(igrp, k2, n_int,           &
     &          vect_sf, sk_v)
!
      use fem_surf_skv_poisson
!
      integer (kind = kint), intent(in) :: n_int, k2, igrp
      real (kind=kreal), intent(in) :: vect_sf(sf_grp1%num_item,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_surf_skv_poisson_wall                                    &
     &   (np_smp, numele,  nnod_4_ele, nnod_4_surf,                     &
     &    nnod_4_surf, node_on_sf, sf_grp1%num_item, surf_item,         &
     &    num_surf_smp, isurf_grp_smp_stack, igrp, k2, n_int,           &
     &    jac1_sf_grp_2d_l%ntot_int, jac1_sf_grp_2d_l%xsf_sf,           &
     &    jac1_sf_grp_2d_l%an_sf, jac1_sf_grp_2d_l%an_sf,               &
     &    vect_sf, sk_v)
!
      end subroutine fem_surf_skv_poisson_wall_1
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_poisson_sph_out_1(igrp, k2, n_int,        &
     &          xe_sf, vect_sf, sk_v)
!
      use fem_surf_skv_poisson
!
      integer (kind = kint), intent(in) :: n_int, k2, igrp
!
      real (kind=kreal), intent(in)                                     &
     &                  :: xe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real (kind=kreal), intent(in) :: vect_sf(sf_grp1%num_item,3)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_surf_skv_poisson_sph_out                                 &
     &   (np_smp, numele,  nnod_4_ele, nnod_4_surf,                     &
     &    nnod_4_surf, node_on_sf, sf_grp1%num_item, surf_item,         &
     &    num_surf_smp, isurf_grp_smp_stack, igrp, k2, n_int,           &
     &    jac1_sf_grp_2d_l%ntot_int, jac1_sf_grp_2d_l%xj_sf,            &
     &    jac1_sf_grp_2d_l%an_sf, jac1_sf_grp_2d_l%an_sf, xe_sf,        &
     &    vect_sf, sk_v)
!
      end subroutine fem_surf_skv_poisson_sph_out_1
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_current_diffuse_1(igrp, k2, nd, n_int,    &
     &          dxe_sf, scalar_sf, sk_v)
!
      use fem_surf_skv_diffuse
!
      integer (kind = kint), intent(in) :: igrp, n_int, k2, nd
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp1%num_item)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_surf_skv_current_diffuse                                 &
     &   (np_smp, numele, nnod_4_ele, nnod_4_surf, nnod_4_surf,         &
     &    node_on_sf, sf_grp1%num_item, surf_item, num_surf_smp,        &
     &    isurf_grp_smp_stack, igrp, k2, nd, n_int,                     &
     &    jac1_sf_grp_2d_q%ntot_int, jac1_sf_grp_2d_q%xj_sf,            &
     &    jac1_sf_grp_2d_q%an_sf, jac1_sf_grp_2d_q%an_sf,               &
     &    dxe_sf, scalar_sf, sk_v)
!
      end subroutine fem_surf_skv_current_diffuse_1
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_diffuse_1st(igrp, k2, nd, n_int,          &
     &          dxe_sf, scalar_sf, ak_d, sk_v)
!
      use fem_surf_skv_diffuse
!
      integer (kind = kint), intent(in) :: igrp, n_int, k2, nd
!
      real (kind=kreal), intent(in)                                     &
     &                  :: dxe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real (kind=kreal), intent(in) :: scalar_sf(sf_grp1%num_item)
      real (kind=kreal), intent(in) :: ak_d(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_surf_skv_diffuse_term                                    &
     &   (np_smp, numele, nnod_4_ele,  nnod_4_surf, nnod_4_surf,        &
     &    node_on_sf, sf_grp1%num_item, surf_item, num_surf_smp,        &
     &    isurf_grp_smp_stack, igrp, k2, nd, n_int,                     &
     &    jac1_sf_grp_2d_q%ntot_int, jac1_sf_grp_2d_q%xj_sf,            &
     &    jac1_sf_grp_2d_q%an_sf, jac1_sf_grp_2d_q%an_sf,               &
     &    ak_d, dxe_sf, scalar_sf, sk_v)
!
      end subroutine fem_surf_skv_diffuse_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_trq_sph_out_1(igrp, k2, n_int,            &
     &          ak_d_velo, xe_sf, vect_sf, sk_v)
!
      use fem_surf_skv_norm_grad
!
      integer (kind = kint), intent(in) :: igrp, k2, n_int
!
      real (kind=kreal), intent(in)                                     &
     &                  :: xe_sf(sf_grp1%num_item,4,nnod_4_surf)
      real (kind=kreal), intent(in) :: vect_sf(sf_grp1%num_item,3)
      real (kind=kreal), intent(in) :: ak_d_velo(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_surf_skv_trq_sph_out                                     &
     &   (np_smp, numele, nnod_4_ele, nnod_4_surf, nnod_4_surf,         &
     &    node_on_sf, sf_grp1%num_item, surf_item, num_surf_smp,        &
     &    isurf_grp_smp_stack, igrp, k2, n_int,                         &
     &    jac1_sf_grp_2d_q%ntot_int, jac1_sf_grp_2d_q%xj_sf,            &
     &    jac1_sf_grp_2d_q%an_sf, jac1_sf_grp_2d_q%an_sf,               &
     &    ak_d_velo, xe_sf, vect_sf, sk_v)
!
!
      end subroutine fem_surf_skv_trq_sph_out_1
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_surf_skv_norm_grad_1(nmax_surf, nmax_ele_surf,     &
     &          ngrp_sf, id_grp_sf, ist_surf, sf_apt, n_int, nd,        &
     &          ak_d, sk_v)
!
      use fem_surf_skv_norm_grad
!
      integer (kind = kint), intent(in) :: nmax_surf, ngrp_sf
      integer (kind = kint), intent(in) :: nmax_ele_surf
      integer (kind = kint), intent(in) :: id_grp_sf(nmax_surf)
      integer (kind = kint), intent(in) :: ist_surf(0:nmax_surf)
!
      integer (kind = kint), intent(in) :: n_int, nd
!
      real (kind=kreal), intent(in) :: sf_apt(nmax_ele_surf)
      real (kind=kreal), intent(in) :: ak_d(numele)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_surf_skv_norm_grad_pg(np_smp, numele, nnod_4_ele,        &
     &    nnod_4_surf, node_on_sf, sf_grp1%num_grp, sf_grp1%num_item,   &
     &    surf_istack, surf_item, num_surf_smp, isurf_grp_smp_stack,    &
     &    nmax_surf, nmax_ele_surf, ngrp_sf, id_grp_sf, ist_surf,       &
     &    sf_apt, n_int, nd, jac1_sf_grp_2d_q%ntot_int,                 &
     &    jac1_sf_grp_2d_q%xj_sf, jac1_sf_grp_2d_q%an_sf,               &
     &    ak_d, sk_v)
!
      end subroutine fem_surf_skv_norm_grad_1
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_norm_poisson_1(nmax_surf, nmax_ele_surf,  &
     &          ngrp_sf, id_grp_sf, ist_surf, sf_apt, n_int, sk_v)
!
      use fem_surf_skv_norm_grad
!
      integer (kind = kint), intent(in) :: nmax_surf, ngrp_sf
      integer (kind = kint), intent(in) :: nmax_ele_surf
      integer (kind = kint), intent(in) :: id_grp_sf(nmax_surf)
      integer (kind = kint), intent(in) :: ist_surf(0:nmax_surf)
!
      integer (kind = kint), intent(in) :: n_int
!
      real (kind=kreal), intent(in) :: sf_apt(nmax_ele_surf)
!
      real (kind=kreal), intent(inout)                                  &
     &                  :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
!
      call fem_surf_skv_norm_poisson(np_smp, numele, nnod_4_ele,        &
     &    nnod_4_surf, node_on_sf, sf_grp1%num_grp, sf_grp1%num_item,   &
     &    surf_istack, surf_item, num_surf_smp, isurf_grp_smp_stack,    &
     &    nmax_surf, nmax_ele_surf, ngrp_sf, id_grp_sf, ist_surf,       &
     &    sf_apt, n_int, jac1_sf_grp_2d_q%ntot_int,                     &
     &    jac1_sf_grp_2d_q%xj_sf, jac1_sf_grp_2d_q%an_sf, sk_v)
!
      end subroutine fem_surf_skv_norm_poisson_1
!
!-----------------------------------------------------------------------
!
      end module fem_surf_skv_poisson_1st
