!fem_surf_skv_poisson_type.f90
!      module fem_surf_skv_poisson_type
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine fem_surf_skv_poisson_wall_t(igrp, k2, n_int,          &
!     &          ele, surf, sf_grp, jac_sf_grp, fem_sf_wk, fem_wk)
!      subroutine fem_surf_skv_poisson_sph_out_t(igrp, k2, n_int,       &
!     &          ele, surf, sf_grp, jac_sf_grp, fem_sf_wk, fem_wk)
!
!      subroutine fem_surf_skv_current_diffuse_t(igrp, k2, nd, n_int,   &
!     &          ele, surf, sf_grp, jac_sf_grp, fem_sf_wk, fem_wk)
!      subroutine fem_surf_skv_diffuse_type(igrp, k2, nd, n_int, ak_d,  &
!     &          ele, surf, sf_grp, jac_sf_grp, fem_sf_wk, fem_wk)
!
!      subroutine fem_surf_skv_trq_sph_out_t(igrp, k2, n_int, ak_d_velo,&
!     &          ele, surf, sf_grp, jac_sf_grp, fem_sf_wk, fem_wk)
!
!      subroutine fem_surf_skv_norm_grad_t(nmax_surf, nmax_ele_surf,    &
!     &          ngrp_sf, id_grp_sf, ist_surf, sf_apt, n_int, nd,       &
!     &          ak_d, ele, surf, sf_grp, jac_sf_grp, fem_wk)
!      subroutine fem_surf_skv_norm_poisson_t(nmax_surf, nmax_ele_surf, &
!     &          ngrp_sf, id_grp_sf, ist_surf, sf_apt, n_int,           &
!     &          ele, surf, sf_grp, jac_sf_grp, fem_wk)
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
      subroutine fem_surf_skv_poisson_wall_t(igrp, k2, n_int,           &
     &          ele, surf, sf_grp, jac_sf_grp, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_poisson
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      integer (kind = kint), intent(in) :: n_int, k2, igrp
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_surf_skv_poisson_wall                                    &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    surf%nnod_4_surf, surf%nnod_4_surf, surf%node_on_sf,          &
     &    sf_grp%num_item, sf_grp%item_sf_grp, sf_grp%num_grp_smp,      &
     &    sf_grp%istack_grp_smp, igrp, k2, n_int, jac_sf_grp%ntot_int,  &
     &    jac_sf_grp%xsf_sf, jac_sf_grp%an_sf, jac_sf_grp%an_sf,        &
     &    fem_sf_wk%vector_sf, fem_wk%sk6)
!
      end subroutine fem_surf_skv_poisson_wall_t
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_poisson_sph_out_t(igrp, k2, n_int,        &
     &          ele, surf, sf_grp, jac_sf_grp, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_poisson
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      integer (kind = kint), intent(in) :: n_int, k2, igrp
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_surf_skv_poisson_sph_out                                 &
     &   (np_smp, ele%numele, ele%nnod_4_ele,                           &
     &    num_linear_sf, num_linear_sf, surf%node_on_sf,                &
     &    sf_grp%num_item, sf_grp%item_sf_grp, sf_grp%num_grp_smp,      &
     &    sf_grp%istack_grp_smp, igrp, k2, n_int, jac_sf_grp%ntot_int,  &
     &    jac_sf_grp%xj_sf, jac_sf_grp%an_sf, jac_sf_grp%an_sf,         &
     &    fem_sf_wk%xe_sf, fem_sf_wk%vector_sf, fem_wk%sk6)
!
      end subroutine fem_surf_skv_poisson_sph_out_t
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_current_diffuse_t(igrp, k2, nd, n_int,    &
     &          ele, surf, sf_grp, jac_sf_grp, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_diffuse
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      integer (kind = kint), intent(in) :: igrp, n_int, k2, nd
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_surf_skv_current_diffuse                                 &
     &  (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,          &
     &   surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,            &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp, sf_grp%istack_grp_smp, &
     &   igrp, k2, nd, n_int, jac_sf_grp%ntot_int, jac_sf_grp%xj_sf,    &
     &   jac_sf_grp%an_sf, jac_sf_grp%an_sf,                            &
     &   fem_sf_wk%dxe_sf, fem_sf_wk%scalar_sf, fem_wk%sk6)
!
      end subroutine fem_surf_skv_current_diffuse_t
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_diffuse_type(igrp, k2, nd, n_int, ak_d,   &
     &          ele, surf, sf_grp, jac_sf_grp, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_diffuse
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      integer (kind = kint), intent(in) :: igrp, n_int, k2, nd
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_surf_skv_diffuse_term                                    &
     &  (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,          &
     &   surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,            &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp, sf_grp%istack_grp_smp, &
     &   igrp, k2, nd, n_int, jac_sf_grp%ntot_int, jac_sf_grp%xj_sf,    &
     &   jac_sf_grp%an_sf, jac_sf_grp%an_sf, ak_d,                      &
     &   fem_sf_wk%dxe_sf, fem_sf_wk%scalar_sf, fem_wk%sk6)
!
      end subroutine fem_surf_skv_diffuse_type
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_surf_skv_trq_sph_out_t(igrp, k2, n_int, ak_d_velo, &
     &          ele, surf, sf_grp, jac_sf_grp, fem_sf_wk, fem_wk)
!
      use fem_surf_skv_norm_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(work_finite_surface_mat), intent(in) :: fem_sf_wk
!
      integer (kind = kint), intent(in) :: igrp, k2, n_int
      real (kind=kreal), intent(in) :: ak_d_velo(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_surf_skv_trq_sph_out                                     &
     &  (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,          &
     &   surf%nnod_4_surf, surf%node_on_sf, sf_grp%num_item,            &
     &   sf_grp%item_sf_grp, sf_grp%num_grp_smp, sf_grp%istack_grp_smp, &
     &   igrp, k2, n_int, jac_sf_grp%ntot_int, jac_sf_grp%xj_sf,        &
     &   jac_sf_grp%an_sf, jac_sf_grp%an_sf, ak_d_velo,                 &
     &   fem_sf_wk%xe_sf, fem_sf_wk%vector_sf, fem_wk%sk6)
!
!
      end subroutine fem_surf_skv_trq_sph_out_t
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_norm_grad_t(nmax_surf, nmax_ele_surf,     &
     &          ngrp_sf, id_grp_sf, ist_surf, sf_apt, n_int, nd,        &
     &          ak_d, ele, surf, sf_grp, jac_sf_grp, fem_wk)
!
      use fem_surf_skv_norm_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      integer (kind = kint), intent(in) :: nmax_surf, ngrp_sf
      integer (kind = kint), intent(in) :: nmax_ele_surf
      integer (kind = kint), intent(in) :: id_grp_sf(nmax_surf)
      integer (kind = kint), intent(in) :: ist_surf(0:nmax_surf)
!
      integer (kind = kint), intent(in) :: n_int, nd
!
      real (kind=kreal), intent(in) :: sf_apt(nmax_ele_surf)
      real (kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_surf_skv_norm_grad_pg                                    &
     &   (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,         &
     &    surf%node_on_sf, sf_grp%num_grp, sf_grp%num_item,             &
     &    sf_grp%istack_grp, sf_grp%item_sf_grp, sf_grp%num_grp_smp,    &
     &    sf_grp%istack_grp_smp, nmax_surf, nmax_ele_surf,              &
     &    ngrp_sf, id_grp_sf, ist_surf, sf_apt, n_int, nd,              &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%an_sf,      &
     &    ak_d, fem_wk%sk6)
!
      end subroutine fem_surf_skv_norm_grad_t
!
!-----------------------------------------------------------------------
!
      subroutine fem_surf_skv_norm_poisson_t(nmax_surf, nmax_ele_surf,  &
     &          ngrp_sf, id_grp_sf, ist_surf, sf_apt, n_int,            &
     &          ele, surf, sf_grp, jac_sf_grp, fem_wk)
!
      use fem_surf_skv_norm_grad
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(jacobians_2d), intent(in) :: jac_sf_grp
      integer (kind = kint), intent(in) :: nmax_surf, ngrp_sf
      integer (kind = kint), intent(in) :: nmax_ele_surf
      integer (kind = kint), intent(in) :: id_grp_sf(nmax_surf)
      integer (kind = kint), intent(in) :: ist_surf(0:nmax_surf)
!
      integer (kind = kint), intent(in) :: n_int
!
      real (kind=kreal), intent(in) :: sf_apt(nmax_ele_surf)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      call fem_surf_skv_norm_poisson                                    &
     &   (np_smp, ele%numele, ele%nnod_4_ele, surf%nnod_4_surf,         &
     &    surf%node_on_sf, sf_grp%num_grp, sf_grp%num_item,             &
     &    sf_grp%istack_grp, sf_grp%item_sf_grp, sf_grp%num_grp_smp,    &
     &    sf_grp%istack_grp_smp, nmax_surf, nmax_ele_surf,              &
     &    ngrp_sf, id_grp_sf, ist_surf, sf_apt, n_int,                  &
     &    jac_sf_grp%ntot_int, jac_sf_grp%xj_sf, jac_sf_grp%an_sf,      &
     &    fem_wk%sk6)
!
      end subroutine fem_surf_skv_norm_poisson_t
!
!-----------------------------------------------------------------------
!
      end module fem_surf_skv_poisson_type
