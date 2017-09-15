!int_surf_ave_fluxes_1sgrp.f90
!      module int_surf_ave_fluxes_1sgrp
!
!     Written by H. Matsui on Aug., 2007
!     Modified by H. Matsui on Nov., 2008
!
!!      subroutine s_int_surf_area_1sgrp                                &
!!     &         (ele, surf, g_FEM, jac_2d_l, jac_2d_q,                 &
!!     &          num_int, num_sgrp, isurf_grp, area)
!!
!!      subroutine s_int_surf_ave_1sgrp                                 &
!!     &         (node, ele, surf, g_FEM, jac_2d_l, jac_2d_q, num_int,  &
!!     &          num_sgrp, isurf_grp, istack_sf_grp_smp, d1_nod, ave_l)
!!
!!      subroutine s_int_vec_flux_1sgrp                                 &
!!     &         (node, ele, surf, g_FEM, jac_2d_l, jac_2d_q,           &
!!     &          num_int, num_sgrp, isurf_grp, istack_sf_grp_smp,      &
!!     &          d1_nod, flux)
!!      subroutine s_int_vec_total_flux_1sgrp                           &
!!     &         (node, ele, surf, g_FEM, jac_2d_l, jac_2d_q,           &
!!     &          num_int, num_sgrp, isurf_grp, istack_sf_grp_smp,      &
!!     &          d1_nod, flux_l)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_2d), intent(in) :: jac_2d_l
!!        type(jacobians_2d), intent(in) :: jac_2d_q
!
      module int_surf_ave_fluxes_1sgrp
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_surface_data
      use t_fem_gauss_int_coefs
      use t_jacobians
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_int_surf_area_1sgrp                                  &
     &         (ele, surf, g_FEM, jac_2d_l, jac_2d_q,                   &
     &          num_int, num_sgrp, isurf_grp, area)
!
      use int_area_normal_4_surface
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_2d_l
      type(jacobians_2d), intent(in) :: jac_2d_q
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_sgrp
      integer (kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
!
      real(kind = kreal), intent(inout) :: area
!
!
      if (ele%nnod_4_ele .eq. num_t_quad) then
        call int_surf_area_1_surf_grp(ele%numele, surf%numsurf,         &
     &      surf%isf_4_ele, ele%interior_ele, g_FEM%max_int_point,      &
     &      g_FEM%maxtot_int_2d, g_FEM%int_start2, g_FEM%owe2d,         &
     &      jac_2d_q%ntot_int, num_int, jac_2d_q%xj_sf, num_sgrp,       &
     &      isurf_grp, area)
      else
        call int_surf_area_1_surf_grp(ele%numele, surf%numsurf,         &
     &      surf%isf_4_ele, ele%interior_ele, g_FEM%max_int_point,      &
     &      g_FEM%maxtot_int_2d, g_FEM%int_start2, g_FEM%owe2d,         &
     &      jac_2d_l%ntot_int, num_int, jac_2d_l%xj_sf, num_sgrp,       &
     &      isurf_grp, area)
      end if
!
      end subroutine s_int_surf_area_1sgrp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_int_surf_ave_1sgrp                                   &
     &         (node, ele, surf, g_FEM, jac_2d_l, jac_2d_q, num_int,    &
     &          num_sgrp, isurf_grp, istack_sf_grp_smp, d1_nod, ave_l)
!
      use int_surf_ave_fluxes_4
      use int_surf_ave_fluxes_8
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_2d_l
      type(jacobians_2d), intent(in) :: jac_2d_q
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_sgrp
      integer (kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer (kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      real(kind = kreal), intent(in) :: d1_nod(node%numnod)
!
      real(kind = kreal), intent(inout) :: ave_l
!
!
      if (ele%nnod_4_ele .eq. num_t_quad) then
        call int_surf_ave_1sgrp_8(node%numnod, ele%numele,              &
     &      surf%numsurf, surf%nnod_4_surf, surf%ie_surf,               &
     &      surf%isf_4_ele, ele%interior_ele, g_FEM%max_int_point,      &
     &      g_FEM%maxtot_int_2d, g_FEM%int_start2, g_FEM%owe2d,         &
     &      jac_2d_q%ntot_int, num_int, jac_2d_q%an_sf,                 &
     &      jac_2d_q%xj_sf, num_sgrp, isurf_grp, istack_sf_grp_smp,     &
     &      d1_nod, ave_l)
      else
        call int_surf_ave_1sgrp_4(node%numnod, ele%numele,              &
     &      surf%numsurf, surf%nnod_4_surf, surf%ie_surf,               &
     &      surf%isf_4_ele, ele%interior_ele, g_FEM%max_int_point,      &
     &      g_FEM%maxtot_int_2d, g_FEM%int_start2, g_FEM%owe2d,         &
     &      jac_2d_l%ntot_int, num_int, jac_2d_l%an_sf,                 &
     &      jac_2d_l%xj_sf, num_sgrp, isurf_grp, istack_sf_grp_smp,     &
     &      d1_nod, ave_l)
      end if
!
      end subroutine s_int_surf_ave_1sgrp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_int_vec_flux_1sgrp                                   &
     &         (node, ele, surf, g_FEM, jac_2d_l, jac_2d_q,             &
     &          num_int, num_sgrp, isurf_grp, istack_sf_grp_smp,        &
     &          d1_nod, flux)
!
      use int_surf_ave_fluxes_4
      use int_surf_ave_fluxes_8
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_2d_l
      type(jacobians_2d), intent(in) :: jac_2d_q
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      real(kind = kreal), intent(in) :: d1_nod(node%numnod,3)
!
      real(kind = kreal), intent(inout) :: flux(num_sgrp)
!
!
      if (ele%nnod_4_ele .eq. num_t_quad) then
        call int_vec_flux_1sgrp_8                                       &
     &     (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,    &
     &      surf%ie_surf, surf%isf_4_ele, ele%interior_ele,             &
     &      num_sgrp, isurf_grp, istack_sf_grp_smp,                     &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_2d, g_FEM%int_start2, &
     &      g_FEM%owe2d, jac_2d_q%ntot_int, num_int, jac_2d_q%an_sf,    &
     &      jac_2d_q%xsf_sf, d1_nod, flux)
      else
        call int_vec_flux_1sgrp_4                                       &
     &     (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,    &
     &      surf%ie_surf, surf%isf_4_ele, ele%interior_ele,             &
     &      num_sgrp, isurf_grp, istack_sf_grp_smp,                     &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_2d, g_FEM%int_start2, &
     &      g_FEM%owe2d, jac_2d_l%ntot_int, num_int, jac_2d_l%an_sf,    &
     &      jac_2d_l%xsf_sf, d1_nod, flux)
      end if
!
      end subroutine s_int_vec_flux_1sgrp
!
!  ---------------------------------------------------------------------
!
      subroutine s_int_vec_total_flux_1sgrp                             &
     &         (node, ele, surf, g_FEM, jac_2d_l, jac_2d_q,             &
     &          num_int, num_sgrp, isurf_grp, istack_sf_grp_smp,        &
     &          d1_nod, flux_l)
!
      use int_surf_ave_fluxes_4
      use int_surf_ave_fluxes_8
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_2d_l
      type(jacobians_2d), intent(in) :: jac_2d_q
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      real(kind = kreal), intent(in) :: d1_nod(node%numnod,3)
!
      real(kind = kreal), intent(inout) :: flux_l
!
!
      if (ele%nnod_4_ele .eq. num_t_quad) then
        call int_vec_tflux_1sgrp_8                                      &
     &     (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,    &
     &      surf%ie_surf, surf%isf_4_ele, ele%interior_ele,             &
     &      num_sgrp, isurf_grp, istack_sf_grp_smp,                     &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_2d, g_FEM%int_start2, &
     &      g_FEM%owe2d, jac_2d_q%ntot_int, num_int, jac_2d_q%an_sf,    &
     &      jac_2d_q%xsf_sf, d1_nod, flux_l)
      else
        call int_vec_tflux_1sgrp_4                                      &
     &     (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,    &
     &      surf%ie_surf, surf%isf_4_ele, ele%interior_ele,             &
     &      num_sgrp, isurf_grp, istack_sf_grp_smp,                     &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_2d, g_FEM%int_start2, &
     &      g_FEM%owe2d, jac_2d_l%ntot_int, num_int, jac_2d_l%an_sf,    &
     &      jac_2d_l%xsf_sf, d1_nod, flux_l)
      end if
!
      end subroutine s_int_vec_total_flux_1sgrp
!
!  ---------------------------------------------------------------------
!
      end module int_surf_ave_fluxes_1sgrp
