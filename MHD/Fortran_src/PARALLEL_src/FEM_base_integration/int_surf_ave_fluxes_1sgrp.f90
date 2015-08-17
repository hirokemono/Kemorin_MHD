!int_surf_ave_fluxes_1sgrp.f90
!      module int_surf_ave_fluxes_1sgrp
!
!     Written by H. Matsui on Aug., 2007
!     Modified by H. Matsui on Nov., 2008
!
!      subroutine s_int_surf_area_1sgrp(num_int, num_sgrp, isurf_grp,   &
!     &          area)
!
!      subroutine s_int_surf_ave_1sgrp(num_int, num_sgrp, isurf_grp,    &
!     &          istack_sf_grp_smp, d1_nod, ave_l)
!
!      subroutine s_int_vec_flux_1sgrp(num_int, num_sgrp, isurf_grp,    &
!     &          istack_sf_grp_smp, d1_nod, flux)
!      subroutine s_int_vec_total_flux_1sgrp(num_int, num_sgrp,         &
!     &          isurf_grp, istack_sf_grp_smp, d1_nod, flux_l)
!
      module int_surf_ave_fluxes_1sgrp
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
!
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians_4_surface
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_int_surf_area_1sgrp(num_int, num_sgrp, isurf_grp,    &
     &          area)
!
      use int_area_normal_4_surface
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_sgrp
      integer (kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
!
      real(kind = kreal), intent(inout) :: area
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_surf_area_1_surf_grp(ele1%numele, surf1%numsurf,       &
     &      surf1%isf_4_ele, e_multi, jac1_2d_q%ntot_int, num_int,      &
     &      jac1_2d_q%xj_sf, num_sgrp, isurf_grp, area)
      else
        call int_surf_area_1_surf_grp(ele1%numele, surf1%numsurf,       &
     &      surf1%isf_4_ele, e_multi, jac1_2d_l%ntot_int, num_int,      &
     &      jac1_2d_l%xj_sf, num_sgrp, isurf_grp, area)
      end if
!
      end subroutine s_int_surf_area_1sgrp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_int_surf_ave_1sgrp(num_int, num_sgrp, isurf_grp,     &
     &          istack_sf_grp_smp, d1_nod, ave_l)
!
      use int_surf_ave_fluxes_4
      use int_surf_ave_fluxes_8
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_sgrp
      integer (kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer (kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      real(kind = kreal), intent(in) :: d1_nod(node1%numnod)
!
      real(kind = kreal), intent(inout) :: ave_l
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_surf_ave_1sgrp_8(node1%numnod, ele1%numele,            &
     &      surf1%numsurf, surf1%nnod_4_surf, surf1%ie_surf,            &
     &      surf1%isf_4_ele, e_multi,                                   &
     &      jac1_2d_q%ntot_int, num_int, jac1_2d_q%an_sf,               &
     &      jac1_2d_q%xj_sf, num_sgrp, isurf_grp, istack_sf_grp_smp,    &
     &      d1_nod, ave_l)
      else
        call int_surf_ave_1sgrp_4(node1%numnod, ele1%numele,            &
     &      surf1%numsurf, surf1%nnod_4_surf, surf1%ie_surf,            &
     &      surf1%isf_4_ele, e_multi,                                   &
     &      jac1_2d_l%ntot_int, num_int, jac1_2d_l%an_sf,               &
     &      jac1_2d_l%xj_sf, num_sgrp, isurf_grp, istack_sf_grp_smp,    &
     &      d1_nod, ave_l)
      end if
!
      end subroutine s_int_surf_ave_1sgrp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_int_vec_flux_1sgrp(num_int, num_sgrp, isurf_grp,     &
     &          istack_sf_grp_smp, d1_nod, flux)
!
      use int_surf_ave_fluxes_4
      use int_surf_ave_fluxes_8
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      real(kind = kreal), intent(in) :: d1_nod(node1%numnod,3)
!
      real(kind = kreal), intent(inout) :: flux(num_sgrp)
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_vec_flux_1sgrp_8(node1%numnod, ele1%numele,            &
     &      surf1%numsurf, surf1%nnod_4_surf, surf1%ie_surf,            &
     &      surf1%isf_4_ele, e_multi, num_sgrp,                         &
     &      isurf_grp, istack_sf_grp_smp, jac1_2d_q%ntot_int,           &
     &      num_int, jac1_2d_q%an_sf, jac1_2d_q%xsf_sf,                 &
     &      d1_nod, flux)
      else
        call int_vec_flux_1sgrp_4(node1%numnod, ele1%numele,            &
     &      surf1%numsurf, surf1%nnod_4_surf, surf1%ie_surf,            &
     &      surf1%isf_4_ele, e_multi, num_sgrp,                         &
     &      isurf_grp, istack_sf_grp_smp, jac1_2d_l%ntot_int,           &
     &      num_int, jac1_2d_l%an_sf, jac1_2d_l%xsf_sf,                 &
     &      d1_nod, flux)
      end if
!
      end subroutine s_int_vec_flux_1sgrp
!
!  ---------------------------------------------------------------------
!
      subroutine s_int_vec_total_flux_1sgrp(num_int, num_sgrp,          &
     &          isurf_grp, istack_sf_grp_smp, d1_nod, flux_l)
!
      use int_surf_ave_fluxes_4
      use int_surf_ave_fluxes_8
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      real(kind = kreal), intent(in) :: d1_nod(node1%numnod,3)
!
      real(kind = kreal), intent(inout) :: flux_l
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
        call int_vec_tflux_1sgrp_8(node1%numnod, ele1%numele,           &
     &      surf1%numsurf, surf1%nnod_4_surf, surf1%ie_surf,            &
     &      surf1%isf_4_ele, e_multi, num_sgrp,                         &
     &      isurf_grp, istack_sf_grp_smp, jac1_2d_q%ntot_int,           &
     &      num_int, jac1_2d_q%an_sf, jac1_2d_q%xsf_sf,                 &
     &      d1_nod, flux_l)
      else
        call int_vec_tflux_1sgrp_4(node1%numnod, ele1%numele,           &
     &      surf1%numsurf, surf1%nnod_4_surf, surf1%ie_surf,            &
     &      surf1%isf_4_ele, e_multi, num_sgrp,                         &
     &      isurf_grp, istack_sf_grp_smp, jac1_2d_l%ntot_int,           &
     &      num_int, jac1_2d_l%an_sf, jac1_2d_l%xsf_sf,                 &
     &      d1_nod, flux_l)
      end if
!
      end subroutine s_int_vec_total_flux_1sgrp
!
!  ---------------------------------------------------------------------
!
      end module int_surf_ave_fluxes_1sgrp
