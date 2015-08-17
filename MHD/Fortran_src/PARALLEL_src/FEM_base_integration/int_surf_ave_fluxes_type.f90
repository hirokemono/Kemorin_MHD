!int_surf_ave_fluxes_type.f90
!      module int_surf_ave_fluxes_type
!
!     Written by H. Matsui on Aug., 2007
!     Modified by H. Matsui on Nov., 2008
!
!      subroutine int_surf_area_1sgrp_type(mesh, surf, jac_2d,          &
!     &          num_int, num_sgrp, isurf_grp, area)
!
!      subroutine int_surf_ave_1sgrp_type(mesh, surf, jac_2d,           &
!     &          num_int, num_sgrp, isurf_grp, istack_sf_grp_smp,       &
!     &          d1_nod, ave_l)
!
!      subroutine int_vec_flux_1sgrp_type(mesh, surf, jac_2d,           &
!     &          num_int, num_sgrp, isurf_grp, istack_sf_grp_smp,       &
!     &          d1_nod, flux)
!      subroutine int_vec_total_flux_1sgrp_type(mesh, surf, jac_2d,     &
!     &          num_int, num_sgrp, isurf_grp, istack_sf_grp_smp,       &
!     &          d1_nod, flux_l)
!        type(mesh_geometry), intent(in) :: mesh
!        type(surface_data), intent(in) :: surf
!        type(jacobians_2d), intent(in) :: jac_2d
!
      module int_surf_ave_fluxes_type
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_constants
!
      use m_fem_gauss_int_coefs
      use t_mesh_data
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
      subroutine int_surf_area_1sgrp_type(mesh, surf, jac_2d,           &
     &          num_int, num_sgrp, isurf_grp, area)
!
      use int_area_normal_4_surface
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(jacobians_2d), intent(in) :: jac_2d
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_sgrp
      integer (kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
!
      real(kind = kreal), intent(inout) :: area
!
!
        call int_surf_area_1_surf_grp(mesh%ele%numele, surf%numsurf,    &
     &      surf%isf_4_ele, mesh%ele%interior_ele, jac_2d%ntot_int,     &
     &      num_int, jac_2d%xj_sf, num_sgrp, isurf_grp, area)
!
      end subroutine int_surf_area_1sgrp_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_surf_ave_1sgrp_type(mesh, surf, jac_2d,            &
     &          num_int, num_sgrp, isurf_grp, istack_sf_grp_smp,        &
     &          d1_nod, ave_l)
!
      use int_surf_ave_fluxes_4
      use int_surf_ave_fluxes_8
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(jacobians_2d), intent(in) :: jac_2d
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_sgrp
      integer (kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer (kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      real(kind = kreal), intent(in) :: d1_nod(mesh%node%numnod)
!
      real(kind = kreal), intent(inout) :: ave_l
!
!
      if (mesh%ele%nnod_4_ele .eq. num_t_quad) then
        call int_surf_ave_1sgrp_8(mesh%node%numnod, mesh%ele%numele,    &
     &      surf%numsurf, surf%nnod_4_surf, surf%ie_surf,               &
     &      surf%isf_4_ele, mesh%ele%interior_ele,                      &
     &      jac_2d%ntot_int, num_int, jac_2d%an_sf, jac_2d%xj_sf,       &
     &      num_sgrp, isurf_grp, istack_sf_grp_smp, d1_nod, ave_l)
      else
        call int_surf_ave_1sgrp_4(mesh%node%numnod, mesh%ele%numele,    &
     &      surf%numsurf, surf%nnod_4_surf, surf%ie_surf,               &
     &      surf%isf_4_ele, mesh%ele%interior_ele,                      &
     &      jac_2d%ntot_int, num_int, jac_2d%an_sf, jac_2d%xj_sf,       &
     &      num_sgrp, isurf_grp, istack_sf_grp_smp, d1_nod, ave_l)
      end if
!
      end subroutine int_surf_ave_1sgrp_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_vec_flux_1sgrp_type(mesh, surf, jac_2d,            &
     &          num_int, num_sgrp, isurf_grp, istack_sf_grp_smp,        &
     &          d1_nod, flux)
!
      use int_surf_ave_fluxes_4
      use int_surf_ave_fluxes_8
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(jacobians_2d), intent(in) :: jac_2d
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_sgrp
      integer (kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      real(kind = kreal), intent(in) :: d1_nod(mesh%node%numnod,3)
!
      real(kind = kreal), intent(inout) :: flux(num_sgrp)
!
!
      if (mesh%ele%nnod_4_ele .eq. num_t_quad) then
        call int_vec_flux_1sgrp_8(mesh%node%numnod, mesh%ele%numele,    &
     &      surf%numsurf,  surf%nnod_4_surf, surf%ie_surf,              &
     &      surf%isf_4_ele, mesh%ele%interior_ele, num_sgrp, isurf_grp, &
     &      istack_sf_grp_smp, jac_2d%ntot_int, num_int,                &
     &      jac_2d%an_sf, jac_2d%xsf_sf, d1_nod, flux)
      else
        call int_vec_flux_1sgrp_4(mesh%node%numnod, mesh%ele%numele,    &
     &      surf%numsurf, surf%nnod_4_surf, surf%ie_surf,               &
     &      surf%isf_4_ele, mesh%ele%interior_ele,                      &
     &      num_sgrp, isurf_grp, istack_sf_grp_smp,                     &
     &      jac_2d%ntot_int, num_int, jac_2d%an_sf, jac_2d%xsf_sf,      &
     &      d1_nod, flux)
      end if
!
      end subroutine int_vec_flux_1sgrp_type
!
!  ---------------------------------------------------------------------
!
      subroutine int_vec_total_flux_1sgrp_type(mesh, surf, jac_2d,      &
     &          num_int, num_sgrp, isurf_grp, istack_sf_grp_smp,        &
     &          d1_nod, flux_l)
!
      use int_surf_ave_fluxes_4
      use int_surf_ave_fluxes_8
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(jacobians_2d), intent(in) :: jac_2d
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: istack_sf_grp_smp(0:np_smp)
      real(kind = kreal), intent(in) :: d1_nod(mesh%node%numnod,3)
!
      real(kind = kreal), intent(inout) :: flux_l
!
!
      if (mesh%ele%nnod_4_ele .eq. num_t_quad) then
        call int_vec_tflux_1sgrp_8(mesh%node%numnod, mesh%ele%numele,   &
     &      surf%numsurf, surf%nnod_4_surf, surf%ie_surf,               &
     &      surf%isf_4_ele, mesh%ele%interior_ele, num_sgrp, isurf_grp, &
     &      istack_sf_grp_smp, jac_2d%ntot_int, num_int,                &
     &      jac_2d%an_sf, jac_2d%xsf_sf, d1_nod, flux_l)
      else
        call int_vec_tflux_1sgrp_4(mesh%node%numnod, mesh%ele%numele,   &
     &      surf%numsurf, surf%nnod_4_surf, surf%ie_surf,               &
     &      surf%isf_4_ele, mesh%ele%interior_ele,                      &
     &      num_sgrp, isurf_grp, istack_sf_grp_smp, jac_2d%ntot_int,    &
     &      num_int, jac_2d%an_sf, jac_2d%xsf_sf, d1_nod, flux_l)
      end if
!
      end subroutine int_vec_total_flux_1sgrp_type
!
!  ---------------------------------------------------------------------
!
      end module int_surf_ave_fluxes_type
