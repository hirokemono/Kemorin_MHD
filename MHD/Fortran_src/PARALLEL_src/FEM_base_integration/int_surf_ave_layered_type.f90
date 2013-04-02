!int_surf_ave_layered_type.f90
!      module int_surf_ave_layered_type
!
!     Written by H. Matsui on Aug., 2007
!     Modified by H. Matsui on Nov., 2008
!     Modified by H. Matsui on June, 2011
!
!      subroutine s_int_surf_ave_layered_type(mesh, surf, jac_2d,       &
!     &          n_layer_d, n_item_layer_d, layer_stack, item_layer,    &
!     &          istack_layer_grp_smp, num_int, d1_nod, ave_l)
!      subroutine int_surf_area_layered_type(mesh, surf, jac_2d,        &
!     &          n_layer_d, n_item_layer_d, layer_stack, item_layer,    &
!     &          num_int, area_l)
!        type(mesh_geometry), intent(in) :: mesh
!        type(surface_data), intent(in) :: surf
!        type(jacobians_2d), intent(in) :: jac_2d
!
      module int_surf_ave_layered_type
!
      use m_precision
!
      use m_geometry_constants
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
      subroutine s_int_surf_ave_layered_type(mesh, surf, jac_2d,        &
     &          n_layer_d, n_item_layer_d, layer_stack, item_layer,     &
     &          istack_layer_grp_smp, num_int, d1_nod, ave_l)
!
      use int_surf_ave_fluxes_4
      use int_surf_ave_fluxes_8
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(jacobians_2d), intent(in) :: jac_2d
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in) :: layer_stack(0:n_layer_d)
      integer (kind = kint), intent(in) :: item_layer(2,n_item_layer_d)
      integer (kind = kint), intent(in)                                 &
     &       :: istack_layer_grp_smp(0:np_smp*n_layer_d)
      real(kind = kreal), intent(in) :: d1_nod(mesh%node%numnod)
!
      real(kind = kreal), intent(inout) :: ave_l(n_layer_d)
!
      integer (kind = kint) :: igrp, ist, num_sgrp, ist_smp
!
!
      if (mesh%ele%nnod_4_ele .eq. num_t_quad) then
        do igrp = 1, n_layer_d
          ist = layer_stack(igrp-1) + 1
          ist_smp = (igrp-1) * np_smp
          num_sgrp = layer_stack(igrp) - layer_stack(igrp-1)
          call int_surf_ave_1sgrp_8(mesh%node%numnod, mesh%ele%numele,  &
     &        surf%numsurf, surf%nnod_4_surf, surf%ie_surf,             &
     &        surf%isf_4_ele, mesh%ele%e_multi, jac_2d%ntot_int,        &
     &        num_int, jac_2d%an_surf, jac_2d%xj_surf, num_sgrp,        &
     &        item_layer(1,ist), istack_layer_grp_smp(ist_smp),         &
     &        d1_nod, ave_l(igrp) )
        end do
!
      else
!
        do igrp = 1, n_layer_d
          ist = layer_stack(igrp-1) + 1
          ist_smp = (igrp-1) * np_smp
          num_sgrp = layer_stack(igrp) - layer_stack(igrp-1)
          call int_surf_ave_1sgrp_4(mesh%node%numnod, mesh%ele%numele,  &
     &        surf%numsurf, surf%nnod_4_surf, surf%ie_surf,             &
     &        surf%isf_4_ele, mesh%ele%e_multi, jac_2d%ntot_int,        &
     &        num_int, jac_2d%an_surf, jac_2d%xj_surf, num_sgrp,        &
     &        item_layer(1,ist), istack_layer_grp_smp(ist_smp),         &
     &        d1_nod, ave_l(igrp) )
        end do
      end if
!
      end subroutine s_int_surf_ave_layered_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine int_surf_area_layered_type(mesh, surf, jac_2d,         &
     &          n_layer_d, n_item_layer_d, layer_stack, item_layer,     &
     &          num_int, area_l)
!
      use int_area_normal_4_surface
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(jacobians_2d), intent(in) :: jac_2d
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in) :: layer_stack(0:n_layer_d)
      integer (kind = kint), intent(in) :: item_layer(2,n_item_layer_d)
!
      real(kind = kreal), intent(inout) :: area_l(n_layer_d)
!
      integer (kind = kint) :: igrp, ist, num_sgrp
!
!
!$omp parallel do private(igrp,ist,num_sgrp)
        do igrp = 1, n_layer_d
          ist = layer_stack(igrp-1) + 1
          num_sgrp = layer_stack(igrp) - layer_stack(igrp-1)
          call int_surf_area_1_surf_grp(mesh%ele%numele, surf%numsurf,  &
     &        surf%isf_4_ele, mesh%ele%e_multi, jac_2d%ntot_int,        &
     &        num_int, jac_2d%xj_surf, num_sgrp, item_layer(1,ist),     &
     &        area_l(igrp))
        end do
!$omp end parallel do
!
      end subroutine int_surf_area_layered_type
!
!  ---------------------------------------------------------------------
!
      end module int_surf_ave_layered_type
