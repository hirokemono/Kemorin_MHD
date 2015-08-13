!int_surf_ave_layered.f90
!      module int_surf_ave_layered
!
!     Written by H. Matsui on Aug., 2007
!     Modified by H. Matsui on Nov., 2008
!     Modified by H. Matsui on June, 2011
!
!      subroutine s_int_surf_ave_layered(n_layer_d, n_item_layer_d,     &
!     &          layer_stack, item_layer, istack_layer_grp_smp,         &
!     &          num_int, d1_nod, ave_l)
!      subroutine s_int_surf_area_layered(n_layer_d, n_item_layer_d,    &
!     &          layer_stack, item_layer, num_int, area_l)
!
      module int_surf_ave_layered
!
      use m_precision
!
      use m_geometry_constants
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
      subroutine s_int_surf_ave_layered(n_layer_d, n_item_layer_d,      &
     &          layer_stack, item_layer, istack_layer_grp_smp,          &
     &          num_int, d1_nod, ave_l)
!
      use int_surf_ave_fluxes_4
      use int_surf_ave_fluxes_8
!
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: n_layer_d, n_item_layer_d
      integer (kind = kint), intent(in) :: layer_stack(0:n_layer_d)
      integer (kind = kint), intent(in) :: item_layer(2,n_item_layer_d)
      integer (kind = kint), intent(in)                                 &
     &       :: istack_layer_grp_smp(0:np_smp*n_layer_d)
      real(kind = kreal), intent(in) :: d1_nod(node1%numnod)
!
      real(kind = kreal), intent(inout) :: ave_l(n_layer_d)
!
      integer (kind = kint) :: igrp, ist, num_sgrp, ist_smp
!
!
      if (ele1%nnod_4_ele .eq. num_t_quad) then
!
        do igrp = 1, n_layer_d
          ist =     layer_stack(igrp-1) + 1
          ist_smp = (igrp-1) * np_smp
          num_sgrp = layer_stack(igrp) - layer_stack(igrp-1)
          call int_surf_ave_1sgrp_8(node1%numnod, ele1%numele, numsurf, &
     &        nnod_4_surf, ie_surf, isf_4_ele, e_multi,                 &
     &        jac1_2d_q%ntot_int, num_int, jac1_2d_q%an_sf,             &
     &        jac1_2d_q%xj_sf, num_sgrp, item_layer(1,ist),             &
     &        istack_layer_grp_smp(ist_smp), d1_nod, ave_l(igrp) )
        end do
!
      else
!
        do igrp = 1, n_layer_d
          ist = layer_stack(igrp-1) + 1
          ist_smp = (igrp-1) * np_smp
          num_sgrp = layer_stack(igrp) - layer_stack(igrp-1)
          call int_surf_ave_1sgrp_4(node1%numnod, ele1%numele, numsurf, &
     &        nnod_4_surf, ie_surf, isf_4_ele, e_multi,                 &
     &        jac1_2d_l%ntot_int, num_int, jac1_2d_l%an_sf,             &
     &        jac1_2d_l%xj_sf, num_sgrp, item_layer(1,ist),             &
     &        istack_layer_grp_smp(ist_smp), d1_nod, ave_l(igrp) )
        end do
!
      end if
!
      end subroutine s_int_surf_ave_layered
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_int_surf_area_layered(n_layer_d, n_item_layer_d,     &
     &          layer_stack, item_layer, num_int, area_l)
!
      use int_area_normal_4_surface
!
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
      if (ele1%nnod_4_ele .eq. num_t_quad) then
!
!$omp parallel do private(igrp,ist,num_sgrp)
        do igrp = 1, n_layer_d
          ist = layer_stack(igrp-1) + 1
          num_sgrp = layer_stack(igrp) - layer_stack(igrp-1)
          call int_surf_area_1_surf_grp(ele1%numele, numsurf,           &
     &           isf_4_ele, e_multi, jac1_2d_q%ntot_int, num_int,       &
     &           jac1_2d_q%xj_sf, num_sgrp, item_layer(1,ist),          &
     &           area_l(igrp))
        end do
!$omp end parallel do
!
      else
!
!$omp parallel do private(igrp,ist,num_sgrp)
        do igrp = 1, n_layer_d
          ist = layer_stack(igrp-1) + 1
          num_sgrp = layer_stack(igrp) - layer_stack(igrp-1)
          call int_surf_area_1_surf_grp(ele1%numele, numsurf,           &
     &           isf_4_ele, e_multi, jac1_2d_l%ntot_int, num_int,       &
     &           jac1_2d_l%xj_sf, num_sgrp, item_layer(1,ist),          &
     &           area_l(igrp))
        end do
!$omp end parallel do
!
      end if
!
      end subroutine s_int_surf_area_layered
!
!  ---------------------------------------------------------------------
!
      end module int_surf_ave_layered
