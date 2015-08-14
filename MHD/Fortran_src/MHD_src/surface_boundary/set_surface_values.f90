!> @file  set_surface_values.f90
!!      module set_surface_values
!!
!!@author  H. Matsui
!!@date Programmed in July, 2005
!!@n    Modified in Jan., 2009
!
!> @brief Structure of connectivity data for group items
!!
!!@verbatim
!!      subroutine allocate_work_4_surf_bc_dat(numnod)
!!      subroutine deallocate_work_4_surf_bc_dat
!!
!!      subroutine set_surf_bc_1st(sf_grp, ngrp_sf, nele_surf, igrp,    &
!!     &          id_sf_dat, i_dest, ist_sf, sf_dat_apt)
!!      subroutine set_surf_bc_on_node_1st(sf_grp, sf_grp_nod, sf_grp_v,&
!!     &          ngrp_sf, nnod_surf, igrp, id_sf_dat, i_dest,          &
!!     &          ist_nod_sf, sf_dat_apt)
!!
!!      subroutine set_surf_bc_dat(ngrp_sf, nele_surf, igrp,            &
!!     &          id_sf_dat, i_dest, num_surf, surf_istack, ist_sf,     &
!!     &           sf_dat_apt)
!!
!!      subroutine set_surf_bc_on_node_dat                              &
!!     &         (ngrp_sf, nnod_surf, igrp, id_sf_dat, i_dest, numnod,  &
!!     &          numele, nnod_4_ele, nnod_4_surf, ie, node_on_sf,      &
!!     &          num_surf, num_surf_bc, surf_istack, surf_item,        &
!!     &          ntot_node_sf_grp, inod_stack_sf_grp, inod_surf_grp,   &
!!     &          coef_sf_nod, a_area_sf_grp, ist_nod_sf, sf_dat_apt)
!!@endverbatim
!
!
!
      module set_surface_values
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: d_surf_nod_tmp(:)
      private :: d_surf_nod_tmp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_surf_bc_dat(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate ( d_surf_nod_tmp(numnod) )
      d_surf_nod_tmp = 0.0d0
!
      end subroutine allocate_work_4_surf_bc_dat
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_work_4_surf_bc_dat
!
      deallocate ( d_surf_nod_tmp )
!
      end subroutine deallocate_work_4_surf_bc_dat
!
!  ---------------------------------------------------------------------
!
      subroutine set_surf_bc_1st(sf_grp, ngrp_sf, nele_surf, igrp,      &
     &          id_sf_dat, i_dest, ist_sf, sf_dat_apt)
!
      use m_boundary_field_IO
      use t_group_data
!
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: igrp, i_dest, id_sf_dat
      integer(kind = kint), intent(in) :: ngrp_sf, nele_surf
!
      integer(kind = kint), intent(inout) :: ist_sf(0:ngrp_sf)
      real(kind = kreal), intent(inout) :: sf_dat_apt(nele_surf)
!
!
      call set_surf_bc_dat(ngrp_sf, nele_surf, igrp, id_sf_dat, i_dest, &
     &    sf_grp%num_grp, sf_grp%istack_grp, ist_sf, sf_dat_apt)
!
      end subroutine set_surf_bc_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_surf_bc_on_node_1st(sf_grp, sf_grp_nod, sf_grp_v,  &
     &          ngrp_sf, nnod_surf, igrp, id_sf_dat, i_dest,            &
     &          ist_nod_sf, sf_dat_apt)
!
      use m_geometry_data
      use t_surface_group_geometry
      use t_group_data
      use t_surface_group_connect
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
      integer(kind = kint), intent(in) :: igrp, i_dest, id_sf_dat
      integer(kind = kint), intent(in) :: ngrp_sf, nnod_surf
!
      integer(kind = kint), intent(inout) :: ist_nod_sf(0:ngrp_sf)
      real(kind = kreal), intent(inout) :: sf_dat_apt(nnod_surf)
!
!
      call set_surf_bc_on_node_dat                                      &
     &   (ngrp_sf, nnod_surf, igrp, id_sf_dat, i_dest,                  &
     &    node1%numnod, ele1%numele, ele1%nnod_4_ele,                   &
     &    nnod_4_surf, ele1%ie, node_on_sf,                             &
     &    sf_grp%num_grp, sf_grp%num_item,                              &
     &    sf_grp%istack_grp, sf_grp%item_sf_grp,                        &
     &    sf_grp_nod%ntot_node_sf_grp, sf_grp_nod%inod_stack_sf_grp,    &
     &    sf_grp_nod%inod_surf_grp, sf_grp_nod%coef_sf_nod,             &
     &    sf_grp_v%a_area_sf_grp, ist_nod_sf, sf_dat_apt)
!
      end subroutine set_surf_bc_on_node_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_surf_bc_dat(ngrp_sf, nele_surf, igrp,              &
     &          id_sf_dat, i_dest, num_surf, surf_istack, ist_sf,       &
     &           sf_dat_apt)
!
      use m_boundary_field_IO
!
      integer(kind = kint), intent(in) :: igrp, i_dest, id_sf_dat
      integer(kind = kint), intent(in) :: ngrp_sf, nele_surf
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
!
      integer(kind = kint), intent(inout) :: ist_sf(0:ngrp_sf)
      real(kind = kreal), intent(inout) :: sf_dat_apt(nele_surf)
!
      integer(kind = kint) :: ist, ied, inum, idat
!
!
      ist_sf(i_dest) = ist_sf(i_dest-1)                                 &
     &                  + surf_istack(igrp) - surf_istack(igrp-1)
      ist = ist_sf(i_dest-1) + 1
      ied = ist_sf(i_dest)
      do inum = ist, ied
        idat = istack_bc_data_IO(id_sf_dat-1)                           &
     &          + inum - ist_sf(i_dest-1)
!
        sf_dat_apt(inum) = boundary_field_IO(idat)
      end do
!
      end subroutine set_surf_bc_dat
!
!  ---------------------------------------------------------------------
!
      subroutine set_surf_bc_on_node_dat                                &
     &         (ngrp_sf, nnod_surf, igrp, id_sf_dat, i_dest, numnod,    &
     &          numele, nnod_4_ele, nnod_4_surf, ie, node_on_sf,        &
     &          num_surf, num_surf_bc, surf_istack, surf_item,          &
     &          ntot_node_sf_grp, inod_stack_sf_grp, inod_surf_grp,     &
     &          coef_sf_nod, a_area_sf_grp, ist_nod_sf, sf_dat_apt)
!
      use m_geometry_constants
      use m_boundary_field_IO
!
      integer(kind = kint), intent(in) :: igrp, i_dest, id_sf_dat
      integer(kind = kint), intent(in) :: ngrp_sf, nnod_surf
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: numnod, nnod_4_surf
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                      :: node_on_sf(nnod_4_surf,nsurf_4_ele)
!
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      integer(kind = kint), intent(in) :: surf_item(2,num_surf)
!
      integer(kind = kint), intent(in) :: ntot_node_sf_grp
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &                      :: inod_surf_grp(ntot_node_sf_grp)
      real(kind = kreal), intent(in) :: coef_sf_nod(ntot_node_sf_grp)
      real(kind = kreal), intent(in) :: a_area_sf_grp(num_surf_bc)
!
      integer(kind = kint), intent(inout) :: ist_nod_sf(0:ngrp_sf)
      real(kind = kreal), intent(inout) :: sf_dat_apt(nnod_surf)
!
      integer(kind = kint) :: ist, ied
      integer(kind = kint) :: inum, iele, isf, inod, idat
      integer(kind = kint) :: k1, kk1
!
!
      ist_nod_sf(i_dest) = ist_nod_sf(i_dest-1)                         &
     &        + inod_stack_sf_grp(igrp) - inod_stack_sf_grp(igrp-1)
!
!$omp workshare
      d_surf_nod_tmp(1:numnod) = 0.0d0
!$omp end workshare
!
      ist = surf_istack(igrp-1)+1
      ied = surf_istack(igrp)
      do k1 = 1, nnod_4_surf
        do inum = ist, ied
          iele = surf_item(1,inum)
          isf = surf_item(2,inum)
          kk1 = node_on_sf(k1,isf)
          inod = ie(iele,kk1)
          idat = istack_bc_data_IO(id_sf_dat-1)                         &
     &            + inum - surf_istack(igrp-1)
!
          d_surf_nod_tmp(inod) = d_surf_nod_tmp(inod)                   &
     &       + boundary_field_IO(idat) * a_area_sf_grp(inum)
        end do
      end do
!
      ist = inod_stack_sf_grp(igrp-1)+1
      ied = inod_stack_sf_grp(igrp)
      do inum = ist, ied
        idat = ist_nod_sf(i_dest-1) + inum - inod_stack_sf_grp(igrp-1)
        inod = inod_surf_grp(inum)
        sf_dat_apt(idat) = d_surf_nod_tmp(inod) / coef_sf_nod(inod)
      end do
!
      end subroutine set_surf_bc_on_node_dat
!
!  ---------------------------------------------------------------------
!
      end module set_surface_values
