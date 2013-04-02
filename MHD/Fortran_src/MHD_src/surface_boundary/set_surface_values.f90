!
!       module set_surface_values
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine allocate_work_4_surf_bc_dat(numnod)
!      subroutine deallocate_work_4_surf_bc_dat
!
!      subroutine set_surf_bc_dat(ngrp_sf, nele_surf, igrp,             &
!     &          id_sf_dat, i_dest, ist_sf, sf_dat_apt)
!
!      subroutine set_surf_bc_on_node_dat(ngrp_sf, nnod_surf,           &
!     &          igrp, id_sf_dat, i_dest, ist_nod_sf, sf_dat_apt)
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
      subroutine set_surf_bc_dat(ngrp_sf, nele_surf, igrp,              &
     &          id_sf_dat, i_dest, ist_sf, sf_dat_apt)
!
      use m_surface_group
      use m_boundary_field_IO
!
      integer(kind = kint), intent(in) :: igrp, i_dest, id_sf_dat
      integer(kind = kint), intent(in) :: ngrp_sf, nele_surf
      integer(kind = kint), intent(inout) :: ist_sf(0:ngrp_sf)
      real(kind = kreal), intent(inout) :: sf_dat_apt(nele_surf)
!
      integer(kind = kint) :: ist, ied, inum, idat
!
!
        ist_sf(i_dest) = ist_sf(i_dest-1)                               &
     &                  + surf_istack(igrp) - surf_istack(igrp-1)
        ist = ist_sf(i_dest-1) + 1
        ied = ist_sf(i_dest)
        do inum = ist, ied
          idat = istack_bc_data_IO(id_sf_dat-1)                         &
     &          + inum - ist_sf(i_dest-1)
!
          sf_dat_apt(inum) = boundary_field_IO(idat)
        end do
!
      end subroutine set_surf_bc_dat
!
!  ---------------------------------------------------------------------
!
      subroutine set_surf_bc_on_node_dat(ngrp_sf, nnod_surf,            &
     &          igrp, id_sf_dat, i_dest, ist_nod_sf, sf_dat_apt)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_group
      use m_surface_group_connect
      use m_surface_group_geometry
      use m_boundary_field_IO
!
      integer(kind = kint), intent(in) :: igrp, i_dest, id_sf_dat
      integer(kind = kint), intent(in) :: ngrp_sf, nnod_surf
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
      d_surf_nod_tmp(1:numnod) = 0.0d0
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
     &          + boundary_field_IO(idat) * a_area_sf_grp(inum)
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
!
      end subroutine set_surf_bc_on_node_dat
!
!  ---------------------------------------------------------------------
!
      end module set_surface_values
