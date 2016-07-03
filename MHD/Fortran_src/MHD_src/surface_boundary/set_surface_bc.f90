!
!       module set_surface_bc
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on Jan., 2009
!
!!      subroutine count_surf_group_from_data                           &
!!     &         (IO_bc, i_sf, ngrp_sf, nele_sf,                        &
!!     &          field_name, num_surf, surf_istack, surf_name)
!!      subroutine  count_surf_nod_group_from_data(IO_bc, i_sf, ngrp_sf,&
!!     &           nnod_sf, field_name, num_surf, inod_stack_sf_grp,    &
!!     &           surf_name)
!!
!!      subroutine  set_surf_group_from_ctl(num_surf, surf_istack,      &
!!     &           ngrp_sf, nele_sf, i_dest, igrp, id_grp, ist_sf,      &
!!     &           sf_dat, surf_magnitude)
!!      subroutine set_surf_group_from_data(IO_bc, sf_grp,              &
!!     &           ngrp_sf, nele_sf, i_dest, igrp, id_grp, ist_sf,      &
!!     &           sf_dat, field_name)
!!
!!      subroutine  set_sf_nod_grp_from_ctl(i_dest, igrp,               &
!!     &           num_surf, inod_stack_sf_grp, ngrp_sf, nnod_sf,       &
!!     &           id_grp, ist_nod, sf_dat, surf_magnitude)
!!      subroutine  set_sf_nod_grp_from_data(IO_bc, i_dest, igrp,       &
!!     &           node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,       &
!!     &           ngrp_sf, nnod_sf, id_grp, ist_nod, sf_dat, field_name)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(surface_group_geometry), intent(in) :: sf_grp_v
!
      module set_surface_bc
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_surf_group_from_data                             &
     &         (IO_bc, i_sf, ngrp_sf, nele_sf,                          &
     &          field_name, num_surf, surf_istack, surf_name)
!
      use t_boundary_field_IO
!
      type(IO_boundary), intent(in) :: IO_bc
      integer(kind=kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: i_sf
      integer(kind = kint), intent(inout) :: ngrp_sf, nele_sf
!
      integer(kind = kint) :: ia
!
      do ia = 1, IO_bc%num_group
        if(IO_bc%group_type(ia) .eq. flag_surf_grp) then
          if ( IO_bc%group_name(ia) .eq. surf_name(i_sf)                &
     &       .and. IO_bc%field_type(ia) .eq. field_name ) then
            ngrp_sf = ngrp_sf + 1
            nele_sf = nele_sf + surf_istack(i_sf) - surf_istack(i_sf-1)
          end if
        end if
      end do
!
      end subroutine count_surf_group_from_data
!
!  ---------------------------------------------------------------------
!
      subroutine  count_surf_nod_group_from_data(IO_bc, i_sf, ngrp_sf,  &
     &           nnod_sf, field_name, num_surf, inod_stack_sf_grp,      &
     &           surf_name)
!
      use t_boundary_field_IO
!
      type(IO_boundary), intent(in) :: IO_bc
      integer(kind=kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: i_sf
      integer(kind = kint), intent(inout) :: ngrp_sf, nnod_sf
!
      integer(kind = kint) :: ia
!
      do ia = 1, IO_bc%num_group
        if(IO_bc%group_type(ia) .eq. flag_surf_grp) then
          if ( IO_bc%group_name(ia) .eq. surf_name(i_sf)                &
     &       .and. IO_bc%field_type(ia) .eq. field_name ) then
            ngrp_sf = ngrp_sf + 1
            nnod_sf = nnod_sf                                           &
     &           + inod_stack_sf_grp(i_sf) - inod_stack_sf_grp(i_sf-1)
          end if
        end if
      end do
!
      end subroutine count_surf_nod_group_from_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine  set_surf_group_from_ctl(num_surf, surf_istack,        &
     &           ngrp_sf, nele_sf, i_dest, igrp, id_grp, ist_sf,        &
     &           sf_dat, surf_magnitude)
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
!
      integer(kind = kint), intent(in) :: ngrp_sf, nele_sf, igrp
      real (kind = kreal), intent(in) :: surf_magnitude
      integer(kind = kint), intent(inout) :: i_dest
      integer(kind = kint), intent(inout) :: id_grp(ngrp_sf)
      integer(kind = kint), intent(inout) :: ist_sf(0:ngrp_sf)
      real(kind = kreal), intent(inout) ::   sf_dat(nele_sf)
!
      integer(kind = kint) :: ist, ied, inum
!
!
      i_dest = i_dest + 1
      id_grp(i_dest) = igrp
!
      ist_sf(i_dest) = ist_sf(i_dest-1)                                 &
     &                + surf_istack(igrp) - surf_istack(igrp-1)
      ist = ist_sf(i_dest-1) + 1
      ied = ist_sf(i_dest)
      do inum = ist, ied
       sf_dat(inum) = surf_magnitude
      end do
!
      end subroutine set_surf_group_from_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine set_surf_group_from_data(IO_bc, sf_grp,                &
     &           ngrp_sf, nele_sf, i_dest, igrp, id_grp, ist_sf,        &
     &           sf_dat, field_name)
!
      use t_group_data
      use t_boundary_field_IO
      use set_surface_values
!
      type(IO_boundary), intent(in) :: IO_bc
      type(surface_group_data), intent(in) :: sf_grp
!
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: ngrp_sf, nele_sf, igrp
      integer(kind = kint), intent(inout) :: i_dest
      integer(kind = kint), intent(inout) :: id_grp(ngrp_sf)
      integer(kind = kint), intent(inout) :: ist_sf(0:ngrp_sf)
      real(kind = kreal), intent(inout) ::   sf_dat(nele_sf)
!
      integer(kind = kint) :: ia
!
!
      do ia = 1, IO_bc%num_group
        if(IO_bc%group_type(ia) .eq. flag_surf_grp) then
          if ( IO_bc%group_name(ia) .eq. sf_grp%grp_name(igrp)          &
     &        .and.  IO_bc%field_type(ia) .eq. field_name) then
            i_dest = i_dest + 1
            id_grp(i_dest) = igrp
!
            call set_surf_bc_dat                                        &
     &         (IO_bc, ngrp_sf, nele_sf, igrp, ia, i_dest,              &
     &          sf_grp%num_grp, sf_grp%istack_grp, ist_sf, sf_dat)
          end if
        end if
      end do
!
      end subroutine set_surf_group_from_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine  set_sf_nod_grp_from_ctl(i_dest, igrp,                 &
     &           num_surf, inod_stack_sf_grp, ngrp_sf, nnod_sf,         &
     &           id_grp, ist_nod, sf_dat, surf_magnitude)
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
!
      integer(kind = kint), intent(in) :: ngrp_sf, nnod_sf, igrp
      integer(kind = kint), intent(inout) :: i_dest
      integer(kind = kint), intent(inout) :: id_grp(ngrp_sf)
      integer(kind = kint), intent(inout) :: ist_nod(0:ngrp_sf)
      real(kind = kreal), intent(in) ::      surf_magnitude
      real(kind = kreal), intent(inout) ::   sf_dat(nnod_sf)
!
      integer(kind = kint) :: ist, ied, inum
!
!
      i_dest = i_dest + 1
      id_grp(i_dest) = igrp
!
      ist_nod(i_dest) = ist_nod(i_dest-1)                               &
     &            + inod_stack_sf_grp(igrp) - inod_stack_sf_grp(igrp-1)
      ist = ist_nod(i_dest-1) + 1
      ied = ist_nod(i_dest)
      do inum = ist, ied
        sf_dat(inum) = surf_magnitude
      end do
!
      end subroutine set_sf_nod_grp_from_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine  set_sf_nod_grp_from_data(IO_bc, i_dest, igrp,         &
     &           node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,         &
     &           ngrp_sf, nnod_sf, id_grp, ist_nod, sf_dat, field_name)
!
      use t_boundary_field_IO
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_group_geometry
      use set_surface_values
!
      type(IO_boundary), intent(in) :: IO_bc
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
!
      character(len=kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: ngrp_sf, nnod_sf, igrp
      integer(kind = kint), intent(inout) :: i_dest
      integer(kind = kint), intent(inout) :: id_grp(ngrp_sf)
      integer(kind = kint), intent(inout) :: ist_nod(0:ngrp_sf)
      real(kind = kreal), intent(inout) :: sf_dat(nnod_sf)
!
      integer(kind = kint) :: ia
!
      do ia = 1, IO_bc%num_group
        if(IO_bc%group_type(ia) .eq. flag_surf_grp) then
          if ( IO_bc%group_name(ia) .eq. sf_grp%grp_name(igrp)          &
     &      .and. IO_bc%field_type(ia) .eq. field_name) then
!
            i_dest = i_dest + 1
            id_grp(i_dest) = igrp
!
            call set_surf_bc_on_node                                    &
     &         (IO_bc, node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v,   &
     &          ngrp_sf, nnod_sf, igrp, ia, i_dest, ist_nod, sf_dat)
          end if
        end if
      end do
!
      end subroutine set_sf_nod_grp_from_data
!
!  ---------------------------------------------------------------------
!
      end module set_surface_bc
