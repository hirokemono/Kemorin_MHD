!set_cube_surface_edge.f90
!      module set_cube_surface_edge
!
!      Written by H. Matsui
!      Modified by H. Matsui on Oct., 2007
!
!      subroutine set_bottom_squre_edge_x(num_h, iele, inod, h_edge,    &
!     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!      subroutine set_bottom_squre_edge_y(num_h, iele, inod, h_edge,    &
!     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
!      subroutine set_bottom_vert_edge(num_h, iele, inod, z_edge,       &
!     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!      subroutine set_wall_vert_edge(num_h, num_v, iele, inod, z_edge,  &
!     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!      subroutine set_top_vert_edge(num_h, num_v, iele, inod, z_edge,   &
!     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
!      subroutine set_side_horiz_edge(num_h, num_v, iele, inod, h_edge, &
!     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
!      subroutine set_top_squre_edge_x(num_h, num_v, iele, inod, h_edge,&
!     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!      subroutine set_top_squre_edge_y(num_h, num_v, iele, inod, h_edge,&
!     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
      module set_cube_surface_edge
!
      use m_precision
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_bottom_squre_edge_x(num_h, iele, inod, h_edge,     &
     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
      integer(kind = kint), intent(in) :: num_h
      integer(kind = kint), intent(in) :: nnod_sf, ntot_edge
      real(kind = kreal), intent(in) :: h_edge(num_h)
!
      integer(kind = kint), intent(inout) :: iele, inod
      integer(kind = kint), intent(inout) :: iedge_20(ntot_edge,3)
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf,3)
!
      integer(kind = kint) :: ix, iy, inod4
!
!   bottom surface (z = -cube_size)
!
      do iy = 1, num_h+1
        do ix = 1, num_h
          inod4 = (num_h+1)*(iy-1) + ix
          iele = iele + 1
          inod = inod + 1
          iedge_20(iele,1) = inod4 + 1
          iedge_20(iele,2) = inod
          iedge_20(iele,3) = inod4
          xyz_sf(inod,1) = h_edge(ix)
          xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
          xyz_sf(inod,3) = xyz_sf(iedge_20(iele,1),3)
        end do
      end do
!
      end subroutine set_bottom_squre_edge_x
!
!   --------------------------------------------------------------------
!
      subroutine set_bottom_squre_edge_y(num_h, iele, inod, h_edge,     &
     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
      integer(kind = kint), intent(in) :: num_h
      integer(kind = kint), intent(in) :: nnod_sf, ntot_edge
      real(kind = kreal), intent(in) :: h_edge(num_h)
!
      integer(kind = kint), intent(inout) :: iele, inod
      integer(kind = kint), intent(inout) :: iedge_20(ntot_edge,3)
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf,3)
!
      integer(kind = kint) :: ix, iy, inod4
!
!   bottom surface (z = -cube_size)
!
      do iy = 1, num_h
        do ix = 1, num_h+1
          inod4 = (num_h+1)*(iy-1) + ix
          iele = iele + 1
          inod = inod + 1
          iedge_20(iele,1) = inod4 + (num_h+1)
          iedge_20(iele,2) = inod
          iedge_20(iele,3) = inod4
          xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
          xyz_sf(inod,2) = h_edge(iy)
          xyz_sf(inod,3) = xyz_sf(iedge_20(iele,1),3)
        end do
      end do
!
      end subroutine set_bottom_squre_edge_y
!
!   --------------------------------------------------------------------
!
      subroutine set_bottom_vert_edge(num_h, iele, inod, z_edge,        &
     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
      integer(kind = kint), intent(in) :: num_h
      integer(kind = kint), intent(in) :: nnod_sf, ntot_edge
      real(kind = kreal), intent(in) :: z_edge
!
      integer(kind = kint), intent(inout) :: iele, inod
      integer(kind = kint), intent(inout) :: iedge_20(ntot_edge,3)
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf,3)
!
      integer(kind = kint) :: ix, iy, nsurf_o
!
!    bottom vertical rod
!
      nsurf_o = (num_h+1)*(num_h+1)
!
      do ix = 1, num_h
        iele = iele + 1
        inod = inod + 1
        iedge_20(iele,1) = ix
        iedge_20(iele,2) = inod
        iedge_20(iele,3) = nsurf_o + ix
        xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
        xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
        xyz_sf(inod,3) = z_edge
      end do
!
      do iy = 1, num_h
        iele = iele + 1
        inod = inod + 1
        iedge_20(iele,1) = (num_h+1)*iy
        iedge_20(iele,2) = inod
        iedge_20(iele,3) = nsurf_o + num_h + iy
        xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
        xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
        xyz_sf(inod,3) = z_edge
      end do
!
      do ix = 1, num_h
        iele = iele + 1
        inod = inod + 1
        iedge_20(iele,1) = nsurf_o - ix + 1
        iedge_20(iele,2) = inod
        iedge_20(iele,3) = nsurf_o + 2*num_h + ix
        xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
        xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
        xyz_sf(inod,3) = z_edge
      end do
!
      do iy = 1, num_h
        iele = iele + 1
        inod = inod + 1
        iedge_20(iele,1) = (num_h+1)*(num_h-iy+1) + 1
        iedge_20(iele,2) = inod
        iedge_20(iele,3) = nsurf_o + 3*num_h + iy
        xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
        xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
        xyz_sf(inod,3) = z_edge
      end do
!
      end subroutine set_bottom_vert_edge
!
!   --------------------------------------------------------------------
!
      subroutine set_wall_vert_edge(num_h, num_v, iele, inod, z_edge,   &
     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
      integer(kind = kint), intent(in) :: num_h, num_v
      integer(kind = kint), intent(in) :: nnod_sf, ntot_edge
      real(kind = kreal), intent(in) :: z_edge(num_v)
!
      integer(kind = kint), intent(inout) :: iele, inod
      integer(kind = kint), intent(inout) :: iedge_20(ntot_edge,3)
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf,3)
!
      integer(kind = kint) :: iz, ixy, inod1, nsurf_o
!
!    bottom vertical rod
!
      nsurf_o = (num_h+1)*(num_h+1)
!
      do iz = 1, num_v-2
        do ixy = 1, 4*num_h
          inod1 = nsurf_o + (4*num_h) * (iz-1) + ixy
!
          iele = iele + 1
          inod = inod + 1
          iedge_20(iele,1) = inod1
          iedge_20(iele,2) = inod
          iedge_20(iele,3) = inod1 + 4*num_h
          xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
          xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
          xyz_sf(inod,3) = z_edge(iz+1)
!
        end do
      end do
!
      end subroutine set_wall_vert_edge
!
!   --------------------------------------------------------------------
!
      subroutine set_top_vert_edge(num_h, num_v, iele, inod, z_edge,    &
     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
      integer(kind = kint), intent(in) :: num_h, num_v
      integer(kind = kint), intent(in) :: nnod_sf, ntot_edge
      real(kind = kreal), intent(in) :: z_edge
!
      integer(kind = kint), intent(inout) :: iele, inod
      integer(kind = kint), intent(inout) :: iedge_20(ntot_edge,3)
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf,3)
!
      integer(kind = kint) :: ix, iy, inod0, nsurf_o
!
!    top vertical rod
!
      nsurf_o = (num_h+1)*(num_h+1)
      inod0 = nsurf_o + 4*(num_h)*(num_v-1)
!
      do ix = 1, num_h
        iele = iele + 1
        inod = inod + 1
        iedge_20(iele,1) = inod0 - 4*num_h + ix
        iedge_20(iele,2) = inod
        iedge_20(iele,3) = inod0 + ix
        xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
        xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
        xyz_sf(inod,3) = z_edge
      end do
!
      do iy = 1, num_h
        iele = iele + 1
        inod = inod + 1
        iedge_20(iele,1) = inod0 - 3*num_h + iy
        iedge_20(iele,2) = inod
        iedge_20(iele,3) = inod0 + (num_h+1)*iy
        xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
        xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
        xyz_sf(inod,3) = z_edge
      end do
!
      do ix = 1, num_h
        iele = iele + 1
        inod = inod + 1
        iedge_20(iele,1) = inod0 - 2*num_h + ix
        iedge_20(iele,2) = inod
        iedge_20(iele,3) = inod0 + nsurf_o - ix + 1
        xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
        xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
        xyz_sf(inod,3) = z_edge
      end do
!
      do iy = 1, num_h
        iele = iele + 1
        inod = inod + 1
        iedge_20(iele,1) = inod0 - num_h + iy
        iedge_20(iele,2) = inod
        iedge_20(iele,3) = inod0 + (num_h+1)*(num_h-iy+1) + 1
        xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
        xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
        xyz_sf(inod,3) = z_edge
      end do
!
      end subroutine set_top_vert_edge
!
!   --------------------------------------------------------------------
!
      subroutine set_side_horiz_edge(num_h, num_v, iele, inod, h_edge,  &
     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
      integer(kind = kint), intent(in) :: num_h, num_v
      integer(kind = kint), intent(in) :: nnod_sf, ntot_edge
      real(kind = kreal), intent(in) :: h_edge(num_h)
!
      integer(kind = kint), intent(inout) :: iele, inod
      integer(kind = kint), intent(inout) :: iedge_20(ntot_edge,3)
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf,3)
!
      integer(kind = kint) :: ix, iy, iz, inod0, inod1, nsurf_o
!
!   side horizontal rod
!
      nsurf_o = (num_h+1)*(num_h+1)
      do iz = 1, num_v-1
!
        do ix = 1, num_h
          inod1 = nsurf_o + (4*num_h) * (iz-1) + ix
          iele = iele + 1
          inod = inod + 1
          iedge_20(iele,1) = inod1
          iedge_20(iele,2) = inod
          iedge_20(iele,3) = inod1 + 1
          xyz_sf(inod,1) = h_edge(ix)
          xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
          xyz_sf(inod,3) = xyz_sf(iedge_20(iele,1),3)
        end do
        do iy = 1, num_h
          inod1 = nsurf_o + (4*num_h) * (iz-1) + num_h+iy
          iele = iele + 1
          inod = inod + 1
          iedge_20(iele,1) = inod1
          iedge_20(iele,2) = inod
          iedge_20(iele,3) = inod1 + 1
          xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
          xyz_sf(inod,2) = h_edge(iy)
          xyz_sf(inod,3) = xyz_sf(iedge_20(iele,1),3)
        end do
        do ix = 1, num_h
          inod1 = nsurf_o + (4*num_h) * (iz-1) + 2*num_h+ix
          iele = iele + 1
          inod = inod + 1
          iedge_20(iele,1) = inod1
          iedge_20(iele,2) = inod
          iedge_20(iele,3) = inod1 + 1
          xyz_sf(inod,1) = h_edge(num_h+1-ix)
          xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
          xyz_sf(inod,3) = xyz_sf(iedge_20(iele,1),3)
        end do
        do iy = 1, num_h-1
          inod1 = nsurf_o + (4*num_h) * (iz-1) + 3*num_h+iy
          iele = iele + 1
          inod = inod + 1
          iedge_20(iele,1) = inod1
          iedge_20(iele,2) = inod
          iedge_20(iele,3) = inod1 + 1
          xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
          xyz_sf(inod,2) = h_edge(num_h+1-iy)
          xyz_sf(inod,3) = xyz_sf(iedge_20(iele,1),3)
        end do
!
        inod0 = nsurf_o + (4*num_h) * (iz-1)
        iele = iele + 1
        inod = inod + 1
        iedge_20(iele,1) = inod0     + 4*num_h
        iedge_20(iele,2) = inod
        iedge_20(iele,3) = inod0 + 1
        xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
        xyz_sf(inod,2) = h_edge(1)
        xyz_sf(inod,3) = xyz_sf(iedge_20(iele,1),3)
      end do
!
      end subroutine set_side_horiz_edge
!
!   --------------------------------------------------------------------
!
      subroutine set_top_squre_edge_x(num_h, num_v, iele, inod, h_edge, &
     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
      integer(kind = kint), intent(in) :: num_h, num_v
      integer(kind = kint), intent(in) :: nnod_sf, ntot_edge
      real(kind = kreal), intent(in) :: h_edge(num_h)
!
      integer(kind = kint), intent(inout) :: iele, inod
      integer(kind = kint), intent(inout) :: iedge_20(ntot_edge,3)
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf,3)
!
      integer(kind = kint) :: ix, iy, inod0, inod1, nsurf_o
!
!   top surface rod
!
      nsurf_o = (num_h+1)*(num_h+1)
      inod0 = nsurf_o + 4*num_h*(num_v-1)
!
      do iy = 1, num_h+1
        do ix = 1, num_h
          inod1 = inod0 + (num_h+1)*(iy-1) + ix
          iele = iele + 1
          inod = inod + 1
          iedge_20(iele,1) = inod1
          iedge_20(iele,2) = inod
          iedge_20(iele,3) = inod1 + 1
          xyz_sf(inod,1) = h_edge(ix)
          xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
          xyz_sf(inod,3) = xyz_sf(iedge_20(iele,1),3)
        end do
      end do
!
      end subroutine set_top_squre_edge_x
!
!   --------------------------------------------------------------------
!
      subroutine set_top_squre_edge_y(num_h, num_v, iele, inod, h_edge, &
     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
      integer(kind = kint), intent(in) :: num_h, num_v
      integer(kind = kint), intent(in) :: nnod_sf, ntot_edge
      real(kind = kreal), intent(in) :: h_edge(num_h)
!
      integer(kind = kint), intent(inout) :: iele, inod
      integer(kind = kint), intent(inout) :: iedge_20(ntot_edge,3)
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf,3)
!
      integer(kind = kint) :: ix, iy, inod0, inod1, nsurf_o
!
!   top surface rod
!
      nsurf_o = (num_h+1)*(num_h+1)
      inod0 = nsurf_o + 4*num_h*(num_v-1)
!
      do iy = 1, num_h
        do ix = 1, num_h+1
          inod1 = inod0 + (num_h+1)*(iy-1) + ix
          iele = iele + 1
          inod = inod + 1
          iedge_20(iele,1) = inod1
          iedge_20(iele,2) = inod
          iedge_20(iele,3) = inod1     + (num_h+1)
          xyz_sf(inod,1) = xyz_sf(iedge_20(iele,1),1)
          xyz_sf(inod,2) = h_edge(iy)
          xyz_sf(inod,3) = xyz_sf(iedge_20(iele,1),3)
        end do
      end do
!
      end subroutine set_top_squre_edge_y
!
!   --------------------------------------------------------------------
!
      end module set_cube_surface_edge
