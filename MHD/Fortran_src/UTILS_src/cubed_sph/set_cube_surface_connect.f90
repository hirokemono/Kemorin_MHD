!set_cube_surface_connect.f90
!      module set_cube_surface_connect
!
!      Written by H. Matsui
!      Modified by H. Matsui on Oct., 2007
!
!      subroutine set_bottom_surf_connect(num_h, iele, nnod_sf,         &
!     &          ntot, ie_sf20)
!
!      subroutine set_bottom_side_connect(num_h, num_v, iele, nnod_sf,  &
!     &          ntot, ie_sf20)
!      subroutine set_side_wall_connect(num_h, num_v, iele, nnod_sf,    &
!     &          ntot, ie_sf20)
!      subroutine set_top_side_connect(num_h, num_v, iele, nnod_sf,     &
!     &          ntot, ie_sf20)
!
!      subroutine set_top_surf_connect(num_h, num_v, iele, nnod_sf,     &
!     &          ntot, ie_sf20)
!
      module set_cube_surface_connect
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
      subroutine set_bottom_surf_connect(num_h, iele, nnod_sf,          &
     &          ntot, ie_sf20)
!
      integer(kind = kint), intent(in) :: num_h
      integer(kind = kint), intent(in) :: ntot, nnod_sf
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint), intent(inout) :: ie_sf20(ntot,8)
!
      integer(kind = kint) :: ix, iy, inod4
      integer(kind = kint) :: iedge3, iedge4
!
!
      do iy = 1, num_h
        do ix = 1, num_h
          iele = iele + 1
!
          inod4 = (num_h+1)*(iy-1) + ix
          ie_sf20(iele,1) = inod4     + (num_h+1)
          ie_sf20(iele,2) = inod4 + 1 + (num_h+1)
          ie_sf20(iele,3) = inod4 + 1
          ie_sf20(iele,4) = inod4
!
          iedge3 = num_h*(iy-1) + ix
          iedge4 = num_h*(num_h+1) + (num_h+1)*(iy-1) + ix
          ie_sf20(iele,5) = nnod_sf + iedge3 + num_h
          ie_sf20(iele,6) = nnod_sf + iedge4 + 1
          ie_sf20(iele,7) = nnod_sf + iedge3
          ie_sf20(iele,8) = nnod_sf + iedge4
        end do
      end do
!
      end subroutine set_bottom_surf_connect
!
! -------------------------------------------------------------------
!
      subroutine set_bottom_side_connect(num_h, num_v, iele, nnod_sf,   &
     &          ntot, ie_sf20)
!
      integer(kind = kint), intent(in) :: num_h, num_v
      integer(kind = kint), intent(in) :: ntot, nnod_sf
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint), intent(inout) :: ie_sf20(ntot,8)
!
      integer(kind = kint) :: ix, iy, inod1, inod4, nsurf_o
      integer(kind = kint) :: iedge1, iedge2, iedge3, iedge4
!
!
      nsurf_o = (num_h+1)*(num_h+1)
      do ix = 1, num_h
        iele = iele + 1
!
        inod1 = ix
        inod4 = nsurf_o + ix
        ie_sf20(iele,1) = inod1
        ie_sf20(iele,2) = inod1 + 1
        ie_sf20(iele,3) = inod4 + 1
        ie_sf20(iele,4) = inod4
!
        iedge1 = ix
        iedge3 = (2*num_h+4*num_v+2)*num_h + ix
        iedge4 = (2*num_h+2)*num_h + ix
        ie_sf20(iele,5) = nnod_sf + iedge1
        ie_sf20(iele,6) = nnod_sf + iedge4 + 1
        ie_sf20(iele,7) = nnod_sf + iedge3
        ie_sf20(iele,8) = nnod_sf + iedge4
      end do
!
      do iy = 1, num_h
        iele = iele + 1
!
        inod1 = (num_h+1)*iy
        inod4 = nsurf_o + num_h + iy
        ie_sf20(iele,1) = inod1
        ie_sf20(iele,2) = inod1 + (num_h+1)
        ie_sf20(iele,3) = inod4 + 1
        ie_sf20(iele,4) = inod4
!
        iedge1 = (  num_h+1)*num_h + (num_h+1)*iy
        iedge3 = (2*num_h+4*num_v+3)*num_h + iy
        iedge4 = (2*num_h+3)*num_h + iy
        ie_sf20(iele,5) = nnod_sf + iedge1
        ie_sf20(iele,6) = nnod_sf + iedge4 + 1
        ie_sf20(iele,7) = nnod_sf + iedge3
        ie_sf20(iele,8) = nnod_sf + iedge4
      end do
!
      do ix = 1, num_h
        iele = iele + 1
!
        inod1 = nsurf_o - ix + 1
        inod4 = nsurf_o + 2*num_h + ix
        ie_sf20(iele,1) = inod1
        ie_sf20(iele,2) = inod1 - 1
        ie_sf20(iele,3) = inod4 + 1
        ie_sf20(iele,4) = inod4
!
        iedge1 = (  num_h+1)*num_h - ix+1
        iedge3 = (2*num_h+4*num_v+4)*num_h + ix
        iedge4 = (2*num_h+4)*num_h + ix
        ie_sf20(iele,5) = nnod_sf + iedge1
        ie_sf20(iele,6) = nnod_sf + iedge4 + 1
        ie_sf20(iele,7) = nnod_sf + iedge3
        ie_sf20(iele,8) = nnod_sf + iedge4
      end do
!
      do iy = 1, num_h-1
        iele = iele + 1
!
        inod1 = (num_h+1)*(num_h-iy+1) + 1
        inod4 = nsurf_o + 3*num_h + iy
        ie_sf20(iele,1) = inod1
        ie_sf20(iele,2) = inod1 - (num_h+1)
        ie_sf20(iele,3) = inod4 + 1
        ie_sf20(iele,4) = inod4
!
        iedge1 = (  num_h+1)*num_h + (num_h+1)*(num_h-iy) + 1
        iedge3 = (2*num_h+4*num_v+5)*num_h + iy
        iedge4 = (2*num_h+5)*num_h + iy
        ie_sf20(iele,5) = nnod_sf + iedge1
        ie_sf20(iele,6) = nnod_sf + iedge4 + 1
        ie_sf20(iele,7) = nnod_sf + iedge3
        ie_sf20(iele,8) = nnod_sf + iedge4
      end do
!
      iele = iele + 1
      ie_sf20(iele,1) =  (num_h+1) + 1
      ie_sf20(iele,2) =  1
      ie_sf20(iele,3) =  nsurf_o + 1
      ie_sf20(iele,4) =  nsurf_o + 4*num_h
!
      iedge1 =   (num_h+1)*num_h + 1
      iedge2 = (2*num_h+2)*num_h + 1
      iedge3 = (2*num_h+4*num_v+6)*num_h
      iedge4 = (2*num_h+6)*num_h
      ie_sf20(iele,5) =  nnod_sf + iedge1
      ie_sf20(iele,6) =  nnod_sf + iedge2
      ie_sf20(iele,7) =  nnod_sf + iedge3
      ie_sf20(iele,8) =  nnod_sf + iedge4
!
      end subroutine set_bottom_side_connect
!
! -------------------------------------------------------------------
!
      subroutine set_side_wall_connect(num_h, num_v, iele, nnod_sf,     &
     &          ntot, ie_sf20)
!
      integer(kind = kint), intent(in) :: num_h, num_v
      integer(kind = kint), intent(in) :: ntot, nnod_sf
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint), intent(inout) :: ie_sf20(ntot,8)
!
      integer(kind = kint) :: ixy, iz, inod1, inod0, nsurf_o
      integer(kind = kint) :: iedge1, iedge2, iedge4
!
!
      nsurf_o = (num_h+1)*(num_h+1)
!
      do iz = 1, num_v-2
        do ixy = 1, 4*num_h - 1
          iele = iele + 1
!
          inod1 = nsurf_o + 4*num_h*(iz-1) + ixy
          ie_sf20(iele,1) = inod1
          ie_sf20(iele,2) = inod1 + 1
          ie_sf20(iele,3) = inod1 + 1 + 4*num_h
          ie_sf20(iele,4) = inod1     + 4*num_h
!
          iedge1 = (2*num_h+4*num_v-2)*num_h + 4*iz*num_h + ixy
          iedge4 = (2*num_h+2)*num_h + 4*iz*num_h + ixy
          ie_sf20(iele,5) =  nnod_sf + iedge1
          ie_sf20(iele,6) =  nnod_sf + iedge4 + 1
          ie_sf20(iele,7) =  nnod_sf + iedge1 + 4*num_h
          ie_sf20(iele,8) =  nnod_sf + iedge4
        end do
!
        iele = iele + 1
!
        inod0 = nsurf_o + (4*num_h) * (iz-1)
        ie_sf20(iele,1) = inod0     + 4*num_h
        ie_sf20(iele,2) = inod0 + 1
        ie_sf20(iele,3) = inod0 + 1 + 4*num_h
        ie_sf20(iele,4) = inod0     + 4*num_h + 4*num_h
!
        iedge1 = (2*num_h+4*num_v+2)*num_h + 4*iz*num_h
        iedge2 = (2*num_h+2)*num_h + 4*iz*num_h + 1
        iedge4 = (2*num_h+6)*num_h + 4*iz*num_h
        ie_sf20(iele,5) =  nnod_sf + iedge1
        ie_sf20(iele,6) =  nnod_sf + iedge2
        ie_sf20(iele,7) =  nnod_sf + iedge1 + 4*num_h
        ie_sf20(iele,8) =  nnod_sf + iedge4
      end do
!
      end subroutine set_side_wall_connect
!
! -------------------------------------------------------------------
!
      subroutine set_top_side_connect(num_h, num_v, iele, nnod_sf,      &
     &          ntot, ie_sf20)
!
      integer(kind = kint), intent(in) :: num_h, num_v
      integer(kind = kint), intent(in) :: ntot, nnod_sf
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint), intent(inout) :: ie_sf20(ntot,8)
!
      integer(kind = kint) :: ix, iy, inod1, inod0, inod4, nsurf_o
      integer(kind = kint) :: iedge1, iedge2, iedge3, iedge4
!
!
      nsurf_o = (num_h+1)*(num_h+1)
      inod0 = nsurf_o + 4*num_h*(num_v-1)
!
      do ix = 1, num_h
        iele = iele + 1
!
        inod1 = inod0 - 4*num_h + ix
        inod4 = inod0 + ix
        ie_sf20(iele,1) = inod1
        ie_sf20(iele,2) = inod1 + 1
        ie_sf20(iele,3) = inod4 + 1
        ie_sf20(iele,4) = inod4
!
        iedge1 = (2*num_h+8*num_v-6)*num_h + ix
        iedge4 = (2*num_h+4*num_v-2)*num_h + ix
        iedge3 = (2*num_h+8*num_v-2)*num_h + ix
        ie_sf20(iele,5) =  nnod_sf + iedge1
        ie_sf20(iele,6) =  nnod_sf + iedge4 + 1
        ie_sf20(iele,7) =  nnod_sf + iedge3
        ie_sf20(iele,8) =  nnod_sf + iedge4
      end do
!
      do iy = 1, num_h
        iele = iele + 1
!
        inod1 = inod0 - 3*num_h + iy
        inod4 = inod0 + (num_h+1)*iy
        ie_sf20(iele,1) = inod1
        ie_sf20(iele,2) = inod1 + 1
        ie_sf20(iele,3) = inod4 + (num_h+1)
        ie_sf20(iele,4) = inod4
!
        iedge1 = (2*num_h+8*num_v-5)*num_h + iy
        iedge4 = (2*num_h+4*num_v-1)*num_h + iy
        iedge3 = (3*num_h+8*num_v-1)*num_h + (num_h+1)*iy
        ie_sf20(iele,5) =  nnod_sf + iedge1
        ie_sf20(iele,6) =  nnod_sf + iedge4 + 1
        ie_sf20(iele,7) =  nnod_sf + iedge3
        ie_sf20(iele,8) =  nnod_sf + iedge4
      end do
!
      do ix = 1, num_h
        iele = iele + 1
!
        inod1 = inod0 - 2*num_h + ix
        inod4 = inod0 + nsurf_o - ix + 1
        ie_sf20(iele,1) = inod1
        ie_sf20(iele,2) = inod1 + 1
        ie_sf20(iele,3) = inod4 - 1
        ie_sf20(iele,4) = inod4
!
        iedge1 = (2*num_h+8*num_v-4)*num_h + ix
        iedge4 = (2*num_h+4*num_v  )*num_h + ix
        iedge3 = (3*num_h+8*num_v-1)*num_h - ix+1
        ie_sf20(iele,5) =  nnod_sf + iedge1
        ie_sf20(iele,6) =  nnod_sf + iedge4 + 1
        ie_sf20(iele,7) =  nnod_sf + iedge3
        ie_sf20(iele,8) =  nnod_sf + iedge4
      end do
!
      do iy = 1, num_h-1
        iele = iele + 1
!
        inod1 = inod0 - num_h + iy
        inod4 = inod0 + (num_h+1)*(num_h-iy+1) + 1
        ie_sf20(iele,1) = inod1
        ie_sf20(iele,2) = inod1 + 1
        ie_sf20(iele,3) = inod4 - (num_h+1)
        ie_sf20(iele,4) = inod4
!
        iedge1 = (2*num_h+8*num_v-3)*num_h + iy
        iedge4 = (2*num_h+4*num_v+1)*num_h + iy
        iedge3 = (3*num_h+8*num_v-1)*num_h + (num_h+1)*(num_h-iy) + 1
        ie_sf20(iele,5) =  nnod_sf + iedge1
        ie_sf20(iele,6) =  nnod_sf + iedge4 + 1
        ie_sf20(iele,7) =  nnod_sf + iedge3
        ie_sf20(iele,8) =  nnod_sf + iedge4
      end do
!
        iele = iele + 1
        ie_sf20(iele,1) = inod0
        ie_sf20(iele,2) = inod0 - 4*num_h + 1
        ie_sf20(iele,3) = inod0 + 1
        ie_sf20(iele,4) = inod0 + (num_h+1) + 1
!
        iedge1 = (2*num_h+8*num_v-2)*num_h
        iedge2 = (2*num_h+4*num_v-2)*num_h + 1
        iedge4 = (2*num_h+4*num_v+2)*num_h
        iedge3 = (3*num_h+8*num_v-1)*num_h + 1
        ie_sf20(iele,5) =  nnod_sf + iedge1
        ie_sf20(iele,6) =  nnod_sf + iedge2
        ie_sf20(iele,7) =  nnod_sf + iedge3
        ie_sf20(iele,8) =  nnod_sf + iedge4
!
      end subroutine set_top_side_connect
!
! -------------------------------------------------------------------
!
      subroutine set_top_surf_connect(num_h, num_v, iele, nnod_sf,      &
     &          ntot, ie_sf20)
!
      integer(kind = kint), intent(in) :: num_h, num_v
      integer(kind = kint), intent(in) :: ntot, nnod_sf
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint), intent(inout) :: ie_sf20(ntot,8)
!
      integer(kind = kint) :: ix, iy, inod1, inod0, nsurf_o
      integer(kind = kint) :: iedge1, iedge4
!
!
      nsurf_o = (num_h+1)*(num_h+1)
      inod0 = nsurf_o + 4*num_h*(num_v-1)
!
      do iy = 1, num_h
        do ix = 1, num_h
          iele = iele + 1
!
          inod1 = inod0 + (num_h+1)*(iy-1) + ix
          ie_sf20(iele,1) = inod1
          ie_sf20(iele,2) = inod1 + 1
          ie_sf20(iele,3) = inod1 + 1 + (num_h+1)
          ie_sf20(iele,4) = inod1     + (num_h+1)
!
          iedge1 = (2*num_h+8*num_v-2)*num_h + num_h*(iy-1) + ix
          iedge4 = (3*num_h+8*num_v-1)*num_h + (num_h+1)*(iy-1) + ix
          ie_sf20(iele,5) =  nnod_sf + iedge1
          ie_sf20(iele,6) =  nnod_sf + iedge4 + 1
          ie_sf20(iele,7) =  nnod_sf + iedge1 + num_h
          ie_sf20(iele,8) =  nnod_sf + iedge4
        end do
      end do
!
      end subroutine set_top_surf_connect
!
! -------------------------------------------------------------------
!
      end module set_cube_surface_connect
