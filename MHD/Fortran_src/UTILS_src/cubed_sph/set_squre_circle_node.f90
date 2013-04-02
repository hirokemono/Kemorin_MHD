!set_cube_surface_edge.f90
!      module set_squre_circle_node
!
!      Written by H. Matsui
!      Modified by H. Matsui on Oct., 2007
!
!      subroutine set_square_node(num_h, nnod_sf, inod,                 &
!     &          cube_size, x_node, xyz_sf)
!      subroutine set_square_edge(num_h, iele, inod, h_edge,            &
!     &          nnod_sf, ntot_edge, xyz_sf, iedge_20)
!
      module set_squre_circle_node
!
      use m_precision
!
      use m_constants
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_square_node(num_h, nnod_sf, inod,                  &
     &          cube_size, x_node, xyz_sf)
!
      integer(kind = kint), intent(in) :: num_h
      integer(kind = kint), intent(in) :: nnod_sf
      real(kind = kreal), intent(in) :: cube_size
      real(kind = kreal), intent(in) :: x_node(num_h+1)
!
      integer(kind = kint), intent(inout) :: inod
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf,3)
!
      integer(kind = kint) :: ix, iy, iz
!
!
      iz = 1
        do ix = 1, num_h
          inod = inod + 1
          xyz_sf(inod,1) =  x_node(ix)
          xyz_sf(inod,2) = -cube_size
          xyz_sf(inod,3) =  zero
        end do
        do iy = 1, num_h
          inod = inod + 1
          xyz_sf(inod,1) =  cube_size
          xyz_sf(inod,2) =  x_node(iy)
          xyz_sf(inod,3) =  zero
        end do
        do ix = 1, num_h
          inod = inod + 1
          xyz_sf(inod,1) =  x_node(num_h+2-ix)
          xyz_sf(inod,2) =  cube_size
          xyz_sf(inod,3) =  zero
        end do
        do iy = 1, num_h
          inod = inod + 1
          xyz_sf(inod,1) = -cube_size
          xyz_sf(inod,2) =  x_node(num_h+2-iy)
          xyz_sf(inod,3) =  zero
        end do
!
      end subroutine set_square_node
!
!   --------------------------------------------------------------------
!
      subroutine set_square_edge(num_h, iele, inod, h_edge,             &
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
      integer(kind = kint) :: ix, iy, iz, inod0, inod1, nsurf_o
!
!   side horizontal rod
!
      nsurf_o = izero
      iz = ione
!
        do ix = 1, num_h
          inod1 = nsurf_o + (4*num_h) * (iz-1) + ix
          iele = iele + 1
          inod = inod + 1
          iedge_20(iele,1) = inod1
          iedge_20(iele,2) = inod
          iedge_20(iele,3) = inod1 + 1
          write(*,*) 'iedge', iele, iedge_20(iele,1:3)
          xyz_sf(inod,1) = h_edge(ix)
          xyz_sf(inod,2) = xyz_sf(iedge_20(iele,1),2)
          xyz_sf(inod,3) = zero
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
          xyz_sf(inod,3) = zero
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
          xyz_sf(inod,3) = zero
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
          xyz_sf(inod,3) = zero
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
        xyz_sf(inod,3) = zero
!
      end subroutine set_square_edge
!
!   --------------------------------------------------------------------
!
      end module set_squre_circle_node
