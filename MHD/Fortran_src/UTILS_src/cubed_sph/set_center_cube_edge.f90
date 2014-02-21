!set_center_cube_edge.f90
!      module set_center_cube_edge
!
!      Written by H. Matsui
!     Modified by H. Matsui on Oct., 2007
!     Modified by H. Matsui on Dec., 2011
!
!      subroutine set_center_cube_x_edge(inod, ifile, num_h, num_v,     &
!     &          x_node, x_edge, v_node)
!      subroutine set_center_cube_y_edge(inod, ifile, num_h, num_v,     &
!     &          x_node, x_edge, v_node)
!      subroutine set_center_cube_z_edge(inod, ifile, num_h, num_v,     &
!     &          x_node, v_edge)
!
!      subroutine set_center_bottom_edge(inod, ifile, num_h,            &
!     &          x_node, v_edge)
!      subroutine set_center_side_edge(inod, ifile, num_h, num_v,       &
!     &          x_node, x_edge, v_node)
!
!      subroutine set_center_surf_edge(inod, ifile, nnod_sf, nedge_sf,  &
!     &          xyz_sf)
!
      module set_center_cube_edge
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_center_cube_x_edge(inod, ifile, num_h, num_v,      &
     &          x_node, x_edge, v_node)
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: num_h, num_v
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      real(kind = kreal), intent(in) :: x_edge(num_h)
      real(kind = kreal), intent(in) :: v_node(num_v+1)
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: ix, iy, iz
!
!
      do iz = 1, num_v-1
        do iy = 1, num_h-1
          do ix = 1, num_h-2
            inod = inod + 1
            write (ifile,'(i10,1p3E25.15e3)') inod,                     &
     &           x_edge(ix+1), x_node(iy+1), v_node(iz+1)
          end do
        end do
      end do
!
      end subroutine set_center_cube_x_edge
!
!   --------------------------------------------------------------------
!
      subroutine set_center_cube_y_edge(inod, ifile, num_h, num_v,      &
     &          x_node, x_edge, v_node)
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: num_h, num_v
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      real(kind = kreal), intent(in) :: x_edge(num_h)
      real(kind = kreal), intent(in) :: v_node(num_v+1)
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: ix, iy, iz
!
!
      do iz = 1, num_v-1
        do iy = 1, num_h-2
          do ix = 1, num_h-1
            inod = inod + 1
            write (ifile,'(i10,1p3E25.15e3)') inod,                     &
     &           x_node(ix+1), x_edge(iy+1), v_node(iz+1)
          end do
        end do
      end do
!
      end subroutine set_center_cube_y_edge
!
!   --------------------------------------------------------------------
!
      subroutine set_center_cube_z_edge(inod, ifile, num_h, num_v,      &
     &          x_node, v_edge)
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: num_h, num_v
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      real(kind = kreal), intent(in) :: v_edge(num_v)
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: ix, iy, iz
!
!
      do iz = 1, num_v-2
        do iy = 1, num_h-1
          do ix = 1, num_h-1
            inod = inod + 1
            write (ifile,'(i10,1p3E25.15e3)') inod,                     &
     &           x_node(ix+1), x_node(iy+1), v_edge(iz+1)
          end do
        end do
      end do
!
      end subroutine set_center_cube_z_edge
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_center_bottom_edge(inod, ifile, num_h,             &
     &          x_node, v_edge)
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: num_h
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      real(kind = kreal), intent(in) :: v_edge
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: ix, iy
!
!
      do iy = 1, num_h-1
        do ix = 1, num_h-1
          inod = inod + 1
          write (ifile,'(i10,1p3E25.15e3)') inod,                       &
     &         x_node(ix+1), x_node(iy+1), v_edge
        end do
      end do
!
      end subroutine set_center_bottom_edge
!
!   --------------------------------------------------------------------
!
      subroutine set_center_side_edge(inod, ifile, num_h, num_v,        &
     &          x_node, x_edge, v_node)
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: num_h, num_v
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      real(kind = kreal), intent(in) :: x_edge(num_h)
      real(kind = kreal), intent(in) :: v_node(num_v+1)
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: ix, iy, iz, inum
!
!
      do iz = 1, num_v-1
!
!  surface (y = -cube_size)
        do ix = 1, num_h-1
          inod = inod + 1
          write (ifile,'(i10,1p3E25.15e3)') inod,                       &
     &         x_node(ix+1), x_edge(1), v_node(iz+1)
        end do
!
!  surface (x=cube_size)
        do iy = 1, num_h-1
          inod = inod + 1
          write (ifile,'(i10,1p3E25.15e3)') inod,                       &
     &         x_edge(num_h), x_node(iy+1), v_node(iz+1)
        end do
!
!  surface (y=cube_size)
        do ix = 1, num_h-1
          inum = (num_h+1) - ix
          inod = inod + 1
          write (ifile,'(i10,1p3E25.15e3)') inod,                       &
     &         x_node(inum), x_edge(num_h), v_node(iz+1)
        end do
!
!  surface (x=-cube_size)
        do iy = 1, num_h-1
          inum = (num_h+1) - iy
          inod = inod + 1
          write (ifile,'(i10,1p3E25.15e3)') inod,                       &
     &         x_edge(1), x_node(inum), v_node(iz+1)
        end do
      end do
!
      end subroutine set_center_side_edge
!
!   --------------------------------------------------------------------
!
      subroutine set_center_surf_edge(inod, ifile, nnod_sf, nedge_sf,   &
     &          xyz_sf)
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: nnod_sf, nedge_sf
      integer(kind = kint), intent(inout) :: inod
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf+nedge_sf,3)
!
      integer(kind = kint) :: inum, irod
!
!
      do irod = 1, nedge_sf
         inum = nnod_sf + irod
         inod = inod + 1
         write (ifile,'(i10,1p3E25.15e3)') inod, xyz_sf(inum,1:3)
      end do
!
      end subroutine set_center_surf_edge
!
!   --------------------------------------------------------------------
!
      end module set_center_cube_edge
