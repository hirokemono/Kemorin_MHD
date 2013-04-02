!cubed_sph_center_connect.f90
!      module cubed_sph_center_connect
!
!     Written by H. Matsui on Apr., 2003
!     Modified by H. Matsui on Oct., 2007
!
!      subroutine set_center_connect_quad(iele, ifile, ifile_20)
!      subroutine set_center_rect_connect_quad(iele, ifile, ifile_20)
!
      module cubed_sph_center_connect
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
      subroutine set_center_connect_quad(iele, ifile, ifile_20, numnod, &
     &          num_h, num_v)
!
      use set_corner_quad_connect
      use set_side_quad_connect
      use set_surface_quad_connect
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(inout) :: iele
!
      integer(kind = kint) :: ix, iy, iz
!
!  corner (x = y = z =-cube_size)
      call set_corner_quad_connect_1(iele, ifile, ifile_20,             &
     &    numnod, num_h, num_v)
!
!  edge (y = z = -cube_size)
      do ix = 1, num_h-2
        call set_side_quad_connect_1(iele, ifile, ifile_20,             &
     &    numnod, num_h, num_v, ix)
      end do
!
!  corner (x = cube_size, y = z = -cube_size)
      call set_corner_quad_connect_2(iele, ifile, ifile_20,             &
     &    numnod, num_h, num_v)
!
!
!  edge (x=-cube_size, z = -cube_size)
      do iy = 1, num_h-2
        call set_side_quad_connect_4(iele, ifile, ifile_20,             &
     &    numnod, num_h, num_v, iy)
!
!  bottom surface (z=-cube_size)
        do ix = 1, num_h-2
          call set_surface_quad_connect_5(iele, ifile, ifile_20,        &
     &        numnod, num_h, num_v, ix, iy)
        end do
!
!  edge (x=cube_size, z = -cube_size)
        call set_side_quad_connect_2(iele, ifile, ifile_20,             &
     &      numnod, num_h, num_v, iy)
      end do
!
!  corner (x=-cube_size, y=cube_size, z=-cube_size)
!
      call set_corner_quad_connect_4(iele, ifile, ifile_20,       &
     &    numnod, num_h, num_v)
!
!  edge (y=cube_size, z = -cube_size)
!
      do ix = 1, num_h-2
        call set_side_quad_connect_3(iele, ifile, ifile_20,             &
     &      numnod, num_h, num_v, ix)
      end do
!
!  corner (x=y=cube_size, z = -cube_size)
!
      call set_corner_quad_connect_3(iele, ifile, ifile_20,             &
     &    numnod, num_h, num_v)
!
!  edge (x = y = -cube_size)
      do iz = 1, num_v-2
        call set_side_quad_connect_9(iele, ifile, ifile_20,             &
     &      numnod, num_h, num_v, iz)
!
!  surface (y = -cube_size)
        do ix = 1, num_h-2
          call set_surface_quad_connect_3(iele, ifile, ifile_20,        &
     &       numnod, num_h, num_v, ix, iz)
        end do
!
!  edge (x = cube_size, y = -cube_size)
        call set_side_quad_connect_10(iele, ifile, ifile_20,            &
     &      numnod, num_h, num_v, iz)
!
!  surface (x=-cube_size)
        do iy = 1, num_h-2
          call set_surface_quad_connect_1(iele, ifile, ifile_20,        &
     &          numnod, num_h, num_v, iy, iz)
!  volume
          do ix = 1, num_h-2
            call set_body_quad_connect_0(iele, ifile, ifile_20,         &
     &          numnod, num_h, num_v, ix, iy, iz)
          end do
!
!  surface (x=cube_size)
          call set_surface_quad_connect_2(iele, ifile, ifile_20,        &
     &          numnod, num_h, num_v, iy, iz)
        end do
!
!  edge (x=-cube_size, y=cube_size)
        call set_side_quad_connect_12(iele, ifile, ifile_20,            &
     &          numnod, num_h, num_v, iz)
!
!  surface (y=cube_size)
        do ix = 1, num_h-2
          call set_surface_quad_connect_4(iele, ifile, ifile_20,        &
     &          numnod, num_h, num_v, ix, iz)
        end do
!
!  edge (x=y=cube_size)
        call set_side_quad_connect_11(iele, ifile, ifile_20,            &
     &          numnod, num_h, num_v, iz)
      end do
!
!  corner (x = y = -cube_size, z =cube_size)
      call set_corner_quad_connect_5(iele, ifile, ifile_20,             &
     &    numnod, num_h, num_v)
!
!  edge ( y = -cube_size, z = cube_size)
      do ix = 1, num_h-2
        call set_side_quad_connect_5(iele, ifile, ifile_20,             &
     &      numnod, num_h, num_v, ix)
      end do
!
!  corner (x = cube_size, y = -cube_size, z = cube_size)
      call set_corner_quad_connect_6(iele, ifile, ifile_20,             &
     &    numnod, num_h, num_v)
!
!  edge (x=-cube_size, z = cube_size)
      do iy = 1, num_h-2
        call set_side_quad_connect_8(iele, ifile, ifile_20,             &
     &      numnod, num_h, num_v, iy)
!
!  top surface (z=cube_size)
        do ix = 1, num_h-2
          call set_surface_quad_connect_6(iele, ifile, ifile_20,        &
     &        numnod, num_h, num_v, ix, iy)
        end do
!
!  edge (x=cube_size, z = cube_size)
        call set_side_quad_connect_6(iele, ifile, ifile_20,             &
     &      numnod, num_h, num_v, iy)
      end do
!
!  corner (x=-cube_size, y=z=cube_size)
      call set_corner_quad_connect_8(iele, ifile, ifile_20,             &
     &    numnod, num_h, num_v)
!
!  edge (y=z=cube_size)
      do ix = 1, num_h-2
        call set_side_quad_connect_7(iele, ifile, ifile_20,             &
     &      numnod, num_h, num_v, ix)
      end do
!
!  corner (x=y=z=cube_size)
      call set_corner_quad_connect_7(iele, ifile, ifile_20,             &
     &    numnod, num_h, num_v)
!
      end subroutine set_center_connect_quad
!
!   --------------------------------------------------------------------
!
      end module cubed_sph_center_connect
