!set_cube_surface_node.f90
!      module set_cube_surface_node
!
!      Written by H. Matsui
!      Modified by H. Matsui on Oct., 2007
!
!      subroutine set_1d_posi_eq_cube(num, cube_size, x_node, x_edge)
!      subroutine set_1d_posi_eq_shell(num, cube_size, x_node, x_edge)
!
!      subroutine set_z_plane_squre_node(num_h, nnod_sf, inod,          &
!     &          cube_size, x_node, xyz_sf)
!      subroutine set_side_plane_squre_node(num_h, num_v, nnod_sf, inod,&
!     &          cube_size, x_node, v_node, xyz_sf)
!
      module set_cube_surface_node
!
      use m_precision
!
      implicit none
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_1d_posi_eq_cube(num, cube_size, x_node, x_edge)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(in) :: cube_size
      real(kind = kreal), intent(inout) :: x_node(num+1), x_edge(num)
!
      integer(kind = kint) :: inod
!
      do inod = 1, num
        x_node(inod) = ( -one + dble(2*inod-2) / dble(num) )       &
     &                  * cube_size
        x_edge(inod) = ( -one + dble(2*inod-1) / dble(num) )       &
     &                  * cube_size
      end do
      x_node(num+1) = cube_size
!
      end subroutine set_1d_posi_eq_cube
!
!   --------------------------------------------------------------------
!
      subroutine set_1d_posi_eq_shell(num, cube_size, x_node, x_edge)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(in) :: cube_size
      real(kind = kreal), intent(inout) :: x_node(num+1), x_edge(num)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: alpha, beta
      real(kind = kreal) :: pi
!
      pi = four * atan(one)
!
      do inod = 1, num
        alpha = pi * ( - one + dble(2*inod-2)/dble(num) ) / four
        beta =  pi * ( - one + dble(2*inod-1)/dble(num) ) / four
        x_node(inod) = cube_size * tan(alpha)
        x_edge(inod) = cube_size * tan(beta)
      end do
      x_node(num+1) = cube_size
!
      end subroutine set_1d_posi_eq_shell
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_z_plane_squre_node(num_h, nnod_sf, inod,           &
     &          z_posi, x_node, xyz_sf)
!
      integer(kind = kint), intent(in) :: num_h
      integer(kind = kint), intent(in) :: nnod_sf
      real(kind = kreal), intent(in) :: z_posi
      real(kind = kreal), intent(in) :: x_node(num_h+1)
!
      integer(kind = kint), intent(inout) :: inod
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf,3)
!
      integer(kind = kint) :: ix, iy
!
!
      do iy = 1, num_h+1
        do ix = 1, num_h+1
          inod = inod + 1
          xyz_sf(inod,1) =  x_node(ix)
          xyz_sf(inod,2) =  x_node(iy)
          xyz_sf(inod,3) =  z_posi
        end do
      end do
!
      end subroutine set_z_plane_squre_node
!
!   --------------------------------------------------------------------
!
      subroutine set_side_plane_squre_node(num_h, num_v, nnod_sf, inod, &
     &          cube_size, x_node, v_node, xyz_sf)
!
      integer(kind = kint), intent(in) :: num_h, num_v
      integer(kind = kint), intent(in) :: nnod_sf
      real(kind = kreal), intent(in) :: cube_size
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      real(kind = kreal), intent(in) :: v_node(num_v+1)
!
      integer(kind = kint), intent(inout) :: inod
      real(kind = kreal), intent(inout) :: xyz_sf(nnod_sf,3)
!
      integer(kind = kint) :: ix, iy, iz
!
!
      do iz = 1, num_v-1
        do ix = 1, num_h
          inod = inod + 1
          xyz_sf(inod,1) =  x_node(ix)
          xyz_sf(inod,2) = -cube_size
          xyz_sf(inod,3) =  v_node(iz+1)
        end do
        do iy = 1, num_h
          inod = inod + 1
          xyz_sf(inod,1) =  cube_size
          xyz_sf(inod,2) =  x_node(iy)
          xyz_sf(inod,3) =  v_node(iz+1)
        end do
        do ix = 1, num_h
          inod = inod + 1
          xyz_sf(inod,1) =  x_node(num_h+2-ix)
          xyz_sf(inod,2) =  cube_size
          xyz_sf(inod,3) =  v_node(iz+1)
        end do
        do iy = 1, num_h
          inod = inod + 1
          xyz_sf(inod,1) = -cube_size
          xyz_sf(inod,2) =  x_node(num_h+2-iy)
          xyz_sf(inod,3) =  v_node(iz+1)
        end do
      end do
!
      end subroutine set_side_plane_squre_node
!
!   --------------------------------------------------------------------
!
      end module set_cube_surface_node
