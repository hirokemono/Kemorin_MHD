!set_center_rect_cube_quad.f90
!      module set_center_rect_cube_quad
!
!      Written by H. Matsui
!     Modified by H. Matsui on Oct., 2007
!     Modified by H. Matsui on Dec., 2011
!
!!      subroutine set_center_cube_quad(ifile, num_h,                   &
!!     &          x_node, x_edge, inod, c_sphere)
!!      subroutine set_center_rect_quad(ifile, num_h, num_v,            &
!!     &          x_node, x_edge, v_node, v_edge, inod, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!
!!      subroutine set_center_square_quad(ifile, num_h,                 &
!!     &          x_node, x_edge, inod, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module set_center_rect_cube_quad
!
      use m_precision
      use m_constants
      use t_cubed_sph_surf_mesh
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_center_cube_quad(ifile, num_h,                     &
     &          x_node, x_edge, inod, c_sphere)
!
      use set_center_cube_edge
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: num_h
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      real(kind = kreal), intent(in) :: x_edge(num_h)
!
      integer(kind = kint), intent(inout) :: inod
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: iele_ref
!
!    center cube
!
      call set_center_cube_x_edge(inod, ifile,                          &
     &   num_h, num_h, x_node, x_edge, x_node)
      iele_ref = (num_h-2)*(num_h-1)**2
!
      call set_center_cube_y_edge(inod, ifile,                          &
     &   num_h, num_h, x_node, x_edge, x_node)
      iele_ref = 2*(num_h-2)*(num_h-1)**2
!
      call set_center_cube_z_edge(inod, ifile,                          &
     &   num_h, num_h, x_node, x_edge)
      iele_ref = 3*(num_h-2)*(num_h-1)**2
!
!  bottom surface (z=-cube_size)
!
      call set_center_bottom_edge(inod, ifile, num_h,                   &
     &    x_node, x_edge(1))
      iele_ref = (3*num_h-5)*(num_h-1)**2
!
      call set_center_side_edge(inod, ifile, num_h, num_h,              &
     &    x_node, x_edge, x_node)
      iele_ref = (3*num_h-1)*(num_h-1)**2
!
!  top surface (z=cube_size)
!
      call set_center_bottom_edge(inod, ifile, num_h,                   &
     &    x_node, x_edge(num_h))
      iele_ref = 3*num_h*(num_h-1)**2
!
!  outer surface
!
      call set_center_surf_edge(inod, ifile,                            &
     &    c_sphere%numnod_sf, c_sphere%numedge_sf, c_sphere%x_csph)
      iele_ref = 3*(num_h)*(num_h+1)**2
!
      end subroutine set_center_cube_quad
!
!   --------------------------------------------------------------------
!
      subroutine set_center_rect_quad(ifile, num_h, num_v,  &
     &          x_node, x_edge, v_node, v_edge, inod, c_sphere)
!
      use set_center_cube_edge
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: num_h
      integer(kind = kint), intent(in) :: num_v
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      real(kind = kreal), intent(in) :: x_edge(num_h)
      real(kind = kreal), intent(in) :: v_node(num_v+1)
      real(kind = kreal), intent(in) :: v_edge(num_v)
!
      integer(kind = kint), intent(inout) :: inod
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: iele_ref, inod_st
!
!    center cube
!
      inod_st = inod
      write(*,*) 'inod', inod
      call set_center_cube_x_edge(inod, ifile,                          &
     &   num_h, num_v, x_node, x_edge, v_node)
      iele_ref = (num_h-2)*(num_h-1)*(num_v-1)
      write(*,*) 'set_center_cube_x_edge', iele_ref, (inod-inod_st)
!
      call set_center_cube_y_edge(inod, ifile,                          &
     &   num_h, num_v, x_node, x_edge, v_node)
      iele_ref = (2*num_h-4)*(num_h-1)*(num_v-1)
      write(*,*) 'set_center_cube_y_edge', iele_ref, (inod-inod_st)
!
      call set_center_cube_z_edge(inod, ifile,                          &
     &    num_h, num_v, x_node, v_edge)
      iele_ref = (3*num_h-5)*(num_h-1)*(num_v-1) - (num_h-1)*(num_h-1)
      write(*,*) 'set_center_cube_z_edge', iele_ref, (inod-inod_st)
!
!  bottom surface (z=-cube_size)
!
      call set_center_bottom_edge(inod, ifile, num_h,                   &
     &    x_node, v_edge(1))
      iele_ref = (3*num_h-5)*(num_h-1)*(num_v-1)
      write(*,*) 'set_center_bottom_edge', iele_ref, (inod-inod_st)
!
      call set_center_side_edge(inod, ifile, num_h, num_v,              &
     &    x_node, x_edge, v_node)
      iele_ref = (3*num_h-1)*(num_h-1)*(num_v-1)
      write(*,*) 'set_center_side_edge', iele_ref, (inod-inod_st)
!
      call set_center_bottom_edge(inod, ifile, num_h,                   &
     &    x_node, v_edge(num_v))
      iele_ref = (3*num_h-1)*(num_h-1)*(num_v-1) + (num_h-1)*(num_h-1)
      write(*,*) 'set_center_bottom_edge', iele_ref, (inod-inod_st)
!
!  outer surface
!
      call set_center_surf_edge(inod, ifile,                            &
     &    c_sphere%numnod_sf, c_sphere%numedge_sf, c_sphere%x_csph)
      iele_ref = (3*num_h-1)*(num_h-1)*(num_v-1) + (num_h-1)*(num_h-1)  &
     &          + c_sphere%numnod_sf
      write(*,*) 'set_center_surf_edge', iele_ref, (inod-inod_st)
!
      end subroutine set_center_rect_quad
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_center_square_quad(ifile, num_h,                   &
     &          x_node, x_edge, inod, c_sphere)
!
      use set_center_cube_edge
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(in) :: num_h
      real(kind = kreal), intent(in) :: x_node(num_h+1)
      real(kind = kreal), intent(in) :: x_edge(num_h)
      integer(kind = kint), intent(inout) :: inod
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: iele_ref
      real(kind = kreal), parameter :: vzero(3) = (/zero,zero,zero/)
!
!    center cube
!
      call set_center_cube_x_edge(inod, ifile,                          &
     &   num_h, itwo, x_node, x_edge, vzero)
      iele_ref = (num_h-2)*(num_h-1)**2
!
      call set_center_cube_y_edge(inod, ifile,                          &
     &   num_h, itwo, x_node, x_edge, vzero)
      iele_ref = 2*(num_h-2)*(num_h-1)**2
!
!
      call set_center_side_edge(inod, ifile, num_h, itwo,               &
     &    x_node, x_edge, vzero)
      iele_ref = (3*num_h-1)*(num_h-1)
!
      call set_center_surf_edge(inod, ifile,                            &
     &    c_sphere%numnod_sf, c_sphere%numedge_sf, c_sphere%x_csph)
!
      end subroutine set_center_square_quad
!
!   --------------------------------------------------------------------
!
      end module set_center_rect_cube_quad
