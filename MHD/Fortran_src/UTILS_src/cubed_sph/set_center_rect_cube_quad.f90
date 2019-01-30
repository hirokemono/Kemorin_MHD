!set_center_rect_cube_quad.f90
!      module set_center_rect_cube_quad
!
!      Written by H. Matsui
!     Modified by H. Matsui on Oct., 2007
!     Modified by H. Matsui on Dec., 2011
!
!      subroutine set_center_cube_quad(inod, ifile)
!      subroutine set_center_rect_quad(inod, ifile)
!
!      subroutine set_center_square_quad(inod, ifile)
!
      module set_center_rect_cube_quad
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
      subroutine set_center_cube_quad(inod, ifile)
!
      use m_constants
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_numref_cubed_sph
!
      use set_center_cube_edge
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: iele_ref
!
!    center cube
!
      call set_center_cube_x_edge(inod, ifile,                          &
     &   num_hemi, num_hemi, x_node, x_edge, x_node)
      iele_ref = (num_hemi-2)*(num_hemi-1)**2
!
      call set_center_cube_y_edge(inod, ifile,                          &
     &   num_hemi, num_hemi, x_node, x_edge, x_node)
      iele_ref = 2*(num_hemi-2)*(num_hemi-1)**2
!
      call set_center_cube_z_edge(inod, ifile,                          &
     &   num_hemi, num_hemi, x_node, x_edge)
      iele_ref = 3*(num_hemi-2)*(num_hemi-1)**2
!
!  bottom surface (z=-cube_size)
!
      call set_center_bottom_edge(inod, ifile, num_hemi,                &
     &    x_node, x_edge(1))
      iele_ref = (3*num_hemi-5)*(num_hemi-1)**2
!
      call set_center_side_edge(inod, ifile, num_hemi, num_hemi,        &
     &    x_node, x_edge, x_node)
      iele_ref = (3*num_hemi-1)*(num_hemi-1)**2
!
!  top surface (z=cube_size)
!
      call set_center_bottom_edge(inod, ifile, num_hemi,                &
     &    x_node, x_edge(num_hemi))
      iele_ref = 3*num_hemi*(num_hemi-1)**2
!
!  outer surface
!
      call set_center_surf_edge(inod, ifile,                            &
     &    c_sphere1%numnod_sf, c_sphere1%numedge_sf, c_sphere1%x_csph)
      iele_ref = 3*(num_hemi)*(num_hemi+1)**2
!
      end subroutine set_center_cube_quad
!
!   --------------------------------------------------------------------
!
      subroutine set_center_rect_quad(inod, ifile)
!
      use m_constants
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_numref_cubed_sph
!
      use set_center_cube_edge
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: iele_ref, inod_st
!
!    center cube
!
      inod_st = inod
      write(*,*) 'inod', inod
      call set_center_cube_x_edge(inod, ifile,                          &
     &   num_hemi, ncube_vertical, x_node, x_edge, v_node)
      iele_ref = (num_hemi-2)*(num_hemi-1)*(ncube_vertical-1)
      write(*,*) 'set_center_cube_x_edge', iele_ref, (inod-inod_st)
!
      call set_center_cube_y_edge(inod, ifile,                          &
     &   num_hemi, ncube_vertical, x_node, x_edge, v_node)
      iele_ref = (2*num_hemi-4)*(num_hemi-1)*(ncube_vertical-1)
      write(*,*) 'set_center_cube_y_edge', iele_ref, (inod-inod_st)
!
      call set_center_cube_z_edge(inod, ifile,                          &
     &    num_hemi, ncube_vertical, x_node, v_edge)
      iele_ref = (3*num_hemi-5)*(num_hemi-1)*(ncube_vertical-1)         &
     &          - (num_hemi-1)*(num_hemi-1)
      write(*,*) 'set_center_cube_z_edge', iele_ref, (inod-inod_st)
!
!  bottom surface (z=-cube_size)
!
      call set_center_bottom_edge(inod, ifile, num_hemi,                &
     &    x_node, v_edge(1))
      iele_ref = (3*num_hemi-5)*(num_hemi-1)*(ncube_vertical-1)
      write(*,*) 'set_center_bottom_edge', iele_ref, (inod-inod_st)
!
      call set_center_side_edge(inod, ifile, num_hemi, ncube_vertical,  &
     &    x_node, x_edge, v_node)
      iele_ref = (3*num_hemi-1)*(num_hemi-1)*(ncube_vertical-1)
      write(*,*) 'set_center_side_edge', iele_ref, (inod-inod_st)
!
      call set_center_bottom_edge(inod, ifile, num_hemi,                &
     &    x_node, v_edge(ncube_vertical))
      iele_ref = (3*num_hemi-1)*(num_hemi-1)*(ncube_vertical-1)         &
     &          + (num_hemi-1)*(num_hemi-1)
      write(*,*) 'set_center_bottom_edge', iele_ref, (inod-inod_st)
!
!  outer surface
!
      call set_center_surf_edge(inod, ifile,                            &
     &    c_sphere1%numnod_sf, c_sphere1%numedge_sf, c_sphere1%x_csph)
      iele_ref = (3*num_hemi-1)*(num_hemi-1)*(ncube_vertical-1)         &
     &          + (num_hemi-1)*(num_hemi-1)                             &
     &          + c_sphere1%numnod_sf
      write(*,*) 'set_center_surf_edge', iele_ref, (inod-inod_st)
!
      end subroutine set_center_rect_quad
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_center_square_quad(inod, ifile)
!
      use m_constants
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_numref_cubed_sph
!
      use set_center_cube_edge
!
      integer(kind = kint), intent(in) :: ifile
      integer(kind = kint), intent(inout) :: inod
!
      integer(kind = kint) :: iele_ref
      real(kind = kreal), parameter :: vzero(3) = (/zero,zero,zero/)
!
!    center cube
!
      call set_center_cube_x_edge(inod, ifile,                          &
     &   num_hemi, itwo, x_node, x_edge, vzero)
      iele_ref = (num_hemi-2)*(num_hemi-1)**2
!
      call set_center_cube_y_edge(inod, ifile,                          &
     &   num_hemi, itwo, x_node, x_edge, vzero)
      iele_ref = 2*(num_hemi-2)*(num_hemi-1)**2
!
!
      call set_center_side_edge(inod, ifile, num_hemi, itwo,            &
     &    x_node, x_edge, vzero)
      iele_ref = (3*num_hemi-1)*(num_hemi-1)
!
      call set_center_surf_edge(inod, ifile,                            &
     &    c_sphere1%numnod_sf, c_sphere1%numedge_sf, c_sphere1%x_csph)
!
      end subroutine set_center_square_quad
!
!   --------------------------------------------------------------------
!
      end module set_center_rect_cube_quad
