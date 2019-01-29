!set_surface_rods_sphere.f90
!      module set_surface_rods_sphere
!
!      subroutine set_cube_rods(inod_sf_end, irod_sf_end)
!      subroutine set_rect_rods(inod_sf_end, irod_sf_end)
!
!      subroutine set_circle_rods(inod_sf_end, irod_sf_end)
!
!      Written by H. Matsui
!      Modified by H. Matsui on Oct., 2007
!
      module set_surface_rods_sphere
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
      subroutine set_cube_rods(inod_sf_end, irod_sf_end)
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use set_cube_surface_edge
!
      integer(kind = kint), intent(inout) :: inod_sf_end, irod_sf_end
!
      integer(kind = kint) :: iele_ref
      integer(kind = kint) :: inod, iele
!
!
      iele_ref = 0
      iele = 0
      inod = inod_sf_end
!
!   bottom surface (z = -cube_size)
      call set_bottom_squre_edge_x(num_hemi, iele, inod, x_edge(1),     &
     &    c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,              &
     &    xyz_surf, iedge_sf20)
      iele_ref = (num_hemi+1)*num_hemi
!
      call set_bottom_squre_edge_y(num_hemi, iele, inod, x_edge(1),     &
     &    c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,              &
     &    xyz_surf, iedge_sf20)
      iele_ref = (2*num_hemi+2)*num_hemi
!
!   bottom vertical rod
!
      call set_bottom_vert_edge(num_hemi, iele, inod, x_edge(1),        &
     &    c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,              &
     &    xyz_surf, iedge_sf20)
      iele_ref = (2*num_hemi+6)*num_hemi
!      write(*,*) 'set_bottom_vert_edge', iele_ref,  iele, inod
!
!   side vertical rod
!
      call set_wall_vert_edge(num_hemi, num_hemi, iele, inod,           &
     &    x_edge(1), c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,   &
     &    xyz_surf, iedge_sf20)
      iele_ref = (6*num_hemi-2)*num_hemi
!
!    top vertical rod
!
      call set_top_vert_edge                                            &
     &   (num_hemi, num_hemi, iele, inod, x_edge(num_hemi),             &
     &    c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,              &
     &    xyz_surf, iedge_sf20)
      iele_ref = (6*num_hemi+2)*num_hemi
!
!   side horizontal rod
!
      call set_side_horiz_edge(num_hemi, num_hemi, iele, inod,          &
     &    x_edge(1), c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,   &
     &    xyz_surf, iedge_sf20)
      iele_ref = (10*num_hemi-2)*num_hemi
!
!  top surface
!
      call set_top_squre_edge_x(num_hemi, num_hemi, iele, inod,         &
     &    x_edge(1), c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,   &
     &    xyz_surf, iedge_sf20)
      iele_ref = (11*num_hemi-1)*num_hemi
!
      call set_top_squre_edge_y(num_hemi, num_hemi, iele, inod,         &
     &    x_edge(1), c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,   &
     &    xyz_surf, iedge_sf20)
      iele_ref = 12*num_hemi*num_hemi
!
      inod_sf_end = inod
      irod_sf_end = iele
!
      end subroutine set_cube_rods
!
! -------------------------------------------------------------------
!
      subroutine set_rect_rods(inod_sf_end, irod_sf_end)
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use set_cube_surface_edge
!
      integer(kind = kint), intent(inout) :: inod_sf_end, irod_sf_end
!
      integer(kind = kint) :: iele_ref
      integer(kind = kint) :: inod, iele
!
!
      iele_ref = 0
      iele = 0
      inod = inod_sf_end
!
!   bottom surface (z = -cube_size)
      call set_bottom_squre_edge_x(num_hemi, iele, inod, x_edge(1),     &
     &    c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,              &
     &    xyz_surf, iedge_sf20)
      iele_ref = (num_hemi+1)*num_hemi
!      write(*,*) 'set_bottom_squre_edge_x end', iele_ref, inod
!
      call set_bottom_squre_edge_y(num_hemi, iele, inod, x_edge(1),     &
     &    c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,              &
     &    xyz_surf, iedge_sf20)
      iele_ref = 2*(num_hemi+1)*num_hemi
!      write(*,*) 'set_bottom_squre_edge_y end', iele_ref, inod
!
!    bottom vertical rod
!
      call set_bottom_vert_edge(num_hemi, iele, inod, v_edge(1),        &
     &    c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,              &
     &    xyz_surf, iedge_sf20)
      iele_ref = (2*num_hemi+6)*num_hemi
!      write(*,*) 'set_bottom_vert_edge end', iele_ref, inod
!
!   side vertical rod
!
      call set_wall_vert_edge(num_hemi, ncube_vertical, iele, inod,     &
     &    v_edge(1), c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,   &
     &    xyz_surf, iedge_sf20)
      iele_ref = (2*num_hemi+4*ncube_vertical-2)*num_hemi
!      write(*,*) 'set_wall_vert_edge end', iele_ref, inod
!
!    top vertical rod
!
      call set_top_vert_edge                                            &
     &   (num_hemi, ncube_vertical, iele, inod, v_edge(ncube_vertical), &
     &    c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,              &
     &    xyz_surf, iedge_sf20)
      iele_ref = (2*num_hemi+4*ncube_vertical+2)*num_hemi
!      write(*,*) 'set_top_vert_edge end', iele_ref, inod
!
!   side horizontal rod
!
      call set_side_horiz_edge(num_hemi, ncube_vertical, iele, inod,    &
     &    x_edge(1), c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,   &
     &    xyz_surf, iedge_sf20)
      iele_ref = (2*num_hemi+8*ncube_vertical-2)*num_hemi
!      write(*,*) 'set_side_horiz_edge end', iele_ref, inod
!
!  top surface
!
      call set_top_squre_edge_x(num_hemi, ncube_vertical, iele, inod,   &
     &    x_edge(1), c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,   &
     &    xyz_surf, iedge_sf20)
      iele_ref = (3*num_hemi+8*ncube_vertical-1)*num_hemi
!      write(*,*) 'set_top_squre_edge_x end', iele_ref, inod
!
      call set_top_squre_edge_y(num_hemi, ncube_vertical, iele, inod,   &
     &    x_edge(1), c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,   &
     &    xyz_surf, iedge_sf20)
      iele_ref = (4*num_hemi+8*ncube_vertical)*num_hemi
!      write(*,*) 'set_top_squre_edge_y end', iele_ref, inod
!
      inod_sf_end = inod
      irod_sf_end = iele
!
      end subroutine set_rect_rods
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_circle_rods(inod_sf_end, irod_sf_end)
!
      use m_constants
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use set_squre_circle_node
!
      integer(kind = kint), intent(inout) :: inod_sf_end, irod_sf_end
!
      integer(kind = kint) :: iele_ref
      integer(kind = kint) :: inod, iele
!
!
      iele_ref = 0
      iele = 0
      inod = inod_sf_end
!
!   side horizontal rod
!
      call set_square_edge(num_hemi, iele, inod, x_edge(1),             &
     &    c_sphere1%numnod_sf20, c_sphere1%ntot_edge_sf20,              &
     &    xyz_surf, iedge_sf20)
      iele_ref = 4*num_hemi
      write(*,*) 'set_square_edge end', iele_ref, inod
!
      inod_sf_end = inod
      irod_sf_end = iele
!
      end subroutine set_circle_rods
!
! -------------------------------------------------------------------
!
      end module set_surface_rods_sphere
