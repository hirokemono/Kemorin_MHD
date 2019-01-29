!set_surf_connect_cubed_sph.f90
!      module set_surf_connect_cubed_sph
!
!      Written by H. Matsui
!      Modified by H. Matsui on Oct., 2007
!
!      subroutine set_cube_surf_connect(iele_sf_end)
!      subroutine set_rect_surf_connect(iele_sf_end)
!
!      subroutine set_coarse_cube_surf_connect(iele_sf_end)
!      subroutine set_coarse_rect_surf_connect(iele_sf_end)
!
      module set_surf_connect_cubed_sph
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
      subroutine set_cube_surf_connect(iele_sf_end)
!
      use m_cubed_sph_surf_mesh
      use m_numref_cubed_sph
      use set_cube_surface_connect
!
      integer(kind = kint), intent(inout) :: iele_sf_end
!
!   bottom surface (z = -cube_size)
!
      iele_sf_end = 0
      call set_bottom_surf_connect                                      &
     &   (num_hemi, iele_sf_end, c_sphere1%numnod_sf,                   &
     &    ntot_ele_sf20, ie_sf20)
!
!    bottom side
!
      call set_bottom_side_connect(num_hemi, num_hemi, iele_sf_end,     &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!     side wall
!
      call set_side_wall_connect(num_hemi, num_hemi, iele_sf_end,       &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!   top rod
!
      call set_top_side_connect(num_hemi, num_hemi, iele_sf_end,        &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!  top surface
!
      call set_top_surf_connect(num_hemi, num_hemi, iele_sf_end,        &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
      end subroutine set_cube_surf_connect
!
! -------------------------------------------------------------------
!
      subroutine set_rect_surf_connect(iele_sf_end)
!
      use m_cubed_sph_surf_mesh
      use m_numref_cubed_sph
      use set_cube_surface_connect
!
      integer(kind = kint), intent(inout) :: iele_sf_end
!
!   bottom surface (z = -cube_size)
!
      iele_sf_end = 0
      call set_bottom_surf_connect                                      &
     &   (num_hemi, iele_sf_end, c_sphere1%numnod_sf,                   &
     &    ntot_ele_sf20, ie_sf20)
!
!    bottom side
!
      call set_bottom_side_connect(num_hemi, ncube_vertical,            &
     &    iele_sf_end, c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!     side wall
!
      call set_side_wall_connect(num_hemi, ncube_vertical, iele_sf_end, &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!   top side
!
      call set_top_side_connect(num_hemi, ncube_vertical, iele_sf_end,  &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!  top surface
!
      call set_top_surf_connect(num_hemi, ncube_vertical, iele_sf_end,  &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
      end subroutine set_rect_surf_connect
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_coarse_cube_surf_connect(iele_sf_end)
!
      use m_cubed_sph_surf_mesh
      use m_numref_cubed_sph
!
      use set_cube_surface_connect
!
      integer(kind = kint), intent(inout) :: iele_sf_end
!
!   bottom surface (z = -cube_size)
!
      call set_bottom_surf_connect                                      &
     &   (n_hemi_c, iele_sf_end,  c_sphere1%numnod_sf,                  &
     &    ntot_ele_sf20, ie_sf20)
!
!    bottom side
!
      call set_bottom_side_connect(n_hemi_c, n_hemi_c, iele_sf_end,     &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!     side wall
!
      call set_side_wall_connect(n_hemi_c, n_hemi_c, iele_sf_end,       &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!   top side
!
      call set_top_side_connect(n_hemi_c, n_hemi_c, iele_sf_end,        &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!  top surface
!
      call set_top_surf_connect(n_hemi_c, n_hemi_c, iele_sf_end,        &
     &     c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
      end subroutine set_coarse_cube_surf_connect
!
!   --------------------------------------------------------------------
!
      subroutine set_coarse_rect_surf_connect(iele_sf_end)
!
      use m_cubed_sph_surf_mesh
      use m_numref_cubed_sph
!
      use set_cube_surface_connect
!
      integer(kind = kint), intent(inout) :: iele_sf_end
!
!   bottom surface (z = -cube_size)
!
      call set_bottom_surf_connect                                      &
     &   (n_hemi_c, iele_sf_end,  c_sphere1%numnod_sf,                  &
     &    ntot_ele_sf20, ie_sf20)
!
!    bottom side
!
      call set_bottom_side_connect(n_hemi_c, n_vert_c, iele_sf_end,     &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!     side wall
!
      call set_side_wall_connect(n_hemi_c, n_vert_c, iele_sf_end,       &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!   top side
!
      call set_top_side_connect(n_hemi_c, n_vert_c, iele_sf_end,        &
     &    c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
!  top surface
!
      call set_top_surf_connect(n_hemi_c, n_vert_c, iele_sf_end,        &
     &     c_sphere1%numnod_sf, ntot_ele_sf20, ie_sf20)
!
      end subroutine set_coarse_rect_surf_connect
!
!   --------------------------------------------------------------------
!
      end module set_surf_connect_cubed_sph
