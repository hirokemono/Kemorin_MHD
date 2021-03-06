!set_surf_connect_cubed_sph.f90
!      module set_surf_connect_cubed_sph
!
!      Written by H. Matsui
!      Modified by H. Matsui on Oct., 2007
!
!!      subroutine set_ntot_ele_sf20(c_sphere)
!!      subroutine set_cube_surf_connect(num_h, iele_sf_end, c_sphere)
!!      subroutine set_rect_surf_connect                                &
!!     &         (num_h, num_v, iele_sf_end, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!
!!      subroutine set_coarse_cube_surf_connect                         &
!!     &         (n_hemi_c, iele_sf_end, c_sphere)
!!      subroutine set_coarse_rect_surf_connect                         &
!!     &         (n_hemi_c, n_vert_c, iele_sf_end, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module set_surf_connect_cubed_sph
!
      use m_precision
      use t_cubed_sph_surf_mesh
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_ntot_ele_sf20(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      c_sphere%ntot_ele_sf20                                            &
     &      =  c_sphere%numele_sf20 + c_sphere%numele_sf_w_coarse
      c_sphere%ntot_edge_sf20                                           &
     &      = c_sphere%numedge_sf20 + c_sphere%numedge_sf_w_coarse
!
      end subroutine set_ntot_ele_sf20
!
!   --------------------------------------------------------------------
!
      subroutine set_cube_surf_connect(num_h, iele_sf_end, c_sphere)
!
      use set_cube_surface_connect
!
      integer(kind = kint), intent(in) :: num_h
!
      integer(kind = kint), intent(inout) :: iele_sf_end
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!   bottom surface (z = -cube_size)
!
      iele_sf_end = 0
      call set_bottom_surf_connect                                      &
     &   (num_h, iele_sf_end, c_sphere%numnod_sf,                       &
     &    c_sphere%ntot_ele_sf20, c_sphere%ie_sf20)
!
!    bottom side
!
      call set_bottom_side_connect(num_h, num_h, iele_sf_end,           &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
!     side wall
!
      call set_side_wall_connect(num_h, num_h, iele_sf_end,             &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
!   top rod
!
      call set_top_side_connect(num_h, num_h, iele_sf_end,              &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
!  top surface
!
      call set_top_surf_connect(num_h, num_h, iele_sf_end,              &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
      end subroutine set_cube_surf_connect
!
! -------------------------------------------------------------------
!
      subroutine set_rect_surf_connect                                  &
     &         (num_h, num_v, iele_sf_end, c_sphere)
!
      use set_cube_surface_connect
!
      integer(kind = kint), intent(in) :: num_h
      integer(kind = kint), intent(in) :: num_v
      integer(kind = kint), intent(inout) :: iele_sf_end
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!   bottom surface (z = -cube_size)
!
      iele_sf_end = 0
      call set_bottom_surf_connect                                      &
     &   (num_h, iele_sf_end, c_sphere%numnod_sf,                       &
     &    c_sphere%ntot_ele_sf20, c_sphere%ie_sf20)
!
!    bottom side
!
      call set_bottom_side_connect(num_h, num_v, iele_sf_end,           &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20, c_sphere%ie_sf20)
!
!     side wall
!
      call set_side_wall_connect(num_h, num_v, iele_sf_end,             &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
!   top side
!
      call set_top_side_connect(num_h, num_v, iele_sf_end,              &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
!  top surface
!
      call set_top_surf_connect(num_h, num_v, iele_sf_end,              &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
      end subroutine set_rect_surf_connect
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_coarse_cube_surf_connect                           &
     &         (n_hemi_c, iele_sf_end, c_sphere)
!
      use set_cube_surface_connect
!
      integer(kind = kint), intent(in) :: n_hemi_c
!
      integer(kind = kint), intent(inout) :: iele_sf_end
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!   bottom surface (z = -cube_size)
!
      call set_bottom_surf_connect                                      &
     &   (n_hemi_c, iele_sf_end,  c_sphere%numnod_sf,                   &
     &    c_sphere%ntot_ele_sf20, c_sphere%ie_sf20)
!
!    bottom side
!
      call set_bottom_side_connect(n_hemi_c, n_hemi_c, iele_sf_end,     &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
!     side wall
!
      call set_side_wall_connect(n_hemi_c, n_hemi_c, iele_sf_end,       &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
!   top side
!
      call set_top_side_connect(n_hemi_c, n_hemi_c, iele_sf_end,        &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
!  top surface
!
      call set_top_surf_connect(n_hemi_c, n_hemi_c, iele_sf_end,        &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
      end subroutine set_coarse_cube_surf_connect
!
!   --------------------------------------------------------------------
!
      subroutine set_coarse_rect_surf_connect                           &
     &         (n_hemi_c, n_vert_c, iele_sf_end, c_sphere)
!
      use set_cube_surface_connect
!
      integer(kind = kint), intent(in) :: n_hemi_c
      integer(kind = kint), intent(in) :: n_vert_c
!
      integer(kind = kint), intent(inout) :: iele_sf_end
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!   bottom surface (z = -cube_size)
!
      call set_bottom_surf_connect                                      &
     &   (n_hemi_c, iele_sf_end,  c_sphere%numnod_sf,                   &
     &    c_sphere%ntot_ele_sf20, c_sphere%ie_sf20)
!
!    bottom side
!
      call set_bottom_side_connect(n_hemi_c, n_vert_c, iele_sf_end,     &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
!     side wall
!
      call set_side_wall_connect(n_hemi_c, n_vert_c, iele_sf_end,       &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
!   top side
!
      call set_top_side_connect(n_hemi_c, n_vert_c, iele_sf_end,        &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
!  top surface
!
      call set_top_surf_connect(n_hemi_c, n_vert_c, iele_sf_end,        &
     &    c_sphere%numnod_sf, c_sphere%ntot_ele_sf20,                   &
     &    c_sphere%ie_sf20)
!
      end subroutine set_coarse_rect_surf_connect
!
!   --------------------------------------------------------------------
!
      end module set_surf_connect_cubed_sph
