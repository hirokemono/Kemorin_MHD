!set_cube_surface.f90
!      module set_cube_surface
!
!        programmed by H.Matsui on Apr., 2006
!
!      subroutine set_cube_skin_eq(inod_sf_end)
!      subroutine set_rect_skin(inod_sf_end)
!      subroutine const_square_surface(inod_sf_end)
!
!      subroutine set_circle_node(inod_ed_end)
!
      module set_cube_surface
!
      use m_precision
!
      implicit  none
!
      private :: const_cube_surface, const_rect_surface
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_cube_skin(inod_sf_end)
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use set_cube_surface_node
!
      integer(kind = kint), intent(inout) ::  inod_sf_end
!
      if ( iflag_mesh .eq. 1 ) then
         write(*,*) 'set positions of surface start (type 1)'
        call set_1d_posi_eq_cube(num_hemi, cube_size, x_node, x_edge)
      else if ( iflag_mesh .eq. 2 ) then
         write(*,*) 'set positions of surface start (type 2)'
        call set_1d_posi_eq_shell(num_hemi, cube_size, x_node, x_edge)
      end if
!
      call const_cube_surface(inod_sf_end)
!
      end subroutine set_cube_skin
!
!   --------------------------------------------------------------------
!
      subroutine set_rect_skin(inod_sf_end)
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use set_cube_surface_node
!
      integer(kind = kint), intent(inout) ::  inod_sf_end
!
      if ( iflag_mesh .eq. 1 ) then
         write(*,*) 'set positions of surface start (type 1)'
        call set_1d_posi_eq_cube(num_hemi, cube_size, x_node, x_edge)
        call set_1d_posi_eq_cube(ncube_vertical, cube_size,             &
     &      v_node, v_edge)
      else if ( iflag_mesh .eq. 2 ) then
         write(*,*) 'set positions of surface start (type 2)'
        call set_1d_posi_eq_shell(num_hemi, cube_size, x_node, x_edge)
        call set_1d_posi_eq_shell(ncube_vertical, cube_size,            &
     &      v_node, v_edge)
      end if
!
      call const_rect_surface(inod_sf_end)
!
      end subroutine set_rect_skin
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine const_square_surface(inod_sf_end)
!
      use m_constants
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use set_cube_surface_node
!
      integer(kind = kint), intent(inout) ::  inod_sf_end
!
!
      inod_sf_end = 0
      call set_z_plane_squre_node(num_hemi, numnod_sf20, inod_sf_end,   &
     &    zero, x_node, xyz_surf)
!
      end subroutine const_square_surface
!
!   --------------------------------------------------------------------
!
      subroutine const_cube_surface(inod_sf_end)
!
      use m_constants
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use set_cube_surface_node
!
      integer(kind = kint), intent(inout) ::  inod_sf_end
!
      integer(kind = kint) :: ix, iy, iz, inod
      real(kind = kreal) :: xx, yy, zz
!
!  bottom surface
!
      inod = 0
      zz = -cube_size
      call set_z_plane_squre_node(num_hemi, numnod_sf20, inod,          &
     &    zz, x_node, xyz_surf)
!
!  wall
!
      call set_side_plane_squre_node(num_hemi, num_hemi,                &
     &    numnod_sf20, inod, cube_size, x_node, x_node, xyz_surf)
!
!  top surface
!
      call set_z_plane_squre_node(num_hemi, numnod_sf20, inod,          &
     &    cube_size, x_node, xyz_surf)
!
      inod_sf_end = inod
!
      end subroutine const_cube_surface
!
!   --------------------------------------------------------------------
!
      subroutine const_rect_surface(inod_sf_end)
!
      use m_constants
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use set_cube_surface_node
!
      integer(kind = kint), intent(inout) ::  inod_sf_end
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: zz
!
!
!  bottom surface
!
      inod = 0
      zz = -cube_size
      call set_z_plane_squre_node(num_hemi, numnod_sf20, inod,          &
     &    zz, x_node, xyz_surf)
!
!  wall
!
      call set_side_plane_squre_node(num_hemi, ncube_vertical,          &
     &    numnod_sf20, inod, cube_size, x_node, v_node, xyz_surf)
!
!  top surface
!
      call set_z_plane_squre_node(num_hemi, numnod_sf20, inod,          &
     &    cube_size, x_node, xyz_surf)
!
      inod_sf_end = inod
!
      end subroutine const_rect_surface
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_circle_node(inod_ed_end)
!
      use m_constants
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use set_cube_surface_node
      use set_squre_circle_node
!
      integer(kind = kint), intent(inout) ::  inod_ed_end
!
!
      if ( iflag_mesh .eq. 1 ) then
         write(*,*) 'set positions of surface start (type 1)'
        call set_1d_posi_eq_cube(num_hemi, cube_size, x_node, x_edge)
      else if ( iflag_mesh .eq. 2 ) then
         write(*,*) 'set positions of surface start (type 2)'
        call set_1d_posi_eq_shell(num_hemi, cube_size, x_node, x_edge)
      end if
!
      inod_ed_end = 0
      call set_square_node(num_hemi, numnod_sf20,                       &
     &    inod_ed_end, cube_size, x_node, xyz_surf)
!
      end subroutine set_circle_node
!
!   --------------------------------------------------------------------
!
      end module set_cube_surface
 