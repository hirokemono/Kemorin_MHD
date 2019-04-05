!set_cube_surface.f90
!      module set_cube_surface
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine set_cube_skin(inod_sf_end, csph_p, c_sphere)
!!      subroutine set_rect_skin                                        &
!!     &         (rad_edge, inod_sf_end, csph_p, c_sphere)
!!        type(numref_cubed_sph), intent(inout) :: csph_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!      subroutine const_square_surface(csph_p, inod_sf_end, c_sphere)
!!        type(numref_cubed_sph), intent(in) :: csph_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!
!!      subroutine set_circle_node(inod_ed_end, csph_p, c_sphere)
!!        type(numref_cubed_sph), intent(inout) :: csph_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module set_cube_surface
!
      use m_precision
      use m_constants
!
      use t_numref_cubed_sph
      use t_cubed_sph_surf_mesh
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
      subroutine set_cube_skin(inod_sf_end, csph_p, c_sphere)
!
      use set_cube_surface_node
!
      integer(kind = kint), intent(inout) ::  inod_sf_end
      type(numref_cubed_sph), intent(inout) :: csph_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      if(csph_p%iflag_mesh .eq. 1) then
!         write(*,*) 'set positions of surface start (type 1)'
        call set_1d_posi_eq_cube(csph_p%num_hemi, csph_p%cube_size,     &
     &     csph_p%x_node, csph_p%x_edge)
      else if(csph_p%iflag_mesh .eq. 2) then
!         write(*,*) 'set positions of surface start (type 2)'
        call set_1d_posi_eq_shell(csph_p%num_hemi, csph_p%cube_size,    &
     &      csph_p%x_node, csph_p%x_edge)
      end if
!
      call const_cube_surface(csph_p, inod_sf_end, c_sphere)
!
      end subroutine set_cube_skin
!
!   --------------------------------------------------------------------
!
      subroutine set_rect_skin                                          &
     &         (rad_edge, inod_sf_end, csph_p, c_sphere)
!
      use set_cube_surface_node
!
      real(kind = kreal), intent(in) :: rad_edge
!
      integer(kind = kint), intent(inout) ::  inod_sf_end
      type(numref_cubed_sph), intent(inout) :: csph_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      real(kind = kreal) :: x_size, z_size
!
!
      x_size = csph_p%cube_size * cos(rad_edge) * sqrt(half)
      z_size = csph_p%cube_size * sin(rad_edge)
!
      if(csph_p%iflag_mesh .eq. 1) then
!         write(*,*) 'set positions of surface start (type 1)'
        call set_1d_posi_eq_cube(csph_p%num_hemi,                       &
     &      x_size, csph_p%x_node, csph_p%x_edge)
        call set_1d_posi_eq_cube(csph_p%ncube_vertical, z_size,         &
     &      csph_p%v_node, csph_p%v_edge)
      else if(csph_p%iflag_mesh .eq. 2) then
!         write(*,*) 'set positions of surface start (type 2)'
        call set_1d_posi_eq_shell(csph_p%num_hemi,                      &
     &      x_size, csph_p%x_node, csph_p%x_edge)
        call set_1d_posi_eq_shell(csph_p%ncube_vertical, z_size,        &
     &      csph_p%v_node, csph_p%v_edge)
      end if
!
      call const_rect_surface                                           &
     &   (x_size, z_size, csph_p, inod_sf_end, c_sphere)
!
      end subroutine set_rect_skin
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine const_square_surface(csph_p, inod_sf_end, c_sphere)
!
      use set_cube_surface_node
!
      type(numref_cubed_sph), intent(in) :: csph_p
!
      integer(kind = kint), intent(inout) ::  inod_sf_end
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      inod_sf_end = 0
      call set_z_plane_squre_node                                       &
     &   (csph_p%num_hemi, c_sphere%numnod_sf20, inod_sf_end,           &
     &    zero, csph_p%x_node, c_sphere%x_csph)
!
      end subroutine const_square_surface
!
!   --------------------------------------------------------------------
!
      subroutine const_cube_surface(csph_p, inod_sf_end, c_sphere)
!
      use set_cube_surface_node
!
      type(numref_cubed_sph), intent(in) :: csph_p
!
      integer(kind = kint), intent(inout) ::  inod_sf_end
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: inod
!
!  bottom surface
!
      inod = 0
      call set_z_plane_squre_node                                       &
     &   (csph_p%num_hemi, c_sphere%numnod_sf20, inod,                  &
     &    (-csph_p%cube_size), csph_p%x_node, c_sphere%x_csph)
!
!  wall
!
      call set_side_plane_squre_node(csph_p%num_hemi, csph_p%num_hemi,  &
     &    c_sphere%numnod_sf20, inod, csph_p%cube_size,                 &
     &    csph_p%x_node, csph_p%x_node, c_sphere%x_csph)
!
!  top surface
!
      call set_z_plane_squre_node                                       &
     &   (csph_p%num_hemi, c_sphere%numnod_sf20, inod,                  &
     &    csph_p%cube_size, csph_p%x_node, c_sphere%x_csph)
!
      inod_sf_end = inod
!
      end subroutine const_cube_surface
!
!   --------------------------------------------------------------------
!
      subroutine const_rect_surface                                     &
     &         (x_size, z_size, csph_p, inod_sf_end, c_sphere)
!
      use set_cube_surface_node
!
      real(kind = kreal), intent(in) :: x_size, z_size
      type(numref_cubed_sph), intent(in) :: csph_p
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
      integer(kind = kint), intent(inout) ::  inod_sf_end
!
      integer(kind = kint) :: inod
!
!  bottom surface
!
      inod = 0
      call set_z_plane_squre_node                                       &
     &   (csph_p%num_hemi, c_sphere%numnod_sf20, inod,                  &
     &    (-z_size), csph_p%x_node, c_sphere%x_csph)
!
!  wall
!
      call set_side_plane_squre_node                                    &
     &   (csph_p%num_hemi, csph_p%ncube_vertical,                       &
     &    c_sphere%numnod_sf20, inod, x_size, csph_p%x_node,            &
     &    csph_p%v_node, c_sphere%x_csph)
!
!  top surface
!
      call set_z_plane_squre_node                                       &
     &   (csph_p%num_hemi, c_sphere%numnod_sf20, inod,                  &
     &    z_size, csph_p%x_node, c_sphere%x_csph)
!
      inod_sf_end = inod
!
      end subroutine const_rect_surface
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_circle_node(inod_ed_end, csph_p, c_sphere)
!
      use set_cube_surface_node
      use set_squre_circle_node
!
      integer(kind = kint), intent(inout) ::  inod_ed_end
      type(numref_cubed_sph), intent(inout) :: csph_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!
      if(csph_p%iflag_mesh .eq. 1) then
         write(*,*) 'set positions of surface start (type 1)'
        call set_1d_posi_eq_cube(csph_p%num_hemi,                       &
     &     csph_p%cube_size, csph_p%x_node, csph_p%x_edge)
      else if(csph_p%iflag_mesh .eq. 2) then
         write(*,*) 'set positions of surface start (type 2)'
        call set_1d_posi_eq_shell(csph_p%num_hemi,                      &
     &      csph_p%cube_size, csph_p%x_node, csph_p%x_edge)
      end if
!
      inod_ed_end = 0
      call set_square_node                                              &
     &   (csph_p%num_hemi, c_sphere%numnod_sf20, inod_ed_end,           &
     &    csph_p%cube_size, csph_p%x_node, c_sphere%x_csph)
!
      end subroutine set_circle_node
!
!   --------------------------------------------------------------------
!
      end module set_cube_surface
 
