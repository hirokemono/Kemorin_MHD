!set_surface_rods_sphere.f90
!      module set_surface_rods_sphere
!
!!      subroutine set_cube_rods                                        &
!!     &         (num_h, x_edge, inod_sf_end, irod_sf_end, c_sphere)
!!      subroutine set_rect_rods(num_h, num_v, x_edge, v_edge,          &
!!     &          inod_sf_end, irod_sf_end, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!
!!      subroutine set_circle_rods                                      &
!!     &         (num_h, x_edge, inod_sf_end, irod_sf_end, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!      Written by H. Matsui
!      Modified by H. Matsui on Oct., 2007
!
      module set_surface_rods_sphere
!
      use m_precision
      use m_constants
!
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
      subroutine set_cube_rods                                          &
     &         (num_h, x_edge, inod_sf_end, irod_sf_end, c_sphere)
!
      use set_cube_surface_edge
!
      integer(kind = kint), intent(in) :: num_h
      real(kind = kreal), intent(in) :: x_edge(num_h)
!
      integer(kind = kint), intent(inout) :: inod_sf_end, irod_sf_end
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
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
      call set_bottom_squre_edge_x(num_h, iele, inod, x_edge(1),        &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (num_h+1)*num_h
!
      call set_bottom_squre_edge_y(num_h, iele, inod, x_edge(1),        &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (2*num_h+2)*num_h
!
!   bottom vertical rod
!
      call set_bottom_vert_edge(num_h, iele, inod, x_edge(1),           &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (2*num_h+6)*num_h
!      write(*,*) 'set_bottom_vert_edge', iele_ref,  iele, inod
!
!   side vertical rod
!
      call set_wall_vert_edge(num_h, num_h, iele, inod,                 &
     &    x_edge(1), c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,     &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (6*num_h-2)*num_h
!
!    top vertical rod
!
      call set_top_vert_edge                                            &
     &   (num_h, num_h, iele, inod, x_edge(num_h),                      &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (6*num_h+2)*num_h
!
!   side horizontal rod
!
      call set_side_horiz_edge(num_h, num_h, iele, inod,                &
     &    x_edge(1), c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,     &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (10*num_h-2)*num_h
!
!  top surface
!
      call set_top_squre_edge_x(num_h, num_h, iele, inod,               &
     &    x_edge(1), c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,     &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (11*num_h-1)*num_h
!
      call set_top_squre_edge_y(num_h, num_h, iele, inod,               &
     &    x_edge(1), c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,     &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = 12*num_h*num_h
!
      inod_sf_end = inod
      irod_sf_end = iele
!
      end subroutine set_cube_rods
!
! -------------------------------------------------------------------
!
      subroutine set_rect_rods(num_h, num_v, x_edge, v_edge,            &
     &          inod_sf_end, irod_sf_end, c_sphere)
!
      use set_cube_surface_edge
!
      integer(kind = kint), intent(in) :: num_h, num_v
      real(kind = kreal), intent(in) :: x_edge(num_h)
      real(kind = kreal), intent(in) :: v_edge(num_v)
!
      integer(kind = kint), intent(inout) :: inod_sf_end, irod_sf_end
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
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
      call set_bottom_squre_edge_x(num_h, iele, inod, x_edge(1),        &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (num_h+1)*num_h
!      write(*,*) 'set_bottom_squre_edge_x end', iele_ref, inod
!
      call set_bottom_squre_edge_y(num_h, iele, inod, x_edge(1),        &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = 2*(num_h+1)*num_h
!      write(*,*) 'set_bottom_squre_edge_y end', iele_ref, inod
!
!    bottom vertical rod
!
      call set_bottom_vert_edge(num_h, iele, inod, v_edge(1),           &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (2*num_h+6)*num_h
!      write(*,*) 'set_bottom_vert_edge end', iele_ref, inod
!
!   side vertical rod
!
      call set_wall_vert_edge(num_h, num_v, iele, inod, v_edge(1),      &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (2*num_h+4*num_v-2)*num_h
!      write(*,*) 'set_wall_vert_edge end', iele_ref, inod
!
!    top vertical rod
!
      call set_top_vert_edge                                            &
     &   (num_h, num_v, iele, inod, v_edge(num_v),                      &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (2*num_h+4*num_v+2)*num_h
!      write(*,*) 'set_top_vert_edge end', iele_ref, inod
!
!   side horizontal rod
!
      call set_side_horiz_edge(num_h, num_v, iele, inod, x_edge(1),     &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (2*num_h+8*num_v-2)*num_h
!      write(*,*) 'set_side_horiz_edge end', iele_ref, inod
!
!  top surface
!
      call set_top_squre_edge_x(num_h, num_v, iele, inod, x_edge(1),    &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (3*num_h+8*num_v-1)*num_h
!      write(*,*) 'set_top_squre_edge_x end', iele_ref, inod
!
      call set_top_squre_edge_y(num_h, num_v, iele, inod, x_edge(1),    &
     &     c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,               &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = (4*num_h+8*num_v)*num_h
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
      subroutine set_circle_rods                                        &
     &         (num_h, x_edge, inod_sf_end, irod_sf_end, c_sphere)
!
      use set_squre_circle_node
!
      integer(kind = kint), intent(in) :: num_h
      real(kind = kreal), intent(in) :: x_edge(num_h)
!
      integer(kind = kint), intent(inout) :: inod_sf_end, irod_sf_end
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
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
      call set_square_edge(num_h, iele, inod, x_edge(1),             &
     &    c_sphere%numnod_sf20, c_sphere%ntot_edge_sf20,                &
     &    c_sphere%x_csph, c_sphere%iedge_sf20)
      iele_ref = 4*num_h
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
