!
!      module set_coarsed_cube_skin
!
!     Written by H. Matsui on Apr., 2003
!     Modified by H. Matsui on Oct., 2007
!
!!      subroutine set_coarse_cube_skin(inod_sf_end, c_sphere)
!!      subroutine set_coarse_rect_skin(inod_sf_end, c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module set_coarsed_cube_skin
!
      use m_precision
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
      subroutine set_coarse_cube_skin(inod_sf_end, c_sphere)
!
      use m_numref_cubed_sph
      use set_coarse_cube_surf_nod
!
      integer (kind = kint), intent(inout) :: inod_sf_end
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!  bottom surface
!
      call set_bottom_course_cube(num_hemi, nskip_s, nskip_fs,          &
     &    c_sphere%numnod_sf_w_coarse, inod_sf_end,                     &
     &    c_sphere%inod_2_org, c_sphere%inod_2_next)
!
!  wall
!
      call set_side_course_cube(num_hemi, num_hemi, nskip_s, nskip_fs,  &
     &    c_sphere%numnod_sf_w_coarse, inod_sf_end,                     &
     &    c_sphere%inod_2_org, c_sphere%inod_2_next)
!
!  top surface
!
      call set_top_course_cube(num_hemi, num_hemi, nskip_s, nskip_fs,   &
     &    c_sphere%numnod_sf_w_coarse, inod_sf_end,                     &
     &    c_sphere%inod_2_org, c_sphere%inod_2_next)
!
      end subroutine set_coarse_cube_skin
!
!   --------------------------------------------------------------------
!
      subroutine set_coarse_rect_skin(inod_sf_end, c_sphere)
!
      use m_numref_cubed_sph
      use set_coarse_cube_surf_nod
!
      integer (kind = kint), intent(inout) :: inod_sf_end
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
!  bottom surface
!
      call set_bottom_course_cube(num_hemi, nskip_s, nskip_fs,          &
     &    c_sphere%numnod_sf_w_coarse, inod_sf_end,                     &
     &    c_sphere%inod_2_org, c_sphere%inod_2_next)
!
!  wall
!
      call set_side_course_cube(num_hemi, ncube_vertical,               &
     &    nskip_s, nskip_fs, c_sphere%numnod_sf_w_coarse, inod_sf_end,  &
     &    c_sphere%inod_2_org, c_sphere%inod_2_next)
!
!  top surface
!
      call set_top_course_cube(num_hemi, ncube_vertical,                &
     &    nskip_s, nskip_fs, c_sphere%numnod_sf_w_coarse, inod_sf_end,  &
     &    c_sphere%inod_2_org, c_sphere%inod_2_next)
!
      end subroutine set_coarse_rect_skin
!
!   --------------------------------------------------------------------
!
      end module set_coarsed_cube_skin
