!merged_ele_4_cubed_sph_surf.f90
!      module merged_ele_4_cubed_sph_surf
!
!      Written by H. Matsui on Apr., 2006
!      Modified by H. Matsui on Oct., 2007
!
!!      subroutine set_merged_element_cube_surf                         &
!!     &         (is_level, csph_p, course_p, c_sphere)
!!      subroutine set_merged_element_rect_surf                         &
!!     &         (is_level, csph_p, course_p, c_sphere)
!!        type(numref_cubed_sph), intent(in) :: csph_p
!!        type(coarse_cubed_sph), intent(in) :: course_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module merged_ele_4_cubed_sph_surf
!
      use m_precision
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
      subroutine set_merged_element_cube_surf                           &
     &         (is_level, csph_p, course_p, c_sphere)
!
      use t_numref_cubed_sph
      use set_coarse_cube_surf_nod
!
      integer(kind = kint), intent(in) :: is_level
      type(numref_cubed_sph), intent(in) :: csph_p
      type(coarse_cubed_sph), intent(in) :: course_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: ist, ied, iele
!
!
      ist = c_sphere%iele_stack_sf(is_level-1) + 1
      ied = c_sphere%iele_stack_sf(is_level)
      do iele = ist, ied
        c_sphere%num_merge_e_sf(iele) = course_p%nl_s**2
      end do
!
!   bottom surface (z = -cube_size)
!
      iele = c_sphere%iele_stack_sf(is_level-1)
      call set_bottom_merged_cube                                       &
     &   (csph_p%num_hemi, course_p%nskip_s, course_p%nskip_fs,         &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
! side wall
!
      call set_side_merged_cube(csph_p%num_hemi, csph_p%num_hemi,       &
     &    course_p%nskip_s, course_p%nskip_fs,                          &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
!  top surface
!
      call set_top_merged_cube(csph_p%num_hemi, csph_p%num_hemi,        &
     &    course_p%nskip_s, course_p%nskip_fs,                          &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
      end subroutine set_merged_element_cube_surf
!
! -------------------------------------------------------------------
!
      subroutine set_merged_element_rect_surf                           &
     &         (is_level, csph_p, course_p, c_sphere)
!
!
      use t_numref_cubed_sph
      use set_coarse_cube_surf_nod
!
      integer(kind = kint), intent(in) :: is_level
      type(numref_cubed_sph), intent(in) :: csph_p
      type(coarse_cubed_sph), intent(in) :: course_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: ist, ied, iele
!
!
      ist = c_sphere%iele_stack_sf(is_level-1) + 1
      ied = c_sphere%iele_stack_sf(is_level)
      do iele = ist, ied
        c_sphere%num_merge_e_sf(iele) = course_p%nl_s**2
      end do
!
!   bottom surface (z = -cube_size)
!
      iele = c_sphere%iele_stack_sf(is_level-1)
      call set_bottom_merged_cube                                       &
     &   (csph_p%num_hemi, course_p%nskip_s, course_p%nskip_fs,         &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
! side wall
!
      call set_side_merged_cube(csph_p%num_hemi,                        &
     &    csph_p%ncube_vertical, course_p%nskip_s, course_p%nskip_fs,   &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
!  top surface
!
      call set_top_merged_cube(csph_p%num_hemi, csph_p%ncube_vertical,  &
     &    course_p%nskip_s, course_p%nskip_fs,                          &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
      end subroutine set_merged_element_rect_surf
!
! -------------------------------------------------------------------
!
      end module merged_ele_4_cubed_sph_surf
