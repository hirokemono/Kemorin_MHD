!merged_ele_4_cubed_sph_surf.f90
!      module merged_ele_4_cubed_sph_surf
!
!      Written by H. Matsui on Apr., 2006
!      Modified by H. Matsui on Oct., 2007
!
!!      subroutine set_merged_element_cube_surf(is_level, c_sphere)
!!      subroutine set_merged_element_rect_surf(is_level, c_sphere)
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
      subroutine set_merged_element_cube_surf(is_level, c_sphere)
!
!
      use m_numref_cubed_sph
      use set_coarse_cube_surf_nod
!
      integer(kind = kint), intent(in) :: is_level
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: ist, ied, iele
!
!
      ist = c_sphere%iele_stack_sf(is_level-1) + 1
      ied = c_sphere%iele_stack_sf(is_level)
      do iele = ist, ied
        c_sphere%num_merge_e_sf(iele) = nl_s*nl_s
      end do
!
!   bottom surface (z = -cube_size)
!
      iele = c_sphere%iele_stack_sf(is_level-1)
      call set_bottom_merged_cube(num_hemi, nskip_s, nskip_fs,          &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
! side wall
!
      call set_side_merged_cube(num_hemi, num_hemi, nskip_s, nskip_fs,  &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
!  top surface
!
      call set_top_merged_cube(num_hemi, num_hemi, nskip_s, nskip_fs,   &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
      end subroutine set_merged_element_cube_surf
!
! -------------------------------------------------------------------
!
      subroutine set_merged_element_rect_surf(is_level, c_sphere)
!
!
      use m_numref_cubed_sph
      use set_coarse_cube_surf_nod
!
      integer(kind = kint), intent(in) :: is_level
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: ist, ied, iele
!
!
      ist = c_sphere%iele_stack_sf(is_level-1) + 1
      ied = c_sphere%iele_stack_sf(is_level)
      do iele = ist, ied
        c_sphere%num_merge_e_sf(iele) = nl_s*nl_s
      end do
!
!   bottom surface (z = -cube_size)
!
      iele = c_sphere%iele_stack_sf(is_level-1)
      call set_bottom_merged_cube(num_hemi, nskip_s, nskip_fs,          &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
! side wall
!
      call set_side_merged_cube                                         &
     &   (num_hemi, ncube_vertical, nskip_s, nskip_fs,                  &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
!  top surface
!
      call set_top_merged_cube                                          &
     &   (num_hemi, ncube_vertical, nskip_s, nskip_fs,                  &
     &    c_sphere%numnod_sf_w_coarse, c_sphere%nmax_merge_sf,          &
     &    iele, c_sphere%imerge_e_sf)
!
      end subroutine set_merged_element_rect_surf
!
! -------------------------------------------------------------------
!
      end module merged_ele_4_cubed_sph_surf
