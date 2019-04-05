!
!      module count_coarse_parameters
!
!      Written by Kemorin on Apr., 2006
!
!!      subroutine cal_coarse_cube_params                               &
!!     &         (icoarse, c_sphere, csph_mesh, csph_p, course_p)
!!      subroutine cal_coarse_rect_params                               &
!!     &          (icoarse, c_sphere, csph_mesh, csph_p, course_p)
!!        type(numref_cubed_sph), intent(in) :: csph_p
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!!        type(coarse_cubed_sph), intent(inout) :: course_p
!
      module count_coarse_parameters
!
      use m_precision
!
      use t_numref_cubed_sph
      use t_cubed_sph_mesh
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
      subroutine cal_coarse_cube_params                                 &
     &         (icoarse, c_sphere, csph_mesh, csph_p, course_p)
!
      integer(kind = kint), intent(in) :: icoarse
      type(numref_cubed_sph), intent(in) :: csph_p
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      type(cubed_sph_mesh), intent(in) :: csph_mesh
!
      type(coarse_cubed_sph), intent(inout) :: course_p
!
!
      course_p%nskip_s = course_p%nstep_coarse(icoarse,1)
      course_p%nskip_r = course_p%nstep_coarse(icoarse,2)
!
      course_p%nl_s = course_p%icoarse_level(icoarse,1)
      course_p%nl_r = course_p%icoarse_level(icoarse,2)
!
      course_p%nskip_fs = course_p%nskip_s / course_p%nl_s
      course_p%nskip_fr = course_p%nskip_r / course_p%nl_r
!
      course_p%n_hemi_c =  csph_p%num_hemi / course_p%nskip_s
      course_p%n_hemi_fc = csph_p%num_hemi / course_p%nskip_fs
!
      course_p%nr_c = c_sphere%nele_shell / course_p%nskip_r
!
      course_p%nl_3 = course_p%nl_s**3
      course_p%nl_shell = course_p%nl_s**2 * course_p%nl_r
!
!
      course_p%numnod_coarse = csph_mesh%inod_stack_csph(icoarse)       &
     &                        - csph_mesh%inod_stack_csph(icoarse-1)
      course_p%numele_coarse = csph_mesh%iele_stack_csph(icoarse)       &
     &                        - csph_mesh%iele_stack_csph(icoarse-1)
!
      course_p%nnod_cube_c = c_sphere%inod_stack_cube(icoarse)          &
     &                      - c_sphere%inod_stack_cube(icoarse-1)
      course_p%nnod_sf_c = c_sphere%inod_stack_sf(icoarse)              &
     &                    - c_sphere%inod_stack_sf(icoarse-1)
!
      course_p%nele_cube_c = c_sphere%iele_stack_cube(icoarse)          &
     &                      - c_sphere%iele_stack_cube(icoarse-1)
      course_p%nele_sf_c = c_sphere%iele_stack_sf(icoarse)              &
     &                    - c_sphere%iele_stack_sf(icoarse-1)
      course_p%nele_shell_c = course_p%nele_sf_c * c_sphere%nele_shell  &
     &                       / course_p%nskip_r
!
      if (icoarse.eq.1) then
        course_p%nnod_cube_fc = c_sphere%numnod_cube
        course_p%nnod_sf_fc =   c_sphere%numnod_sf
        course_p%nele_cube_fc = c_sphere%numele_cube
        course_p%nele_sf_fc =   c_sphere%numele_sf
      else
        course_p%nnod_cube_fc                                           &
     &      = c_sphere%inod_stack_cube(icoarse-1)                       &
     &       - c_sphere%inod_stack_cube(icoarse-2)
        course_p%nnod_sf_fc                                             &
     &      = c_sphere%inod_stack_sf(icoarse-1)                         &
     &       - c_sphere%inod_stack_sf(icoarse-2)

        course_p%nele_cube_fc                                           &
     &      = c_sphere%iele_stack_cube(icoarse-1)                       &
     &       - c_sphere%iele_stack_cube(icoarse-2)
        course_p%nele_sf_fc                                             &
     &      = c_sphere%iele_stack_sf(icoarse-1)                         &
     &       - c_sphere%iele_stack_sf(icoarse-2)
      end if
!
!
      end subroutine cal_coarse_cube_params
!
!   --------------------------------------------------------------------
!
      subroutine cal_coarse_rect_params                                 &
     &          (icoarse, c_sphere, csph_mesh, csph_p, course_p)
!
      integer(kind = kint), intent(in) :: icoarse
      type(numref_cubed_sph), intent(in) :: csph_p
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      type(cubed_sph_mesh), intent(in) :: csph_mesh
!
      type(coarse_cubed_sph), intent(inout) :: course_p
!
!
      course_p%nskip_s = course_p%nstep_coarse(icoarse,1)
      course_p%nskip_r = course_p%nstep_coarse(icoarse,2)
!
      course_p%nl_s = course_p%icoarse_level(icoarse,1)
      course_p%nl_r = course_p%icoarse_level(icoarse,2)
!
      course_p%nskip_fs = course_p%nskip_s / course_p%nl_s
      course_p%nskip_fr = course_p%nskip_r / course_p%nl_r
!
      course_p%n_hemi_c =  csph_p%num_hemi / course_p%nskip_s
      course_p%n_hemi_fc = csph_p%num_hemi / course_p%nskip_fs
!
      course_p%n_vert_c =  csph_p%ncube_vertical / course_p%nskip_s
!
      course_p%nr_c = c_sphere%nele_shell / course_p%nskip_r
!
      course_p%nl_3 = course_p%nl_s**3
      course_p%nl_shell = course_p%nl_s**2 * course_p%nl_r
!
!
      course_p%numnod_coarse = csph_mesh%inod_stack_csph(icoarse)       &
     &                        - csph_mesh%inod_stack_csph(icoarse-1)
      course_p%numele_coarse = csph_mesh%iele_stack_csph(icoarse)       &
     &                        - csph_mesh%iele_stack_csph(icoarse-1)
!
      course_p%nnod_cube_c = c_sphere%inod_stack_cube(icoarse)          &
     &                      - c_sphere%inod_stack_cube(icoarse-1)
      course_p%nnod_sf_c = c_sphere%inod_stack_sf(icoarse)              &
     &                    - c_sphere%inod_stack_sf(icoarse-1)
!
      course_p%nele_cube_c = c_sphere%iele_stack_cube(icoarse)          &
     &                      - c_sphere%iele_stack_cube(icoarse-1)
      course_p%nele_sf_c = c_sphere%iele_stack_sf(icoarse)              &
     &                    - c_sphere%iele_stack_sf(icoarse-1)
      course_p%nele_shell_c = course_p%nele_sf_c * c_sphere%nele_shell  &
     &                       / course_p%nskip_r
!
      if (icoarse.eq.1) then
        course_p%nnod_cube_fc = c_sphere%numnod_cube
        course_p%nnod_sf_fc =   c_sphere%numnod_sf
        course_p%nele_cube_fc = c_sphere%numele_cube
        course_p%nele_sf_fc =   c_sphere%numele_sf
      else
        course_p%nnod_cube_fc = c_sphere%inod_stack_cube(icoarse-1)     &
     &                         - c_sphere%inod_stack_cube(icoarse-2)
        course_p%nnod_sf_fc = c_sphere%inod_stack_sf(icoarse-1)         &
     &                       - c_sphere%inod_stack_sf(icoarse-2)

        course_p%nele_cube_fc = c_sphere%iele_stack_cube(icoarse-1)     &
     &                         - c_sphere%iele_stack_cube(icoarse-2)
        course_p%nele_sf_fc = c_sphere%iele_stack_sf(icoarse-1)         &
     &                       - c_sphere%iele_stack_sf(icoarse-2)
      end if
!
!
      end subroutine cal_coarse_rect_params
!
!   --------------------------------------------------------------------
!
      end module count_coarse_parameters
