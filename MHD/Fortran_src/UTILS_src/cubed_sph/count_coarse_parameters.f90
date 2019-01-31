!
!      module count_coarse_parameters
!
!!      subroutine cal_coarse_cube_params(icoarse, c_sphere, csph_mesh)
!!      subroutine cal_coarse_rect_params(icoarse, c_sphere, csph_mesh)
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!
!      Written by Kemorin on Apr., 2006
!
      module count_coarse_parameters
!
      use m_precision
!
      use m_numref_cubed_sph
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
      subroutine cal_coarse_cube_params(icoarse, c_sphere, csph_mesh)
!
      integer(kind = kint), intent(in) :: icoarse
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      type(cubed_sph_mesh), intent(in) :: csph_mesh
!
      nskip_s = nstep_coarse(icoarse,1)
      nskip_r = nstep_coarse(icoarse,2)
!
      nl_s = icoarse_level(icoarse,1)
      nl_r = icoarse_level(icoarse,2)
!
      nskip_fs = nskip_s / nl_s
      nskip_fr = nskip_r / nl_r
!
      n_hemi_c = num_hemi / nskip_s
      n_hemi_fc = num_hemi / nskip_fs
!
      nr_c = c_sphere%nele_shell / nskip_r
!
      nl_3 = nl_s*nl_s*nl_s
      nl_shell = nl_s*nl_s*nl_r
!
!
      numnod_coarse = csph_mesh%inod_stack_csph(icoarse)                &
     &               - csph_mesh%inod_stack_csph(icoarse-1)
      numele_coarse = csph_mesh%iele_stack_csph(icoarse)                &
     &               - csph_mesh%iele_stack_csph(icoarse-1)
!
      nnod_cube_c = c_sphere%inod_stack_cube(icoarse)                   &
     &             - c_sphere%inod_stack_cube(icoarse-1)
      nnod_sf_c = c_sphere%inod_stack_sf(icoarse)                       &
     &           - c_sphere%inod_stack_sf(icoarse-1)
!
      nele_cube_c = c_sphere%iele_stack_cube(icoarse)                   &
     &             - c_sphere%iele_stack_cube(icoarse-1)
      nele_sf_c = c_sphere%iele_stack_sf(icoarse)                       &
     &           - c_sphere%iele_stack_sf(icoarse-1)
      nele_shell_c = nele_sf_c * c_sphere%nele_shell / nskip_r
!
      if (icoarse.eq.1) then
        nnod_cube_fc = c_sphere%numnod_cube
        nnod_sf_fc =   c_sphere%numnod_sf
        nele_cube_fc = c_sphere%numele_cube
        nele_sf_fc =   c_sphere%numele_sf
      else
        nnod_cube_fc                                                    &
     &      = c_sphere%inod_stack_cube(icoarse-1)                       &
     &       - c_sphere%inod_stack_cube(icoarse-2)
        nnod_sf_fc                                                      &
     &      = c_sphere%inod_stack_sf(icoarse-1)                         &
     &       - c_sphere%inod_stack_sf(icoarse-2)

        nele_cube_fc                                                    &
     &      = c_sphere%iele_stack_cube(icoarse-1)                       &
     &       - c_sphere%iele_stack_cube(icoarse-2)
        nele_sf_fc                                                      &
     &      = c_sphere%iele_stack_sf(icoarse-1)                         &
     &       - c_sphere%iele_stack_sf(icoarse-2)
      end if
!
!
      end subroutine cal_coarse_cube_params
!
!   --------------------------------------------------------------------
!
      subroutine cal_coarse_rect_params(icoarse, c_sphere, csph_mesh)
!
      integer(kind = kint), intent(in) :: icoarse
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      type(cubed_sph_mesh), intent(in) :: csph_mesh
!
      nskip_s = nstep_coarse(icoarse,1)
      nskip_r = nstep_coarse(icoarse,2)
!
      nl_s = icoarse_level(icoarse,1)
      nl_r = icoarse_level(icoarse,2)
!
      nskip_fs = nskip_s / nl_s
      nskip_fr = nskip_r / nl_r
!
      n_hemi_c =  num_hemi / nskip_s
      n_hemi_fc = num_hemi / nskip_fs
!
      n_vert_c =  ncube_vertical / nskip_s
!
      nr_c = c_sphere%nele_shell / nskip_r
!
      nl_3 = nl_s*nl_s*nl_s
      nl_shell = nl_s*nl_s*nl_r
!
!
      numnod_coarse = csph_mesh%inod_stack_csph(icoarse)                &
     &               - csph_mesh%inod_stack_csph(icoarse-1)
      numele_coarse = csph_mesh%iele_stack_csph(icoarse)                &
     &               - csph_mesh%iele_stack_csph(icoarse-1)
!
      nnod_cube_c = c_sphere%inod_stack_cube(icoarse)                   &
     &             - c_sphere%inod_stack_cube(icoarse-1)
      nnod_sf_c = c_sphere%inod_stack_sf(icoarse)                       &
     &           - c_sphere%inod_stack_sf(icoarse-1)
!
      nele_cube_c = c_sphere%iele_stack_cube(icoarse)                   &
     &             - c_sphere%iele_stack_cube(icoarse-1)
      nele_sf_c = c_sphere%iele_stack_sf(icoarse)                       &
     &           - c_sphere%iele_stack_sf(icoarse-1)
      nele_shell_c = nele_sf_c * c_sphere%nele_shell / nskip_r
!
      if (icoarse.eq.1) then
        nnod_cube_fc = c_sphere%numnod_cube
        nnod_sf_fc =   c_sphere%numnod_sf
        nele_cube_fc = c_sphere%numele_cube
        nele_sf_fc =   c_sphere%numele_sf
      else
        nnod_cube_fc = c_sphere%inod_stack_cube(icoarse-1)              &
     &                - c_sphere%inod_stack_cube(icoarse-2)
        nnod_sf_fc = c_sphere%inod_stack_sf(icoarse-1)                  &
     &              - c_sphere%inod_stack_sf(icoarse-2)

        nele_cube_fc = c_sphere%iele_stack_cube(icoarse-1)              &
     &                - c_sphere%iele_stack_cube(icoarse-2)
        nele_sf_fc = c_sphere%iele_stack_sf(icoarse-1)                  &
     &              - c_sphere%iele_stack_sf(icoarse-2)
      end if
!
!
      end subroutine cal_coarse_rect_params
!
!   --------------------------------------------------------------------
!
      end module count_coarse_parameters
