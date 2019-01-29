!
!      module count_coarse_parameters
!
!      subroutine cal_coarse_cube_params(icoarse)
!      subroutine cal_coarse_rect_params(icoarse)
!
!      Written by Kemorin on Apr., 2006
!
      module count_coarse_parameters
!
      use m_precision
!
      use m_numref_cubed_sph
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine cal_coarse_cube_params(icoarse)
!
      integer(kind = kint), intent(in) :: icoarse
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
      nr_c = c_sphere1%nele_shell / nskip_r
!
      nl_3 = nl_s*nl_s*nl_s
      nl_shell = nl_s*nl_s*nl_r
!
!
      numnod_coarse = inod_stack(icoarse) - inod_stack(icoarse-1)
      numele_coarse = iele_stack(icoarse) - iele_stack(icoarse-1)
!
      nnod_cube_c = inod_stack_cube(icoarse)                            &
     &             - inod_stack_cube(icoarse-1)
      nnod_sf_c = inod_stack_sf(icoarse) - inod_stack_sf(icoarse-1)
!
      nele_cube_c = iele_stack_cube(icoarse)                            &
     &             - iele_stack_cube(icoarse-1)
      nele_sf_c = iele_stack_sf(icoarse)-iele_stack_sf(icoarse-1)
      nele_shell_c = nele_sf_c * c_sphere1%nele_shell / nskip_r
!
      if (icoarse.eq.1) then
        nnod_cube_fc = c_sphere1%numnod_cube
        nnod_sf_fc =   c_sphere1%numnod_sf
        nele_cube_fc = numele_cube
        nele_sf_fc =   c_sphere1%numele_sf
      else
        nnod_cube_fc                                                    &
     &      = inod_stack_cube(icoarse-1) - inod_stack_cube(icoarse-2)
        nnod_sf_fc                                                      &
     &      =  inod_stack_sf(icoarse-1) - inod_stack_sf(icoarse-2)

        nele_cube_fc                                                    &
     &      = iele_stack_cube(icoarse-1) - iele_stack_cube(icoarse-2)
        nele_sf_fc                                                      &
     &      = iele_stack_sf(icoarse-1)-iele_stack_sf(icoarse-2)
      end if
!
!
      end subroutine cal_coarse_cube_params
!
!   --------------------------------------------------------------------
!
      subroutine cal_coarse_rect_params(icoarse)
!
      integer(kind = kint), intent(in) :: icoarse
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
      nr_c = c_sphere1%nele_shell / nskip_r
!
      nl_3 = nl_s*nl_s*nl_s
      nl_shell = nl_s*nl_s*nl_r
!
!
      numnod_coarse = inod_stack(icoarse) - inod_stack(icoarse-1)
      numele_coarse = iele_stack(icoarse) - iele_stack(icoarse-1)
!
      nnod_cube_c = inod_stack_cube(icoarse)                            &
     &             - inod_stack_cube(icoarse-1)
      nnod_sf_c = inod_stack_sf(icoarse) - inod_stack_sf(icoarse-1)
!
      nele_cube_c = iele_stack_cube(icoarse)                            &
     &             - iele_stack_cube(icoarse-1)
      nele_sf_c = iele_stack_sf(icoarse)-iele_stack_sf(icoarse-1)
      nele_shell_c = nele_sf_c * c_sphere1%nele_shell / nskip_r
!
      if (icoarse.eq.1) then
        nnod_cube_fc = c_sphere1%numnod_cube
        nnod_sf_fc =   c_sphere1%numnod_sf
        nele_cube_fc = numele_cube
        nele_sf_fc =   c_sphere1%numele_sf
      else
        nnod_cube_fc                                                    &
     &      = inod_stack_cube(icoarse-1) - inod_stack_cube(icoarse-2)
        nnod_sf_fc                                                      &
     &      =  inod_stack_sf(icoarse-1) - inod_stack_sf(icoarse-2)

        nele_cube_fc                                                    &
     &      = iele_stack_cube(icoarse-1) - iele_stack_cube(icoarse-2)
        nele_sf_fc                                                      &
     &      = iele_stack_sf(icoarse-1)-iele_stack_sf(icoarse-2)
      end if
!
!
      end subroutine cal_coarse_rect_params
!
!   --------------------------------------------------------------------
!
      end module count_coarse_parameters
