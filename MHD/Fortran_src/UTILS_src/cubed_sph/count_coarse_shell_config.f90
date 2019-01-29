!count_coarse_shell_config.f90
!      module count_coarse_shell_config
!
!        programmed by H.Matsui on Apr., 2006
!
!
!      subroutine count_coarse_cubed_shell
!      subroutine count_coarse_rect_shell
!
      module count_coarse_shell_config
!
      use m_precision
!
      implicit  none
!
      private :: count_coarse_cubed_shell_nums
      private :: count_coarse_rect_shell_nums
      private :: count_coarse_radial_nums
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine count_coarse_cubed_shell
!
!
      call count_coarse_cubed_shell_nums
      call count_coarse_radial_nums
!
      end subroutine count_coarse_cubed_shell
!
!   --------------------------------------------------------------------
!
      subroutine count_coarse_rect_shell
!
!
      call count_coarse_rect_shell_nums
      call count_coarse_radial_nums
!
      end subroutine count_coarse_rect_shell
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_coarse_cubed_shell_nums
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use count_shell_configration
!
      integer(kind = kint) :: icoarse, num_h
!
      integer(kind = kint) :: nnod_cube, nele_cube
      integer(kind = kint) :: nedge_cube, nsurf_cube
      integer(kind = kint) :: nnod_cube20
      integer(kind = kint) :: nnod_sf, nele_sf
      integer(kind = kint) :: nedge_sf
!
!
!
      inod_stack_cube(0) =  0
      iele_stack_cube(0) =  0
      iedge_stack_cube(0) = 0
      isurf_stack_cube(0) = 0
      inod_stack_sf(0) =    0
      iele_stack_sf(0) =    0
      iedge_stack_sf(0) =   0
!
      do icoarse = 1, max_coarse_level
        num_h = num_hemi / nstep_coarse(icoarse,1)
!
        call count_center_cube_size(num_h,                              &
     &          nnod_cube, nele_cube, nedge_cube, nsurf_cube,           &
     &          nnod_cube20, nnod_sf, nele_sf, nedge_sf)
!
        inod_stack_cube(icoarse) = inod_stack_cube(icoarse-1)           &
     &                           + nnod_cube
!
        iele_stack_cube(icoarse) = iele_stack_cube(icoarse-1)           &
     &                             + nele_cube
        iedge_stack_cube(icoarse) = iedge_stack_cube(icoarse-1)         &
     &                             + nedge_cube
        isurf_stack_cube(icoarse) = isurf_stack_cube(icoarse-1)         &
     &                             + nsurf_cube
!
        inod_stack_sf(icoarse) = inod_stack_sf(icoarse-1) +   nnod_sf
        iele_stack_sf(icoarse) = iele_stack_sf(icoarse-1) +   nele_sf
        iedge_stack_sf(icoarse) = iedge_stack_sf(icoarse-1) + nedge_sf
      end do
      numnod_cube_w_coarse = c_sphere1%numnod_cube20                    &
     &      + inod_stack_cube(max_coarse_level)
      numele_cube_w_coarse = c_sphere1%numele_cube20                    &
     &      + iele_stack_cube(max_coarse_level)
      numedge_cube_w_coarse = c_sphere1%numedge_cube                    &
     &      + iedge_stack_cube(max_coarse_level)
      nsurf_cube_w_coarse =   c_sphere1%numsurf_cube                    &
     &      + isurf_stack_cube(max_coarse_level)
!
      numnod_sf_w_coarse =  inod_stack_sf(max_coarse_level)
      numele_sf_w_coarse =  iele_stack_sf(max_coarse_level)
      numedge_sf_w_coarse = iedge_stack_sf(max_coarse_level)
!
      end subroutine count_coarse_cubed_shell_nums
!
!   --------------------------------------------------------------------
!
      subroutine count_coarse_rect_shell_nums
!
      use m_numref_cubed_sph
      use m_cubed_sph_surf_mesh
      use count_shell_configration
!
      integer(kind = kint) :: icoarse, num_h, num_v
!
      integer(kind = kint) :: nnod_cube, nele_cube
      integer(kind = kint) :: nedge_cube, nsurf_cube
      integer(kind = kint) :: nnod_cube20
      integer(kind = kint) :: nnod_sf, nele_sf
      integer(kind = kint) :: nedge_sf
!
!
!
      inod_stack_cube(0) =  0
      iele_stack_cube(0) =  0
      iedge_stack_cube(0) = 0
      isurf_stack_cube(0) = 0
      inod_stack_sf(0) =    0
      iele_stack_sf(0) =    0
      iedge_stack_sf(0) =   0
!
      do icoarse = 1, max_coarse_level
        num_h = num_hemi / nstep_coarse(icoarse,1)
        num_v = ncube_vertical / nstep_coarse(icoarse,1)
!
        call count_center_rect_size(num_h, num_v,                       &
     &          nnod_cube, nele_cube, nedge_cube, nsurf_cube,           &
     &          nnod_cube20, nnod_sf, nele_sf, nedge_sf)
!
        inod_stack_cube(icoarse) = inod_stack_cube(icoarse-1)           &
     &                           + nnod_cube
!
        iele_stack_cube(icoarse) = iele_stack_cube(icoarse-1)           &
     &                             + nele_cube
        iedge_stack_cube(icoarse) = iedge_stack_cube(icoarse-1)         &
     &                             + nedge_cube
        isurf_stack_cube(icoarse) = isurf_stack_cube(icoarse-1)         &
     &                             + nsurf_cube
!
        inod_stack_sf(icoarse) = inod_stack_sf(icoarse-1) +   nnod_sf
        iele_stack_sf(icoarse) = iele_stack_sf(icoarse-1) +   nele_sf
        iedge_stack_sf(icoarse) = iedge_stack_sf(icoarse-1) + nedge_sf
      end do
      numnod_cube_w_coarse = c_sphere1%numnod_cube20                    &
     &      + inod_stack_cube(max_coarse_level)
      numele_cube_w_coarse = c_sphere1%numele_cube20                    &
     &      + iele_stack_cube(max_coarse_level)
      numedge_cube_w_coarse = c_sphere1%numedge_cube                    &
     &      + iedge_stack_cube(max_coarse_level)
      nsurf_cube_w_coarse =   c_sphere1%numsurf_cube                    &
     &      + isurf_stack_cube(max_coarse_level)
!
      numnod_sf_w_coarse =  inod_stack_sf(max_coarse_level)
      numele_sf_w_coarse =  iele_stack_sf(max_coarse_level)
      numedge_sf_w_coarse = iedge_stack_sf(max_coarse_level)
!
      end subroutine count_coarse_rect_shell_nums
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_coarse_radial_nums
!
      use m_numref_cubed_sph
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_radius
      use count_shell_configration
!
      integer(kind = kint) :: icoarse, num, num_r
!
!
      num = 1
      do icoarse = 1, max_coarse_level
        num = max(num,icoarse_level(icoarse,1))
      end do
      nmax_merge_sf = num*num
!
      do icoarse = 1, max_coarse_level
        num_r = c_sphere1%nele_shell / nstep_coarse(icoarse,2)
!
        inod_stack(icoarse) = inod_stack(icoarse-1)                     &
     &    + ( inod_stack_cube(icoarse) - inod_stack_cube(icoarse-1) )   &
     &    + ( inod_stack_sf(icoarse) - inod_stack_sf(icoarse-1) )       &
     &     * (num_r+1)
        iele_stack(icoarse) = iele_stack(icoarse-1)                     &
     &    + ( iele_stack_cube(icoarse) - iele_stack_cube(icoarse-1) )   &
     &    + ( iele_stack_sf(icoarse) - iele_stack_sf(icoarse-1) )       &
     &     * num_r
        iedge_stack(icoarse) = iedge_stack(icoarse-1)                   &
     &    + ( iedge_stack_cube(icoarse) - iedge_stack_cube(icoarse-1) ) &
     &    + ( inod_stack_sf(icoarse) - inod_stack_sf(icoarse-1)         &
     &       + iedge_stack_sf(icoarse) - iedge_stack_sf(icoarse-1) )    &
     &     * num_r
        isurf_stack(icoarse) = isurf_stack(icoarse-1)                   &
     &    + ( isurf_stack_cube(icoarse) - isurf_stack_cube(icoarse-1) ) &
     &    + ( iedge_stack_sf(icoarse) - iedge_stack_sf(icoarse-1)       &
     &       + iele_stack_sf(icoarse) - iele_stack_sf(icoarse-1) )      &
     &     * num_r
      end do
!
      end subroutine count_coarse_radial_nums
!
!
!   --------------------------------------------------------------------
!
      end module count_coarse_shell_config
