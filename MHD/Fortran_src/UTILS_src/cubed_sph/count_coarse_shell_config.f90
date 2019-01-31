!count_coarse_shell_config.f90
!      module count_coarse_shell_config
!
!        programmed by H.Matsui on Apr., 2006
!
!
!!      subroutine count_coarse_cubed_shell(c_sphere, csph_mesh)
!!      subroutine count_coarse_rect_shell(c_sphere, csph_mesh)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!!        type(cubed_sph_mesh), intent(inout) :: csph_mesh
!
      module count_coarse_shell_config
!
      use m_precision
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_mesh
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
      subroutine count_coarse_cubed_shell(c_sphere, csph_mesh)
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
      type(cubed_sph_mesh), intent(inout) :: csph_mesh
!
!
      call count_coarse_cubed_shell_nums(c_sphere)
      call count_nmax_merge_sf(c_sphere)
      call count_coarse_radial_nums(c_sphere, csph_mesh)
!
      end subroutine count_coarse_cubed_shell
!
!   --------------------------------------------------------------------
!
      subroutine count_coarse_rect_shell(c_sphere, csph_mesh)
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
      type(cubed_sph_mesh), intent(inout) :: csph_mesh
!
!
      call count_coarse_rect_shell_nums(c_sphere)
      call count_nmax_merge_sf(c_sphere)
      call count_coarse_radial_nums(c_sphere, csph_mesh)
!
      end subroutine count_coarse_rect_shell
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_coarse_cubed_shell_nums(c_sphere)
!
      use m_numref_cubed_sph
      use count_shell_configration
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
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
      c_sphere%inod_stack_cube(0) =  0
      c_sphere%iele_stack_cube(0) =  0
      c_sphere%iedge_stack_cube(0) = 0
      c_sphere%isurf_stack_cube(0) = 0
      c_sphere%inod_stack_sf(0) =    0
      c_sphere%iele_stack_sf(0) =    0
      c_sphere%iedge_stack_sf(0) =   0
!
      do icoarse = 1, max_coarse_level
        num_h = num_hemi / nstep_coarse(icoarse,1)
!
        call count_center_cube_size(num_h,                              &
     &          nnod_cube, nele_cube, nedge_cube, nsurf_cube,           &
     &          nnod_cube20, nnod_sf, nele_sf, nedge_sf)
!
        c_sphere%inod_stack_cube(icoarse)                               &
     &      = c_sphere%inod_stack_cube(icoarse-1) + nnod_cube
!
        c_sphere%iele_stack_cube(icoarse)                               &
     &      = c_sphere%iele_stack_cube(icoarse-1) + nele_cube
        c_sphere%iedge_stack_cube(icoarse)                              &
     &      = c_sphere%iedge_stack_cube(icoarse-1) + nedge_cube
        c_sphere%isurf_stack_cube(icoarse)                              &
     &      = c_sphere%isurf_stack_cube(icoarse-1) + nsurf_cube
!
        c_sphere%inod_stack_sf(icoarse)                                 &
     &      = c_sphere%inod_stack_sf(icoarse-1) +   nnod_sf
        c_sphere%iele_stack_sf(icoarse)                                 &
     &      = c_sphere%iele_stack_sf(icoarse-1) +   nele_sf
        c_sphere%iedge_stack_sf(icoarse)                                &
     &      = c_sphere%iedge_stack_sf(icoarse-1) + nedge_sf
      end do
      c_sphere%numnod_cube_w_coarse = c_sphere%numnod_cube20            &
     &      + c_sphere%inod_stack_cube(max_coarse_level)
      c_sphere%numele_cube_w_coarse = c_sphere%numele_cube20            &
     &      + c_sphere%iele_stack_cube(max_coarse_level)
      c_sphere%numedge_cube_w_coarse = c_sphere%numedge_cube            &
     &      + c_sphere%iedge_stack_cube(max_coarse_level)
      c_sphere%nsurf_cube_w_coarse =   c_sphere%numsurf_cube            &
     &      + c_sphere%isurf_stack_cube(max_coarse_level)
!
      c_sphere%numnod_sf_w_coarse                                       &
     &     = c_sphere%inod_stack_sf(max_coarse_level)
      c_sphere%numele_sf_w_coarse                                       &
     &     = c_sphere%iele_stack_sf(max_coarse_level)
      c_sphere%numedge_sf_w_coarse                                      &
     &     = c_sphere%iedge_stack_sf(max_coarse_level)
!
      end subroutine count_coarse_cubed_shell_nums
!
!   --------------------------------------------------------------------
!
      subroutine count_coarse_rect_shell_nums(c_sphere)
!
      use m_numref_cubed_sph
      use count_shell_configration
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
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
      c_sphere%inod_stack_cube(0) =  0
      c_sphere%iele_stack_cube(0) =  0
      c_sphere%iedge_stack_cube(0) = 0
      c_sphere%isurf_stack_cube(0) = 0
      c_sphere%inod_stack_sf(0) =    0
      c_sphere%iele_stack_sf(0) =    0
      c_sphere%iedge_stack_sf(0) =   0
!
      do icoarse = 1, max_coarse_level
        num_h = num_hemi / nstep_coarse(icoarse,1)
        num_v = ncube_vertical / nstep_coarse(icoarse,1)
!
        call count_center_rect_size(num_h, num_v,                       &
     &          nnod_cube, nele_cube, nedge_cube, nsurf_cube,           &
     &          nnod_cube20, nnod_sf, nele_sf, nedge_sf)
!
        c_sphere%inod_stack_cube(icoarse)                               &
     &          = c_sphere%inod_stack_cube(icoarse-1) + nnod_cube
!
        c_sphere%iele_stack_cube(icoarse)                               &
     &          = c_sphere%iele_stack_cube(icoarse-1) + nele_cube
        c_sphere%iedge_stack_cube(icoarse)                              &
     &          = c_sphere%iedge_stack_cube(icoarse-1) + nedge_cube
        c_sphere%isurf_stack_cube(icoarse)                              &
     &          = c_sphere%isurf_stack_cube(icoarse-1) + nsurf_cube
!
        c_sphere%inod_stack_sf(icoarse)                                 &
     &          = c_sphere%inod_stack_sf(icoarse-1) +   nnod_sf
        c_sphere%iele_stack_sf(icoarse)                                 &
     &          = c_sphere%iele_stack_sf(icoarse-1) +   nele_sf
        c_sphere%iedge_stack_sf(icoarse)                                &
     &          = c_sphere%iedge_stack_sf(icoarse-1) + nedge_sf
      end do
      c_sphere%numnod_cube_w_coarse = c_sphere%numnod_cube20            &
     &      + c_sphere%inod_stack_cube(max_coarse_level)
      c_sphere%numele_cube_w_coarse = c_sphere%numele_cube20            &
     &      + c_sphere%iele_stack_cube(max_coarse_level)
      c_sphere%numedge_cube_w_coarse = c_sphere%numedge_cube            &
     &      + c_sphere%iedge_stack_cube(max_coarse_level)
      c_sphere%nsurf_cube_w_coarse =   c_sphere%numsurf_cube            &
     &      + c_sphere%isurf_stack_cube(max_coarse_level)
!
      c_sphere%numnod_sf_w_coarse                                       &
     &         =  c_sphere%inod_stack_sf(max_coarse_level)
      c_sphere%numele_sf_w_coarse                                       &
     &         =  c_sphere%iele_stack_sf(max_coarse_level)
      c_sphere%numedge_sf_w_coarse                                      &
     &         = c_sphere%iedge_stack_sf(max_coarse_level)
!
      end subroutine count_coarse_rect_shell_nums
!
!   --------------------------------------------------------------------
!
      subroutine count_nmax_merge_sf(c_sphere)
!
      use m_numref_cubed_sph
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: icoarse, num
!
!
      num = 1
      do icoarse = 1, max_coarse_level
        num = max(num,icoarse_level(icoarse,1))
      end do
      c_sphere%nmax_merge_sf = num*num
!
      end subroutine count_nmax_merge_sf
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine count_coarse_radial_nums(c_sphere, csph_mesh)
!
      use m_numref_cubed_sph
      use count_shell_configration
!
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      type(cubed_sph_mesh), intent(inout) :: csph_mesh
!
      integer(kind = kint) :: icoarse, num_r
!
!
      do icoarse = 1, max_coarse_level
        num_r = c_sphere%nele_shell / nstep_coarse(icoarse,2)
!
        csph_mesh%inod_stack_csph(icoarse)                              &
     &     = csph_mesh%inod_stack_csph(icoarse-1)                       &
     &    + ( c_sphere%inod_stack_cube(icoarse)                         &
     &       - c_sphere%inod_stack_cube(icoarse-1) )                    &
     &    + ( c_sphere%inod_stack_sf(icoarse)                           &
     &       - c_sphere%inod_stack_sf(icoarse-1) ) * (num_r+1)
        csph_mesh%iele_stack_csph(icoarse)                              &
     &     = csph_mesh%iele_stack_csph(icoarse-1)                       &
     &    + ( c_sphere%iele_stack_cube(icoarse)                         &
     &       - c_sphere%iele_stack_cube(icoarse-1) )                    &
     &    + ( c_sphere%iele_stack_sf(icoarse)                           &
     &       - c_sphere%iele_stack_sf(icoarse-1) ) * num_r
        csph_mesh%iedge_stack_csph(icoarse)                             &
     &     = csph_mesh%iedge_stack_csph(icoarse-1)                      &
     &    + ( c_sphere%iedge_stack_cube(icoarse)                        &
     &       - c_sphere%iedge_stack_cube(icoarse-1) )                   &
     &    + ( c_sphere%inod_stack_sf(icoarse)                           &
     &       - c_sphere%inod_stack_sf(icoarse-1)                        &
     &       + c_sphere%iedge_stack_sf(icoarse)                         &
     &       - c_sphere%iedge_stack_sf(icoarse-1) ) * num_r
        csph_mesh%isurf_stack_csph(icoarse)                             &
     &     = csph_mesh%isurf_stack_csph(icoarse-1)                      &
     &    + ( c_sphere%isurf_stack_cube(icoarse)                        &
     &       - c_sphere%isurf_stack_cube(icoarse-1) )                   &
     &    + ( c_sphere%iedge_stack_sf(icoarse)                          &
     &       - c_sphere%iedge_stack_sf(icoarse-1)                       &
     &       + c_sphere%iele_stack_sf(icoarse)                          &
     &       - c_sphere%iele_stack_sf(icoarse-1) ) * num_r
      end do
!
      end subroutine count_coarse_radial_nums
!
!   --------------------------------------------------------------------
!
      end module count_coarse_shell_config
