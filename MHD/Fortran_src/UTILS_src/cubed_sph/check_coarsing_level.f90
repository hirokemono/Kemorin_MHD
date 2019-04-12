!
!      module check_coarsing_level
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine check_cube_coarsing_level                            &
!!     &         (nr_icb, rprm_csph, csph_p, course_p, c_sphere)
!!      subroutine check_rect_coarsing_level                            &
!!     &         (nr_icb, rprm_csph, csph_p, course_p, c_sphere)
!!        type(cubed_sph_radius), intent(in) :: rprm_csph
!!        type(numref_cubed_sph), intent(in) :: csph_p
!!        type(coarse_cubed_sph), intent(inout) :: course_p
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module check_coarsing_level
!
      use m_precision
!
      use t_numref_cubed_sph
      use t_cubed_sph_radius
      use t_cubed_sph_surf_mesh
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine check_cube_coarsing_level                              &
     &         (nr_icb, rprm_csph, csph_p, course_p, c_sphere)
!
      integer(kind = kint), intent(in) :: nr_icb
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(numref_cubed_sph), intent(in) :: csph_p
      type(coarse_cubed_sph), intent(inout) :: course_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: i, ilevel_tmp
      integer(kind = kint) :: i_sflag, i_riflag, i_roflag, i_rmflag
!
!
      do i = 1, course_p%max_coarse_level
        course_p%nstep_coarse(i,1)                                      &
     &    = course_p%nstep_coarse(i-1,1) * course_p%icoarse_level(i,1)
        course_p%nstep_coarse(i,2)                                      &
     &    = course_p%nstep_coarse(i-1,2) * course_p%icoarse_level(i,2)
      end do
!
      ilevel_tmp = course_p%max_coarse_level
      do i = ilevel_tmp, 1, -1
        i_sflag =  mod(csph_p%num_hemi,course_p%nstep_coarse(i,1))
        i_riflag = mod(nr_icb,course_p%nstep_coarse(i,2))
        i_roflag = mod(rprm_csph%nr_ocore,course_p%nstep_coarse(i,2))
        i_rmflag = mod(rprm_csph%nr_exter,course_p%nstep_coarse(i,2))
!
        if (i_riflag.eq.0 .and. i_roflag.eq.0 .and. i_rmflag.eq.0       &
     &     .and. i_sflag.eq.0) then
          exit
        else if (i_riflag.eq.0 .and. i_roflag.eq.0 .and. i_rmflag.eq.0  &
     &     .and. i_sflag.ne.0) then
          course_p%nstep_coarse(i,1) = course_p%nstep_coarse(i-1,1)
        else
          if (i_sflag.eq.0) then
            course_p%nstep_coarse(i,2) = course_p%nstep_coarse(i-1,2)
          else
            course_p%max_coarse_level = course_p%max_coarse_level - 1
          end if
        end if
!
      end do
!
       c_sphere%max_merge_e = 1
       do i = 1, course_p%max_coarse_level
         course_p%nl_3 = course_p%icoarse_level(i,1)**3
         course_p%nl_shell = course_p%icoarse_level(i,1)**2             &
     &                      * course_p%icoarse_level(i,2)
         c_sphere%max_merge_e = max(c_sphere%max_merge_e,course_p%nl_3)
         c_sphere%max_merge_e                                           &
     &         = max(c_sphere%max_merge_e,course_p%nl_shell)
       end do
!
      end subroutine check_cube_coarsing_level
!
!   --------------------------------------------------------------------
!
      subroutine check_rect_coarsing_level                              &
     &         (nr_icb, rprm_csph, csph_p, course_p, c_sphere)
!
      integer(kind = kint), intent(in) :: nr_icb
      type(cubed_sph_radius), intent(in) :: rprm_csph
      type(numref_cubed_sph), intent(in) :: csph_p
      type(coarse_cubed_sph), intent(inout) :: course_p
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: i, ilevel_tmp
      integer(kind = kint) :: i_sflag, i_riflag, i_roflag, i_rmflag
      integer(kind = kint) :: i_vflag
!
!
      do i = 1, course_p%max_coarse_level
        course_p%nstep_coarse(i,1)                                      &
     &    = course_p%nstep_coarse(i-1,1) * course_p%icoarse_level(i,1)
        course_p%nstep_coarse(i,2)                                      &
     &    = course_p%nstep_coarse(i-1,2) * course_p%icoarse_level(i,2)
      end do
!
      ilevel_tmp = course_p%max_coarse_level
      do i = ilevel_tmp, 1, -1
        i_sflag = mod(csph_p%num_hemi,course_p%nstep_coarse(i,1))
        i_vflag = mod(csph_p%ncube_vertical,course_p%nstep_coarse(i,1))
        i_riflag = mod(nr_icb,course_p%nstep_coarse(i,2))
        i_roflag = mod(rprm_csph%nr_ocore,course_p%nstep_coarse(i,2))
        i_rmflag = mod(rprm_csph%nr_exter,course_p%nstep_coarse(i,2))
!
        if (i_riflag.eq.0 .and. i_roflag.eq.0 .and. i_rmflag.eq.0       &
     &     .and. i_sflag.eq.0 .and. i_vflag.eq.0) then
          exit
        else if (i_riflag.eq.0 .and. i_roflag.eq.0 .and. i_rmflag.eq.0  &
     &     .and. i_sflag.ne.0 .and. i_vflag.eq.0) then
          course_p%nstep_coarse(i,1) = course_p%nstep_coarse(i-1,1)
        else
          if (i_sflag.eq.0 .and. i_vflag.eq.0) then
            course_p%nstep_coarse(i,2) = course_p%nstep_coarse(i-1,2)
          else
            course_p%max_coarse_level = course_p%max_coarse_level - 1
          end if
        end if
!
      end do
!
       c_sphere%max_merge_e = 1
       do i = 1, course_p%max_coarse_level
         course_p%nl_3 = course_p%icoarse_level(i,1)**3
         course_p%nl_shell = course_p%icoarse_level(i,1)**2             &
     &                      * course_p%icoarse_level(i,2)
         c_sphere%max_merge_e = max(c_sphere%max_merge_e,course_p%nl_3)
         c_sphere%max_merge_e                                           &
     &         = max(c_sphere%max_merge_e,course_p%nl_shell)
       end do
!
      end subroutine check_rect_coarsing_level
!
!   --------------------------------------------------------------------
!
      end module check_coarsing_level
