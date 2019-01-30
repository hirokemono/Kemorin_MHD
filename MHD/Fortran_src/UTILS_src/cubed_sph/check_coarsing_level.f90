!
!      module check_coarsing_level
!
!        programmed by H.Matsui on Apr., 2006
!
!!      subroutine check_cube_coarsing_level(c_sphere)
!!      subroutine check_rect_coarsing_level(c_sphere)
!!        type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      module check_coarsing_level
!
      use m_precision
!
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
      subroutine check_cube_coarsing_level(c_sphere)
!
      use m_numref_cubed_sph
      use m_cubed_sph_radius
      use m_cubed_sph_grp_param
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: i, ilevel_tmp
      integer(kind = kint) :: i_sflag, i_riflag, i_roflag, i_rmflag
!
!
      do i = 1, max_coarse_level
        nstep_coarse(i,1) = nstep_coarse(i-1,1) * icoarse_level(i,1)
        nstep_coarse(i,2) = nstep_coarse(i-1,2) * icoarse_level(i,2)
      end do
!
      ilevel_tmp = max_coarse_level
      do i = ilevel_tmp, 1, -1
        i_sflag =  mod(num_hemi,nstep_coarse(i,1))
        i_riflag = mod(nr_icb,nstep_coarse(i,2))
        i_roflag = mod(nr_ocore,nstep_coarse(i,2))
        i_rmflag = mod(nr_exter,nstep_coarse(i,2))
!
        if (i_riflag.eq.0 .and. i_roflag.eq.0 .and. i_rmflag.eq.0       &
     &     .and. i_sflag.eq.0) then
          exit
        else if (i_riflag.eq.0 .and. i_roflag.eq.0 .and. i_rmflag.eq.0  &
     &     .and. i_sflag.ne.0) then
          nstep_coarse(i,1) = nstep_coarse(i-1,1)
        else
          if (i_sflag.eq.0) then
            nstep_coarse(i,2) = nstep_coarse(i-1,2)
          else
            max_coarse_level = max_coarse_level - 1
          end if
        end if
!
      end do
!
       c_sphere%max_merge_e = 1
       do i = 1, max_coarse_level
         nl_3 = icoarse_level(i,1)**3
         nl_shell = icoarse_level(i,1)**2 * icoarse_level(i,2)
         c_sphere%max_merge_e = max(c_sphere%max_merge_e,nl_3)
         c_sphere%max_merge_e = max(c_sphere%max_merge_e,nl_shell)
       end do
!
      end subroutine check_cube_coarsing_level
!
!   --------------------------------------------------------------------
!
      subroutine check_rect_coarsing_level(c_sphere)
!
      use m_numref_cubed_sph
      use m_cubed_sph_radius
      use m_cubed_sph_grp_param
!
      type(cubed_sph_surf_mesh), intent(inout) :: c_sphere
!
      integer(kind = kint) :: i, ilevel_tmp
      integer(kind = kint) :: i_sflag, i_riflag, i_roflag, i_rmflag
      integer(kind = kint) :: i_vflag
!
!
      do i = 1, max_coarse_level
        nstep_coarse(i,1) = nstep_coarse(i-1,1) * icoarse_level(i,1)
        nstep_coarse(i,2) = nstep_coarse(i-1,2) * icoarse_level(i,2)
      end do
!
      ilevel_tmp = max_coarse_level
      do i = ilevel_tmp, 1, -1
        i_sflag =  mod(num_hemi,nstep_coarse(i,1))
        i_vflag =  mod(ncube_vertical,nstep_coarse(i,1))
        i_riflag = mod(nr_icb,nstep_coarse(i,2))
        i_roflag = mod(nr_ocore,nstep_coarse(i,2))
        i_rmflag = mod(nr_exter,nstep_coarse(i,2))
!
        if (i_riflag.eq.0 .and. i_roflag.eq.0 .and. i_rmflag.eq.0       &
     &     .and. i_sflag.eq.0 .and. i_vflag.eq.0) then
          exit
        else if (i_riflag.eq.0 .and. i_roflag.eq.0 .and. i_rmflag.eq.0  &
     &     .and. i_sflag.ne.0 .and. i_vflag.eq.0) then
          nstep_coarse(i,1) = nstep_coarse(i-1,1)
        else
          if (i_sflag.eq.0 .and. i_vflag.eq.0) then
            nstep_coarse(i,2) = nstep_coarse(i-1,2)
          else
            max_coarse_level = max_coarse_level - 1
          end if
        end if
!
      end do
!
       c_sphere%max_merge_e = 1
       do i = 1, max_coarse_level
         nl_3 = icoarse_level(i,1)**3
         nl_shell = icoarse_level(i,1)**2 * icoarse_level(i,2)
         c_sphere%max_merge_e = max(c_sphere%max_merge_e,nl_3)
         c_sphere%max_merge_e = max(c_sphere%max_merge_e,nl_shell)
       end do
!
      end subroutine check_rect_coarsing_level
!
!   --------------------------------------------------------------------
!
      end module check_coarsing_level
