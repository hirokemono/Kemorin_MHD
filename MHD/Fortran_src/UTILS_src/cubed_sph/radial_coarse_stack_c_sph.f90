!radial_coarse_stack_c_sph.f90
!      module radial_coarse_stack_c_sph
!
!     Written by H. Matsui on Apr., 2006
!     Modified by H. Matsui on Oct., 2007
!
!!      subroutine radial_coarse_stack                                  &
!!     &         (ifile, icoarse, course_p, c_sphere)
!!        type(coarse_cubed_sph), intent(in) :: course_p
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      module radial_coarse_stack_c_sph
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
      subroutine radial_coarse_stack                                    &
     &         (ifile, icoarse, course_p, c_sphere)
!
      use t_numref_cubed_sph
!
      integer(kind = kint), intent(in) :: icoarse, ifile
      type(coarse_cubed_sph), intent(in) :: course_p
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
      integer(kind = kint) :: i, k, iele0, iele, iele_sf, inod0
      integer(kind = kint) :: ie(8)
!
!
      iele = course_p%nele_cube_c
      do k = 1, course_p%nr_c
        do iele0 = 1, course_p%nele_sf_c
          iele = iele + 1
          iele_sf = c_sphere%numele_sf                                  &
     &             + c_sphere%iele_stack_sf(icoarse-1) + iele0
          inod0 = course_p%nnod_cube_c + course_p%nnod_sf_c*(k-1)
!
          ie(1) = inod0 + c_sphere%ie_sf20(iele_sf,1)
          ie(2) = inod0 + c_sphere%ie_sf20(iele_sf,2)
          ie(3) = inod0 + c_sphere%ie_sf20(iele_sf,3)
          ie(4) = inod0 + c_sphere%ie_sf20(iele_sf,4)
          ie(5) = inod0 + c_sphere%ie_sf20(iele_sf,1)                   &
     &           + course_p%nnod_sf_c
          ie(6) = inod0 + c_sphere%ie_sf20(iele_sf,2)                   &
     &           + course_p%nnod_sf_c
          ie(7) = inod0 + c_sphere%ie_sf20(iele_sf,3)                   &
     &           + course_p%nnod_sf_c
          ie(8) = inod0 + c_sphere%ie_sf20(iele_sf,4)                   &
     &           + course_p%nnod_sf_c
!
         write(ifile,'(10i16)') iele, (ie(i),i=1,8)
       end do
      end do
!
      end subroutine radial_coarse_stack
!
!   --------------------------------------------------------------------
!
      end module radial_coarse_stack_c_sph
