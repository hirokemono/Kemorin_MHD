!radial_coarse_stack_c_sph.f90
!      module radial_coarse_stack_c_sph
!
!     Written by H. Matsui on Apr., 2006
!     Modified by H. Matsui on Oct., 2007
!
!      subroutine radial_coarse_stack(icoarse, ifile)
!
      module radial_coarse_stack_c_sph
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine radial_coarse_stack(icoarse, ifile)
!
      use m_numref_cubed_sph
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_radius
!
      integer(kind = kint), intent(in) :: icoarse, ifile
!
      integer(kind = kint) :: i, k, iele0, iele, iele_sf, inod0
!
!
      iele = nele_cube_c
      do k = 1, nr_c
!
        do iele0 = 1, nele_sf_c
!
          iele = iele + 1
          iele_sf = c_sphere1%numele_sf                                 &
     &             + c_sphere1%iele_stack_sf(icoarse-1) + iele0
          inod0 = nnod_cube_c + nnod_sf_c*(k-1)
!
          ie(1) = inod0 + c_sphere1%ie_sf20(iele_sf,1)
          ie(2) = inod0 + c_sphere1%ie_sf20(iele_sf,2)
          ie(3) = inod0 + c_sphere1%ie_sf20(iele_sf,3)
          ie(4) = inod0 + c_sphere1%ie_sf20(iele_sf,4)
          ie(5) = inod0 + c_sphere1%ie_sf20(iele_sf,1) + nnod_sf_c
          ie(6) = inod0 + c_sphere1%ie_sf20(iele_sf,2) + nnod_sf_c
          ie(7) = inod0 + c_sphere1%ie_sf20(iele_sf,3) + nnod_sf_c
          ie(8) = inod0 + c_sphere1%ie_sf20(iele_sf,4) + nnod_sf_c
!
         write(ifile,'(10i16)') iele, (ie(i),i=1,8)
!
       end do
      end do
!
      end subroutine radial_coarse_stack
!
!   --------------------------------------------------------------------
!
      end module radial_coarse_stack_c_sph
