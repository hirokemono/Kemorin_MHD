!set_refined_nod_2_sphere.f90
!      module set_refined_nod_2_sphere
!
!     Written by H. Matsui on Oct., 2007
!
!!      subroutine set_x_refine_2_sphere                                &
!!     &         (ntot_nod_refine, x_refine, sph_refine)
!
      module set_refined_nod_2_sphere
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_x_refine_2_sphere                                  &
     &         (ntot_nod_refine, x_refine, sph_refine)
!
      integer(kind = kint), intent(in) :: ntot_nod_refine
      real(kind = kreal), intent(inout) :: x_refine(ntot_nod_refine,3)
      real(kind = kreal), intent(inout)                                 &
     &      :: sph_refine(ntot_nod_refine,3)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, ntot_nod_refine
        sph_refine(inod,2) = sqrt( x_refine(inod,1)*x_refine(inod,1)    &
     &                           + x_refine(inod,2)*x_refine(inod,2)    &
     &                           + x_refine(inod,3)*x_refine(inod,3) )
!
        if ( sph_refine(inod,2) .eq. zero) then
          sph_refine(inod,2) = zero
        else
          sph_refine(inod,2) = one / sph_refine(inod,2)
        end if
!
        x_refine(inod,1) =  x_refine(inod,1)                            &
     &                     * sph_refine(inod,1) * sph_refine(inod,2)
        x_refine(inod,2) =  x_refine(inod,2)                            &
     &                     * sph_refine(inod,1) * sph_refine(inod,2)
        x_refine(inod,3) =  x_refine(inod,3)                            &
     &                     * sph_refine(inod,1) * sph_refine(inod,2)
      end do
!$omp end parallel do
!
      end subroutine set_x_refine_2_sphere
!
!  ---------------------------------------------------------------------
!
      end module set_refined_nod_2_sphere
