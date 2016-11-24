!
!     module m_spherical_harmonics
!.......................................................................
!
!      Written by H.Matsui
!      modified by H. Matsui on June, 2007
!
!       subroutine allocate_index_4_sph(nth)
!
!       subroutine deallocate_index_4_sph
!
      module m_spherical_harmonics
!
      use m_precision
!
      implicit  none
! 
!
      integer ( kind = kint) :: ltr_tri_sph
      integer ( kind = kint) :: jmax_tri_sph
!
      integer ( kind = kint), dimension(:,:), allocatable:: idx
      real   ( kind = kreal), dimension(:,:), allocatable:: g
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_index_4_sph(nth)
!
      integer(kind = kint), intent(in) :: nth
!
      ltr_tri_sph =  nth
      jmax_tri_sph = nth*(nth+2)
!
      allocate ( idx(0:jmax_tri_sph,2) )
      allocate ( g(0:jmax_tri_sph,17) )
!
      idx = 0
      g = 0.0d0
!
      end subroutine allocate_index_4_sph
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_index_4_sph
!
        deallocate ( idx, g )
!
       end subroutine deallocate_index_4_sph
!
! -----------------------------------------------------------------------
!
      end module m_spherical_harmonics

