!
!      module m_interpolated_geometry
!
      module m_interpolated_geometry
!
!     Written by H. Matsui on Sep., 2006
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint), allocatable :: inod_global_itp(:)
!
      real(kind = kreal), allocatable :: xx_interpolate(:,:)
!
      real(kind = kreal), allocatable :: xx_diff_itp(:,:)
!
!     subroutine allocate_interpolate_geometry
!     subroutine deallocate_interpolate_geometry
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_interpolate_geometry(numnod)
!
      integer(kind = kint) :: numnod
!
      allocate( inod_global_itp(numnod) )
      allocate( xx_interpolate(numnod,3) )
      allocate( xx_diff_itp(numnod,3) )
!
      inod_global_itp = 0
      xx_interpolate = 0.0d0
      xx_diff_itp = 0.0d0
!
      end subroutine allocate_interpolate_geometry
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_interpolate_geometry
!
      deallocate( inod_global_itp )
      deallocate( xx_interpolate )
      deallocate( xx_diff_itp )
!
      end subroutine deallocate_interpolate_geometry
!
! ----------------------------------------------------------------------
!
      end module m_interpolated_geometry
