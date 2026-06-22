!>@file   m_z_filter_values.f90
!!        module m_z_filter_values
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief commutive filter data in vertical direction
!!
!!@verbatim
!!      subroutine allocate_filter_values
!!      subroutine deallocate_filter_values
!!      subroutine check_integrated_values(id_rank)
!!@endverbatim
      module m_z_filter_values
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint), private :: nfilter6_1
      real(kind = kreal), dimension(:), allocatable :: f_mom_full
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_filter_values(nfilter)
!
      integer (kind = kint) :: nfilter
!
      nfilter6_1 = 6*nfilter + 1
      allocate( f_mom_full(0:nfilter6_1) )
!
      f_mom_full = 0.0d0
!
      end subroutine allocate_filter_values
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_filter_values
!
      deallocate( f_mom_full )
!
      end subroutine deallocate_filter_values
!
!  ---------------------------------------------------------------------
!
      subroutine check_integrated_values(id_rank)
!
      integer :: id_rank
!
      write(50+id_rank,*) 'f_mom_full'
      write(50+id_rank,'(1p5e16.8)') f_mom_full(0:nfilter6_1)
!
      end subroutine check_integrated_values
!
!  ---------------------------------------------------------------------
!
      end module m_z_filter_values
