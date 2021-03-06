!
!     module m_z_filter_values
!
!      Written by Kemorin
!
      module m_z_filter_values
!
      use m_precision
!
      implicit none
!
!
      real(kind = kreal), dimension(:), allocatable :: f_mom_full
!
!
      integer (kind = kint) :: nfilter2_1, nfilter2_2, nfilter2_4
      integer (kind = kint) :: nfilter2_3, nfilter6_1
      private :: nfilter2_1, nfilter2_2, nfilter2_3
      private :: nfilter2_4, nfilter6_1
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
      nfilter2_1 = 2*nfilter + 1
      nfilter2_2 = 2*nfilter + 2
      nfilter2_3 = 2*nfilter + 3
      nfilter2_4 = 2*nfilter + 4
      nfilter6_1 = 6*nfilter + 1
!
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
