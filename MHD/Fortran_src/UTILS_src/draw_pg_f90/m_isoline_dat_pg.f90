!
!      module m_isoline_dat_pg
!
!      subroutine set_linevalue(xmax, xmin, ncl, xc)
!      subroutine allocate_data_4_isoline
!      subroutine deallocate_data_4_isoline
!
      module m_isoline_dat_pg
!
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint) :: nline
      integer(kind = kint) :: nmax_line = 0
      real(kind= kreal), allocatable :: xc(:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_linevalue(xmax, xmin, ncl)
!
      integer(kind = kint), intent(in) :: ncl
      real(kind = kreal), intent(in) :: xmax, xmin
!
      integer :: ic
!*
!*  ------   set normarize factor   ---------------
!*
      do ic = 1 ,ncl
        xc(ic) = xmin + (dble(ic) - half)*(xmax - xmin) / dble(ncl)
      end do
!*
      end subroutine set_linevalue
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_data_4_isoline
!
      allocate ( xc(nmax_line) )
      xc = 0.0d0
!
      end subroutine allocate_data_4_isoline
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_data_4_isoline
!
      deallocate ( xc )
!
      end subroutine deallocate_data_4_isoline
!
! ----------------------------------------------------------------------
!
      end module m_isoline_dat_pg
