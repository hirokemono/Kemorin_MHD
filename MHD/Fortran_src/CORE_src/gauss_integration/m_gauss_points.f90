!
!      module m_gauss_points
!
!      Written by H. Matsui
!
!       subroutine allocate_gauss_points
!       subroutine allocate_gauss_colatitude
!       subroutine deallocate_gauss_points
!       subroutine deallocate_gauss_colatitude
!
      module m_gauss_points
!
      use m_precision
!
      implicit none
!
      integer(kind=kint) :: n_point = 400
      real(kind = kreal), dimension(:), allocatable :: w_point
      real(kind = kreal), dimension(:), allocatable :: w_coefs
!
      real(kind = kreal), dimension(:), allocatable :: w_colat
      real(kind = kreal), dimension(:), allocatable :: w_col_deg
!
      real(kind = kreal), dimension(:), allocatable :: w_azim
      real(kind = kreal), dimension(:), allocatable :: w_azim_deg
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine allocate_gauss_points
!
       allocate(w_point(n_point))
       allocate(w_coefs(n_point))
!
       w_point = 0.0d0
       w_coefs = 0.0d0
!
       end subroutine allocate_gauss_points
!
! -----------------------------------------------------------------------
!
       subroutine allocate_gauss_colatitude
!
       allocate(w_colat(n_point))
       allocate(w_col_deg(n_point))
       allocate(w_azim(2*n_point))
       allocate(w_azim_deg(2*n_point))
!
       w_colat = 0.0d0
       w_col_deg = 0.0d0
       w_azim = 0.0d0
       w_azim_deg = 0.0d0
!
       end subroutine allocate_gauss_colatitude
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_gauss_points
!
       deallocate(w_point)
       deallocate(w_coefs)
!
       end subroutine deallocate_gauss_points
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_gauss_colatitude
!
       deallocate(w_colat)
       deallocate(w_col_deg)
       deallocate(w_azim)
       deallocate(w_azim_deg)
!
       end subroutine deallocate_gauss_colatitude
!
! -----------------------------------------------------------------------
!
      end module m_gauss_points
