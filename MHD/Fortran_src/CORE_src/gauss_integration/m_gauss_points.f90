!>@file   m_gauss_points.f90
!!        module m_gauss_points
!!
!! @author H. Matsui
!! @date   Programmed in 2003
!!
!
!> @brief Constants for Gauss-Legendre integration
!!
!!@verbatim
!!       subroutine allocate_gauss_points(num_gauss)
!!       subroutine allocate_gauss_colatitude
!!       subroutine deallocate_gauss_points
!!       subroutine deallocate_gauss_colatitude
!!
!! *************************************************
!!
!!      subroutine construct_gauss_coefs
!!
!! construct points and coefficients for 
!! Gauss-Legendre integration
!!
!!    Integration area:  -1 < x < 1
!!
!! *************************************************
!!
!!      subroutine set_gauss_colatitude
!!
!! construct position of Gauss-Legendre points
!!
!! *************************************************
!!@endverbatim
!
      module m_gauss_points
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Number of Gauss points
      integer(kind=kint) :: n_point = 400
!>      Position of Gauss points (@$f-1<x<1@$f)
      real(kind = kreal), dimension(:), allocatable :: w_point
!>      Coefficients of Gauss integration
      real(kind = kreal), dimension(:), allocatable :: w_coefs
!
!>      Position of Gauss-Legendre colatitude
      real(kind = kreal), dimension(:), allocatable :: w_colat
!>      Position of Gauss-Legendre colatitude in degree
      real(kind = kreal), dimension(:), allocatable :: w_col_deg
!
!>      longitude of spherical grid
      real(kind = kreal), dimension(:), allocatable :: w_azim
!>      longitude of spherical grid in degree
      real(kind = kreal), dimension(:), allocatable :: w_azim_deg
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_gauss_points(num_gauss)
!
      integer(kind = kint), intent(in) :: num_gauss
!
!
      n_point = num_gauss
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
! -----------------------------------------------------------------------
!
      subroutine construct_gauss_coefs
!
      use gauss_integration
!
!
      call const_gauss_points_coefs(n_point, w_point, w_coefs)
!
      end subroutine construct_gauss_coefs 
!
! -----------------------------------------------------------------------
!
      subroutine set_gauss_colatitude
!
      use gauss_integration
!
!
      call allocate_gauss_colatitude
      call set_gauss_points_sph(n_point, w_point,                       &
     &    w_colat, w_col_deg, w_azim, w_azim_deg)
!
      end subroutine set_gauss_colatitude
!
! -----------------------------------------------------------------------
!
      end module m_gauss_points
