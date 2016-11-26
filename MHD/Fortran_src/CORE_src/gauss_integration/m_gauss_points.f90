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
      use t_gauss_points
!
      implicit none
!
      type(gauss_points), save :: gauss1
!
!
      end module m_gauss_points
