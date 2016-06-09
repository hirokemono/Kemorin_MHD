!>@file   m_schmidt_poly_on_rtm.f90
!!@brief  module m_schmidt_poly_on_rtm
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief Parameters for LEgendre transforms
!!
      module m_schmidt_poly_on_rtm
!
      use m_precision
      use t_schmidt_poly_on_rtm
!
      implicit none
!
!>      Structures for Legendre polynomials for spherical transform
      type(legendre_4_sph_trans), save :: leg1
!
      end module m_schmidt_poly_on_rtm
