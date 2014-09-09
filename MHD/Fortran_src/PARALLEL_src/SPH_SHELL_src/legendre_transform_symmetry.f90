!>@file   legendre_transform_symmetry.f90
!!@brief  module legendre_transform_symmetry
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform considering symmetry
!!
!!
!!@verbatim
!!      subroutine leg_backward_trans_sym_org(ncomp, nvector, nscalar)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_forward_trans_sym_org(ncomp, nvector, nscalar)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal) 
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_transform_symmetry
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_backward_trans_sym_org(ncomp, nvector, nscalar)
!
      use legendre_bwd_trans_symmetry
      use merge_polidal_toroidal_v
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call clear_bwd_legendre_trans(ncomp)
      if(nvector .gt. 0) then
        call leg_bwd_trans_vector_sym_org(ncomp, nvector)
      end if
      if(nscalar .gt. 0) then
        call leg_bwd_trans_scalar_sym_org(ncomp, nvector, nscalar)
      end if
!
      end subroutine leg_backward_trans_sym_org
!
! -----------------------------------------------------------------------
!
      subroutine leg_forward_trans_sym_org(ncomp, nvector, nscalar)
!
      use legendre_fwd_trans_symmetry
      use merge_polidal_toroidal_v
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call clear_fwd_legendre_trans(ncomp)
      if(nvector .gt. 0) then
        call leg_fwd_trans_vector_sym_org(ncomp, nvector)
      end if
      if(nscalar .gt. 0) then
        call leg_fwd_trans_scalar_sym_org(ncomp, nvector, nscalar)
      end if
!
      end subroutine leg_forward_trans_sym_org
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_symmetry

