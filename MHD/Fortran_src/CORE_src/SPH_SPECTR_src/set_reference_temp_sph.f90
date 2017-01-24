!>@file   set_reference_temp_sph.f90
!!@brief  module set_reference_temp_sph
!!
!!@author H. Matsui
!!@date Programmed June., 1994
!!@date Modified Apr., 2009
!
!> @brief Set diffusive temperature profile
!!@n      with fixed temperature boundary
!!
!!@verbatim
!!      subroutine set_reftemp_4_sph(idx_rj_degree_zero, nidx_rj,       &
!!     &         reftemp_rj, i_ref, i_gref, nnod_rj, ntot_phys_rj, d_rj)
!!        type(phys_address), intent(in) :: ipol
!!***********************************************************************
!!*
!!*     ref_temp(k,0) : reference of temperature  (output)
!!*     ref_temp(k,1) : dT_0 / dr
!!*
!!*                          c2
!!*      ref_temp(k) = c1 + ------
!!*                          r(k)
!!*
!!*                      dto(k)
!!*     dref_temp(k) = ---------
!!*                        dr
!!*                         c2
!!*                  = - --------
!!*                        rs(k)
!!*
!!***********************************************************************
!!@endverbatim
!
      module set_reference_temp_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_reftemp_4_sph(idx_rj_degree_zero, nidx_rj,         &
     &         reftemp_rj, i_ref, i_gref, nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) ::  nidx_rj(2)
      integer(kind = kint), intent(in) ::  idx_rj_degree_zero
      integer(kind = kint), intent(in) ::  nnod_rj, ntot_phys_rj
      integer(kind = kint), intent(in) ::  i_ref, i_gref
      real(kind=kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:1)
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) ::  kk, inod
!
!
      if (i_ref*i_gref .le. izero) return
      if (idx_rj_degree_zero .le. izero) return
!
      do kk = 1, nidx_rj(1)
        inod = idx_rj_degree_zero + (kk-1) * nidx_rj(2)
        d_rj(inod,i_ref) =  reftemp_rj(kk,0)
        d_rj(inod,i_gref) = reftemp_rj(kk,1)
      end do
!
      end subroutine set_reftemp_4_sph
!
!  -------------------------------------------------------------------
!
      end module set_reference_temp_sph
