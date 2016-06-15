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
!!     &          ar_1d_rj, nlayer_ICB, nlayer_CMB,                     &
!!     &          r_hot, r_cold, temp_hot, temp_cold, ipol,             &
!!     &          nnod_rj, ntot_phys_rj, d_rj)
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
      use t_phys_address
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
     &          ar_1d_rj, nlayer_ICB, nlayer_CMB,                       &
     &          r_hot, r_cold, temp_hot, temp_cold, ipol,               &
     &          nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) ::  nidx_rj(2)
      integer(kind = kint), intent(in) ::  nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) ::  idx_rj_degree_zero
      integer(kind = kint), intent(in) ::  nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: ar_1d_rj(nidx_rj(1),3)
      real(kind = kreal), intent(in) :: r_hot, r_cold
      real(kind = kreal), intent(in) :: temp_hot, temp_cold
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: k, kk, inod
      real(kind = kreal) :: c1, c2
!
      return
!
      c1 = ( r_cold*temp_cold - r_hot*temp_hot ) /   (r_cold - r_hot)
      c2 = r_cold * r_hot * (temp_hot - temp_cold) / (r_cold - r_hot)
!
      d_rj(1:nnod_rj,ipol%i_ref_t) =  zero
      d_rj(1:nnod_rj,ipol%i_gref_t) = zero
!
      if (idx_rj_degree_zero .le. izero) return
!
      do kk = nlayer_ICB, nlayer_CMB
        k = kk - nlayer_ICB
        inod = idx_rj_degree_zero + (kk-1) * nidx_rj(2)
        d_rj(inod,ipol%i_ref_t) =    c1 + c2 * ar_1d_rj(kk,1)
        d_rj(inod,ipol%i_gref_t) = - c2 * ar_1d_rj(kk,2)
      end do
!
      do kk = 1 ,nlayer_ICB-1
        k = nlayer_ICB - kk
        inod = idx_rj_degree_zero + (kk-1) * nidx_rj(2)
        d_rj(inod,ipol%i_ref_t) =  temp_hot
      end do
!
      end subroutine set_reftemp_4_sph
!
!  -------------------------------------------------------------------
!
      end module set_reference_temp_sph
