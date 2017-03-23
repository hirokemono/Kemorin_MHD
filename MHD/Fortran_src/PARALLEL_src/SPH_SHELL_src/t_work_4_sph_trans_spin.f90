!>@file   t_work_4_sph_trans_spin.f90
!!@brief  module t_work_4_sph_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Field data for Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine allocate_work_sph_trans                              &
!!     &         (ncomp, nnod_rtm, nnod_rlm, WK_l_spi)
!!      subroutine deallocate_work_sph_trans(WK_l_spi)
!!      subroutine clear_fwd_legendre_work(ncomp, nnod_rlm, WK_l_spi)
!!      subroutine clear_bwd_legendre_work(ncomp, nnod_rtm, WK_l_spi)
!!
!!    Data for single vector field
!!      radial component:      vr_rtm_wk(3*i_rtm-2)
!!      elevetional component: vr_rtm_wk(3*i_rtm-1)
!!      azimuthal component:   vr_rtm_wk(3*i_rtm  )
!!
!!    Data for single vector spectrum
!!      Poloidal component:          sp_rlm_wk(3*i_rlm-2)
!!      diff. of Poloidal component: sp_rlm_wk(3*i_rlm-1)
!!      Toroidal component:          sp_rlm_wk(3*i_rlm  )
!!
!!    Data for single scalar
!!      field: vr_rtm_wk(i_rtm)
!!      spectr: sp_rlm_wk(i_rlm)
!!
!!@endverbatim
!!
!!@n @param  ncomp  number of components for Legendre transform
!!@n @param  icomp_st  start component to clear
!!@n @param  icomp_ed  end component to clear
!
      module t_work_4_sph_trans_spin
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type leg_trns_spin_work
!>        field data for Legendre transform  @f$ f(r,\theta,m) @f$ 
!!@n       size:  vr_rtm(ncomp*nidx_rtm(2)*nidx_rtm(1)*nidx_rtm(3))
        real(kind = kreal), allocatable :: vr_rtm_wk(:)
!
!>        Spectr data for Legendre transform  @f$ f(r,l,m) @f$ 
!>@n        size: sp_rlm(ncomp*nidx_rlm(2)*nidx_rtm(1))
        real(kind = kreal), allocatable :: sp_rlm_wk(:)
      end type leg_trns_spin_work
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_sph_trans                                &
     &         (ncomp, nnod_rtm, nnod_rlm, WK_l_spi)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: nnod_rtm, nnod_rlm
!
      type(leg_trns_spin_work), intent(inout) :: WK_l_spi
!
!
      allocate(WK_l_spi%sp_rlm_wk(nnod_rlm*ncomp))
      allocate(WK_l_spi%vr_rtm_wk(nnod_rtm*ncomp))
!
      call clear_bwd_legendre_work(ncomp, nnod_rtm, WK_l_spi)
      call clear_fwd_legendre_work(ncomp, nnod_rlm, WK_l_spi)
!
      end subroutine allocate_work_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_sph_trans(WK_l_spi)
!
      type(leg_trns_spin_work), intent(inout) :: WK_l_spi
!
      deallocate(WK_l_spi%vr_rtm_wk, WK_l_spi%sp_rlm_wk)
!
      end subroutine deallocate_work_sph_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine clear_fwd_legendre_work(ncomp, nnod_rlm, WK_l_spi)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: nnod_rlm
!
      type(leg_trns_spin_work), intent(inout) :: WK_l_spi
!
!
      if(ncomp .le. 0) return
!$omp parallel workshare
      WK_l_spi%sp_rlm_wk(1:nnod_rlm*ncomp) = 0.0d0
!$omp end parallel workshare
!
      end subroutine clear_fwd_legendre_work
!
! ----------------------------------------------------------------------
!
      subroutine clear_bwd_legendre_work(ncomp, nnod_rtm, WK_l_spi)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: nnod_rtm
!
      type(leg_trns_spin_work), intent(inout) :: WK_l_spi
!
!
      if(ncomp .le. 0) return
!$omp parallel workshare
      WK_l_spi%vr_rtm_wk(1:nnod_rtm*ncomp) = 0.0d0
!$omp end parallel workshare
!
      end subroutine clear_bwd_legendre_work
!
! ----------------------------------------------------------------------
!
      end module t_work_4_sph_trans_spin
