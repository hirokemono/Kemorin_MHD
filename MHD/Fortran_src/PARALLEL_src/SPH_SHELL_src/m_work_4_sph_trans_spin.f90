!>@file   m_work_4_sph_trans_spin.f90
!!@brief  module m_work_4_sph_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Field data for Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine allocate_work_sph_trans_spin(ncomp)
!!      subroutine deallocate_work_sph_trans_spin
!!
!!      subroutine clear_b_trans_spin(icomp_st, icomp_ed)
!!      subroutine clear_f_trans_spin(icomp_st, icomp_ed)
!!
!!
!!    Data for single vector field
!!      radial component:      vr_rtp(3*i_rtp-2)
!!      elevetional component: vr_rtp(3*i_rtp-1)
!!      azimuthal component:   vr_rtp(2*i_rtp  )
!!
!!    Data for single vector spectrum
!!      Poloidal component:          sp_rj(3*i_rj-2)
!!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!!      Toroidal component:          sp_rj(3*i_rj  )
!!
!!    Data for single scalar
!!      field: vr_rtp(i_rtp)
!!      spectr: sp_rj(i_rj)
!!
!!@endverbatim
!!
!!@n @param  ncomp  number of components for Legendre transform
!!@n @param  icomp_st  start component to clear
!!@n @param  icomp_ed  end component to clear
!
      module m_work_4_sph_trans_spin
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!>     field data for Legendre transform
!!@n       original layout: vr_rtm_spin(l_rtm,m_rtm,k_rtm,icomp)
!!@n       size: vr_rtm_spin(nidx_rtm(2),nidx_rtm(3),nidx_rtm(1)*ncomp)
      real(kind = kreal), allocatable :: vr_rtm_spin(:,:,:)
!
!>     spectr data for Legendre transform
!!@n      original layout: sp_rlm_spin(j_rlm,k_rtm,icomp)
!!@n        size: sp_rlm_spin(nidx_rlm(2),nidx_rtm(1)*ncomp)
      real(kind = kreal), allocatable :: sp_rlm_spin(:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_sph_trans_spin(ncomp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint) :: num1
!
!
      num1 = ncomp*nidx_rlm(1)
      allocate(sp_rlm_spin(nidx_rlm(2),num1))
      allocate(vr_rtm_spin(nidx_rtm(2),nidx_rtm(3),num1))
!
      sp_rlm_spin = 0.0d0
      vr_rtm_spin = 0.0d0
!
      end subroutine allocate_work_sph_trans_spin
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_sph_trans_spin
!
      deallocate(vr_rtm_spin, sp_rlm_spin)
!
      end subroutine deallocate_work_sph_trans_spin
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine clear_b_trans_spin(icomp_st, icomp_ed)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: icomp_st, icomp_ed
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm, kst, ked
!
!
      kst = (icomp_st-1) * nidx_rtm(1) + 1
      ked = icomp_ed * nidx_rtm(1)
!$omp parallel do private(m_rtm,k_rtm,l_rtm)
      do k_rtm = kst, ked
        do m_rtm = 1, nidx_rtm(3)
          do l_rtm = 1, nidx_rtm(2)
            vr_rtm_spin(l_rtm,m_rtm,k_rtm) = zero
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_b_trans_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_spin(icomp_st, icomp_ed)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: icomp_st, icomp_ed
      integer(kind = kint) :: j_rlm, k_rtm, kst, ked
!
!
      kst = (icomp_st-1) * nidx_rtm(1) + 1
      ked = icomp_ed * nidx_rtm(1)
!$omp parallel do private(k_rtm,j_rlm)
      do k_rtm = kst, ked
          do j_rlm = 1, nidx_rlm(2)
            sp_rlm_spin(j_rlm,k_rtm) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_f_trans_spin
!
! -----------------------------------------------------------------------
!
      end module m_work_4_sph_trans_spin
