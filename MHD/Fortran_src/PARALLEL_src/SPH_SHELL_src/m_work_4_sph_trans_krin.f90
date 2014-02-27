!>@file   m_work_4_sph_trans_krin.f90
!!@brief  module m_work_4_sph_trans_krin
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Field data for Legendre transform
!!       (innermost loop is field and radius)
!!
!!@verbatim
!!      subroutine allocate_work_sph_trans_krin(nb_sph_trans)
!!      subroutine deallocate_work_sph_trans_krin
!!
!!      subroutine clear_b_trans_krin(icomp_st, icomp_ed)
!!
!!      subroutine clear_f_trans_krin(icomp_st, icomp_ed)
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
!!             maximum number of fields for Legendre transform
!!@n @param  nb  number of fields to be transformed
!
      module m_work_4_sph_trans_krin
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!>     field data for Legendre transform
!!@n       original layout: vr_rtm_krin(i_fld,k_rtm,l_rtm,m_rtm,nd)
!!@n       size: vr_rtm_krin(nb*nidx_rtm(1),nidx_rtm(2)*nidx_rtm(3),3)
      real(kind = kreal), allocatable :: vr_rtm_krin(:,:,:)
!
!>     spectr data for Legendre transform
!!@n      original layout: sp_rlm_krin(k_rtm,j_rlm,i_fld)
!!@n      size: sp_rlm_krin(nidx_rtm(1),nidx_rlm(2),nb)
      real(kind = kreal), allocatable :: sp_rlm_krin(:,:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_sph_trans_krin(nb_sph_trans)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb_sph_trans
      integer(kind = kint) :: num1
!
!
      allocate(sp_rlm_krin(nidx_rlm(1),nidx_rlm(2),3*nb_sph_trans))
      sp_rlm_krin = 0.0d0
!
      num1 = nidx_rtm(2)*nidx_rtm(3)
      allocate(vr_rtm_krin(nidx_rtm(1),num1,3*nb_sph_trans))
      vr_rtm_krin = 0.0d0
!
      end subroutine allocate_work_sph_trans_krin
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_sph_trans_krin
!
      deallocate(vr_rtm_krin, sp_rlm_krin)
!
      end subroutine deallocate_work_sph_trans_krin
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_b_trans_krin(icomp_st, icomp_ed)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: icomp_st, icomp_ed
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm, i_rtm_1, nd
!
!
!$omp parallel
      do nd = icomp_st, icomp_ed
!$omp do private(k_rtm,l_rtm,i_rtm_1)
        do m_rtm = 1, nidx_rtm(3)
          do l_rtm = 1, nidx_rtm(2)
            do k_rtm = 1, nidx_rtm(1)
              i_rtm_1 = l_rtm + (m_rtm-1) * nidx_rtm(2)
              vr_rtm_krin(k_rtm,i_rtm_1,nd) = zero
            end do
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine clear_b_trans_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_krin(icomp_st, icomp_ed)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: icomp_st, icomp_ed
!
      integer(kind = kint) :: j_rlm, k_rlm, nd
!
!
!$omp parallel
      do nd = icomp_st, icomp_ed
!$omp do private(k_rlm,j_rlm)
        do k_rlm = 1, nidx_rtm(1)
          do j_rlm = 1, nidx_rlm(2)
            sp_rlm_krin(k_rlm,j_rlm,nd) = zero
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine clear_f_trans_krin
!
! -----------------------------------------------------------------------
!
      end module m_work_4_sph_trans_krin
