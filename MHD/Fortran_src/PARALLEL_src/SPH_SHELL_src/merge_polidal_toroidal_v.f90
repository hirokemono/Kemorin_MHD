!>@file   merge_polidal_toroidal_v.f90
!!@brief  module merge_polidal_toroidal_v
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Take products with radius for spherical transform
!!
!!@verbatim
!!      subroutine const_vect_sph_b_trans(nb, vr_rtm)
!!      subroutine prod_r_vect_sph_f_trans(nb, vr_rtm)
!!@endverbatim
!
       module merge_polidal_toroidal_v
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_vect_sph_b_trans(nb, vr_rtm)
!
      integer(kind = kint), intent(in) :: nb
      real(kind = kreal), intent(inout) :: vr_rtm(3*nb*nnod_rtm)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm
      integer(kind = kint) :: nd, inod, lnod
!
!$omp parallel do private(ip,ist,ied,i_rtm,k_rtm,l_rtm,nd,inod,lnod)
      do ip = 1, np_smp
        ist = nb*inod_rtm_smp_stack(ip-1) + 1
        ied = nb*inod_rtm_smp_stack(ip)
!cdir nodep
        do i_rtm = ist, ied
          nd = 1 + mod(i_rtm-1,nb)
          inod = 1 + (i_rtm - nd) / nb
          l_rtm = 1 + mod((inod-1),nidx_rtm(2))
          lnod = 1 + (inod - l_rtm) / nidx_rtm(2)
          k_rtm = 1 + mod((lnod-1),nidx_rtm(1))
          vr_rtm(3*i_rtm-2) = a_r_1d_rlm_r(k_rtm)*a_r_1d_rlm_r(k_rtm)   &
     &                       * vr_rtm(3*i_rtm-2) 
          vr_rtm(3*i_rtm-1) = a_r_1d_rlm_r(k_rtm)*vr_rtm(3*i_rtm-1)
          vr_rtm(3*i_rtm  ) = a_r_1d_rlm_r(k_rtm)*vr_rtm(3*i_rtm  )
        end do
      end do
!$omp end parallel do
!
      end subroutine const_vect_sph_b_trans
!
! -----------------------------------------------------------------------
!
      subroutine prod_r_vect_sph_f_trans(nb, vr_rtm)
!
      integer(kind = kint), intent(in) :: nb
      real(kind = kreal), intent(inout) :: vr_rtm(3*nb*nnod_rtm)
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: i_rtm, k_rtm, l_rtm
      integer(kind = kint) :: nd, inod, lnod
!
!$omp parallel do private(ip,ist,ied,i_rtm,k_rtm,l_rtm,nd,inod,lnod)
      do ip = 1, np_smp
        ist = nb*inod_rtm_smp_stack(ip-1) + 1
        ied = nb*inod_rtm_smp_stack(ip)
!cdir nodep
        do i_rtm = ist, ied
          nd = 1 + mod(i_rtm-1,nb)
          inod = 1 + (i_rtm - nd) / nb
          l_rtm = 1 + mod((inod-1),nidx_rtm(2))
          lnod = 1 + (inod - l_rtm) / nidx_rtm(2)
          k_rtm = 1 + mod((lnod-1),nidx_rtm(1))
          vr_rtm(3*i_rtm-2)                                             &
     &              = radius_1d_rlm_r(k_rtm)*radius_1d_rlm_r(k_rtm)     &
     &               * vr_rtm(3*i_rtm-2) 
          vr_rtm(3*i_rtm-1) = radius_1d_rlm_r(k_rtm)*vr_rtm(3*i_rtm-1)
          vr_rtm(3*i_rtm  ) = radius_1d_rlm_r(k_rtm)*vr_rtm(3*i_rtm  )
        end do
      end do
!$omp end parallel do
!
      end subroutine prod_r_vect_sph_f_trans
!
! -----------------------------------------------------------------------
!
      end module merge_polidal_toroidal_v
