!>@file   set_sp_to_rtm_rocBLAS_smp.f90
!!@brief  module set_sp_to_rtm_rocBLAS_smp
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform using matrix multi
!!
!!@verbatim
!!      subroutine set_vec_rtm_sym_matmul_big_smp(nnod_rtm, nidx_rtm,   &
!!     &         istep_rtm, asin_theta_1d_rtm, weight_rtm, nkr,         &
!!     &         mp_rlm, mn_rlm, nle_rtm, nlo_rtm, ncomp, nvector,      &
!!     &         irev_sr_rtm, n_WR, WR, symp_r, asmp_p, asmp_r, symp_p)
!!      subroutine set_scl_rtm_sym_matmul_big_smp                       &
!!     &         (nnod_rtm, nidx_rtm, istep_rtm, weight_rtm, nkr,       &
!!     &          mp_rlm, nle_rtm, nlo_rtm, ncomp, nvector, nscalar,    &
!!     &          irev_sr_rtm, n_WR, WR, symp, asmp)
!!        integer(kind = kint), intent(in) :: nnod_rtm
!!        integer(kind = kint), intent(in) :: nidx_rtm(3)
!!        integer(kind = kint), intent(in) :: istep_rtm(3)
!!        real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
!!        real(kind = kreal), intent(in):: asin_theta_1d_rtm(nidx_rtm(2))
!!        integer(kind = kint), intent(in) :: nkr
!!        integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
!!        integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!!        integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
!!        real (kind=kreal), intent(in) :: WR(n_WR)
!!        real(kind=kreal), intent(inout):: symp_r(ncomp*nkr,nle_rtm)
!!        real(kind=kreal), intent(inout):: asmp_p(2*nkr*nvector,nle_rtm)
!!        real(kind=kreal), intent(inout):: asmp_r(ncomp*nkr,nle_rtm)
!!        real(kind=kreal), intent(inout):: symp_p(2*nkr*nvector,nle_rtm)
!!        real(kind = kreal), intent(inout) :: symp(ncomp*nkr,nle_rtm)
!!        real(kind = kreal), intent(inout) :: asmp(ncomp*nkr,nle_rtm)
!!@endverbatim
!!
      module set_sp_to_rtm_rocBLAS_smp
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_vec_rtm_sym_matmul_big_smp(nnod_rtm, nidx_rtm,     &
     &         istep_rtm, asin_theta_1d_rtm, weight_rtm, nkr,           &
     &         mp_rlm, mn_rlm, nle_rtm, nlo_rtm, ncomp, nvector,        &
     &         irev_sr_rtm, n_WR, WR, symp_r, asmp_p, asmp_r, symp_p)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind=kreal), intent(inout) :: symp_r(ncomp*nkr,nle_rtm)
      real(kind=kreal), intent(inout) :: asmp_p(2*nkr*nvector,nle_rtm)
      real(kind=kreal), intent(inout) :: asmp_r(ncomp*nkr,nle_rtm)
      real(kind=kreal), intent(inout) :: symp_p(2*nkr*nvector,nle_rtm)
!
!
      integer(kind = kint) :: kk, k_rtm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      real(kind = kreal) :: wp_rtm, asin_rtm
!
!
      nkrv = nkr * nvector
!
!$omp  parallel do collapse(3)                                          &
!$omp& private(lp_rtm,ln_rtm,wp_rtm,asin_rtm,kk,k_rtm,nd,               &
!$omp&         ip_rtpm,ip_rtnm,in_rtpm,in_rtnm,                         &
!$omp&         ipp_recv,ipn_recv,inp_recv,inn_recv)
      do lp_rtm = 1, nlo_rtm
        do nd = 1, nvector
          do k_rtm = 1, nidx_rtm(1)
            kk = k_rtm + (nd-1) * nidx_rtm(1)
!
            ln_rtm = nidx_rtm(2) - lp_rtm + 1
            wp_rtm =   weight_rtm(lp_rtm)
            asin_rtm = asin_theta_1d_rtm(lp_rtm)
!
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mn_rlm-1) * istep_rtm(3)
            in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mn_rlm-1) * istep_rtm(3)
          ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          ipn_recv = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
          inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
          inn_recv = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
            symp_r(kk,lp_rtm) =      (WR(ipp_recv-2) + WR(ipn_recv-2))  &
     &                              * wp_rtm
            symp_p(kk+nkrv,lp_rtm) = (WR(ipp_recv-1) + WR(ipn_recv-1))  &
     &                              * wp_rtm
            symp_p(kk,lp_rtm) =      (WR(ipp_recv  ) + WR(ipn_recv  ))  &
     &                              * wp_rtm
!
            asmp_r(kk,lp_rtm) =      (WR(ipp_recv-2) - WR(ipn_recv-2))  &
     &                              * wp_rtm
            asmp_p(kk+nkrv,lp_rtm) = (WR(ipp_recv-1) - WR(ipn_recv-1))  &
     &                              * wp_rtm
            asmp_p(kk,lp_rtm) =      (WR(ipp_recv  ) - WR(ipn_recv  ))  &
     &                              * wp_rtm
!
            symp_r(kk+nkrv,lp_rtm) =  (WR(inp_recv-1) + WR(inn_recv-1)) &
     &                              * wp_rtm * asin_rtm
            symp_r(kk+2*nkrv,lp_rtm)= (WR(inp_recv  ) + WR(inn_recv  )) &
     &                              * wp_rtm * asin_rtm
!
            asmp_r(kk+nkrv,lp_rtm) =  (WR(inp_recv-1) - WR(inn_recv-1)) &
     &                              * wp_rtm * asin_rtm
            asmp_r(kk+2*nkrv,lp_rtm)= (WR(inp_recv  ) - WR(inn_recv  )) &
     &                              * wp_rtm * asin_rtm
          end do
        end do
      end do
!$omp end parallel do
!
!   Equator (if necessary)
!$omp  parallel do collapse(3)                                          &
!$omp& private(lp_rtm,wp_rtm,asin_rtm,kk,k_rtm,nd,                      &
!$omp&         ip_rtpm,in_rtpm,ipp_recv,inp_recv)
      do lp_rtm = nlo_rtm+1, nle_rtm
        do nd = 1, nvector
          do k_rtm = 1, nidx_rtm(1)
            kk = k_rtm + (nd-1) * nidx_rtm(1)
!
            wp_rtm = weight_rtm(lp_rtm)
            asin_rtm = asin_theta_1d_rtm(lp_rtm)
!
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mn_rlm-1) * istep_rtm(3)
            ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
            inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
            symp_r(kk,lp_rtm) =      WR(ipp_recv-2) * wp_rtm
            symp_p(kk+nkrv,lp_rtm) = WR(ipp_recv-1) * wp_rtm
            symp_p(kk,lp_rtm) =      WR(ipp_recv  ) * wp_rtm
!
            asmp_r(kk,lp_rtm) =      0.0d0
            asmp_p(kk+nkrv,lp_rtm) = 0.0d0
            asmp_p(kk,lp_rtm) =      0.0d0
!
            symp_r(kk+nkrv,lp_rtm) =   WR(inp_recv-1)                   &
     &                                * wp_rtm * asin_rtm
            symp_r(kk+2*nkrv,lp_rtm) = WR(inp_recv  )                   &
     &                                * wp_rtm * asin_rtm
!
            asmp_r(kk+nkrv,lp_rtm) =   0.0d0
            asmp_r(kk+2*nkrv,lp_rtm) = 0.0d0
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_vec_rtm_sym_matmul_big_smp
!
! -----------------------------------------------------------------------
!
      subroutine set_scl_rtm_sym_matmul_big_smp                         &
     &         (nnod_rtm, nidx_rtm, istep_rtm, weight_rtm, nkr,         &
     &          mp_rlm, nle_rtm, nlo_rtm, ncomp, nvector, nscalar,      &
     &          irev_sr_rtm, n_WR, WR, symp, asmp)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: symp(ncomp*nkr,nle_rtm)
      real(kind = kreal), intent(inout) :: asmp(ncomp*nkr,nle_rtm)
!
      integer(kind = kint) :: kk, k_rtm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_recv, ipn_recv
!
!
      nkrv = nkr * nvector
!
!$omp  parallel do collapse(2)                                          &
!$omp& private(lp_rtm,ln_rtm,kk,k_rtm,nd,                               &
!$omp&         ip_rtpm,ip_rtnm,ipp_recv,ipn_recv)
      do lp_rtm = 1, nlo_rtm
        do nd = 1, nscalar
          do k_rtm = 1, nidx_rtm(1)
            kk = k_rtm + (nd-1) * nidx_rtm(1)
            ln_rtm = nidx_rtm(2) - lp_rtm + 1
!
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            ipp_recv = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
            ipn_recv = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
            symp(kk+3*nkrv,lp_rtm) = (WR(ipp_recv) + WR(ipn_recv))      &
     &                              * weight_rtm(lp_rtm)
            asmp(kk+3*nkrv,lp_rtm) = (WR(ipp_recv) - WR(ipn_recv))      &
     &                              * weight_rtm(lp_rtm)
          end do
        end do
      end do
!$omp end parallel do
!
!   Equator (if necessary)
!$omp  parallel do collapse(2)                                          &
!$omp& private(lp_rtm,kk,k_rtm,nd,ip_rtpm,ipp_recv)
      do lp_rtm = nlo_rtm+1, nle_rtm
        do nd = 1, nscalar
          do k_rtm = 1, nidx_rtm(1)
            kk = k_rtm + (nd-1) * nidx_rtm(1)
!
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            ipp_recv = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
            symp(kk+3*nkrv,lp_rtm) = WR(ipp_recv) * weight_rtm(lp_rtm)
            asmp(kk+3*nkrv,lp_rtm) = 0.0d0
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_scl_rtm_sym_matmul_big_smp
!
! -----------------------------------------------------------------------
!
      end module set_sp_to_rtm_rocBLAS_smp
