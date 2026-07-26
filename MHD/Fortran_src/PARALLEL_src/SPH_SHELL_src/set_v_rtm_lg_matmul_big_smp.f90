!>@file   set_v_rtm_lg_matmul_big_smp.f90
!!@brief  module set_v_rtm_lg_matmul_big_smp
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform using matrix multi
!!
!!@verbatim
!!      subroutine cal_v_rtm_sym_matmul_big_smp                         &
!!     &         (nnod_rtm, nidx_rtm, istep_rtm, asin_theta_1d_rtm,     &
!!     &          nkr, mp_rlm, mn_rlm, nl_rtm, symp_r, asmp_p,          &
!!     &          asmp_r, symp_p, ncomp, nvector, irev_sr_rtm, n_WS, WS)
!!      subroutine cal_s_rtm_sym_matmul_big_smp                         &
!!     &         (nnod_rtm, nidx_rtm, istep_rtm, nkr, mp_rlm, nl_rtm,   &
!!     &          symp, asmp, ncomp, nvector, nscalar, irev_sr_rtm,     &
!!     &          n_WS, WS)
!!        integer(kind = kint), intent(in) :: nnod_rtm
!!        integer(kind = kint), intent(in) :: nidx_rtm(3)
!!        integer(kind = kint), intent(in) :: istep_rtm(3)
!!        real(kind = kreal), intent(in):: asin_theta_1d_rtm(nidx_rtm(2))
!!        integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!!        integer(kind = kint), intent(in) :: nkr
!!        integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
!!        integer(kind = kint), intent(in) :: nl_rtm
!!        real(kind = kreal), intent(inout) :: symp_r(nl_rtm,ncomp*nkr)
!!        real(kind = kreal), intent(in) :: asmp_p(nl_rtm,2*nvector*nkr)
!!        real(kind = kreal), intent(inout) :: asmp_r(nl_rtm,ncomp*nkr)
!!        real(kind = kreal), intent(in) :: symp_p(nl_rtm,2*nvector*nkr)
!!        real(kind = kreal), intent(in) :: symp(nl_rtm,ncomp*nkr)
!!        real(kind = kreal), intent(in) :: asmp(nl_rtm,ncomp*nkr)
!!        integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
!!        integer(kind = kint), intent(in) :: n_WS
!!        real (kind=kreal), intent(inout):: WS(n_WS)
!!@endverbatim
!!
      module set_v_rtm_lg_matmul_big_smp
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
      subroutine cal_v_rtm_sym_matmul_big_smp                           &
     &         (nnod_rtm, nidx_rtm, istep_rtm, asin_theta_1d_rtm,       &
     &          nkr, mp_rlm, mn_rlm, nl_rtm, symp_r, asmp_p,            &
     &          asmp_r, symp_p, ncomp, nvector, irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nl_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout) :: symp_r(nl_rtm,ncomp*nkr)
      real(kind = kreal), intent(in) ::    asmp_p(nl_rtm,2*nvector*nkr)
      real(kind = kreal), intent(inout) :: asmp_r(nl_rtm,ncomp*nkr)
      real(kind = kreal), intent(in) ::    symp_p(nl_rtm,2*nvector*nkr)
!
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, k_rtm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_send, inp_send, ipn_send, inn_send
!
!
      nkrv = nkr * nvector
!$omp parallel do collapse(2) private(kk,lp_rtm)
      do kk = 1, nkrv
        do lp_rtm = 1, nl_rtm
          symp_r(lp_rtm,kk+nkrv)                                        &
     &         = - symp_r(lp_rtm,kk+nkrv) *   asin_theta_1d_rtm(lp_rtm)
          symp_r(lp_rtm,kk+2*nkrv)                                      &
     &         = - symp_r(lp_rtm,kk+2*nkrv) * asin_theta_1d_rtm(lp_rtm)
          asmp_r(lp_rtm,kk+nkrv)                                        &
     &         = - asmp_r(lp_rtm,kk+nkrv) *   asin_theta_1d_rtm(lp_rtm)
          asmp_r(lp_rtm,kk+2*nkrv)                                      &
     &         = - asmp_r(lp_rtm,kk+2*nkrv) * asin_theta_1d_rtm(lp_rtm)
        end do
      end do
!$omp end parallel do
!
!$omp  parallel do collapse(3)                                          &
!$omp& private(kk,k_rtm,nd,lp_rtm,ln_rtm,                               &
!$omp&         ip_rtpm,in_rtpm,ip_rtnm,in_rtnm,                         &
!$omp&         ipp_send,inp_send,ipn_send,inn_send)
      do lp_rtm = 1, nidx_rtm(2)/2
        do nd = 1, nvector
          do k_rtm = 1, nidx_rtm(1)
            kk = k_rtm + (nd-1) * nidx_rtm(1)
!
            ln_rtm =  nidx_rtm(2) - lp_rtm + 1
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mn_rlm-1) * istep_rtm(3)
            ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mn_rlm-1) * istep_rtm(3)
!
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
            ipn_send = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
            inn_send = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
            WS(ipp_send-2) = WS(ipp_send-2)                             &
     &            + symp_r(lp_rtm,kk) +     asmp_r(lp_rtm,kk)
            WS(ipp_send-1) = WS(ipp_send-1)                             &
     &            + asmp_p(lp_rtm,kk+nkrv) + symp_p(lp_rtm,kk+nkrv)
            WS(ipp_send  ) = WS(ipp_send  )                             &
     &            - asmp_p(lp_rtm,kk) -     symp_p(lp_rtm,kk)
!
            WS(inp_send-1) = WS(inp_send-1)                             &
     &            + symp_r(lp_rtm,kk+nkrv) +   asmp_r(lp_rtm,kk+nkrv)
            WS(inp_send  ) = WS(inp_send  )                             &
     &            + symp_r(lp_rtm,kk+2*nkrv) + asmp_r(lp_rtm,kk+2*nkrv)
!
!
            WS(ipn_send-2) = WS(ipn_send-2)                             &
     &            + symp_r(lp_rtm,kk) -     asmp_r(lp_rtm,kk)
            WS(ipn_send-1) = WS(ipn_send-1)                             &
     &            - asmp_p(lp_rtm,kk+nkrv) + symp_p(lp_rtm,kk+nkrv)
            WS(ipn_send  ) = WS(ipn_send  )                             &
     &            + asmp_p(lp_rtm,kk) -     symp_p(lp_rtm,kk)
!
            WS(inn_send-1) = WS(inn_send-1)                             &
     &            + symp_r(lp_rtm,kk+nkrv) -   asmp_r(lp_rtm,kk+nkrv)
            WS(inn_send  ) = WS(inn_send  )                             &
     &            + symp_r(lp_rtm,kk+2*nkrv) - asmp_r(lp_rtm,kk+2*nkrv)
          end do
        end do
      end do
!$omp end parallel do
!
!$omp  parallel do collapse(3)                                          &
!$omp& private(kk,k_rtm,nd,lp_rtm,ln_rtm,                               &
!$omp&         ip_rtpm,in_rtpm,ipp_send,inp_send)
      do lp_rtm = nidx_rtm(2)/2+1, nl_rtm
        do nd = 1, nvector
          do k_rtm = 1, nidx_rtm(1)
            kk = k_rtm + (nd-1) * nidx_rtm(1)
!
            ln_rtm =  nidx_rtm(2) - nidx_rtm(2)/2-1 + 1
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mn_rlm-1) * istep_rtm(3)
!
            ipp_send = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
            inp_send = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
            WS(ipp_send-2) = WS(ipp_send-2) + symp_r(lp_rtm,kk)
            WS(ipp_send-1) = WS(ipp_send-1) + symp_p(lp_rtm,kk+nkrv)
            WS(ipp_send  ) = WS(ipp_send  ) - symp_p(lp_rtm,kk)
!
            WS(inp_send-1) = WS(inp_send-1) + symp_r(lp_rtm,kk+nkrv)
            WS(inp_send  ) = WS(inp_send  ) + symp_r(lp_rtm,kk+2*nkrv)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_v_rtm_sym_matmul_big_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_s_rtm_sym_matmul_big_smp                           &
     &         (nnod_rtm, nidx_rtm, istep_rtm, nkr, mp_rlm, nl_rtm,     &
     &          symp, asmp, ncomp, nvector, nscalar, irev_sr_rtm,       &
     &          n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nl_rtm
      real(kind = kreal), intent(in) :: symp(nl_rtm,ncomp*nkr)
      real(kind = kreal), intent(in) :: asmp(nl_rtm,ncomp*nkr)
!
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, k_rtm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, nkrv
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_send, ipn_send
!
!
      nkrv = nkr * nvector
!
!$omp  parallel do collapse(2)                                          &
!$omp& private(kk,k_rtm,nd,lp_rtm,ln_rtm,                               &
!$omp&         ip_rtpm,ip_rtnm,ipp_send,ipn_send)
      do lp_rtm = 1, nidx_rtm(2)/2
        do nd = 1, nscalar
          do k_rtm = 1, nidx_rtm(1)
            kk = k_rtm + (nd-1) * nidx_rtm(1)
!
            ln_rtm =  nidx_rtm(2) - lp_rtm + 1
!
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
!
            ipp_send = nd + 3*nvector                                   &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
            ipn_send = nd + 3*nvector                                   &
     &                  + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
            WS(ipp_send) = WS(ipp_send)                                 &
     &                + symp(lp_rtm,kk+3*nkrv) + asmp(lp_rtm,kk+3*nkrv)
            WS(ipn_send) = WS(ipn_send)                                 &
     &                + symp(lp_rtm,kk+3*nkrv) - asmp(lp_rtm,kk+3*nkrv)
        end do
        end do
      end do
!$omp end parallel do
!
!$omp  parallel do collapse(3)                                          &
!$omp& private(kk,k_rtm,nd,lp_rtm,ip_rtpm,ipp_send)
      do lp_rtm = nidx_rtm(2)/2+1, nl_rtm
        do nd = 1, nscalar
          do k_rtm = 1, nidx_rtm(1)
            kk = k_rtm + (nd-1) * nidx_rtm(1)
!
            ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                     &
     &                  + (k_rtm-1) *  istep_rtm(1)                     &
     &                  + (mp_rlm-1) * istep_rtm(3)
            ipp_send = nd + 3*nvector                                   &
     &                    + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
!
            WS(ipp_send) = WS(ipp_send) + symp(lp_rtm,kk+3*nkrv)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_s_rtm_sym_matmul_big_smp
!
! -----------------------------------------------------------------------
!
      end module set_v_rtm_lg_matmul_big_smp
