!>@file   legendre_bwd_sym_matmul.f90
!!@brief  module legendre_bwd_sym_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Developping version)
!!
!!@verbatim
!!      subroutine alloc_vec_bleg_sym_mat(nvector)
!!      subroutine alloc_scl_bleg_sym_mat(nscalar)
!!      subroutine dealloc_vec_bleg_sym_mat
!!      subroutine dealloc_scl_bleg_sym_mat
!!
!!      subroutine leg_b_trans_vec_sym_matmul(ncomp, nvector,           &
!!     &          irev_sr_rlm, n_WR, WR, vr_rtm_1)
!!      subroutine leg_b_trans_scl_sym_matmul(ncomp, nvector, nscalar,  &
!!     &          irev_sr_rlm, n_WR, WR, vr_rtm_1)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!
!!     field data for Legendre transform
!!       original layout: vr_rtm(l_rtm,m_rtm,k_rlm,icomp)
!!       size: vr_rtm(nidx_rtm(2),nidx_rtm(1)*ncomp,nidx_rtm(3))
!!      real(kind = kreal), allocatable :: vr_rtm(:,:,:)
!!
!!     spectr data for Legendre transform
!!       original layout: sp_rlm(j_rlm,k_rlm,icomp)
!!        size: sp_rlm(nidx_rlm(2),nidx_rtm(1)*ncomp)
!!      real(kind = kreal), allocatable :: sp_rlm(:,:)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_sym_matmul
!
      use m_precision
      use m_constants
      use m_work_time
      use calypso_mpi
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      implicit none
!
      integer(kind = kint), private :: num_lj
      real(kind = kreal), allocatable, private :: Pg3_je(:,:)
      real(kind = kreal), allocatable, private :: dPdt_je(:,:)
      real(kind = kreal), allocatable, private :: Pgv_je(:,:)
      real(kind = kreal), allocatable, private :: Pg3_jo(:,:)
      real(kind = kreal), allocatable, private :: dPdt_jo(:,:)
      real(kind = kreal), allocatable, private :: Pgv_jo(:,:)
!
      real(kind = kreal), allocatable, private :: P_je(:,:)
      real(kind = kreal), allocatable, private :: P_jo(:,:)
!
      integer(kind = kint), private :: nvec_jk
      real(kind = kreal), allocatable, private :: pol_e(:,:)
      real(kind = kreal), allocatable, private :: dpl_e(:,:)
      real(kind = kreal), allocatable, private :: tor_e(:,:)
      real(kind = kreal), allocatable, private :: pol_o(:,:)
      real(kind = kreal), allocatable, private :: dpl_o(:,:)
      real(kind = kreal), allocatable, private :: tor_o(:,:)
!
      integer(kind = kint), private :: nvec_lk
      real(kind = kreal), allocatable, private :: symp_r(:,:)
      real(kind = kreal), allocatable, private :: asmp_t(:,:)
      real(kind = kreal), allocatable, private :: asmp_p(:,:)
      real(kind = kreal), allocatable, private :: symn_t(:,:)
      real(kind = kreal), allocatable, private :: symn_p(:,:)
      real(kind = kreal), allocatable, private :: asmp_r(:,:)
      real(kind = kreal), allocatable, private :: symp_t(:,:)
      real(kind = kreal), allocatable, private :: symp_p(:,:)
      real(kind = kreal), allocatable, private :: asmn_t(:,:)
      real(kind = kreal), allocatable, private :: asmn_p(:,:)
!
      integer(kind = kint), private :: nscl_jk
      real(kind = kreal), allocatable, private :: scl_e(:,:)
      real(kind = kreal), allocatable, private :: scl_o(:,:)
!
      integer(kind = kint), private :: nscl_lk
      real(kind = kreal), allocatable, private :: symp(:,:)
      real(kind = kreal), allocatable, private :: asmp(:,:)
!
      real(kind = kreal), private :: st_elapsed
      real(kind = kreal), private :: elaps(4)
      integer, external :: omp_get_max_threads
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_vec_bleg_sym_mat(nvector)
!
      integer(kind = kint), intent(in) ::nvector
!
!      write(*,*) 'istep_rtp', istep_rtp, nidx_rtp
!      write(*,*) 'istep_rtm', istep_rtm, nidx_rtm
!      write(*,*) 'istep_rlm', istep_rlm, nidx_rlm
!      write(*,*) 'istep_rj', istep_rj, nidx_rj
!
!
      num_lj = ((nidx_rtm(2)+1)/2) * maxdegree_rlm
      allocate(Pg3_je(num_lj,np_smp))
      allocate(dPdt_je(num_lj,np_smp))
      allocate(Pgv_je(num_lj,np_smp))
      allocate(Pg3_jo(num_lj,np_smp))
      allocate(dPdt_jo(num_lj,np_smp))
      allocate(Pgv_jo(num_lj,np_smp))
!
      nvec_jk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nvector
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpl_e(nvec_jk,np_smp))
      allocate(tor_e(nvec_jk,np_smp))
      allocate(pol_o(nvec_jk,np_smp))
      allocate(dpl_o(nvec_jk,np_smp))
      allocate(tor_o(nvec_jk,np_smp))
!
      nvec_lk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nvector
      allocate(symp_r(nvec_lk,np_smp))
      allocate(asmp_r(nvec_lk,np_smp))
      allocate(symp_t(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
      allocate(asmn_p(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(asmn_t(nvec_lk,np_smp))
      allocate(symp_p(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
!
      end subroutine alloc_vec_bleg_sym_mat
!
! -----------------------------------------------------------------------
!
      subroutine alloc_scl_bleg_sym_mat(nscalar)
!
      integer(kind = kint), intent(in) :: nscalar
!
!
      num_lj = ((nidx_rtm(2)+1)/2) * maxdegree_rlm
      allocate(P_je(num_lj,np_smp))
      allocate(P_jo(num_lj,np_smp))
!
      nscl_jk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nscalar
      allocate(scl_e(nscl_jk,np_smp))
      allocate(scl_o(nscl_jk,np_smp))
!
      nscl_lk = ((nidx_rtm(2)+1)/2) * maxidx_rlm_smp(1)*nscalar
      allocate(symp(nscl_lk,np_smp))
      allocate(asmp(nscl_lk,np_smp))
!
      end subroutine alloc_scl_bleg_sym_mat
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_vec_bleg_sym_mat
!
!
      deallocate(Pg3_je, dPdt_je, Pgv_je, Pg3_jo, dPdt_jo, Pgv_jo)
!
      deallocate(pol_e, dpl_e, tor_e, pol_o, dpl_o, tor_o)
!
      deallocate(symp_r, symp_t, symp_p, symn_t, symn_p)
      deallocate(asmp_r, asmp_t, asmp_p, asmn_t, asmn_p)
!
      end subroutine dealloc_vec_bleg_sym_mat
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_scl_bleg_sym_mat
!
!
      deallocate(P_je, P_jo)
      deallocate(scl_e, scl_o)
      deallocate(symp, asmp)
!
      end subroutine dealloc_scl_bleg_sym_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_vec_sym_matmul(ncomp, nvector,             &
     &          irev_sr_rlm, n_WR, WR, vr_rtm_1)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: vr_rtm_1(ncomp*nnod_rtm)
!
      integer(kind = kint) :: ip, kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: ll, lp_rtm, ln_rtm, i_rlm, i_recv
      integer(kind = kint) :: mp_rlm, mn_rlm, j_rlm, jj
      integer(kind = kint) :: i_jk, i_lj, i_lk, nl_rtm
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
      real(kind = kreal) :: a2r_1d_rlm_r
!
!
      call alloc_vec_bleg_sym_mat(nvector)
!
      elaps(1:4) = 0
      nl_rtm = (nidx_rtm(2) + 1)/2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kk,kr_nd,jj,ll,lp_rtm,ln_rtm,              &
!$omp&                    k_rlm,nd,i_jk,i_lj,i_lk,i_rlm,i_recv,         &
!$omp&                    ip_rtpm,in_rtpm,ip_rtnm,in_rtnm,              &
!$omp&                    j_rlm,mp_rlm,mn_rlm,a2r_1d_rlm_r,             &
!$omp&                    st_elapsed)                                   &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nvector*idx_rtm_smp_stack(ip-1,1)
        nkr(ip) = nvector                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
          do mp_rlm = 1, nidx_rtm(3)
            mn_rlm = nidx_rtm(3) - mp_rlm + 1
            jst(ip) = lstack_rlm(mp_rlm-1)
            nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            n_jk_e(ip) = (nj_rlm(ip)+1)/2
            n_jk_o(ip) = nj_rlm(ip)/2
!   even l-m
            st_elapsed = MPI_WTIME()
            do jj = 1, n_jk_e(ip)
              j_rlm = 2*jj + jst(ip) - 1
              do ll = 1, nl_rtm
                lp_rtm =  ll
                i_lj = ll + (jj-1) * nl_rtm
                Pg3_je(i_lj,ip) =  P_jl(j_rlm,lp_rtm)                   &
     &                            * g_sph_rlm(j_rlm,3)
                dPdt_je(i_lj,ip) = dPdt_jl(j_rlm,lp_rtm)
                Pgv_je(i_lj,ip) = -P_jl(j_rlm,lp_rtm)                   &
     &                           * dble(idx_gl_1d_rlm_j(j_rlm,3))       &
     &                            *asin_theta_1d_rtm(lp_rtm)
              end do
            end do
!   odd l-m
            do jj = 1, n_jk_o(ip)
              j_rlm = 2*jj + jst(ip)
              do ll = 1, nl_rtm
                lp_rtm =  ll
                i_lj = ll + (jj-1) * nl_rtm
                Pg3_jo(i_lj,ip) = P_jl(j_rlm,lp_rtm)                    &
     &                           * g_sph_rlm(j_rlm,3)
                dPdt_jo(i_lj,ip) = dPdt_jl(j_rlm,lp_rtm)
                Pgv_jo(i_lj,ip) = -P_jl(j_rlm,lp_rtm)                   &
     &                           * dble(idx_gl_1d_rlm_j(j_rlm,3))       &
     &                            *asin_theta_1d_rtm(lp_rtm)
              end do
            end do
            elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
            st_elapsed = MPI_WTIME()
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
              a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
!   even l-m
              do jj = 1, n_jk_e(ip)
                i_rlm = 1 + (2*jj + jst(ip) - 2) * istep_rlm(2)         &
     &                    + (k_rlm-1) *            istep_rlm(1)
                i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
                i_jk = jj + (kk-1) * n_jk_e(ip)
!
                pol_e(i_jk,ip) = WR(i_recv-2) * a2r_1d_rlm_r
                dpl_e(i_jk,ip) = WR(i_recv-1) * a_r_1d_rlm_r(k_rlm)
                tor_e(i_jk,ip) = WR(i_recv  ) * a_r_1d_rlm_r(k_rlm)
              end do
!   odd l-m
              do jj = 1, n_jk_o(ip)
                i_rlm = 1 + (2*jj + jst(ip)-1) * istep_rlm(2)           &
     &                    + (k_rlm-1) *          istep_rlm(1)
                i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
                i_jk = jj + (kk-1) * n_jk_o(ip)
                pol_o(i_jk,ip) = WR(i_recv-2) * a2r_1d_rlm_r
                dpl_o(i_jk,ip) = WR(i_recv-1) * a_r_1d_rlm_r(k_rlm)
                tor_o(i_jk,ip) = WR(i_recv  ) * a_r_1d_rlm_r(k_rlm)
              end do
            end do
            elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
            st_elapsed = MPI_WTIME()
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),      &
     &          Pg3_je(1,ip), pol_e(1,ip), symp_r(1,ip))
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),      &
     &          dPdt_je(1,ip), dpl_e(1,ip), asmp_t(1,ip))
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),      &
     &          dPdt_je(1,ip), tor_e(1,ip), asmp_p(1,ip))
!
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),      &
     &          Pgv_je(1,ip), tor_e(1,ip), symn_t(1,ip))
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),      &
     &          Pgv_je(1,ip), dpl_e(1,ip), symn_p(1,ip))
!   odd l-m
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),      &
     &          Pg3_jo(1,ip), pol_o(1,ip), asmp_r(1,ip))
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),      &
     &          dPdt_jo(1,ip), dpl_o(1,ip), symp_t(1,ip))
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),      &
     &          dPdt_jo(1,ip), tor_o(1,ip), symp_p(1,ip))
!
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),      &
     &          Pgv_jo(1,ip), tor_o(1,ip), asmn_t(1,ip))
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),      &
     &          Pgv_jo(1,ip), dpl_o(1,ip), asmn_p(1,ip))
            elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
            st_elapsed = MPI_WTIME()
            do ll = 1, nl_rtm
              lp_rtm =  ll
              ln_rtm =  nidx_rtm(2) - lp_rtm + 1
              do kk = 1, nkr(ip)
                i_lk = ll + (kk-1) * nl_rtm
                kr_nd = kk + kst(ip)
                k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
                nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
                ip_rtpm = 3*nd + ncomp*((lp_rtm-1) * istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mp_rlm-1) * istep_rtm(3))
                in_rtpm = 3*nd + ncomp*((lp_rtm-1) * istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mn_rlm-1) * istep_rtm(3))
                ip_rtnm = 3*nd + ncomp*((ln_rtm-1) * istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mp_rlm-1) * istep_rtm(3))
                in_rtnm = 3*nd + ncomp*((ln_rtm-1) * istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mn_rlm-1) * istep_rtm(3))
!
                vr_rtm_1(ip_rtpm-2) = vr_rtm_1(ip_rtpm-2)                   &
     &                + symp_r(i_lk,ip) + asmp_r(i_lk,ip)
                vr_rtm_1(ip_rtpm-1) = vr_rtm_1(ip_rtpm-1)                   &
     &                + asmp_t(i_lk,ip) + symp_t(i_lk,ip)
                vr_rtm_1(ip_rtpm  ) = vr_rtm_1(ip_rtpm  )                   &
     &                - asmp_p(i_lk,ip) - symp_p(i_lk,ip)
!
                vr_rtm_1(in_rtpm-1) = vr_rtm_1(in_rtpm-1)                   &
     &                + symn_t(i_lk,ip) + asmn_t(i_lk,ip)
                vr_rtm_1(in_rtpm  ) = vr_rtm_1(in_rtpm  )                   &
     &                + symn_p(i_lk,ip) + asmn_p(i_lk,ip)
!
!
                vr_rtm_1(ip_rtnm-2) = vr_rtm_1(ip_rtnm-2)                   &
     &                + symp_r(i_lk,ip) - asmp_r(i_lk,ip)
                vr_rtm_1(ip_rtnm-1) = vr_rtm_1(ip_rtnm-1)                   &
     &                - asmp_t(i_lk,ip) + symp_t(i_lk,ip)
                vr_rtm_1(ip_rtnm  ) = vr_rtm_1(ip_rtnm  )                   &
     &                + asmp_p(i_lk,ip) - symp_p(i_lk,ip)
!
                vr_rtm_1(in_rtnm-1) = vr_rtm_1(in_rtnm-1)                   &
     &                + symn_t(i_lk,ip) - asmn_t(i_lk,ip)
                vr_rtm_1(in_rtnm  ) = vr_rtm_1(in_rtnm  )                   &
     &                + symn_p(i_lk,ip) - asmn_p(i_lk,ip)
              end do
            end do
            elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
          end do
!
      end do
!$omp end parallel do
!
      call dealloc_vec_bleg_sym_mat
      elapsed(41:44)                                                    &
     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      call start_eleps_time(45)
!   Equator (if necessary)
      if(mod(nidx_rtm(2),2) .eq. 0) return
      lp_rtm = (nidx_rtm(2)+1) / 2
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kk,kr_nd,mp_rlm,k_rlm,nd,ip_rtpm)
      do ip = 1, np_smp
        kst(ip) = nvector*idx_rtm_smp_stack(ip-1,1)
        nkr(ip) = nvector                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
        do mp_rlm = 1, nidx_rtm(3)
          do kk = 1, nkr(ip)
            kr_nd = kk + kst(ip)
            k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
            nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
            ip_rtpm = 3*nd + ncomp*((lp_rtm-1) * istep_rtm(2)           &
     &                            + (k_rlm-1) *  istep_rtm(1)           &
     &                            + (mp_rlm-1) * istep_rtm(3))
!
            vr_rtm_1(ip_rtpm-2)  = half * vr_rtm_1(ip_rtpm-2)
            vr_rtm_1(ip_rtpm-1)  = half * vr_rtm_1(ip_rtpm-1)
            vr_rtm_1(ip_rtpm  )  = half * vr_rtm_1(ip_rtpm  )
          end do
        end do
      end do
!$omp end parallel do
      call end_eleps_time(45)
!
      end subroutine leg_b_trans_vec_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_scl_sym_matmul(ncomp, nvector, nscalar,    &
     &          irev_sr_rlm, n_WR, WR, vr_rtm_1)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: vr_rtm_1(ncomp*nnod_rtm)
!
      integer(kind = kint) :: ip, kk, kr_nd, nd, i_jk, i_lj, i_lk
      integer(kind = kint) :: ll, lp_rtm, ln_rtm, nl_rtm
      integer(kind = kint) :: k_rlm, i_rlm, i_recv
      integer(kind = kint) :: mp_rlm, jj, j_rlm, ip_rtpm, ip_rtnm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
      integer(kind = kint) :: n_jk_e(np_smp), n_jk_o(np_smp)
!
!
      call alloc_scl_bleg_sym_mat(nscalar)
!
      elaps(1:4) = 0
      nl_rtm = (nidx_rtm(2) + 1)/2
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kk,kr_nd,jj,j_rlm,ll,ip_rtpm,ip_rtnm,      &
!$omp&                    k_rlm,nd,mp_rlm,lp_rtm,ln_rtm,i_lj,i_jk,i_lk, &
!$omp&                    i_rlm,i_recv,st_elapsed) &
!$omp& reduction(+:elaps)
      do ip = 1, np_smp
        kst(ip) = nscalar*idx_rtm_smp_stack(ip-1,1)
        nkr(ip) = nscalar                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
!
          do mp_rlm = 1, nidx_rtm(3)
            jst(ip) = lstack_rlm(mp_rlm-1)
            nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
            n_jk_e(ip) = (nj_rlm(ip)+1)/2
            n_jk_o(ip) = nj_rlm(ip)/2
!
!   even l-m
            st_elapsed = MPI_WTIME()
            do jj = 1, n_jk_e(ip)
              j_rlm = 2*jj + jst(ip) - 1
              do ll = 1, nl_rtm
                lp_rtm =  ll
                i_lj = ll + (jj-1) * nl_rtm
                P_je(i_lj,ip) = P_jl(j_rlm,lp_rtm)
              end do
            end do
!   odd l-m

            do jj = 1, n_jk_o(ip)
              j_rlm = 2*jj + jst(ip)
              do ll = 1, nl_rtm
                lp_rtm =  ll
                i_lj = ll + (jj-1) * nl_rtm
                P_jo(i_lj,ip) = P_jl(j_rlm,lp_rtm)
              end do
            end do
            elaps(1) = MPI_WTIME() - st_elapsed + elaps(1)
!
            st_elapsed = MPI_WTIME()
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!   even l-m
              do jj = 1, n_jk_e(ip)
                j_rlm = 2*jj + jst(ip) - 1
                i_rlm = 1 + (2*jj + jst(ip) - 2) * istep_rlm(2)         &
     &                    + (k_rlm-1) *            istep_rlm(1)
                i_recv = nd + 3*nvector                                 &
     &                      + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
                i_jk = jj + (kk-1) * n_jk_e(ip)
                scl_e(i_jk,ip) = WR(i_recv)
              end do
!   odd l-m
              do jj = 1, n_jk_o(ip)
                i_rlm = 1 + (2*jj + jst(ip) - 1) * istep_rlm(2)         &
     &                    + (k_rlm-1) *            istep_rlm(1)
                i_recv = nd + 3*nvector                                 &
     &                      + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
                i_jk = jj + (kk-1) * n_jk_o(ip)
                scl_o(i_jk,ip) = WR(i_recv)
              end do
            end do
            elaps(2) = MPI_WTIME() - st_elapsed + elaps(2)
!
!   even l-m
            st_elapsed = MPI_WTIME()
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_e(ip),      &
     &          P_je(1,ip), scl_e(1,ip), symp(1,ip))
!   odd l-m
            call matmul_bwd_leg_trans(nl_rtm, nkr(ip), n_jk_o(ip),      &
     &          P_jo(1,ip), scl_o(1,ip), asmp(1,ip))
            elaps(3) = MPI_WTIME() - st_elapsed + elaps(3)
!
            st_elapsed = MPI_WTIME()
            do ll = 1, nl_rtm
              lp_rtm =  ll
              ln_rtm =  nidx_rtm(2) - lp_rtm + 1
              do kk = 1, nkr(ip)
                i_lk = ll + (kk-1) * nl_rtm
                kr_nd = kk + kst(ip)
                k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
                nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
                ip_rtpm = nd + 3*nvector                                &
     &                        + ncomp*((lp_rtm-1) * istep_rtm(2)        &
     &                               + (k_rlm-1) *  istep_rtm(1)        &
     &                               + (mp_rlm-1) * istep_rtm(3))
                ip_rtnm = nd + 3*nvector                                &
     &                        + ncomp*((ln_rtm-1) *  istep_rtm(2)       &
     &                                + (k_rlm-1) *  istep_rtm(1)       &
     &                                + (mp_rlm-1) * istep_rtm(3))
!
                vr_rtm_1(ip_rtpm) = vr_rtm_1(ip_rtpm)                       &
     &                + symp(i_lk,ip) + asmp(i_lk,ip)
                vr_rtm_1(ip_rtnm) = vr_rtm_1(ip_rtnm)                       &
     &                + symp(i_lk,ip) - asmp(i_lk,ip)
              end do
            end do
            elaps(4) = MPI_WTIME() - st_elapsed + elaps(4)
          end do
!
      end do
!$omp end parallel do
!
      call dealloc_scl_bleg_sym_mat
      elapsed(41:44)                                                    &
     &     = elaps(1:4)/ dble(omp_get_max_threads()) + elapsed(41:44)
!
      call start_eleps_time(45)
!   Equator (if necessary)
      if(mod(nidx_rtm(2),2) .eq. 0) return
      lp_rtm = (nidx_rtm(2)+1) / 2
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kk,kr_nd,k_rlm,nd,mp_rlm,ip_rtpm)
      do ip = 1, np_smp
        kst(ip) = nscalar*idx_rtm_smp_stack(ip-1,1)
        nkr(ip) = nscalar                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
        do mp_rlm = 1, nidx_rtm(3)
          do kk = 1, nkr(ip)
            kr_nd = kk + kst(ip)
            k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
            nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
            ip_rtpm = nd + 3*nvector                                    &
     &                   + ncomp*((lp_rtm-1) * istep_rtm(2)             &
     &                          + (k_rlm-1) *  istep_rtm(1)             &
     &                          + (mp_rlm-1) * istep_rtm(3))
!
            vr_rtm_1(ip_rtpm) = half * vr_rtm_1(ip_rtpm)
          end do
        end do
      end do
!$omp end parallel do
      call end_eleps_time(45)
!
      end subroutine leg_b_trans_scl_sym_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine matmat_bwd_leg_trans(nl_rtm, nkr, n_jk,                &
     &          P_lj, S_jk, V_lk)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!
      integer(kind = kint) :: jj, kk, ll
      real(kind = kreal) :: s
!
!
      do kk = 1, nkr
        do ll = 1, nl_rtm
          s = 0.0d0
          do jj = 1, n_jk
            s = s + P_lj(ll,jj) * S_jk(jj,kk)
          end do
          V_lk(ll,kk) = s
        end do
      end do
!
      end subroutine matmat_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmul_bwd_leg_trans(nl_rtm, nkr, n_jk,                &
     &          P_lj, S_jk, V_lk)
!
      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!
!
      V_lk = matmul(P_lj,S_jk)
!
      end subroutine matmul_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
!      subroutine dgemm_bwd_leg_trans(nl_rtm, nkr, n_jk,                &
!     &          P_lj, S_jk, V_lk)
!
!      integer(kind = kint), intent(in) :: n_jk, nkr, nl_rtm
!      real(kind = kreal), intent(in) :: P_lj(nl_rtm,n_jk)
!      real(kind = kreal), intent(in) :: S_jk(n_jk,nkr)
!
!      real(kind = kreal), intent(inout) :: V_lk(nl_rtm,nkr)
!
!
!      if(n_jk .eq. 0) then
!        V_lk = 0.0d0
!      else
!        call DGEMM('N', 'N', nl_rtm, nkr, n_jk, one,                   &
!     &      P_lj, nl_rtm, S_jk, n_jk, zero, V_lk, nl_rtm)
!      end if
!
!      end subroutine dgemm_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      end module legendre_bwd_sym_matmul
