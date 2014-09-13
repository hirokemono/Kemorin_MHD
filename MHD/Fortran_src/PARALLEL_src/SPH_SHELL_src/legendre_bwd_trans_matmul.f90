!>@file   legendre_bwd_trans_matmul.f90
!!@brief  module legendre_bwd_trans_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Developping version)
!!
!!@verbatim
!!      subroutine alloc_vec_bleg_mat_test(nvector)
!!      subroutine alloc_scl_bleg_mat_test(nscalar)
!!      subroutine dealloc_vec_bleg_mat_test
!!      subroutine dealloc_scl_bleg_mat_test
!!
!!      subroutine leg_b_trans_vector_matmul(ncomp, nvector)
!!      subroutine leg_b_trans_scalar_matmul(ncomp, nvector, nscalar)
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
      module legendre_bwd_trans_matmul
!
      use m_precision
      use m_constants
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
!
      real(kind = kreal), allocatable, private :: P_je(:,:)
!
      integer(kind = kint), private :: nvec_jk
      real(kind = kreal), allocatable, private :: pol_e(:,:)
      real(kind = kreal), allocatable, private :: dpl_e(:,:)
      real(kind = kreal), allocatable, private :: tor_e(:,:)
!
      integer(kind = kint), private :: nvec_lk
      real(kind = kreal), allocatable, private :: symp_r(:,:)
      real(kind = kreal), allocatable, private :: asmp_t(:,:)
      real(kind = kreal), allocatable, private :: asmp_p(:,:)
      real(kind = kreal), allocatable, private :: symn_t(:,:)
      real(kind = kreal), allocatable, private :: symn_p(:,:)
!
      integer(kind = kint), private :: nscl_jk
      real(kind = kreal), allocatable, private :: scl_e(:,:)
!
      integer(kind = kint), private :: nscl_lk
      real(kind = kreal), allocatable, private :: symp(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_vec_bleg_mat_test(nvector)
!
      integer(kind = kint), intent(in) ::nvector
!
!
      num_lj = nidx_rtm(2) * maxdegree_rlm
      allocate(Pg3_je(num_lj,np_smp))
      allocate(dPdt_je(num_lj,np_smp))
      allocate(Pgv_je(num_lj,np_smp))
!
      nvec_jk =nidx_rtm(2) * maxidx_rlm_smp(1)*nvector
      allocate(pol_e(nvec_jk,np_smp))
      allocate(dpl_e(nvec_jk,np_smp))
      allocate(tor_e(nvec_jk,np_smp))
!
      nvec_lk = nidx_rtm(2) * maxidx_rlm_smp(1)*nvector
      allocate(symp_r(nvec_lk,np_smp))
      allocate(asmp_t(nvec_lk,np_smp))
      allocate(symn_p(nvec_lk,np_smp))
      allocate(symn_t(nvec_lk,np_smp))
      allocate(asmp_p(nvec_lk,np_smp))
!
      end subroutine alloc_vec_bleg_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine alloc_scl_bleg_mat_test(nscalar)
!
      integer(kind = kint), intent(in) :: nscalar
!
!
      num_lj = nidx_rtm(2) * maxdegree_rlm
      allocate(P_je(num_lj,np_smp))
!
      nscl_jk = nidx_rtm(2) * maxidx_rlm_smp(1)*nscalar
      allocate(scl_e(nscl_jk,np_smp))
!
      nscl_lk = nidx_rtm(2) * maxidx_rlm_smp(1)*nscalar
      allocate(symp(nscl_lk,np_smp))
!
      end subroutine alloc_scl_bleg_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_vec_bleg_mat_test
!
!
      deallocate(Pg3_je, dPdt_je, Pgv_je)
      deallocate(pol_e, dpl_e, tor_e)
!
      deallocate(symp_r, symn_t, symn_p, asmp_t, asmp_p)
!
      end subroutine dealloc_vec_bleg_mat_test
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_scl_bleg_mat_test
!
!
      deallocate(P_je)
      deallocate(scl_e)
      deallocate(symp)
!
      end subroutine dealloc_scl_bleg_mat_test
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_vector_matmul(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: nb_nri, ip, kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: l_rtm, ip_rtm, in_rtm
      integer(kind = kint) :: mp_rlm, mn_rlm, j_rlm, jj, i_rlm
      integer(kind = kint) :: i_jk, i_lj, i_lk
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
      real(kind = kreal) :: a2r_1d_rlm_r
!
!
      call alloc_vec_bleg_mat_test(nvector)
!
      nb_nri = nvector*nidx_rtm(1)
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kk,kr_nd,jj,l_rtm,i_rlm,ip_rtm,in_rtm,     &
!$omp&                    k_rlm,nd,i_jk,i_lj,i_lk,                      &
!$omp&                    j_rlm,mp_rlm,mn_rlm,a2r_1d_rlm_r)
      do ip = 1, np_smp
        kst(ip) = nvector*idx_rtm_smp_stack(ip-1,1)
        nkr(ip) = nvector                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
          do mp_rlm = 1, nidx_rtm(3)
            mn_rlm = nidx_rtm(3) - mp_rlm + 1
            jst(ip) = lstack_rlm(mp_rlm-1)
            nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!   all hermonics
            do jj = 1, nj_rlm(ip)
              j_rlm = jj + jst(ip)
              do l_rtm = 1, nidx_rtm(2)
                i_lj = l_rtm + (jj-1) * nidx_rtm(2)
                Pg3_je(i_lj,ip) =  P_jl(j_rlm,l_rtm)                    &
     &                            * g_sph_rlm(j_rlm,3)
                dPdt_je(i_lj,ip) = dPdt_jl(j_rlm,l_rtm)
                Pgv_je(i_lj,ip) = -P_jl(j_rlm,l_rtm)                    &
     &                           * dble(idx_gl_1d_rlm_j(j_rlm,3))       &
     &                            *asin_theta_1d_rtm(l_rtm)
              end do
            end do
!   odd l-m
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
              a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
!   even l-m
              do jj = 1, nj_rlm(ip)
                j_rlm = jj + jst(ip)
                i_rlm = 3*nd + ncomp * ((j_rlm-1) * istep_rlm(2)        &
     &                                + (k_rlm-1) * istep_rlm(1))
                i_jk = jj + (kk-1) * nj_rlm(ip)
                pol_e(i_jk,ip) = sp_rlm(i_rlm-2) * a2r_1d_rlm_r
                dpl_e(i_jk,ip) = sp_rlm(i_rlm-1) * a_r_1d_rlm_r(k_rlm)
                tor_e(i_jk,ip) = sp_rlm(i_rlm  ) * a_r_1d_rlm_r(k_rlm)
              end do
            end do
!
!   even l-m
            call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip), &
     &          Pg3_je(1,ip), pol_e(1,ip), symp_r(1,ip))
            call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip), &
     &          dPdt_je(1,ip), dpl_e(1,ip), asmp_t(1,ip))
            call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip), &
     &          dPdt_je(1,ip), tor_e(1,ip), asmp_p(1,ip))
!
            call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip), &
     &          Pgv_je(1,ip), tor_e(1,ip), symn_t(1,ip))
            call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip), &
     &          Pgv_je(1,ip), dpl_e(1,ip), symn_p(1,ip))
!
            do l_rtm = 1, nidx_rtm(2)
              do kk = 1, nkr(ip)
                kr_nd = kk + kst(ip)
                k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
                nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
                ip_rtm = 3*nd + ncomp * ((l_rtm-1) *  istep_rtm(2)      &
     &                                 + (k_rlm-1) *  istep_rtm(1)      &
     &                                 + (mp_rlm-1) * istep_rtm(3))
                in_rtm = 3*nd + ncomp * ((l_rtm-1) *  istep_rtm(2)      &
     &                                 + (k_rlm-1) *  istep_rtm(1)      &
     &                                 + (mn_rlm-1) * istep_rtm(3))
!
                i_lk = l_rtm + (kk-1) * nidx_rtm(2)
!
                vr_rtm(ip_rtm-2) = vr_rtm(ip_rtm-2) + symp_r(i_lk,ip)
                vr_rtm(ip_rtm-1) = vr_rtm(ip_rtm-1) + asmp_t(i_lk,ip)
                vr_rtm(ip_rtm  ) = vr_rtm(ip_rtm  ) - asmp_p(i_lk,ip)
!
                vr_rtm(in_rtm-1) = vr_rtm(in_rtm-1) + symn_t(i_lk,ip)
                vr_rtm(in_rtm  ) = vr_rtm(in_rtm  ) + symn_p(i_lk,ip)
              end do
            end do
          end do
!
      end do
!$omp end parallel do
!
      call dealloc_vec_bleg_mat_test
!
      end subroutine leg_b_trans_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine leg_b_trans_scalar_matmul(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: ip, kk, kr_nd, k_rlm, i_jk, i_lj, i_lk
      integer(kind = kint) :: l_rtm, nd, ip_rtm
      integer(kind = kint) :: mp_rlm, jj, j_rlm, i_rlm
      integer(kind = kint) :: kst(np_smp), nkr(np_smp)
      integer(kind = kint) :: jst(np_smp), nj_rlm(np_smp)
!
!
      call alloc_scl_bleg_mat_test(nscalar)
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kk,kr_nd,k_rlm,nd,jj,j_rlm,i_rlm,          &
!$omp&                    mp_rlm,l_rtm,ip_rtm,i_lj,i_jk,i_lk)
      do ip = 1, np_smp
        kst(ip) = nscalar*idx_rtm_smp_stack(ip-1,1)
        nkr(ip) = nscalar                                               &
     &       * (idx_rtm_smp_stack(ip,  1) - idx_rtm_smp_stack(ip-1,1))
!
          do mp_rlm = 1, nidx_rtm(3)
            jst(ip) = lstack_rlm(mp_rlm-1)
            nj_rlm(ip) = lstack_rlm(mp_rlm) - lstack_rlm(mp_rlm-1)
!
            do jj = 1, nj_rlm(ip)
              j_rlm = jj + jst(ip)
              do l_rtm = 1, nidx_rtm(2)
                i_lj = l_rtm + (jj-1) * nidx_rtm(2)
                P_je(i_lj,ip) = P_jl(j_rlm,l_rtm)
              end do
            end do
!
            do kk = 1, nkr(ip)
              kr_nd = kk + kst(ip)
              k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
              nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
              do jj = 1, nj_rlm(ip)
                j_rlm = jj + jst(ip)
                i_rlm = nd + 3*nvector                                  &
     &                     + ncomp * ((j_rlm-1) * istep_rlm(2)          &
     &                              + (k_rlm-1) * istep_rlm(1))
                i_jk = jj + (kk-1) * nj_rlm(ip)
                scl_e(i_jk,ip) = sp_rlm(i_rlm)
              end do
            end do
!
!   even l-m
            call matmul_bwd_leg_trans(nidx_rtm(2), nkr(ip), nj_rlm(ip), &
     &          P_je(1,ip), scl_e(1,ip), symp(1,ip))
!
            do l_rtm = 1, nidx_rtm(2)
              do kk = 1, nkr(ip)
                kr_nd = kk + kst(ip)
                k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
                nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
                ip_rtm = nd + 3*nvector                                 &
     &                      + ncomp * ((l_rtm-1) *  istep_rtm(2)        &
     &                               + (k_rlm-1) *  istep_rtm(1)        &
     &                               + (mp_rlm-1) * istep_rtm(3))
                i_lk = l_rtm + (kk-1) * nidx_rtm(2)
                vr_rtm(ip_rtm) = vr_rtm(ip_rtm) + symp(i_lk,ip)
              end do
            end do
          end do
!
      end do
!$omp end parallel do
!
      call dealloc_scl_bleg_mat_test
!
      end subroutine leg_b_trans_scalar_matmul
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
      end module legendre_bwd_trans_matmul
