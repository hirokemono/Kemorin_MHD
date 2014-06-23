!>@file   legendre_fwd_trans_krin.f90
!!@brief  module legendre_fwd_trans_krin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (innermost loop is field and radius)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_krin(ncomp, nvector,         &
!!     &          vr_rtm_krin, sp_rlm_spin)
!!      subroutine legendre_f_trans_scalar_krin(ncomp, nvector, nscalar,&
!!     &          vr_rtm_krin, sp_rlm_spin)
!!        Input:  vr_rtm_krin
!!        Output: sp_rlm_spin
!!@endverbatim
!!
!!@n @param  ncomp    number of components to be transformed
!!@n @param  nvector  number of vector to be transformed
!!@n @param  nscalar  number of scalar to be transformed
!
      module legendre_fwd_trans_krin
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_vector_krin(ncomp, nvector,           &
     &          vr_rtm_krin, sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_krin(nidx_rtm(1)*ncomp,nidx_rtm(2),nidx_rtm(3))
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_spin(nidx_rtm(1)*ncomp,nidx_rlm(2))
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, l_rtm
      integer(kind = kint) :: kr_nd, k_rtm, nb_nri
      real(kind = kreal) :: sp1(nvector*nidx_rlm(1))
      real(kind = kreal) :: sp2(nvector*nidx_rlm(1))
      real(kind = kreal) :: sp3(nvector*nidx_rlm(1))
      real(kind = kreal) :: Pvw_l(nidx_rtm(2))
      real(kind = kreal) :: dPvw_l(nidx_rtm(2))
      real(kind = kreal) :: Pgvw_l(nidx_rtm(2))
!
!
      nb_nri = nvector*nidx_rlm(1)
!$omp parallel do private(j_rlm,l_rtm,mp_rlm,mn_rlm,kr_nd,              &
!$omp&                    k_rtm,Pvw_l,dPvw_l,Pgvw_l,sp1,sp2,sp3)
      do j_rlm = 1, nidx_rlm(2)
        mp_rlm = mdx_p_rlm_rtm(j_rlm)
        mn_rlm = mdx_n_rlm_rtm(j_rlm)
        Pvw_l(1:nidx_rtm(2)) =  Pvw_lj(1:nidx_rtm(2),j_rlm)
        dPvw_l(1:nidx_rtm(2)) = dPvw_lj(1:nidx_rtm(2),j_rlm)
        Pgvw_l(1:nidx_rtm(2)) = Pgvw_lj(1:nidx_rtm(2),j_rlm)
!
        sp1(1:nb_nri) = 0.0d0
        sp2(1:nb_nri) = 0.0d0
        sp3(1:nb_nri) = 0.0d0
        do l_rtm = 1, nidx_rtm(2)
          do kr_nd = 1, nb_nri
            sp1(kr_nd) = sp1(kr_nd)                                     &
     &       +  vr_rtm_krin(kr_nd,         l_rtm,mp_rlm)*Pvw_l(l_rtm)
!
            sp2(kr_nd) = sp2(kr_nd)                                     &
     &       + (vr_rtm_krin(kr_nd+nb_nri,  l_rtm,mp_rlm)*dPvw_l(l_rtm)  &
     &        - vr_rtm_krin(kr_nd+2*nb_nri,l_rtm,mn_rlm)*Pgvw_l(l_rtm))
!
            sp3(kr_nd) = sp3(kr_nd)                                     &
     &       - (vr_rtm_krin(kr_nd+nb_nri,  l_rtm,mn_rlm)*Pgvw_l(l_rtm)  &
     &        + vr_rtm_krin(kr_nd+2*nb_nri,l_rtm,mp_rlm)*dPvw_l(l_rtm))
          end do
        end do
!
        do kr_nd = 1, nb_nri
          k_rtm = 1 + mod((kr_nd-1),nidx_rlm(1))
          sp_rlm_spin(kr_nd,         j_rlm)                             &
     &        = sp1(kr_nd) * radius_1d_rlm_r(k_rtm)                     &
     &                      *radius_1d_rlm_r(k_rtm)
          sp_rlm_spin(kr_nd+nb_nri,  j_rlm)                             &
     &        = sp2(kr_nd) * radius_1d_rlm_r(k_rtm)
          sp_rlm_spin(kr_nd+2*nb_nri,j_rlm)                             &
     &        = sp3(kr_nd) * radius_1d_rlm_r(k_rtm)
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_krin(ncomp, nvector, nscalar,  &
     &          vr_rtm_krin, sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_krin(nidx_rtm(1)*ncomp,nidx_rtm(2),nidx_rtm(3))
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_spin(nidx_rtm(1)*ncomp,nidx_rlm(2))
!
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm
      integer(kind = kint) :: kst, nb_nri, kr_nd
      real(kind = kreal) :: Pws_l(nidx_rtm(2))
      real(kind = kreal) :: sp1(nscalar*nidx_rlm(1))
!
!
      kst = 3*nvector * nidx_rlm(1)
      nb_nri = nscalar*nidx_rlm(1)
!$omp parallel do private(j_rlm,l_rtm,mp_rlm,Pws_l,sp1)
      do j_rlm = 1, nidx_rlm(2)
        mp_rlm = mdx_p_rlm_rtm(j_rlm)
        Pws_l(1:nidx_rtm(2)) =  Pws_lj(1:nidx_rtm(2),j_rlm)
        do l_rtm = 1, nidx_rtm(2)
          sp1(1:nscalar*nidx_rtm(1)) = 0.0d0
!
        do kr_nd = 1, nb_nri
            sp1(kr_nd) = sp1(kr_nd)                                     &
     &            + vr_rtm_krin(kr_nd+kst,l_rtm,mp_rlm) * Pws_l(l_rtm)
          end do
        end do
!
        do kr_nd = 1, nb_nri
          sp_rlm_spin(kr_nd+kst,j_rlm) = sp1(kr_nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_krin
