!>@file   legendre_fwd_trans_mdin.f90
!!@brief  module legendre_fwd_trans_mdin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_mdin(ncomp, nvector,         &
!!     &          vr_rtm_spin, sp_rlm_spin)
!!      subroutine legendre_f_trans_scalar_mdin(ncomp, nscalar, nvector,&
!!     &          vr_rtm_spin, sp_rlm_spin)
!!        Input:  vr_rtm_spin
!!        Output: sp_rlm_spin
!!@endverbatim
!!
!!@n @param  ncomp    number of components to be transformed
!!@n @param  nvector  number of vector to be transformed
!!@n @param  nscalar  number of fields to be transformed
!
      module legendre_fwd_trans_mdin
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
      subroutine legendre_f_trans_vector_mdin(ncomp, nvector,           &
     &          vr_rtm_spin, sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(3),nidx_rtm(1)*ncomp)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rtm(1)*ncomp)
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, l_rtm
      integer(kind = kint) :: nb_nri, kr_nd, k_rlm
      real(kind = kreal) :: sp1, sp2, sp3
      real(kind = kreal) :: Pvw_l(nidx_rtm(2))
      real(kind = kreal) :: dPvw_l(nidx_rtm(2))
      real(kind = kreal) :: Pgvw_l(nidx_rtm(2))
!
!
      nb_nri = nvector*nidx_rlm(1)
!$omp parallel do private(j_rlm,kr_nd,l_rtm,mp_rlm,mn_rlm,              &
!$omp&                    sp1,sp2,sp3,Pvw_l,dPvw_l,Pgvw_l)
      do j_rlm = 1, nidx_rlm(2)
        mp_rlm = mdx_p_rlm_rtm(j_rlm)
        mn_rlm = mdx_n_rlm_rtm(j_rlm)
        Pvw_l(1:nidx_rtm(2)) =  Pvw_lj(1:nidx_rtm(2),j_rlm)
        dPvw_l(1:nidx_rtm(2)) = dPvw_lj(1:nidx_rtm(2),j_rlm)
        Pgvw_l(1:nidx_rtm(2)) = Pgvw_lj(1:nidx_rtm(2),j_rlm)
        do kr_nd = 1, nb_nri
          sp1 = 0.0d0
          sp2 = 0.0d0
          sp3 = 0.0d0
          do l_rtm = 1, nidx_rtm(2)
            sp1 = sp1                                                   &
     &       +  vr_rtm_spin(l_rtm,mp_rlm,kr_nd         )*Pvw_l(l_rtm)
!
            sp2 = sp2                                                   &
     &       + (vr_rtm_spin(l_rtm,mp_rlm,kr_nd+nb_nri  )*dPvw_l(l_rtm)  &
     &        - vr_rtm_spin(l_rtm,mn_rlm,kr_nd+2*nb_nri)*Pgvw_l(l_rtm))
!
            sp3 = sp3                                                   &
     &       - (vr_rtm_spin(l_rtm,mn_rlm,kr_nd+nb_nri  )*Pgvw_l(l_rtm)  &
     &        + vr_rtm_spin(l_rtm,mp_rlm,kr_nd+2*nb_nri)*dPvw_l(l_rtm))
          end do
          sp_rlm_spin(j_rlm,kr_nd         ) = sp1
          sp_rlm_spin(j_rlm,kr_nd+nb_nri  ) = sp2
          sp_rlm_spin(j_rlm,kr_nd+2*nb_nri) = sp3
        end do
      end do
!$omp end parallel do
!
!
!$omp parallel private(kr_nd,k_rlm)
      do kr_nd = 1, nb_nri
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
!        nd =  1 + (kr_nd - k_rlm) / nidx_rlm(1)
!$omp do private(j_rlm)
        do j_rlm = 1, nidx_rlm(2)
          sp_rlm_spin(j_rlm,kr_nd         )                             &
     &        = sp_rlm_spin(j_rlm,kr_nd         )                       &
     &         * radius_1d_rlm_r(k_rlm)*radius_1d_rlm_r(k_rlm)
          sp_rlm_spin(j_rlm,kr_nd+nb_nri  )                             &
     &        = sp_rlm_spin(j_rlm,kr_nd+nb_nri  )                       &
     &         * radius_1d_rlm_r(k_rlm)
          sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)                             &
     &        = sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)                       &
     &         * radius_1d_rlm_r(k_rlm)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine legendre_f_trans_vector_mdin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_mdin(ncomp, nscalar, nvector,  &
     &          vr_rtm_spin, sp_rlm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nscalar, nvector
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(3),nidx_rtm(1)*ncomp)
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rtm(1)*ncomp)
!
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm
      integer(kind = kint) :: kr_nd, kst, ked
      real(kind = kreal) :: sp1
      real(kind = kreal) :: Pws_l(nidx_rtm(2))
!
!
      kst = 1 + 3*nvector * nidx_rtm(1)
      ked = (nscalar + 3*nvector) * nidx_rtm(1)
!$omp parallel do private(j_rlm,kr_nd,l_rtm,mp_rlm,sp1,Pws_l)
      do j_rlm = 1, nidx_rlm(2)
        mp_rlm = mdx_p_rlm_rtm(j_rlm)
        Pws_l(1:nidx_rtm(2)) =  Pws_lj(1:nidx_rtm(2),j_rlm)
        do kr_nd = kst, ked
!        do nd = nvector+1, nvector+nscalar
!          do k_rlm = 1, nidx_rlm(1)
!            kr_nd = k_rlm + (nd-1) * nidx_rlm(1)
!
          sp1 = 0.0d0
          do l_rtm = 1, nidx_rtm(2)
            sp1 = sp1 + vr_rtm_spin(l_rtm,mp_rlm,kr_nd) * Pws_l(l_rtm)
          end do
          sp_rlm_spin(j_rlm,kr_nd) = sp1
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_scalar_mdin
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_mdin

