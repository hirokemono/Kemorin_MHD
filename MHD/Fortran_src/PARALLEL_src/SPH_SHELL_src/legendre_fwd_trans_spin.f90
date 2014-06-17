!>@file   legendre_fwd_trans_spin.f90
!!@brief  module legendre_fwd_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_spin(ncomp, nvector,         &
!!     &          vr_rtm_spin, sp_rlm_spin)
!!      subroutine legendre_f_trans_scalar_spin(ncomp, nscalar, nvector,&
!!     &          vr_rtm_spin, sp_rlm_spin)
!!        Input:  vr_rtm_spin
!!        Output: sp_rlm_spin
!!@endverbatim
!!
!!@n @param  ncomp    number of components to be transformed
!!@n @param  nvector  number of vector to be transformed
!!@n @param  nscalar  number of fields to be transformed
!
      module legendre_fwd_trans_spin
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
      subroutine legendre_f_trans_vector_spin(ncomp, nvector,           &
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
!
!
      nb_nri = nvector*nidx_rlm(1)
!$omp parallel private(kr_nd,k_rlm)
      do kr_nd = 1, nb_nri
!      do nd = 1, nvector
!        do k_rlm = 1, nidx_rlm(1)
!          kr_nd = k_rlm + (nd-1) * nidx_rlm(1)
!
!$omp do private(j_rlm,l_rtm,mp_rlm,mn_rlm)
        do j_rlm = 1, nidx_rlm(2)
          mp_rlm = mdx_p_rlm_rtm(j_rlm)
          mn_rlm = mdx_n_rlm_rtm(j_rlm)
          do l_rtm = 1, nidx_rtm(2)
            sp_rlm_spin(j_rlm,kr_nd         )                           &
     &         = sp_rlm_spin(j_rlm,kr_nd           )                    &
     &           + vr_rtm_spin(l_rtm,mp_rlm,kr_nd         )             &
     &            * Pvw_lj(l_rtm,j_rlm)
!
            sp_rlm_spin(j_rlm,kr_nd+nb_nri  )                           &
     &         = sp_rlm_spin(j_rlm,kr_nd+nb_nri  )                      &
     &         + ( vr_rtm_spin(l_rtm,mp_rlm,kr_nd+nb_nri  )             &
     &          * dPvw_lj(l_rtm,j_rlm)                                  &
     &           - vr_rtm_spin(l_rtm,mn_rlm,kr_nd+2*nb_nri)             &
     &          * Pgvw_lj(l_rtm,j_rlm))
!
            sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)                           &
     &         = sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)                      &
     &         - ( vr_rtm_spin(l_rtm,mn_rlm,kr_nd+nb_nri  )             &
     &          * Pgvw_lj(l_rtm,j_rlm)                                  &
     &           + vr_rtm_spin(l_rtm,mp_rlm,kr_nd+2*nb_nri)             &
     &          * dPvw_lj(l_rtm,j_rlm))
          end do
        end do
!$omp end do nowait
!
!
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
      end subroutine legendre_f_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_spin(ncomp, nscalar, nvector,  &
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
!
!
      kst = 1 + 3*nvector * nidx_rtm(1)
      ked = (nscalar + 3*nvector) * nidx_rtm(1)
!$omp parallel private(kr_nd)
      do kr_nd = kst, ked
!      do nd = nvector+1, nvector+nscalar
!        do k_rlm = 1, nidx_rlm(1)
!          kr_nd = k_rlm + (nd-1) * nidx_rlm(1)
!
!$omp do private(j_rlm,l_rtm,mp_rlm)
        do j_rlm = 1, nidx_rlm(2)
          mp_rlm = mdx_p_rlm_rtm(j_rlm)
          do l_rtm = 1, nidx_rtm(2)
            sp_rlm_spin(j_rlm,kr_nd) = sp_rlm_spin(j_rlm,kr_nd)         &
     &         + vr_rtm_spin(l_rtm,mp_rlm,kr_nd) * Pws_lj(l_rtm,j_rlm)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine legendre_f_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_spin

