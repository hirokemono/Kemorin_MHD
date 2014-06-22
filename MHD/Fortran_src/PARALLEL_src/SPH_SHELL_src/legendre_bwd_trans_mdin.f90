!>@file   legendre_bwd_trans_mdin.f90
!!@brief  module legendre_bwd_trans_mdin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (innermost loop is meridional grid)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_spin(ncomp, nvector,         &
!!     &          sp_rlm_spin, vr_rtm_spin)
!!      subroutine legendre_b_trans_scalar_spin(ncomp, nscalar, nvector,&
!!     &          sp_rlm_spin, vr_rtm_spin)
!!        Input:  vr_rtm_spin
!!        Output: sp_rlm_spin
!!@endverbatim
!!
!!@n @param  ncomp    number of components to be transformed
!!@n @param  nvector  number of vector to be transformed
!!@n @param  nscalar  number of fields to be transformed
!
      module legendre_bwd_trans_spin
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
      subroutine legendre_b_trans_vector_spin(ncomp, nvector,           &
     &          sp_rlm_spin, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout)                                 &
     &    :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1)*ncomp)
      real(kind = kreal), intent(inout)                                 &
     &    :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(3),nidx_rtm(1)*ncomp)
!
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, l_rtm
      integer(kind = kint) :: nb_nri, kr_nd, k_rlm
      real(kind = kreal) :: a2r_1d_rlm_r
      real(kind = kreal) :: sp1, sp2, sp3
      real(kind = kreal) :: Pg3_l(nidx_rtm(2))
      real(kind = kreal) :: dPdt_l(nidx_rtm(2))
      real(kind = kreal) :: Pgv_l(nidx_rtm(2))
!
!
      nb_nri = nvector*nidx_rtm(1)
!$omp parallel private(kr_nd,k_rlm,a2r_1d_rlm_r)
      do kr_nd = 1, nb_nri
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
!        nd =  1 + (kr_nd - k_rlm) / nidx_rlm(1)
!$omp do private(j_rlm)
        do j_rlm = 1, nidx_rlm(2)
          sp_rlm_spin(j_rlm,kr_nd         )                             &
     &        = sp_rlm_spin(j_rlm,kr_nd         ) * a2r_1d_rlm_r
          sp_rlm_spin(j_rlm,kr_nd+nb_nri  )                             &
     &        = sp_rlm_spin(j_rlm,kr_nd+nb_nri  ) * a_r_1d_rlm_r(k_rlm)
          sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)                             &
     &        = sp_rlm_spin(j_rlm,kr_nd+2*nb_nri) * a_r_1d_rlm_r(k_rlm)
        end do
!$omp end do
      end do
!$omp end parallel
!
!$omp parallel do private(j_rlm,kr_nd,l_rtm,mp_rlm,                     &
!$omp&                    sp1,sp2,sp3,Pg3_l,dPdt_l)
      do j_rlm = 1, nidx_rlm(2)
        mp_rlm = mdx_p_rlm_rtm(j_rlm)
!
        Pg3_l(1:nidx_rtm(2)) =  Pg3_lj(1:nidx_rtm(2),j_rlm)
        dPdt_l(1:nidx_rtm(2)) = dPdt_rtm(1:nidx_rtm(2),j_rlm)
        do kr_nd = 1, nb_nri
          sp1 = sp_rlm_spin(j_rlm,kr_nd         )
          sp2 = sp_rlm_spin(j_rlm,kr_nd+nb_nri  )
          sp3 = sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)
          do l_rtm = 1, nidx_rtm(2)
            vr_rtm_spin(l_rtm,mp_rlm,kr_nd         )                    &
     &          = vr_rtm_spin(l_rtm,mp_rlm,kr_nd         )              &
     &         + sp1*Pg3_l(l_rtm)
!
            vr_rtm_spin(l_rtm,mp_rlm,kr_nd+nb_nri  )                    &
     &          = vr_rtm_spin(l_rtm,mp_rlm,kr_nd+nb_nri  )              &
     &         + sp2*dPdt_l(l_rtm)
!
            vr_rtm_spin(l_rtm,mp_rlm,kr_nd+2*nb_nri)                    &
     &          = vr_rtm_spin(l_rtm,mp_rlm,kr_nd+2*nb_nri)              &
     &         - sp3*dPdt_l(l_rtm)
          end do
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(j_rlm,kr_nd,l_rtm,mn_rlm,sp2,sp3,Pgv_l)
      do j_rlm = 1, nidx_rlm(2)
        do kr_nd = 1, nb_nri
          mn_rlm = mdx_n_rlm_rtm(j_rlm)
          Pgv_l(1:nidx_rtm(2)) =  Pgv_lj(1:nidx_rtm(2),j_rlm)
          sp2 = sp_rlm_spin(j_rlm,kr_nd+nb_nri  )
          sp3 = sp_rlm_spin(j_rlm,kr_nd+2*nb_nri)
          do l_rtm = 1, nidx_rtm(2)
            vr_rtm_spin(l_rtm,mn_rlm,kr_nd+nb_nri  )                    &
     &           = vr_rtm_spin(l_rtm,mn_rlm,kr_nd+nb_nri  )             &
     &            + sp3*Pgv_l(l_rtm)
!
            vr_rtm_spin(l_rtm,mn_rlm,kr_nd+2*nb_nri)                    &
     &           = vr_rtm_spin(l_rtm,mn_rlm,kr_nd+2*nb_nri)             &
     &            + sp2*Pgv_l(l_rtm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_spin(ncomp, nscalar, nvector,  &
     &          sp_rlm_spin, vr_rtm_spin)
!
      integer(kind = kint), intent(in) :: ncomp, nscalar, nvector
      real(kind = kreal), intent(in)                                    &
     &      :: sp_rlm_spin(nidx_rlm(2),nidx_rlm(1)*ncomp)
      real(kind = kreal), intent(inout)                                 &
     &      :: vr_rtm_spin(nidx_rtm(2),nidx_rtm(3),nidx_rtm(1)*ncomp)
!
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm
      integer(kind = kint) :: kr_nd, kst, ked
      real(kind = kreal) :: sp1
      real(kind = kreal) :: P_l(nidx_rtm(2))
!
!
      kst = 1 + 3*nvector * nidx_rtm(1)
      ked = (nscalar + 3*nvector) * nidx_rtm(1)
!$omp parallel do private(j_rlm,kr_nd,l_rtm,mp_rlm,sp1,P_l)
      do j_rlm = 1, nidx_rlm(2)
        mp_rlm = mdx_p_rlm_rtm(j_rlm)
!
        P_l(1:nidx_rtm(2)) =  P_rtm(1:nidx_rtm(2),j_rlm)
        do kr_nd = kst, ked
!        do nd = nvector+1, nvector+nscalar
!          do k_rtm = 1,  nidx_rtm(1)
!            kr_nd = k_rlm + (nd-1) * nidx_rlm(1)
!
          sp1 = sp_rlm_spin(j_rlm,kr_nd)
          do l_rtm = 1, nidx_rtm(2)
            vr_rtm_spin(l_rtm,mp_rlm,kr_nd)                             &
     &              = vr_rtm_spin(l_rtm,mp_rlm,kr_nd) + sp1*P_l(l_rtm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_mdin
