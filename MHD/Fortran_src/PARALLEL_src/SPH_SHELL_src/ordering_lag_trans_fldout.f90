!ordering_lag_trans_fldout.f90
!      module ordering_lag_trans_fldout
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine order_b_trans_vector_fldout(ncomp, ist_vect, nfld)
!      subroutine order_b_trans_scalar_fldout(ncomp, ist_scalar, nfld)
!      subroutine order_f_trans_vector_fldout(ncomp, ist_vect, nfld)
!      subroutine order_f_trans_scalar_fldout(ncomp, ist_scalar, nfld)
!
!      subroutine back_f_trans_vector_fldout(ncomp, ist_vect, nfld)
!      subroutine back_f_trans_scalar_fldout(ncomp, ist_scalar, nfld)
!      subroutine back_b_trans_vector_fldout(ncomp, ist_vect, nfld)
!      subroutine back_b_trans_scalar_fldout(ncomp, ist_scalar, nfld)
!
      module ordering_lag_trans_fldout
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_work_4_sph_trans_fldout
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_vector_fldout(ncomp, ist_vect, nfld)
!
      integer(kind = kint), intent(in) :: ncomp, ist_vect, nfld
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: i_rlm_1, k_rtm, i_fld
!
!
!$omp parallel do private(j_rlm,i_fld,i_rlm,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do i_fld = 1, nfld
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = ist_vect + 3*(i_fld-1)                              &
     &             + (j_rlm-1) * ncomp                                  &
     &             + (k_rtm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
            sp_rlm_fdout(i_rlm_1,3*i_fld-2) = sp_rlm(i_rlm+1)
            sp_rlm_fdout(i_rlm_1,3*i_fld-1) = sp_rlm(i_rlm+2)
            sp_rlm_fdout(i_rlm_1,3*i_fld  ) = sp_rlm(i_rlm+3)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_vector_fldout
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_scalar_fldout(ncomp, ist_scalar, nfld)
!
      integer(kind = kint), intent(in) :: ncomp, ist_scalar, nfld
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: i_rlm_1, k_rtm, i_fld
!
!
!$omp parallel do private(j_rlm,i_fld,i_rlm,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do i_fld = 1, nfld
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = ist_scalar + i_fld                                  &
     &             + (j_rlm-1) * ncomp                                  &
     &             + (k_rtm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
            sp_rlm_fdout(i_rlm_1,i_fld) = sp_rlm(i_rlm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_scalar_fldout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_vector_fldout(ncomp, ist_vect, nfld)
!
      integer(kind = kint), intent(in) :: ncomp, ist_vect, nfld
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm, i_rtm_1, i_fld
!
!
!$omp parallel do private(k_rtm,l_rtm,i_fld,i_rtm,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do i_fld = 1, nfld
            do l_rtm = 1, nidx_rtm(2)
              i_rtm =  ist_vect + (3*i_fld-1)                           &
     &               + (l_rtm-1) * ncomp                                &
     &               + (k_rtm-1) * ncomp * nidx_rtm(2)                  &
     &               + (m_rtm-1) * ncomp * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm + (k_rtm-1) * nidx_rtm(2)                 &
     &                        + (m_rtm-1) * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm_fdout(i_rtm_1,3*i_fld-2) = vr_rtm(i_rtm+1)
              vr_rtm_fdout(i_rtm_1,3*i_fld-1) = vr_rtm(i_rtm+2)
              vr_rtm_fdout(i_rtm_1,3*i_fld  ) = vr_rtm(i_rtm+3)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_vector_fldout
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_scalar_fldout(ncomp, ist_scalar, nfld)
!
      integer(kind = kint), intent(in) :: ncomp, ist_scalar, nfld
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm, i_rtm_1, i_fld
!
!
!$omp parallel do private(k_rtm,l_rtm,i_fld,i_rtm,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do i_fld = 1, nfld
            do l_rtm = 1, nidx_rtm(2)
              i_rtm = ist_scalar + i_fld                                &
     &               + (l_rtm-1) * ncomp                                &
     &               + (k_rtm-1) * ncomp * nidx_rtm(2)                  &
     &               + (m_rtm-1) * ncomp * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm + (k_rtm-1) * nidx_rtm(2)                 &
     &                        + (m_rtm-1) * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm_fdout(i_rtm_1,i_fld) = vr_rtm(i_rtm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_scalar_fldout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_vector_fldout(ncomp, ist_vect, nfld)
!
      integer(kind = kint), intent(in) :: ncomp, ist_vect, nfld
!
      integer(kind = kint) :: i_rlm, j_rlm, k_rtm
      integer(kind = kint) :: i_rlm_1, i_fld
!
!
!$omp parallel do private(j_rlm,i_fld,i_rlm,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do i_fld = 1, nfld
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = ist_vect + 3*(i_fld-1)                              &
     &             + (j_rlm-1) * ncomp                                  &
     &             + (k_rtm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
            sp_rlm(i_rlm+1) = sp_rlm_fdout(i_rlm_1,3*i_fld-2)
            sp_rlm(i_rlm+2) = sp_rlm_fdout(i_rlm_1,3*i_fld-1)
            sp_rlm(i_rlm+3) = sp_rlm_fdout(i_rlm_1,3*i_fld  )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_vector_fldout
!
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_scalar_fldout(ncomp, ist_scalar, nfld)
!
      integer(kind = kint), intent(in) :: ncomp, ist_scalar, nfld
!
      integer(kind = kint) :: i_rlm, j_rlm, k_rtm
      integer(kind = kint) :: i_rlm_1, i_fld
!
!
!$omp parallel do private(j_rlm,i_fld,i_rlm,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do i_fld = 1, nfld
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = ist_scalar + i_fld                                  &
     &             + (j_rlm-1) * ncomp                                  &
     &             + (k_rtm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
            sp_rlm(i_rlm  ) = sp_rlm_fdout(i_rlm_1,i_fld)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_scalar_fldout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_vector_fldout(ncomp, ist_vect, nfld)
!
      integer(kind = kint), intent(in) :: ncomp, ist_vect, nfld
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm, i_rtm_1, i_fld
!
!
!$omp parallel do private(k_rtm,l_rtm,i_fld,i_rtm,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do i_fld = 1, nfld
            do l_rtm = 1, nidx_rtm(2)
              i_rtm =  ist_vect + (3*i_fld-1)                           &
     &               + (l_rtm-1) * ncomp                                &
     &               + (k_rtm-1) * ncomp * nidx_rtm(2)                  &
     &               + (m_rtm-1) * ncomp * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm + (k_rtm-1) * nidx_rtm(2)                 &
     &                        + (m_rtm-1) * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm(i_rtm+1)  = vr_rtm_fdout(i_rtm_1,3*i_fld-2)
              vr_rtm(i_rtm+2)  = vr_rtm_fdout(i_rtm_1,3*i_fld-1)
              vr_rtm(i_rtm+3)  = vr_rtm_fdout(i_rtm_1,3*i_fld  )
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_vector_fldout
!
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_scalar_fldout(ncomp, ist_scalar, nfld)
!
      integer(kind = kint), intent(in) :: ncomp, ist_scalar, nfld
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm, i_rtm_1, i_fld
!
!
!$omp parallel do private(k_rtm,l_rtm,i_fld,i_rtm,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do i_fld = 1, nfld
            do l_rtm = 1, nidx_rtm(2)
              i_rtm = ist_scalar + i_fld                                &
     &               + (l_rtm-1) * ncomp                                &
     &               + (k_rtm-1) * ncomp * nidx_rtm(2)                  &
     &               + (m_rtm-1) * ncomp * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm-1 + (k_rtm-1) * nidx_rtm(2)               &
     &                          + (m_rtm-1) * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm(i_rtm)  = vr_rtm_fdout(i_rtm_1,i_fld)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_scalar_fldout
!
! -----------------------------------------------------------------------
!
      end module ordering_lag_trans_fldout

