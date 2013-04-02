!
!      module cal_sph_fdm_coefs
!
!     Written by H. Matsui on Jan., 2010
!
!      subroutine s_cal_fdm_coefs
!
      module cal_sph_fdm_coefs
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_fdm_matrix
      use m_fdm_coefs
!
      implicit none
!
      private :: cal_dr2nod_mat_coefs, cal_dr2nod_e_mat_coefs
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_fdm_coefs
!
      use m_machine_parameter
!
!
      call allocate_fdm_coefs(nidx_rj(1))
!
      call cal_dr2nod_mat_coefs
      call cal_dr2nod_e_mat_coefs
!
     if(iflag_debug .gt. 0) then
       call check_fdm_2_coefs(nidx_rj(1), radius_1d_rj_r(1))
       call check_fdm_2e_coefs(nidx_rj(1), radius_1d_rj_r(1))
     end if
!
!      call deallocate_fdm_matrices
!
      end subroutine s_cal_fdm_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_dr2nod_mat_coefs
!
      integer(kind= kint) :: k
!
!
!$omp parallel do private (k)
      do k = 1, nidx_rj(1)
        d1nod_mat_fdm_2(k,-1) = mat_fdm_2(2,3,k)
        d1nod_mat_fdm_2(k, 0) = mat_fdm_2(2,1,k)
        d1nod_mat_fdm_2(k, 1) = mat_fdm_2(2,2,k)
!
        d2nod_mat_fdm_2(k,-1) = mat_fdm_2(3,3,k)
        d2nod_mat_fdm_2(k, 0) = mat_fdm_2(3,1,k)
        d2nod_mat_fdm_2(k, 1) = mat_fdm_2(3,2,k)
      end do
!$omp end parallel do
!
      end subroutine cal_dr2nod_mat_coefs
!
! -----------------------------------------------------------------------
!
      subroutine cal_dr2nod_e_mat_coefs
!
      integer(kind= kint) :: k
!
!
!$omp parallel do private (k)
      do k = 1, nidx_rj(1)
        d_nod_mat_fdm_2e(k,0) = mat_fdm_2e(1,2,k)
        d_nod_mat_fdm_2e(k,1) = mat_fdm_2e(1,1,k)
!
        d1nod_mat_fdm_2e(k,0) = mat_fdm_2e(2,2,k)
        d1nod_mat_fdm_2e(k,1) = mat_fdm_2e(2,1,k)
      end do
!$omp end parallel do
!
      end subroutine cal_dr2nod_e_mat_coefs
!
! -----------------------------------------------------------------------
!
      end module cal_sph_fdm_coefs
