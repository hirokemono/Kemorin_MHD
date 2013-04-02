!set_sph_exp_nod_center.f90
!      module set_sph_exp_nod_center
!
!     Written by H. Matsui on Jan., 2010
!
!      subroutine cal_sph_nod_center_b_and_j(is_fld, is_rot)
!      subroutine cal_dsdr_sph_center_2(is_fld)
!      subroutine cal_sph_nod_center_rot2(is_fld, is_rot)
!      subroutine cal_sph_nod_center_diffuse2(is_fld, is_diffuse)
!
      module set_sph_exp_nod_center
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_sph_spectr_data
      use m_fdm_coefs
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_center_b_and_j(is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      real(kind = kreal) :: d1s_dr1, d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_p1
!
!
!$omp parallel do private(inod,i_p1,d1s_dr1,d2s_dr2,d1t_dr1)
      do j = 1, nidx_rj(2)
        inod = j
        i_p1 = inod + nidx_rj(2)
!
        d1s_dr1 =  d1nod_mat_fdm_2(ione,0) * d_rj(inod,is_fld)          &
     &           + d1nod_mat_fdm_2(ione,1) * d_rj(i_p1,is_fld)
        d2s_dr2 =  d2nod_mat_fdm_2(ione,0) * d_rj(inod,is_fld  )        &
     &           + d2nod_mat_fdm_2(ione,1) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(ione,0) * d_rj(inod,is_fld+2)        &
     &           + d1nod_mat_fdm_2(ione,1) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_fld+1) = d1s_dr1
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                     *ar_1d_rj(ione,2)*d_rj(inod,is_fld) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_center_b_and_j
!
! -----------------------------------------------------------------------
!
      subroutine cal_dsdr_sph_center_2(is_fld)
!
      integer(kind = kint), intent(in) :: is_fld
!
      real(kind = kreal) :: d1s_dr1
      integer(kind = kint) :: inod, i_p1
!
!
!$omp parallel do private(inod,i_p1,d1s_dr1)
      do inod = 1, nidx_rj(2)
        i_p1 = inod + nidx_rj(2)
        d1s_dr1 =  d1nod_mat_fdm_2(ione,0) * d_rj(inod,is_fld)          &
     &           + d1nod_mat_fdm_2(ione,1) * d_rj(i_p1,is_fld)
!
        d_rj(inod,is_fld+1) = d1s_dr1
      end do
!$omp end parallel do
!
      end subroutine cal_dsdr_sph_center_2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_center_rot2(is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      real(kind = kreal) :: d2s_dr2, d1t_dr1
      integer(kind = kint) :: j, inod, i_p1
!
!
!$omp parallel do private(inod,i_p1,d2s_dr2,d1t_dr1)
      do j = 1, nidx_rj(2)
        inod = j
        i_p1 = inod + nidx_rj(2)
!
        d2s_dr2 =  d2nod_mat_fdm_2(ione, 0) * d_rj(inod,is_fld  )       &
     &           + d2nod_mat_fdm_2(ione, 1) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(ione, 0) * d_rj(inod,is_fld+2)       &
     &           + d1nod_mat_fdm_2(ione, 1) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_rot  ) = d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) = d1t_dr1
        d_rj(inod,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                     *ar_1d_rj(ione,2)*d_rj(inod,is_fld) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_center_rot2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_center_diffuse2(is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_diffuse
!
      real(kind = kreal) :: d2s_dr2, d2t_dr2
      integer(kind = kint) :: j, inod, i_p1
!
!
!$omp parallel do private(inod,i_p1,d2s_dr2,d2t_dr2)
      do j = 1, nidx_rj(2)
        inod = j
        i_p1 = inod + nidx_rj(2)
!
        d2s_dr2 =  d2nod_mat_fdm_2(ione, 0) * d_rj(inod,is_fld  )       &
     &           + d2nod_mat_fdm_2(ione, 1) * d_rj(i_p1,is_fld  )
        d2t_dr2 =  d2nod_mat_fdm_2(ione, 0) * d_rj(inod,is_fld+2)       &
     &           + d2nod_mat_fdm_2(ione, 1) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_diffuse  ) = d2s_dr2                               &
     &         - g_sph_rj(j,3)*ar_1d_rj(ione,2)*d_rj(inod,is_fld  )
        d_rj(inod,is_diffuse+2) = d2t_dr2                               &
     &         - g_sph_rj(j,3)*ar_1d_rj(ione,2)*d_rj(inod,is_fld+2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_center_diffuse2
!
! -----------------------------------------------------------------------
!
      end module set_sph_exp_nod_center
