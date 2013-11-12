!>@file   set_sph_exp_free_ICB_s4t2.f90
!!@brief  module set_sph_exp_free_ICB_s4t2
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with free-slip boundary at ICB
!!        using 4-th order FDM for poloidal velocity
!!
!!@verbatim
!!      subroutine cal_sph_icb_free_v_and_w_s4t2(is_fld, is_rot)
!!      subroutine cal_sph_icb_free_rot_s4t2(is_fld, is_rot)
!!      subroutine cal_sph_icb_free_diffuse_s4t2(coef_d,                &
!!     &          is_fld, is_diffuse)
!!@endverbatim
!!
!!@n @param coef_d     Coefficient for diffusion term
!!@n @param is_fld     Address of poloidal velocity in d_rj
!!@n @param is_rot     Address of poloidal vorticity in d_rj
!!@n @param is_diffuse Address of poloidal viscousity in d_rj
!
      module set_sph_exp_free_ICB_s4t2
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_sph_spectr_data
      use m_boundary_params_sph_MHD
      use m_fdm_coefs
      use m_coef_fdm_free_ICB
      use m_vp_coef_fdm4_free_ICB
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_icb_free_v_and_w_s4t2(is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      integer(kind = kint) :: kr_in
      integer(kind = kint) :: inod, j, i_p1, i_p2, i_p3
      real(kind = kreal) :: d1s_dr1, d2s_dr2, d1t_dr1
!
!
      kr_in= nlayer_ICB
!$omp parallel do private(inod,i_p1,i_p2,i_p3,j,d1s_dr1,d2s_dr2,d1t_dr1)
      do j = 1, nidx_rj(2)
        inod = j + (kr_in-1) * nidx_rj(2)
        i_p1 = inod + nidx_rj(2)
        i_p2 = i_p1 + nidx_rj(2)
        i_p3 = i_p2 + nidx_rj(2)
!
!
        d1s_dr1 =  coef_fdm_free_ICB_vp4( 2,2) * d_rj(i_p2,is_fld  )    &
     &           + coef_fdm_free_ICB_vp4( 1,2) * d_rj(i_p1,is_fld  )
        d2s_dr2 =  coef_fdm_free_ICB_vp4( 2,3) * d_rj(i_p2,is_fld  )    &
     &           + coef_fdm_free_ICB_vp4( 1,3) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  coef_fdm_free_ICB_vt2( 0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_fld  ) =  zero
        d_rj(inod,is_fld+1) =  d1s_dr1
        d_rj(inod,is_rot  ) =  d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =  d1t_dr1
        d_rj(inod,is_rot+2) = -d2s_dr2
!
!
        d1s_dr1 =  coef_fdm_free_ICB1_vp4( 2,2) * d_rj(i_p3,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4( 1,2) * d_rj(i_p2,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4( 0,2) * d_rj(i_p1,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4(-1,2) * d_rj(inod,is_fld  )
        d2s_dr2 =  coef_fdm_free_ICB1_vp4( 2,3) * d_rj(i_p3,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4( 1,3) * d_rj(i_p2,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4( 0,3) * d_rj(i_p1,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4(-1,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(-1,kr_in+1) * d_rj(inod,is_fld+2)    &
     &           + d1nod_mat_fdm_2( 0,kr_in+1) * d_rj(i_p1,is_fld+2)    &
     &           + d1nod_mat_fdm_2( 1,kr_in+1) * d_rj(i_p2,is_fld+2)
!
        d_rj(i_p1,is_fld+1) =  d1s_dr1
        d_rj(i_p1,is_rot  ) =  d_rj(i_p1,is_fld+2)
        d_rj(i_p1,is_rot+1) =  d1t_dr1
        d_rj(i_p1,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                  * ar_1d_rj(kr_in,2)*d_rj(i_p1,is_fld  ) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_icb_free_v_and_w_s4t2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_icb_free_rot_s4t2(is_fld, is_rot)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_rot
!
      integer(kind = kint) :: kr_in
      integer(kind = kint) :: inod, j, i_p1, i_p2, i_p3
      real(kind = kreal) :: d2s_dr2, d1t_dr1
!
!
      kr_in= nlayer_ICB
!$omp parallel do private(inod,i_p1,i_p2,i_p3,j,d2s_dr2,d1t_dr1)
      do j = 1, nidx_rj(2)
        inod = j + (kr_in-1) * nidx_rj(2)
        i_p1 = inod + nidx_rj(2)
        i_p2 = i_p1 + nidx_rj(2)
        i_p3 = i_p2 + nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_free_ICB_vp4( 2,3) * d_rj(i_p2,is_fld  )    &
     &           + coef_fdm_free_ICB_vp4( 1,3) * d_rj(i_p1,is_fld  )
        d1t_dr1 =  coef_fdm_free_ICB_vt2( 0,2) * d_rj(inod,is_fld+2)
!
        d_rj(inod,is_rot  ) =  d_rj(inod,is_fld+2)
        d_rj(inod,is_rot+1) =  d1t_dr1
        d_rj(inod,is_rot+2) = -d2s_dr2
!
!
        d2s_dr2 =  coef_fdm_free_ICB1_vp4( 2,3) * d_rj(i_p3,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4( 1,3) * d_rj(i_p2,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4( 0,3) * d_rj(i_p1,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4(-1,3) * d_rj(inod,is_fld  )
        d1t_dr1 =  d1nod_mat_fdm_2(-1,kr_in+1) * d_rj(inod,is_fld+2)    &
     &           + d1nod_mat_fdm_2( 0,kr_in+1) * d_rj(i_p1,is_fld+2)    &
     &           + d1nod_mat_fdm_2( 1,kr_in+1) * d_rj(i_p2,is_fld+2)
!
        d_rj(i_p1,is_rot  ) =  d_rj(i_p1,is_fld+2)
        d_rj(i_p1,is_rot+1) =  d1t_dr1
        d_rj(i_p1,is_rot+2) = - ( d2s_dr2 - g_sph_rj(j,3)               &
     &                  * ar_1d_rj(kr_in,2)*d_rj(i_p1,is_fld  ) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_icb_free_rot_s4t2
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_icb_free_diffuse_s4t2(coef_d,                  &
     &          is_fld, is_diffuse)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: is_diffuse
      real(kind = kreal), intent(in) :: coef_d
!
      integer(kind = kint) :: kr_in
      integer(kind = kint) :: inod, j, i_p1, i_p2, i_p3
      real(kind = kreal) :: d2s_dr2, d2t_dr2
!
!
      kr_in= nlayer_ICB
!$omp parallel do private(inod,i_p1,i_p2,i_p3,j,d2s_dr2,d2t_dr2)
      do j = 1, nidx_rj(2)
        inod = j + (kr_in-1) * nidx_rj(2)
        i_p1 = inod + nidx_rj(2)
        i_p2 = i_p1 + nidx_rj(2)
        i_p3 = i_p2 + nidx_rj(2)
!
        d2s_dr2 =  coef_fdm_free_ICB_vp4( 2,3) * d_rj(i_p2,is_fld  )    &
     &           + coef_fdm_free_ICB_vp4( 1,3) * d_rj(i_p1,is_fld  )
        d2t_dr2 =  coef_fdm_free_ICB_vt2( 0,3) * d_rj(inod,is_fld+2)    &
     &           + coef_fdm_free_ICB_vt2( 1,3) * d_rj(i_p1,is_fld+2)
!
        d_rj(inod,is_diffuse  ) =  coef_d * (d2s_dr2                    &
     &    - g_sph_rj(j,3)*ar_1d_rj(kr_in,2)*d_rj(inod,is_fld  ) )
        d_rj(inod,is_diffuse+2) =  coef_d * (d2t_dr2                    &
     &    - g_sph_rj(j,3)*ar_1d_rj(kr_in,2)*d_rj(inod,is_fld+2) )
!
        d2s_dr2 =  coef_fdm_free_ICB1_vp4( 2,3) * d_rj(i_p3,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4( 1,3) * d_rj(i_p2,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4( 0,3) * d_rj(i_p1,is_fld  )   &
     &           + coef_fdm_free_ICB1_vp4(-1,3) * d_rj(inod,is_fld  )
        d2t_dr2 =  d2nod_mat_fdm_2(-1,kr_in+1) * d_rj(inod,is_fld+2)    &
     &           + d2nod_mat_fdm_2( 0,kr_in+1) * d_rj(i_p1,is_fld+2)    &
     &           + d2nod_mat_fdm_2( 1,kr_in+1) * d_rj(i_p2,is_fld+2)
!
        d_rj(i_p1,is_diffuse  ) =  coef_d * (d2s_dr2                    &
     &    - g_sph_rj(j,3)*ar_1d_rj(kr_in,2)*d_rj(i_p1,is_fld  ) )
        d_rj(i_p1,is_diffuse+2) =  coef_d * (d2t_dr2                    &
     &    - g_sph_rj(j,3)*ar_1d_rj(kr_in,2)*d_rj(i_p1,is_fld+2) )
      end do
!$omp end parallel do
!
      end subroutine cal_sph_icb_free_diffuse_s4t2
!
! -----------------------------------------------------------------------
!
      end module set_sph_exp_free_ICB_s4t2
