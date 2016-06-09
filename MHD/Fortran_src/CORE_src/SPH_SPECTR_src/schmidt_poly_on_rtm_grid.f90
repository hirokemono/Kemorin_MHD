!>@file   schmidt_poly_on_rtm_grid.f90
!!@brief  module schmidt_poly_on_rtm_grid
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!
!>@brief  Copy Legendre polynomials for spherical transform
!!
!!
!!@verbatim
!!      subroutine s_cal_schmidt_poly_rtm                               &
!!     &         (l_truncation, sph_rj, sph_rtm, sph_rlm, leg)
!!        integer(kind = kint), intent(in) :: l_truncation
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(legendre_4_sph_trans), intent(inout) :: leg
!!@endverbatim
!
      module schmidt_poly_on_rtm_grid
!
      use m_precision
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_spheric_rj_data
!
      implicit none
!
!      private :: set_gauss_points_rtm, set_lagender_4_rlm
!      private :: set_lagender_pole_rlm
!      private :: copy_sph_normalization_2_rlm
!      private :: copy_sph_normalization_2_rj
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_schmidt_poly_rtm                                 &
     &         (l_truncation, sph_rj, sph_rtm, sph_rlm, leg)
!
      use t_schmidt_poly_on_rtm
      use m_gauss_points
      use set_legendre_matrices
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
!
      type(legendre_4_sph_trans), intent(inout) :: leg
!
!
      call allocate_gauss_points(sph_rtm%nidx_rtm(2))
      call allocate_gauss_colatitude
      call alloc_gauss_colat_rtm(sph_rtm%nidx_rtm(2), leg)
!
      call set_gauss_points_rtm(leg)
!
      call deallocate_gauss_colatitude
      call deallocate_gauss_points
!
!     set Legendre polynomials
!
!
      call alloc_schmidt_normalize                                      &
     &   (sph_rlm%nidx_rlm(2), sph_rj%nidx_rj(2), leg)
      call copy_sph_normalization_2_rlm(sph_rlm, leg%g_sph_rlm)
      call copy_sph_normalization_2_rj(sph_rj, leg%g_sph_rj)
!
      call alloc_schmidt_poly_rtm                                       &
     &   (sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2), leg)
      call set_lagender_4_rlm(l_truncation, sph_rtm, sph_rlm,           &
     &    leg%g_colat_rtm, leg%P_rtm, leg%dPdt_rtm)
!
      call alloc_trans_schmidt_rtm                                      &
     &   (sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2), leg)
      call set_trans_legendre_rtm                                       &
     &   (sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2),                     &
     &    leg%P_rtm, leg%dPdt_rtm, leg%P_jl, leg%dPdt_jl)
!
      call alloc_schmidt_p_rtm_pole(sph_rlm%nidx_rlm(2), leg)
      call set_lagender_pole_rlm(l_truncation, sph_rtm, sph_rlm,        &
     &    leg%P_pole_rtm, leg%dPdt_pole_rtm)
!
      end subroutine s_cal_schmidt_poly_rtm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_gauss_points_rtm(leg)
!
      use t_schmidt_poly_on_rtm
      use m_gauss_points
!
      type(legendre_4_sph_trans), intent(inout) :: leg
!
!     set gauss colatitudes
!
      call construct_gauss_coefs
      call set_gauss_colatitude
!
!$omp parallel workshare
      leg%g_point_rtm(1:n_point) = w_point(1:n_point)
      leg%g_colat_rtm(1:n_point) = w_colat(1:n_point)
      leg%weight_rtm(1:n_point) =  w_coefs(1:n_point)
!$omp end parallel workshare
!
      end subroutine set_gauss_points_rtm
!
! -----------------------------------------------------------------------
!
      subroutine copy_sph_normalization_2_rlm(sph_rlm, g_sph_rlm)
!
      use m_constants
      use spherical_harmonics
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      real(kind = kreal), intent(inout)                                 &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
!
      integer(kind = kint) :: j, ll, mm
      integer(kind = kint) :: idx_lm(2)
      real(kind = kreal) :: g_lm(17)
!
!$omp parallel do private(j,ll,mm,idx_lm,g_lm)
      do j = 1, sph_rlm%nidx_rlm(2)
        ll = sph_rlm%idx_gl_1d_rlm_j(j,2)
        mm = sph_rlm%idx_gl_1d_rlm_j(j,3)
!
        call sph_normalizations(ll, mm, idx_lm, g_lm)
        g_sph_rlm(j,1:17) =  g_lm(1:17)
!
        if(ll.eq.0 .and. mm.eq.0) g_sph_rlm(j,3) = half
      end do
!$omp end parallel do 
!
      end subroutine copy_sph_normalization_2_rlm
!
! -----------------------------------------------------------------------
!
      subroutine copy_sph_normalization_2_rj(sph_rj, g_sph_rj)
!
      use spherical_harmonics
!
      type(sph_rj_grid), intent(in) :: sph_rj
      real(kind = kreal), intent(inout)                                 &
     &           :: g_sph_rj(sph_rj%nidx_rj(2),17)
!
      integer(kind = kint) :: j, ll, mm
      integer(kind = kint) :: idx_lm(2)
      real(kind = kreal) :: g_lm(17)
!
!
!$omp parallel do private(j,ll,mm,idx_lm,g_lm)
      do j = 1, sph_rj%nidx_rj(2)
        ll = sph_rj%idx_gl_1d_rj_j(j,2)
        mm = sph_rj%idx_gl_1d_rj_j(j,3)
!
        call sph_normalizations(ll, mm, idx_lm, g_lm)
        g_sph_rj(j,1:13) =  g_lm(1:13)
      end do
!$omp end parallel do 
!
      end subroutine copy_sph_normalization_2_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_lagender_4_rlm(l_truncation, sph_rtm, sph_rlm,     &
     &          g_colat_rtm, P_rtm, dPdt_rtm)
!
      use m_machine_parameter
      use m_work_4_sph_trans
      use schmidt_fix_m
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
!
      integer(kind = kint), intent(in) :: l_truncation
      real(kind= kreal), intent(in) :: g_colat_rtm(sph_rtm%nidx_rtm(2))
!
      real(kind= kreal), intent(inout)                                  &
     &           :: P_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
      real(kind= kreal), intent(inout)                                  &
     &           :: dPdt_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
!
      integer(kind = kint) :: ip, i, j, l, m, mm, jj
      integer(kind = kint) :: jst, jed, lst, led
      real(kind = kreal) :: p_m(0:l_truncation), dp_m(0:l_truncation)
      real(kind = kreal) :: pmp1(0:l_truncation), pmn1(0:l_truncation)
      real(kind = kreal) :: df_m(0:l_truncation+2)
!
!
!$omp parallel do                                                       &
!$omp& private(i,j,l,m,mm,jj,jst,jed,lst,led,p_m,dp_m,pmn1,pmp1,df_m)
      do ip = 1, np_smp
        lst = sph_rtm%istack_rtm_lt_smp(ip-1) + 1
        led = sph_rtm%istack_rtm_lt_smp(ip  )
        do i = lst, led
!
          do m = 1, sph_rtm%nidx_rtm(3)
            mm = abs(sph_rtm%idx_gl_1d_rtm_m(m,2))
            jst = lstack_rlm(m-1) + 1
            jed = lstack_rlm(m)
!
            call schmidt_legendres_m(l_truncation, mm, g_colat_rtm(i),  &
     &          p_m, dp_m, pmn1, pmp1, df_m)
!
            do j = jst, jed
              jj = sph_rlm%idx_gl_1d_rlm_j(j,1)
              l =  sph_rlm%idx_gl_1d_rlm_j(j,2)
              P_rtm(i,j) =    p_m(l)
              dPdt_rtm(i,j) = dp_m(l)
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine set_lagender_4_rlm
!
! -----------------------------------------------------------------------
!
      subroutine set_lagender_pole_rlm(l_truncation, sph_rtm, sph_rlm,  &
     &          P_pole_rtm, dPdt_pole_rtm)
!
      use m_constants
      use m_work_4_sph_trans
!
      use schmidt_fix_m
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: P_pole_rtm(2,sph_rlm%nidx_rlm(2))
      real(kind = kreal), intent(inout)                                 &
     &                   :: dPdt_pole_rtm(2,sph_rlm%nidx_rlm(2))
!
      integer(kind = kint) :: j, l, m, mm, jj, jst, jed
      real(kind = kreal) :: pi
      real(kind = kreal) :: p_m(0:l_truncation), dp_m(0:l_truncation)
      real(kind = kreal) :: pmp1(0:l_truncation), pmn1(0:l_truncation)
      real(kind = kreal) :: df_m(0:l_truncation+2)
!
!
      do m = 1, sph_rtm%nidx_rtm(3)
        mm = abs(sph_rtm%idx_gl_1d_rtm_m(m,2))
        jst = lstack_rlm(m-1) + 1
        jed = lstack_rlm(m)
!
        if(mm .le. 1) then
          call schmidt_legendres_m(l_truncation, mm, zero,              &
     &          p_m, dp_m, pmn1, pmp1, df_m)
        else
          p_m(0:l_truncation) = 0.0d0
          dp_m(0:l_truncation) = 0.0d0
        end if
!
        do j = jst, jed
          jj = sph_rlm%idx_gl_1d_rlm_j(j,1)
          l =  sph_rlm%idx_gl_1d_rlm_j(j,2)
          P_pole_rtm(1,j) =    p_m(l)
          dPdt_pole_rtm(1,j) = dp_m(l)
        end do
      end do
!
      pi = four * atan(one)
      do m = 1, sph_rtm%nidx_rtm(3)
        mm = abs(sph_rtm%idx_gl_1d_rtm_m(m,2))
        jst = lstack_rlm(m-1) + 1
        jed = lstack_rlm(m)
!
        if(mm .le. 1) then
          call schmidt_legendres_m(l_truncation, mm, pi,               &
     &          p_m, dp_m, pmn1, pmp1, df_m)
        else
          p_m(0:l_truncation) = 0.0d0
          dp_m(0:l_truncation) = 0.0d0
        end if
!
        do j = jst, jed
          jj = sph_rlm%idx_gl_1d_rlm_j(j,1)
          l =  sph_rlm%idx_gl_1d_rlm_j(j,2)
          P_pole_rtm(2,j) =    p_m(l)
          dPdt_pole_rtm(2,j) = dp_m(l)
        end do
      end do
!
      end subroutine set_lagender_pole_rlm
!
! -----------------------------------------------------------------------
!
      end module schmidt_poly_on_rtm_grid
