!set_radial_mat_sph.f90
!      module set_radial_mat_sph
!
!     Written by H. Matsui on Apr, 2009
!
!
!*
!*               | a(2,1)  a(1,2)  ........     0         0     |
!*               | a(3,1)  a(2,2)  ........     .         .     |
!*               |   0     a(3,2)  ........     .         .     |
!*    a(i,j)  =  |   .       0     ........     0         .     |
!*               | ...... a(3,k-1)  a(2,k)  a(1,k+1) .......... |
!*               |   .       .     ........  a(1,N-2)     0     |
!*               |   .       .     ........  a(2,N-2)  a(1,N-1) |
!*               |   0       0     ........  a(3,N-2)  a(2,N-1) |
!
!   Original band matrix
!      band_a(i-j+iband+1,j) = a(i,j)
!      band_a(k,j) = a(k+j-iband-1,j)
!   3-band matrix
!      band_a(i-j+2,j) = a(i,j)
!      band_a(k,j) = a(k+j-2,j)
!
!
!      subroutine set_radial_scalar_evo_mat_sph(nri, jmax, kr_st, kr_ed,&
!     &          coef_imp, coef_d, evo_mat)
!      subroutine set_radial_vect_evo_mat_sph(nri, jmax, kr_st, kr_ed,  &
!     &          coef_imp, coef_d, evo_mat)
!
!      subroutine set_radial_vp_mat_sph(kr_st, kr_ed)
!      subroutine set_radial_press_mat_sph(kr_st, kr_ed)
!
      module set_radial_mat_sph
!
      use m_precision
!
      use m_parallel_var_dof
      use m_constants
      use m_t_int_parameter
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
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
      subroutine set_radial_scalar_evo_mat_sph(nri, jmax, kr_st, kr_ed, &
     &          coef_imp, coef_d, evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      real(kind = kreal), intent(in) :: coef_imp, coef_d
!
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp do private (k,j)
      do k = kr_st, kr_ed
        do j = 1, jmax
          evo_mat(3,k-1,j)                                              &
     &          =     - coef_imp*dt*coef_d * (  d2nod_mat_fdm_2(k,-1)   &
     &                 + two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k,-1) )
          evo_mat(2,k,  j)                                              &
     &          = one + coef_imp*dt*coef_d * ( -d2nod_mat_fdm_2(k, 0)   &
     &                 - two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 0)    &
     &                 + g_sph_rj(j,3)*ar_1d_rj(k,2) )
          evo_mat(1,k+1,j)                                              &
     &          =     - coef_imp*dt*coef_d * (  d2nod_mat_fdm_2(k, 1)   &
     &                 + two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 1) )
        end do
      end do
!$omp end do nowait
!
      end subroutine set_radial_scalar_evo_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_radial_vect_evo_mat_sph(nri, jmax, kr_st, kr_ed,   &
     &          coef_imp, coef_d, evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      real(kind = kreal), intent(in) :: coef_imp, coef_d
!
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp do private (k,j)
      do k = kr_st, kr_ed
        do j = 1, jmax
          evo_mat(3,k-1,j)                                              &
     &          =     - coef_imp*dt*coef_d *    d2nod_mat_fdm_2(k,-1)
          evo_mat(2,k,  j)                                              &
     &          = one + coef_imp*dt*coef_d * ( -d2nod_mat_fdm_2(k, 0)   &
     &                 + g_sph_rj(j,3)*ar_1d_rj(k,2) )
          evo_mat(1,k+1,j)                                              &
     &          =     - coef_imp*dt*coef_d *    d2nod_mat_fdm_2(k, 1)
        end do
      end do
!$omp end do nowait
!
      end subroutine set_radial_vect_evo_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_radial_vp_mat_sph(kr_st, kr_ed)
!
      use m_radial_matrices_sph
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
!
      integer(kind = kint) :: k, j
!
!
!$omp do private (k,j)
      do k = kr_st, kr_ed
        do j = 1, nidx_rj(2)
          vs_poisson_mat(3,k-1,j) = - d2nod_mat_fdm_2(k,-1)
          vs_poisson_mat(2,k,  j) = - d2nod_mat_fdm_2(k, 0)             &
     &                             + g_sph_rj(j,3)*ar_1d_rj(k,2)
          vs_poisson_mat(1,k+1,j) = - d2nod_mat_fdm_2(k, 1)
        end do
      end do
!$omp end do nowait
!
      end subroutine set_radial_vp_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_radial_press_mat_sph(kr_st, kr_ed)
!
      use m_physical_property
      use m_radial_matrices_sph
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
!
      integer(kind = kint) :: k, j
!
!
!$omp do private (k,j)
      do k = kr_st, kr_ed
        do j = 1, nidx_rj(2)
          p_poisson_mat(3,k-1,j) = coef_press * (d2nod_mat_fdm_2(k,-1)  &
     &                    + two*ar_1d_rj(k,1) * d1nod_mat_fdm_2(k,-1))
          p_poisson_mat(2,k,  j) = coef_press * (d2nod_mat_fdm_2(k, 0)  &
     &                    + two*ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 0)   &
     &                    - g_sph_rj(j,3)*ar_1d_rj(k,2) )
          p_poisson_mat(1,k+1,j) = coef_press * (d2nod_mat_fdm_2(k, 1)  &
     &                    + two*ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 1) )
        end do
      end do
!$omp end do nowait
!
      end subroutine set_radial_press_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_vp_evo_mat_sph_by_mat(kr_st, kr_ed)
!
      use mat_product_3band_mul
      use m_radial_matrices_sph
!
      integer(kind = kint), intent(in) :: kr_st, kr_ed
!
!
      call cal_mat_product_3band_mul(nidx_rj(1), nidx_rj(2),            &
     &    kr_st, kr_ed, wt_evo_mat, vs_poisson_mat, vp_evo_mat)
!
      end subroutine set_vp_evo_mat_sph_by_mat
!
! -----------------------------------------------------------------------
!
      end module set_radial_mat_sph
