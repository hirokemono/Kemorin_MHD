!>@file   cal_vpol_press_sph_fdm_mat.f90
!!@brief  module cal_vpol_press_sph_fdm_mat
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_bc_sph_mhd(bc_IO, sph_params, sph_rj,          &
!!     &          radial_rj_grp, MHD_prop, MHD_BC, sph_MHD_bc)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(group_data), intent(in) :: radial_rj_grp
!!        real(kind = kreal), intent(in) :: h_rho(nri)
!!        type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!!@endverbatim
!
      module cal_vpol_press_sph_fdm_mat
!
      use m_precision
      use m_constants
      use t_spheric_rj_data
      use t_fdm_coefs
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_unit_mat_vsp_evo(sph_rj, kr_in, kr_out, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private (k,j)
      do j = 1, sph_rj%nidx_rj(2)
        do k = 1, kr_in-1
          mat7(4,2*k-1,j) = one
          mat7(4,2*k,  j) = one
        end do
        mat7(4,2*kr_in-1,j) = one
        mat7(4,2*kr_in,  j) = zero
        do k = kr_in+1, kr_out
          mat7(4,2*k-1,j) = zero
          mat7(4,2*k,  j) = zero
        end do
        do k = kr_out+1, sph_rj%nidx_rj(1)
          mat7(4,2*k-1,j) = one
          mat7(4,2*k,  j) = one
        end do
      end do
!$omp end parallel do
!
      end subroutine set_unit_mat_vsp_evo
!
! -----------------------------------------------------------------------
!
      subroutine cal_vpol_press_sph_mat                                 &
     &         (sph_rj, g_sph_rj,             &
     &          kr_in, kr_out, coef_p, coef_d, fdm_2,         &
     &          fdm_3e,      &
     &          d1nod_mat_fdm_e1, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      type(fdm_matrix), intent(in) :: fdm_2(2)
      type(fdm_matrix), intent(in) :: fdm_3e(0:3)
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_e1(sph_rj%nidx_rj(1),0:1)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
      real(kind = kreal) :: mat_grad_p( 0:1)
      real(kind = kreal) :: mat_visous(-1:1)
!
!
!$omp parallel do                                                       &
!$omp& private(k,j,r_mid,ar_mid,d_mid,c_d3,c_d1,c_d0,hdiv_visous)
      do k = kr_in+2, kr_out-1
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
        do j = 1, sph_rj%nidx_rj(2)
          c_d1 =  g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
!
          hdiv_visous(-2:1) = coef_d * (c_d3 * fdm_3e(3)%dmat(k,-2:1)   &
     &                                + c_d1 * fdm_3e(1)%dmat(k,-2:1)   &
     &                                + c_d0 * fdm_3e(0)%dmat(k,-2:1))
!
          if((2*k-4) .gt. 0) mat7(7,2*k-4,j) = - hdiv_visous(-2)
          mat7(6,2*k-3,j) = zero
          mat7(5,2*k-2,j) = - hdiv_visous(-1)
!
          mat7(4,2*k-1,j) = coef_p
!
          mat7(3,2*k,  j) = - hdiv_visous( 0)
          mat7(2,2*k+1,j) = zero
          mat7(1,2*k+2,j) =   hdiv_visous( 1)
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private (k,j,c_d2,c_d0,mat_visous,mat_grad_p)
      do k = kr_in+1, kr_out-1
        c_d2 =  one
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = -g_sph_rj(j,3) * sph_rj%ar_1d_rj(k,2)
          mat_grad_p( 0:1) = coef_p * d1nod_mat_fdm_e1(k,0:1)
          mat_visous(-1:1) = coef_d *  c_d2 * fdm_2(2)%dmat(k,-1:1)
          mat_visous( 0) =   mat_visous( 0) + coef_d * c_d0
!
!
          if((2*k-3) .gt. 0) mat7(7,2*k-3,j) = zero
          if((2*k-2) .gt. 0) then
            mat7(6,2*k-2,j) = - mat_visous(-1)
          end if
          mat7(5,2*k-1,j) =     mat_grad_p(0)
!
          mat7(4,2*k,  j) =   - mat_visous( 0)
!
          mat7(3,2*k+1,j) =     mat_grad_p(1)
          mat7(2,2*k+2,j) =   - mat_visous(1)
          if((2*k+3) .le. 2*sph_rj%nidx_rj(1)) mat7(1,2*k+3,j) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_vpol_press_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine cal_exp_sph_vpol_diffusions(sph_rj, istep_rj,       &
     &          g_sph_rj, kr_in, kr_out,      &
     &          coef_p, coef_d, fdm_2, fdm_3e,  &
     &          d1nod_mat_fdm_e1, e_press,                              &
     &          is_velo, is_viscous, is_grad_p,                         &
     &          n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      type(fdm_matrix), intent(in) :: fdm_2(2)
      type(fdm_matrix), intent(in) :: fdm_3e(0:3)
      real(kind = kreal), intent(in) :: d1nod_mat_fdm_e1(sph_rj%nidx_rj(1),0:1)
!
      integer(kind = kint), intent(in) :: is_velo, is_viscous, is_grad_p
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: e_press(n_point)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n2, i_n1, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3)
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
      real(kind = kreal) :: mat_grad_p( 0:1)
      real(kind = kreal) :: mat_visous(-1:1)
!
!
!$omp parallel do private(k,j,r_mid,ar_mid,c_d3,c_d1,c_d0,              &
!$omp&                    iele,i_p1,inod,i_n1,i_n2,hdiv_visous)
      do k = kr_in+2, kr_out-1
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          i_p1 = iele + istep_rj(2)
          inod = iele
          i_n1 = iele - istep_rj(2)
          i_n2 = i_n1 - istep_rj(2)
!
          c_d1 =  g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
!
          hdiv_visous(-2:1) = coef_d * (c_d3 * fdm_3e(3)%dmat(k,-2:1)   &
     &                                + c_d1 * fdm_3e(1)%dmat(k,-2:1)   &
     &                                + c_d0 * fdm_3e(0)%dmat(k,-2:1))
!
          e_hdiv_viscous(iele) =  hdiv_visous(-2) * d_rj(i_n2,is_velo)  &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(k,j,c_d2,c_d0,inod,i_n1,i_p1,                 &
!$omp&                    mat_visous,mat_grad_p)
      do k = kr_in+1, kr_out-1
        c_d2 =  one
        do j = 1, sph_rj%nidx_rj(2)
          inod = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          i_n1 = inod - istep_rj(2)
          i_p1 = inod + istep_rj(2)
!
          c_d0 = -g_sph_rj(j,3) * sph_rj%ar_1d_rj(k,2)
          mat_grad_p( 0:1) = coef_p * d1nod_mat_fdm_e1(k,0:1)
          mat_visous(-1:1) = coef_d * c_d2 * fdm_2(2)%dmat(k,-1:1)
          mat_visous( 0) = mat_visous( 0) + coef_d * c_d0
!
          d_rj(inod,is_viscous) =  mat_visous(-1) * d_rj(i_n1,is_velo)  &
     &                           + mat_visous( 0) * d_rj(inod,is_velo)  &
     &                           + mat_visous(-1) * d_rj(i_p1,is_velo)
          d_rj(inod,is_grad_p) =   mat_grad_p( 0) * e_press(i_n1)       &
     &                           + mat_grad_p( 1) * e_press(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_exp_sph_vpol_diffusions
!
! -----------------------------------------------------------------------
!
      subroutine add_val_viscosity_sph_mat(sph_rj,   &
     &          g_sph_rj, kr_in, kr_out,                      &
     &          coef_p, coef_d, relative_d, h_nu, fdm_2,      &
     &          fdm_3e, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      type(fdm_matrix), intent(in) :: fdm_2(2)
      type(fdm_matrix), intent(in) :: fdm_3e(0:3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
      real(kind = kreal) :: mat_visous(-1:1)
!
!
!$omp parallel do                                                       &
!$omp& private(k,j,r_mid,ar_mid,d_mid,c_d2,c_d1,c_d0,hdiv_visous)
      do k = kr_in+2, kr_out-1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(k)
        c_d1 = two * ar_mid(1) * h_nu(k)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(k)
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm_3e(2)%dmat(k,-2:1)   &
     &                                + c_d1 * fdm_3e(1)%dmat(k,-2:1)   &
     &                                + c_d0 * fdm_3e(0)%dmat(k,-2:1))
!
          if((2*k-4) .gt. 0) then
            mat7(7,2*k-4,j) = d_mid * mat7(7,2*k-4,j) - hdiv_visous(-2)
          end if
          mat7(5,2*k-2,j) = d_mid * mat7(5,2*k-2,j) - hdiv_visous(-1)
!
!          mat7(4,2*k-1,j) = coef_p
!
          mat7(3,2*k,  j) = d_mid * mat7(3,2*k,  j) - hdiv_visous( 0)
          mat7(1,2*k+2,j) = d_mid * mat7(1,2*k+2,j) - hdiv_visous( 1)
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private (k,j,c_d0,c_d1,mat_visous)
      do k = kr_in+1, kr_out-1
        c_d1 = two * h_nu(k)
        c_d0 = - four * h_nu(k) * sph_rj%ar_1d_rj(k,1)
        do j = 1, sph_rj%nidx_rj(2)
          mat_visous(-1:1) = coef_d * relative_d(k)                     &
     &                      * c_d1 * fdm_2(1)%dmat(k,-1:1)
          mat_visous( 0) = mat_visous( 0)                               &
     &                    + coef_d * relative_d(k)  * c_d0
!
          if((2*k-2) .gt. 0) then
            mat7(6,2*k-2,j) = relative_d(k) * mat7(6,2*k-2,j)           &
     &                       - mat_visous(-1)
          end if
!          mat7(5,2*k-1,j) = coef_p * d1nod_mat_fdm_e1(k, 0)
          mat7(4,2*k,  j) = relative_d(k) *mat7(4,2*k,  j)              &
     &                     - mat_visous( 0)
!          mat7(3,2*k+1,j) = coef_p * d1nod_mat_fdm_e1(k, 1)
          mat7(2,2*k+2,j) = relative_d(k) *mat7(2,2*k+2,j)              &
     &                     - mat_visous( 1)
        end do
      end do
!$omp end parallel do
!
      end subroutine add_val_viscosity_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_val_viscosity(sph_rj, istep_rj,         &
     &          g_sph_rj, kr_in, kr_out,      &
     &          coef_p, coef_d, relative_d, h_nu, fdm_2,      &
     &          fdm_3e,      &
     &          is_velo, is_viscous, n_point, ntot_phys_rj, d_rj,       &
     &          e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      type(fdm_matrix), intent(in) :: fdm_2(2)
      type(fdm_matrix), intent(in) :: fdm_3e(0:3)
!
      integer(kind = kint), intent(in) :: is_velo, is_viscous
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n2, i_n1, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
      real(kind = kreal) :: mat_visous(-1:1)
!
!
!$omp parallel do private(k,j,r_mid,ar_mid,d_mid,c_d2,c_d1,c_d0,        &
!$omp&                    iele,i_p1,inod,i_n1,i_n2,hdiv_visous)
      do k = kr_in+2, kr_out-1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(k)
        c_d1 = two * ar_mid(1) * h_nu(k)
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          i_p1 = iele + istep_rj(2)
          inod = iele
          i_n1 = iele - istep_rj(2)
          i_n2 = i_n1 - istep_rj(2)
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(k)
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm_3e(2)%dmat(k,-2:1)   &
     &                                + c_d1 * fdm_3e(1)%dmat(k,-2:1)   &
     &                                + c_d0 * fdm_3e(0)%dmat(k,-2:1))
!
          e_hdiv_viscous(iele) =  d_mid * e_hdiv_viscous(iele)          &
     &                          + hdiv_visous(-2) * d_rj(i_n2,is_velo)  &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private (k,j,c_d0,c_d1,inod,i_n1,i_p1,mat_visous)
      do k = kr_in+1, kr_out-1
        c_d1 = two * h_nu(k)
        c_d0 = - four * h_nu(k) * sph_rj%ar_1d_rj(k,1)
        do j = 1, sph_rj%nidx_rj(2)
          inod = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          i_n1 = inod - istep_rj(2)
          i_p1 = inod + istep_rj(2)
!
          mat_visous(-1:1) = coef_d * relative_d(k)                     &
     &                      * c_d1 * fdm_2(1)%dmat(k,-1:1)
          mat_visous( 0) = mat_visous( 0)                               &
     &                    + coef_d * relative_d(k)  * c_d0
!
          d_rj(inod,is_viscous) = relative_d(k) * d_rj(inod,is_viscous) &
     &                           + mat_visous(-1) * d_rj(i_n1,is_velo)  &
     &                           + mat_visous( 0) * d_rj(inod,is_velo)  &
     &                           + mat_visous(-1) * d_rj(i_p1,is_velo)
        end do
      end do
!$omp end parallel do
!
      end subroutine add_exp_sph_val_viscosity
!
! -----------------------------------------------------------------------
!
      subroutine add_val_density_sph_mat(sph_rj,     &
     &          g_sph_rj, kr_in, kr_out, coef_p, coef_d,      &
     &          relative_d, h_rho, h_nu, fdm_2, fdm_3e, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      type(fdm_matrix), intent(in) :: fdm_2(2)
      type(fdm_matrix), intent(in) :: fdm_3e(0:3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
      real(kind = kreal) :: mat_visous(-1:1)
!
!
!$omp parallel do                                                       &
!$omp& private(k,j,r_mid,ar_mid,d_mid,c_d2,c_d1,c_d0,hdiv_visous)
      do k = kr_in+2, kr_out-1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(k,0)
        c_d1 = two * ar_mid(1) * h_rho(k,0)  + h_rho(k,1)               &
     &        + h_nu(k) * h_rho(k,0)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(k,0) * two / three
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm_3e(2)%dmat(k,-2:1)   &
     &                                + c_d1 * fdm_3e(1)%dmat(k,-2:1)   &
     &                                + c_d0 * fdm_3e(0)%dmat(k,-2:1))
!
          if((2*k-4) .gt. 0) then
            mat7(7,2*k-4,j) = mat7(7,2*k-4,j) - hdiv_visous(-2)
          end if
          mat7(5,2*k-2,j) = mat7(5,2*k-2,j) - hdiv_visous(-1)
!          mat7(4,2*k-1,j) = coef_p
          mat7(3,2*k,  j) = mat7(3,2*k,  j) - hdiv_visous( 0)
          mat7(1,2*k+2,j) = mat7(1,2*k+2,j) - hdiv_visous( 1)
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private (k,j,c_d0,c_d1,mat_visous)
      do k = kr_in+1, kr_out-1
        c_d1 = - h_rho(k,0) / three
        c_d0 = - (four / three) * (h_rho(k,0) * sph_rj%ar_1d_rj(k,1)    &
     &                           + h_rho(k,0) * h_nu(k) + h_rho(k,1))
        do j = 1, sph_rj%nidx_rj(2)
          mat_visous(-1:1) = coef_d * relative_d(k)                     &
     &                      * c_d1 * fdm_2(1)%dmat(k,-1:1)
          mat_visous( 0) = mat_visous( 0)                               &
     &                    + coef_d * relative_d(k)  * c_d0
!
          if((2*k-2) .gt. 0) then
            mat7(6,2*k-2,j) = mat7(6,2*k-2,j) - mat_visous(-1)
          end if
          mat7(4,2*k,  j) =   mat7(4,2*k,  j) - mat_visous( 0)
          mat7(2,2*k+2,j) =   mat7(2,2*k+2,j) - mat_visous( 1)
        end do
      end do
!$omp end parallel do
!
      end subroutine add_val_density_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_val_density(sph_rj, istep_rj,           &
     &          g_sph_rj, kr_in, kr_out,      &
     &          coef_p, coef_d, relative_d, h_rho, h_nu,                &
     &          fdm_2, fdm_3e,                       &
     &          is_velo, is_viscous, n_point, ntot_phys_rj, d_rj,       &
     &          e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      type(fdm_matrix), intent(in) :: fdm_2(2)
      type(fdm_matrix), intent(in) :: fdm_3e(0:3)
!
      integer(kind = kint), intent(in) :: is_velo, is_viscous
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
      real(kind = kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n2, i_n1, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
      real(kind = kreal) :: mat_visous(-1:1)
!
!
!$omp parallel do private(k,j,r_mid,ar_mid,d_mid,c_d2,c_d1,c_d0,        &
!$omp&                    iele,i_p1,inod,i_n1,i_n2,hdiv_visous)
      do k = kr_in+2, kr_out-1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(k,0)
        c_d1 = two * ar_mid(1) * h_rho(k,0)  + h_rho(k,1)               &
     &        + h_nu(k) * h_rho(k,0)
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          i_p1 = iele + istep_rj(2)
          inod = iele
          i_n1 = iele - istep_rj(2)
          i_n2 = i_n1 - istep_rj(2)
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(k,0) * two / three
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm_3e(2)%dmat(k,-2:1)   &
     &                                + c_d1 * fdm_3e(1)%dmat(k,-2:1)   &
     &                                + c_d0 * fdm_3e(0)%dmat(k,-2:1))
!
          e_hdiv_viscous(iele) = e_hdiv_viscous(iele)                   &
     &                          + hdiv_visous(-2) * d_rj(i_n2,is_velo)  &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private (k,j,c_d0,c_d1,inod,i_n1,i_p1,mat_visous)
      do k = kr_in+1, kr_out-1
        c_d1 = - h_rho(k,0) / three
        c_d0 = - (four / three) * (h_rho(k,0) * sph_rj%ar_1d_rj(k,1)    &
     &                           + h_rho(k,0) * h_nu(k) + h_rho(k,1))
        do j = 1, sph_rj%nidx_rj(2)
          inod = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          i_n1 = inod - istep_rj(2)
          i_p1 = inod + istep_rj(2)
!
          mat_visous(-1:1) = coef_d * relative_d(k)                     &
     &                      * c_d1 * fdm_2(1)%dmat(k,-1:1)
          mat_visous( 0) = mat_visous( 0)                               &
     &                    + coef_d * relative_d(k)  * c_d0
!
          d_rj(inod,is_viscous) = d_rj(inod,is_viscous)                 &
     &                           + mat_visous(-1) * d_rj(i_n1,is_velo)  &
     &                           + mat_visous( 0) * d_rj(inod,is_velo)  &
     &                           + mat_visous(-1) * d_rj(i_p1,is_velo)
        end do
      end do
!$omp end parallel do
!
      end subroutine add_exp_sph_val_density
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vpol_press_sph_CMB_mat(sph_rj,  &
     &          g_sph_rj, kr_out, coef_p, coef_d, fdm3e_CMB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,0:3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3)
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
!$omp parallel do private (j,c_d1,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d1 =  g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
          hdiv_visous(-2:1) = coef_d * (c_d3 * fdm3e_CMB_mat(-2:1,3)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,0))
!
          if((2*k-4) .gt. 0) then
            mat7(7,2*k-4,j) = - hdiv_visous(-2)
          end if
          mat7(6,2*k-3,j) = zero
          mat7(5,2*k-2,j) =   - hdiv_visous(-1)
          mat7(4,2*k-1,j) = coef_p
          mat7(3,2*k,  j) =   - hdiv_visous( 0)
!          mat7(2,2*k+1,j) = zero
!          mat7(1,2*k+2,j) =  - hdiv_visous( 1)
        end do
!$omp end parallel do
!
!$omp parallel do private(j)
        do j = 1, sph_rj%nidx_rj(2)
          if((2*kr_out-3) .gt. 0) mat7(7,2*kr_out-3,j) = zero
          if((2*kr_out-2) .gt. 0) mat7(6,2*kr_out-2,j) = zero
          mat7(5,2*kr_out-1,j) = zero
          mat7(4,2*kr_out,  j) = one
          if((2*kr_out+1) .le. 2*sph_rj%nidx_rj(1)) mat7(3,2*kr_out+1,j) = zero
          if((2*kr_out+2) .le. 2*sph_rj%nidx_rj(1)) mat7(2,2*kr_out+2,j) = zero
          if((2*kr_out+3) .le. 2*sph_rj%nidx_rj(1)) mat7(1,2*kr_out+3,j) = zero
        end do
!$omp end parallel do
!
      end subroutine set_vpol_press_sph_CMB_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_exp_sph_hdiv_viscous_CMB                           &
     &         (sph_rj, istep_rj, g_sph_rj, kr_out,  &
     &          coef_p, coef_d, fdm3e_CMB_mat, is_velo,                 &
     &          n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,0:3)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n2, i_n1
      real(kind = kreal) :: r_mid, ar_mid(3)
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
!$omp  parallel do                                                      &
!$omp& private(j,c_d1,c_d0,iele,inod,i_n1,i_n2,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          inod = iele
          i_n1 = iele - istep_rj(2)
          i_n2 = i_n1 - istep_rj(2)
!
          c_d1 =  g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
          hdiv_visous(-2:1) = coef_d * (c_d3 * fdm3e_CMB_mat(-2:1,3)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,0))
!
          e_hdiv_viscous(iele) =  hdiv_visous(-2) * d_rj(i_n2,is_velo)  &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)
        end do
!$omp end parallel do
!
      end subroutine set_exp_sph_hdiv_viscous_CMB
!
! -----------------------------------------------------------------------
!
      subroutine add_val_viscosity_sph_CMB_mat                          &
     &         (sph_rj, g_sph_rj, kr_out,            &
     &          coef_d, relative_d, h_nu, fdm3e_CMB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,0:3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(k)
        c_d1 = two * ar_mid(1) * h_nu(k)
!$omp parallel do private(j,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(k)
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_CMB_mat(-2:1,2)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,0))
!
          if((2*k-4) .gt. 0) then
            mat7(7,2*k-4,j) = d_mid* mat7(7,2*k-4,j) - hdiv_visous(-2)
          end if
          mat7(5,2*k-2,j) =   d_mid* mat7(5,2*k-2,j) - hdiv_visous(-1)
!          mat7(4,2*k-1,j) =  coef_p
          mat7(3,2*k,  j) =   d_mid* mat7(3,2*k,  j) - hdiv_visous( 0)
!          mat7(1,2*k+2,j) =  d_mid* mat7(1,2*k+2,j) - hdiv_visous( 1)
        end do
!$omp end parallel do
!
      end subroutine add_val_viscosity_sph_CMB_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_val_nu_CMB                            &
     &         (sph_rj, istep_rj, g_sph_rj, kr_out,  &
     &          coef_d, relative_d, h_nu, fdm3e_CMB_mat,                &
     &          is_velo, n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,0:3)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n2, i_n1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(k)
        c_d1 = two * ar_mid(1) * h_nu(k)
!$omp parallel do private(j,c_d0,iele,inod,i_n1,i_n2,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          inod = iele
          i_n1 = iele - istep_rj(2)
          i_n2 = i_n1 - istep_rj(2)
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(k)
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_CMB_mat(-2:1,2)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,0))
!
          e_hdiv_viscous(iele) =  d_mid * e_hdiv_viscous(iele)          &
     &                          + hdiv_visous(-2) * d_rj(i_n2,is_velo)  &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)
        end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_val_nu_CMB
!
! -----------------------------------------------------------------------
!
      subroutine add_val_density_sph_CMB_mat                            &
     &         (sph_rj, g_sph_rj, kr_out,            &
     &          coef_d, relative_d, h_rho, h_nu, fdm3e_CMB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,0:3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(k,0)
        c_d1 = two * ar_mid(1) * h_rho(k,0)  + h_rho(k,1)               &
     &        + h_nu(k) * h_rho(k,0)
!$omp parallel do private(j,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(k,0) * two / three
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_CMB_mat(-2:1,2)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,0))
!
          if((2*k-4) .gt. 0) then
            mat7(7,2*k-4,j) = mat7(7,2*k-4,j) - hdiv_visous(-2)
          end if
          mat7(5,2*k-2,j) =   mat7(5,2*k-2,j) - hdiv_visous(-1)
!          mat7(4,2*k-1,j) =   coef_p
          mat7(3,2*k,  j) =   mat7(3,2*k,  j) - hdiv_visous( 0)
!          mat7(1,2*k+2,j) =  mat7(1,2*k+2,j) - hdiv_visous( 1)
        end do
!$omp end parallel do
!
      end subroutine add_val_density_sph_CMB_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_val_rho_CMB                           &
     &         (sph_rj, istep_rj, g_sph_rj, kr_out,  &
     &          coef_d, relative_d, h_rho, h_nu, fdm3e_CMB_mat,         &
     &          is_velo, n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      integer(kind = kint), intent(in) :: kr_out
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_CMB_mat(-2:1,0:3)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n2, i_n1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_out
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(k,0)
        c_d1 = two * ar_mid(1) * h_rho(k,0)  + h_rho(k,1)               &
     &        + h_nu(k) * h_rho(k,0)
!$omp parallel do private(j,c_d0,iele,inod,i_n1,i_n2,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          inod = iele
          i_n1 = iele - istep_rj(2)
          i_n2 = i_n1 - istep_rj(2)
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(k,0) * two / three
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_CMB_mat(-2:1,2)    &
     &                                + c_d1 * fdm3e_CMB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_CMB_mat(-2:1,0))
!
          e_hdiv_viscous(iele) = e_hdiv_viscous(iele)                   &
     &                          + hdiv_visous(-2) * d_rj(i_n2,is_velo)  &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)
        end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_val_rho_CMB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vpol_press_sph_ICB_mat(sph_rj,  &
     &          g_sph_rj, kr_in, coef_p, coef_d, fdm3e_ICB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,0:3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3)
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
!$omp parallel do private(j,c_d1,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d1 =  g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
          hdiv_visous(-2:1) = coef_d * (c_d3 * fdm3e_ICB_mat(-2:1,3)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,0))
!
!          if((2*k-4) .gt. 0) mat7(7,2*k-4,j) = - hdiv_visous(-2)
          mat7(6,2*k-3,j) =   zero
          mat7(5,2*k-2,j) = - hdiv_visous(-1)
!
          mat7(4,2*k-1,j) =   coef_p
!
          mat7(3,2*k,  j) = - hdiv_visous(0)
          mat7(2,2*k+1,j) =   zero
          mat7(1,2*k+2,j) = - hdiv_visous(1)
        end do
!$omp end parallel do

!$omp parallel do private(j)
        do j = 1, sph_rj%nidx_rj(2)
          if((2*kr_in-3) .gt. 0) mat7(7,2*kr_in-3,j) = zero
          if((2*kr_in-2) .gt. 0) mat7(6,2*kr_in-2,j) = zero
          mat7(5,2*kr_in-1,j) = zero
          mat7(4,2*kr_in,  j) = one
          if((2*kr_in+1) .le. 2*sph_rj%nidx_rj(1)) mat7(3,2*kr_in+1,j) = zero
          if((2*kr_in+2) .le. 2*sph_rj%nidx_rj(1)) mat7(2,2*kr_in+2,j) = zero
          if((2*kr_in+3) .le. 2*sph_rj%nidx_rj(1)) mat7(1,2*kr_in+3,j) = zero
        end do
!$omp end parallel do
!
      end subroutine set_vpol_press_sph_ICB_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_exp_sph_hdiv_viscous_ICB                           &
     &         (sph_rj, istep_rj, g_sph_rj, kr_in,   &
     &          coef_d, fdm3e_ICB_mat, is_velo,                         &
     &          n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,0:3)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n1, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3)
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
!$omp parallel do private(j,c_d1,c_d0,iele,i_p1,inod,i_n1,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          i_p1 = iele + istep_rj(2)
          inod = iele
          i_n1 = iele - istep_rj(2)
!
          c_d1 =  g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
          hdiv_visous(-2:1) = coef_d * (c_d3 * fdm3e_ICB_mat(-2:1,3)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,0))
!
          e_hdiv_viscous(iele) =  hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
!$omp end parallel do

      end subroutine set_exp_sph_hdiv_viscous_ICB
!
! -----------------------------------------------------------------------
!
      subroutine add_val_viscosity_sph_ICB_mat                          &
     &         (sph_rj, g_sph_rj, kr_in,             &
     &          coef_d, relative_d, h_nu, fdm3e_ICB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,0:3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(k)
        c_d1 = two * ar_mid(1) * h_nu(k)
!$omp parallel do private(j,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(k)
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_ICB_mat(-2:1,2)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,0))
!
!          if((2*k-4) .gt. 0) then
!            mat7(7,2*k-4,j) = d_mid * mat7(7,2*k-4,j) - hdiv_visous(-2)
!          end if
          mat7(5,2*k-2,j) = d_mid * mat7(5,2*k-2,j) - hdiv_visous(-1)
!          mat7(4,2*k-1,j) = coef_p
          mat7(3,2*k,  j) = d_mid * mat7(3,2*k,  j) - hdiv_visous( 0)
          mat7(1,2*k+2,j) = d_mid * mat7(1,2*k+2,j) - hdiv_visous( 1)
        end do
!$omp end parallel do
!
      end subroutine add_val_viscosity_sph_ICB_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_val_nu_ICB                            &
     &         (sph_rj, istep_rj, g_sph_rj, kr_in,   &
     &          coef_d, relative_d, h_nu, fdm3e_ICB_mat, is_velo,       &
     &          n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,0:3)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n1, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(k)
        c_d1 = two * ar_mid(1) * h_nu(k)
!$omp parallel do private(j,c_d0,i_p1,iele,inod,i_n1,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          i_p1 = iele + istep_rj(2)
          inod = iele
          i_n1 = iele - istep_rj(2)
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(k)
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_ICB_mat(-2:1,2)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,0))
!
          e_hdiv_viscous(iele) = d_mid * e_hdiv_viscous(iele)           &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_val_nu_ICB
!
! -----------------------------------------------------------------------
!
      subroutine add_val_density_sph_ICB_mat                            &
     &         (sph_rj, g_sph_rj, kr_in,             &
     &          coef_d, relative_d, h_rho, h_nu, fdm3e_ICB_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,0:3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(k,0)
        c_d1 = two * ar_mid(1) * h_rho(k,0)  + h_rho(k,1)               &
     &        + h_nu(k) * h_rho(k,0)
!$omp parallel do private(j,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(k,0) * two / three
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_ICB_mat(-2:1,2)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,0))
!
!          if((2*k-4) .gt. 0) then
!            mat7(7,2*k-4,j) = mat7(7,2*k-4,j) - hdiv_visous(-2)
!          end if
          mat7(5,2*k-2,j) = mat7(5,2*k-2,j) - hdiv_visous(-1)
!          mat7(4,2*k-1,j) = coef_p
          mat7(3,2*k,  j) = mat7(3,2*k,  j) - hdiv_visous( 0)
          mat7(1,2*k+2,j) = mat7(1,2*k+2,j) - hdiv_visous( 1)
        end do
!$omp end parallel do
!
      end subroutine add_val_density_sph_ICB_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_val_rho_ICB                           &
     &         (sph_rj, istep_rj, g_sph_rj, kr_in,   &
     &          coef_d, relative_d, h_rho, h_nu, fdm3e_ICB_mat,         &
     &          is_velo, n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      integer(kind = kint), intent(in) :: kr_in
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_ICB_mat(-2:1,0:3)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_n1, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
      k = kr_in+1
        d_mid = half * (relative_d(k-1) +     relative_d(k))
        r_mid = half * (sph_rj%radius_1d_rj_r(k-1)                      &
     &                + sph_rj%radius_1d_rj_r(k  ))
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(k,0)
        c_d1 = two * ar_mid(1) * h_rho(k,0)  + h_rho(k,1)               &
     &        + h_nu(k) * h_rho(k,0)
!$omp parallel do private(j,c_d0,iele,i_p1,inod,i_n1,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (k-1) * istep_rj(1) + (j-1) * istep_rj(2)
          i_p1 = iele + istep_rj(2)
          inod = iele
          i_n1 = iele - istep_rj(2)
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(k,0) * two / three
          hdiv_visous(-2:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_ICB_mat(-2:1,2)    &
     &                                + c_d1 * fdm3e_ICB_mat(-2:1,1)    &
     &                                + c_d0 * fdm3e_ICB_mat(-2:1,0))
!
          e_hdiv_viscous(iele) = e_hdiv_viscous(iele)                   &
     &                          + hdiv_visous(-1) * d_rj(i_n1,is_velo)  &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_val_rho_ICB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vpol_press_sph_center_mat                          &
     &         (sph_rj, g_sph_rj, coef_p, coef_d,    &
     &          fdm3e_center_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,0:3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: j
      real(kind = kreal) :: r_mid, ar_mid(3)
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
        r_mid = half * sph_rj%radius_1d_rj_r(1)
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
!$omp parallel do private(j,c_d1,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d1 =  g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
          hdiv_visous( 0:1) = coef_d * (c_d3 * fdm3e_center_mat(0:1,3)  &
     &                                + c_d1 * fdm3e_center_mat(0:1,1)  &
     &                                + c_d0 * fdm3e_center_mat(0:1,0))
!
          mat7(4,1,j) = coef_p
!
          mat7(3,2,j) =      - hdiv_visous( 0)
          mat7(2,3,j) = zero
          mat7(1,4,j) =      - hdiv_visous( 1)
        end do
!$omp end parallel do
!
      end subroutine set_vpol_press_sph_center_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_exp_sph_hdiv_viscous_CTR                           &
     &         (sph_rj, istep_rj, g_sph_rj,          &
     &          coef_d, fdm3e_center_mat, is_velo,                      &
     &          n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,0:3)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
!
      integer(kind = kint) :: j
      integer(kind = kint) :: inod, iele, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3)
      real(kind = kreal) :: c_d3, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(-2:1)
!
!
        r_mid = half * sph_rj%radius_1d_rj_r(1)
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d3 = -one
!$omp parallel do private(j,c_d1,c_d0,iele,i_p1,inod,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (j-1) * istep_rj(2)
          i_p1 = iele + istep_rj(2)
          inod = iele
!
          c_d1 =  g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
          hdiv_visous( 0:1) = coef_d * (c_d3 * fdm3e_center_mat(0:1,3)  &
     &                                + c_d1 * fdm3e_center_mat(0:1,1)  &
     &                                + c_d0 * fdm3e_center_mat(0:1,0))
!
          e_hdiv_viscous(iele) =  hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
!$omp end parallel do
!
      end subroutine set_exp_sph_hdiv_viscous_CTR
!
! -----------------------------------------------------------------------
!
      subroutine add_val_viscosity_sph_CTR_mat                          &
     &         (sph_rj, g_sph_rj, coef_d,            &
     &          relative_d, h_nu, fdm3e_center_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,0:3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(0:1)
!
!
        d_mid =        relative_d(1)
        r_mid = half * sph_rj%radius_1d_rj_r(1)
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(1)
        c_d1 = two * ar_mid(1) * h_nu(1)
!$omp parallel do private(j,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(1)
          hdiv_visous(0:1)                                              &
     &              = coef_d * d_mid * (c_d2 * fdm3e_center_mat(0:1,2)  &
     &                                + c_d1 * fdm3e_center_mat(0:1,1)  &
     &                                + c_d0 * fdm3e_center_mat(0:1,0))
!
!          mat7(4,1,j) = coef_p
!
          mat7(3,2,j) = d_mid * mat7(3,2,j) - hdiv_visous( 0)
          mat7(1,4,j) = d_mid * mat7(1,4,j) - hdiv_visous( 1)
        end do
!$omp end parallel do
!
      end subroutine add_val_viscosity_sph_CTR_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_val_nu_CTR                            &
     &         (sph_rj, istep_rj, g_sph_rj, coef_d,  &
     &          relative_d, h_nu, fdm3e_center_mat, is_velo,            &
     &          n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,0:3)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: j
      integer(kind = kint) :: inod, iele, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(0:1)
!
!
        d_mid =        relative_d(1)
        r_mid = half * sph_rj%radius_1d_rj_r(1)
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu(1)
        c_d1 = two * ar_mid(1) * h_nu(1)
!$omp parallel do private(j,c_d0,i_p1,iele,inod,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (j-1) * istep_rj(2)
          i_p1 = iele + istep_rj(2)
          inod = iele
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu(1)
          hdiv_visous(0:1)                                              &
     &              = coef_d * d_mid * (c_d2 * fdm3e_center_mat(0:1,2)  &
     &                                + c_d1 * fdm3e_center_mat(0:1,1)  &
     &                                + c_d0 * fdm3e_center_mat(0:1,0))
!
          e_hdiv_viscous(iele) = d_mid * e_hdiv_viscous(iele)           &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_val_nu_CTR
!
! -----------------------------------------------------------------------
!
      subroutine add_val_density_sph_CTR_mat                            &
     &         (sph_rj, g_sph_rj,                    &
     &          coef_d, relative_d, h_rho, h_nu,                        &
     &          fdm3e_center_mat, mat7)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,0:3)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(0:1)
!
!
      k = 1
        d_mid = relative_d(1)
        r_mid = half * sph_rj%radius_1d_rj_r(1)
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(1,0)
        c_d1 = two * ar_mid(1) * h_rho(1,0)  + h_rho(1,1)               &
     &        + h_nu(1) * h_rho(1,0)
!$omp parallel do private(j,c_d0,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(1,0) * two / three
          hdiv_visous( 0:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_center_mat(0:1,2)  &
     &                                + c_d1 * fdm3e_center_mat(0:1,1)  &
     &                                + c_d0 * fdm3e_center_mat(0:1,0))
!
!          mat7(4,1,j) = coef_p
!
          mat7(3,2,j) = mat7(3,2,j) - hdiv_visous(0)
          mat7(1,4,j) = mat7(1,4,j) - hdiv_visous(1)
        end do
!$omp end parallel do
!
      end subroutine add_val_density_sph_CTR_mat
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_val_rho_CTR                           &
     &         (sph_rj, istep_rj, g_sph_rj, coef_d,  &
     &          relative_d, h_rho, h_nu, fdm3e_center_mat, is_velo,     &
     &          n_point, ntot_phys_rj, d_rj, e_hdiv_viscous)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: istep_rj(2)
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,0:3)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!
      integer(kind = kint) :: k, j
      integer(kind = kint) :: inod, iele, i_p1
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
      real(kind = kreal) :: hdiv_visous(0:1)
!
!
      k = 1
        d_mid = relative_d(1)
        r_mid = half * sph_rj%radius_1d_rj_r(1)
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho(1,0)
        c_d1 = two * ar_mid(1) * h_rho(1,0)  + h_rho(1,1)               &
     &        + h_nu(1) * h_rho(1,0)
!$omp parallel do private(j,c_d0,i_p1,iele,inod,hdiv_visous)
        do j = 1, sph_rj%nidx_rj(2)
          iele = 1 + (j-1) * istep_rj(2)
          i_p1 = iele + istep_rj(2)
          inod = iele
!
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_rho(1,0) * two / three
          hdiv_visous( 0:1)                                             &
     &              = coef_d * d_mid * (c_d2 * fdm3e_center_mat(0:1,2)  &
     &                                + c_d1 * fdm3e_center_mat(0:1,1)  &
     &                                + c_d0 * fdm3e_center_mat(0:1,0))
!
          e_hdiv_viscous(iele) = d_mid * e_hdiv_viscous(iele)           &
     &                          + hdiv_visous( 0) * d_rj(inod,is_velo)  &
     &                          + hdiv_visous( 1) * d_rj(i_p1,is_velo)
        end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_val_rho_CTR
!
! -----------------------------------------------------------------------
!
      end module cal_vpol_press_sph_fdm_mat
