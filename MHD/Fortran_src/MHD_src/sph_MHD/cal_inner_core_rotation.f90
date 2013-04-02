!cal_inner_core_rotation.f90
!
!     Written by H. Matsui on Nov., 2012
!
!      subroutine set_inner_core_rotation
!      subroutine set_icore_viscous_matrix
!      subroutine cal_icore_viscous_drag_explicit
!      subroutine cal_icure_coriolis_explicit
!      subroutine int_icore_toroidal_lorentz
!
      module cal_inner_core_rotation
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_phys_address
      use m_sph_spectr_data
      use m_physical_property
!
      implicit  none
!
      private :: int_icore_tor_lorentz_l1, cal_icore_viscous_drag_l1
      private :: set_rotate_icb_vt_sph_mat
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_inner_core_rotation
!
!
      call set_inner_core_rot_l1(idx_rj_degree_one(-1))
      call set_inner_core_rot_l1(idx_rj_degree_one( 0))
      call set_inner_core_rot_l1(idx_rj_degree_one( 1))
!
      end subroutine set_inner_core_rotation
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_icore_viscous_matrix
!
!
      call set_rotate_icb_vt_sph_mat(idx_rj_degree_one(-1))
      call set_rotate_icb_vt_sph_mat(idx_rj_degree_one( 0))
      call set_rotate_icb_vt_sph_mat(idx_rj_degree_one( 1))
!
      end subroutine set_icore_viscous_matrix
!
! ----------------------------------------------------------------------
!
      subroutine cal_icore_viscous_drag_explicit
!
!
      call cal_icore_viscous_drag_l1(idx_rj_degree_one(-1))
      call cal_icore_viscous_drag_l1(idx_rj_degree_one( 0))
      call cal_icore_viscous_drag_l1(idx_rj_degree_one( 1))
!
      end subroutine cal_icore_viscous_drag_explicit
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_icure_coriolis_explicit
!
      integer(kind = kint) :: k
      integer(kind = kint) :: i11s, i10c, i11c
!
!
      if(idx_rj_degree_one( 1) .le. 0) return
!
      k = nlayer_ICB
      i11s = idx_rj_degree_one(-1) + (nlayer_ICB-1)*nidx_rj(2)
      i10c = idx_rj_degree_one( 0) + (nlayer_ICB-1)*nidx_rj(2)
      i11c = idx_rj_degree_one( 1) + (nlayer_ICB-1)*nidx_rj(2)
!
      d_rj(i11s,ipol%i_rot_Coriolis)                                    &
     &       =  omega_rj(k,0,2)*d_rj(i11c,ipol%i_vort)                  &
     &        - omega_rj(k,0,3)*d_rj(i10c,ipol%i_vort)
      d_rj(i11c,ipol%i_rot_Coriolis)                                    &
     &       =  omega_rj(k,0,1)*d_rj(i10c,ipol%i_vort)                  &
     &        - omega_rj(k,0,2)*d_rj(i11s,ipol%i_vort)
      d_rj(i10c,ipol%i_rot_Coriolis)                                    &
     &       =  omega_rj(k,0,3)*d_rj(i11s,ipol%i_vort)                  &
     &        - omega_rj(k,0,1)*d_rj(i11c,ipol%i_vort)
!
      d_rj(i11s,ipol%i_rot_Coriolis) = -two*coef_cor*radius_1d_rj_r(k)  &
     &                                * d_rj(i11s,ipol%i_rot_Coriolis)
      d_rj(i11c,ipol%i_rot_Coriolis) = -two*coef_cor*radius_1d_rj_r(k)  &
     &                                * d_rj(i11c,ipol%i_rot_Coriolis)
      d_rj(i10c,ipol%i_rot_Coriolis) = -two*coef_cor*radius_1d_rj_r(k)  &
     &                                * d_rj(i10c,ipol%i_rot_Coriolis)
!
      d_rj(i11s,itor%i_coriolis) = d_rj(i11s,ipol%i_rot_Coriolis)
      d_rj(i11c,itor%i_coriolis) = d_rj(i11c,ipol%i_rot_Coriolis)
      d_rj(i10c,itor%i_coriolis) = d_rj(i10c,ipol%i_rot_Coriolis)
!
      end subroutine cal_icure_coriolis_explicit
!
! ----------------------------------------------------------------------
!
      subroutine int_icore_toroidal_lorentz
!
!
      call int_icore_tor_lorentz_l1(idx_rj_degree_one(-1))
      call int_icore_tor_lorentz_l1(idx_rj_degree_one( 0))
      call int_icore_tor_lorentz_l1(idx_rj_degree_one( 1))
!
      end subroutine int_icore_toroidal_lorentz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_rotate_icb_vt_sph_mat(idx_rj_l0)
!
      use m_t_int_parameter
      use m_physical_property
      use m_schmidt_poly_on_rtm
      use m_radial_matrices_sph
      use m_fdm_coefs
!
      integer(kind = kint), intent(in) :: idx_rj_l0
!
!
      if(idx_rj_l0 .le. 0) return
!
!       vt_evo_mat(3,nlayer_ICB-1,idx_rj_l0) = zero
        vt_evo_mat(2,nlayer_ICB,  idx_rj_l0)                            &
     &          = one + coef_imp_v*dt*coef_d_velo * five                &
     &           * (+ dr_1d_rj(nlayer_ICB, 2)                           &
     &              + two*ar_1d_rj(nlayer_ICB,1) )                      &
     &           * a_r_1d_rj_r(nlayer_ICB)
        vt_evo_mat(1,nlayer_ICB+1,idx_rj_l0)                            &
     &          = - coef_imp_v*dt*coef_d_velo * five                    &
     &             * dr_1d_rj(nlayer_ICB, 2)                            &
     &             * a_r_1d_rj_r(nlayer_ICB)
!
!        vt_evo_mat(2,nlayer_ICB,  idx_rj_l0)                           &
!     &          = one + coef_imp_v*dt*coef_d_velo * five               &
!     &           * ( -d1nod_mat_fdm_2(nlayer_ICB,-1)                   &
!     &                * radius_1d_rj_r(nlayer_ICB-1)**2                &
!     &                * ar_1d_rj(nlayer_ICB,2)                         &
!     &              - d1nod_mat_fdm_2(nlayer_ICB, 0)                   &
!     &              + two*ar_1d_rj(nlayer_ICB,1) )
!        vt_evo_mat(1,nlayer_ICB+1,idx_rj_l0)                           &
!     &          = - coef_imp_v*dt*coef_d_velo * five                   &
!     &             * d1nod_mat_fdm_2(nlayer_ICB, 1)
!
      end subroutine set_rotate_icb_vt_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_inner_core_rot_l1(idx_rj_l0)
!
      integer(kind = kint), intent(in) :: idx_rj_l0
!
      integer(kind = kint) :: i10c, i10c_ri
      integer(kind = kint) :: k
      real(kind = kreal) :: ratio
!
!
      if(idx_rj_l0 .le. 0) return
!
      i10c_ri = idx_rj_l0 + (nlayer_ICB-1)*nidx_rj(2)
!$omp parallel do private(k,i10c,ratio)
      do k = 1, nlayer_ICB-1
        i10c = idx_rj_l0 + (k-1)*nidx_rj(2)
!
        ratio = radius_1d_rj_r(k)*radius_1d_rj_r(k)                     &
     &         * ar_1d_rj(nlayer_ICB,2)
!
        d_rj(i10c,itor%i_velo) =   ratio * d_rj(i10c_ri,itor%i_velo)
        d_rj(i10c,ipol%i_vort) =   ratio * d_rj(i10c_ri,ipol%i_vort)
        d_rj(i10c,ipol%i_vort+1) = two *   d_rj(i10c_ri,ipol%i_vort)    &
     &                       * radius_1d_rj_r(k)*ar_1d_rj(nlayer_ICB,2)
      end do
!$omp end parallel do
!
      i10c = idx_rj_l0 + (nlayer_ICB-1)*nidx_rj(2)
      d_rj(i10c,ipol%i_vort+1) = two *   d_rj(i10c_ri,ipol%i_vort)      &
     &                      * ar_1d_rj(nlayer_ICB,1)
!
      end subroutine set_inner_core_rot_l1
!
! ----------------------------------------------------------------------
!
      subroutine cal_icore_viscous_drag_l1(idx_rj_l0)
!
      use m_fdm_coefs
!
      integer(kind = kint), intent(in) :: idx_rj_l0
!
      integer(kind = kint) ::  i10c_ri, i10c_r1
      real(kind = kreal) :: mat_1, mat_0
!
!
      if(idx_rj_l0 .le. 0) return
!
      i10c_ri = idx_rj_l0 + (nlayer_ICB-1)*nidx_rj(2)
      i10c_r1 = idx_rj_l0 +  nlayer_ICB * nidx_rj(2)
!
!      mat_0 = d1nod_mat_fdm_2(nlayer_ICB,-1)                           &
!     &       * radius_1d_rj_r(nlayer_ICB-1)**2 * ar_1d_rj(nlayer_ICB,2)&
!     &      + d1nod_mat_fdm_2(nlayer_ICB, 0)                           &
!     &       - two*ar_1d_rj(nlayer_ICB,1)
!      mat_1 = d1nod_mat_fdm_2(nlayer_ICB, 1)
!
      mat_0 = - dr_1d_rj(nlayer_ICB, 2)                                 &
     &        - two*ar_1d_rj(nlayer_ICB,1)
      mat_1 =   dr_1d_rj(nlayer_ICB, 2)
!
      d_rj(i10c_ri,ipol%i_w_diffuse) =  five * a_r_1d_rj_r(nlayer_ICB)  &
     &                            * (mat_0 * d_rj(i10c_ri,ipol%i_vort)  &
     &                             + mat_1 * d_rj(i10c_r1,ipol%i_vort))
!
      end subroutine cal_icore_viscous_drag_l1
!
! ----------------------------------------------------------------------
!
      subroutine int_icore_tor_lorentz_l1(idx_rj_l0)
!
      integer(kind = kint), intent(in) :: idx_rj_l0
!
      integer(kind = kint) :: k
      integer(kind = kint) :: i10c_i, i10c_o
      real(kind = kreal) :: sk_10c
!
!
      if(idx_rj_l0 .le. 0) return
!
      i10c_o = idx_rj_l0
      sk_10c = d_rj(i10c_o,itor%i_lorentz)                              &
     &          * radius_1d_rj_r(1)*radius_1d_rj_r(1) * dr_1d_rj(1,0)
!
!$omp parallel do reduction(+:sk_10c) private(i10c_i,i10c_o)
      do k = 1, nlayer_ICB-1
        i10c_i = idx_rj_l0 + (k-1)*nidx_rj(2)
        i10c_o = idx_rj_l0 + (k  )*nidx_rj(2)
!
        sk_10c = sk_10c                                                 &
     &        + (d_rj(i10c_i,itor%i_lorentz) * radius_1d_rj_r(k  )**2   &
     &         + d_rj(i10c_o,itor%i_lorentz) * radius_1d_rj_r(k+1)**2)  &
     &        * dr_1d_rj(k,0)
      end do
!$omp end parallel do
!
      i10c_o = idx_rj_l0 + (nlayer_ICB-1)*nidx_rj(2)
      d_rj(i10c_o,itor%i_lorentz) = half * five * sk_10c                &
     &                           * a_r_1d_rj_r(nlayer_ICB)**3
      d_rj(i10c_o,ipol%i_rot_Lorentz) = d_rj(i10c_o,itor%i_lorentz)
!
      end subroutine int_icore_tor_lorentz_l1
!
! ----------------------------------------------------------------------
!
      end module cal_inner_core_rotation
