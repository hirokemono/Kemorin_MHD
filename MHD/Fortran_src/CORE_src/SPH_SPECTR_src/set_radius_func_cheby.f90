!
!      module set_radius_func_cheby
!
!      Programmed by H. Matsui on June., 1994
!      modified by H. Matsui on Apr., 2009
!
!      subroutine set_dr_for_cheby
!      subroutine nod_r_2nd_fdm_coefs_cheby
!*
!***********************************************************************
!
!    delta r for Chebyshev grid
!*     dr_1d_rj(k,0)   : (dr/dn)
!*     dr_1d_rj(k,1)   : (d^2r/dn^2)
!*     dr_1d_rj(k,2)   : 1 / (dr/dn)
!*
!*   Inner core (nlayer_CMB < k < nlayer_CMB)
!*     r(k,0) = ri - 0.5 * L* ( 1-cos(pi* (k-nlayer_ICB) /nri) )
!*   outer core (nlayer_ICB < k < nlayer_CMB)
!*     r(k,0) = ri + 0.5 * L* ( 1-cos(pi* (k-nlayer_ICB) /nri) )
!*   External (nlayer_CMB < k < nlayer_CMB + (nlayer_CMB-nlayer_ICB)/2 )
!*     r(k,0) = ro + 0.5 * L* ( 1-cos(pi* (k-nlayer_CMB) /nri) )
!*
!***********************************************************************
!*
      module set_radius_func_cheby
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
!
      private :: nod_r_2nd_fdm_coef_cheby
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_dr_for_cheby
!
      use m_machine_parameter
      use boundary_radius_func
!
      real(kind = kreal) ::  pi
      real(kind = kreal) ::  shell
      integer(kind = kint) :: k, kst, ked, kr, nri
!
!
      nri = nlayer_CMB - nlayer_ICB
      shell = radius_1d_rj_r(nlayer_CMB) - radius_1d_rj_r(nlayer_ICB)
      pi = four * atan(one)
!
!* ----------  center  --------
!*
      if (nlayer_ICB .gt. 1) call set_non_equi_dr_center
!*
!* ----------  inner core --------
!*
      do kr = 2, nlayer_ICB-1
        k = kr - nlayer_ICB
!*
        dr_1d_rj(kr,0) = -(half*pi*shell*sin( pi*dble(k)/dble(nri) ))   &
     &                  / dble(nri)
        dr_1d_rj(kr,1) = -half*shell*pi*pi*cos( pi*dble(k)/dble(nri) )  &
     &                  / (dble(nri)*dble(nri))
        dr_1d_rj(kr,2) = one / dr_1d_rj(kr,0)
      end do
!
!* ----------  inner core boundary --------
!*
      call set_equi_dr_ICB
!
!* ----------  outer core --------
!*
      do kr = nlayer_ICB+1, nlayer_CMB-1
        k = kr - nlayer_ICB
!*
        dr_1d_rj(kr,0) = (half*pi*shell*sin( pi*dble(k)/dble(nri) ))    &
     &                  / dble(nri)
        dr_1d_rj(kr,1) = half*shell*pi*pi*cos( pi*dble(k)/dble(nri) )   &
     &                  / (dble(nri)*dble(nri))
        dr_1d_rj(kr,2) = one / dr_1d_rj(kr,0)
      end do
!*
!* ----------  core mantle boundary --------
!*
      call set_equi_dr_CMB
      if (nlayer_CMB .eq. nidx_rj(1)) return
!
!* ---------- outer boundary of domain --------
!*
      call set_equi_dr_outside
!*
!* ----------  external of shell --------
!*
      kst = nlayer_CMB + 1
      ked = nlayer_CMB + (nlayer_CMB - nlayer_ICB)/2 - 1
      do kr = kst, min(ked,nidx_rj(1)-1)
        k = kr - nlayer_CMB
!*
        dr_1d_rj(kr,0) = (half*pi*shell*sin( pi*dble(k)/dble(nri) ))    &
     &                  / dble(nri)
        dr_1d_rj(kr,1) =  half*shell*pi*pi*cos( pi*dble(k)/dble(nri) )  &
     &                  / (dble(nri)*dble(nri))
        dr_1d_rj(kr,2) = one / dr_1d_rj(kr,0)
      end do
      if (ked .gt. nidx_rj(1)-1) return
!*
!
      kst = nlayer_CMB + (nlayer_CMB - nlayer_ICB)/2
      ked = nidx_rj(1) - 1
      do k = kst, ked
        dr_1d_rj(k,0) = radius_1d_rj_r(k+1) - radius_1d_rj_r(k)
        dr_1d_rj(k,1) = radius_1d_rj_r(k) - radius_1d_rj_r(k-1)
        dr_1d_rj(k,2) =  (radius_1d_rj_r(k+1)-radius_1d_rj_r(k))        &
     &                 * (radius_1d_rj_r(k)-radius_1d_rj_r(k-1))        &
     &                 * (radius_1d_rj_r(k+1)-radius_1d_rj_r(k-1))
        dr_1d_rj(k,2) = one /dr_1d_rj(k,2)
      end do
!
      end subroutine set_dr_for_cheby
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine nod_r_2nd_fdm_coefs_cheby
!
      use m_fdm_coefs
      use set_radius_func_noequi
!
      integer(kind = kint) :: kr, kst, ked
!
!
      if(nlayer_ICB .gt. 1) then
        call nod_r_2nd_fdm_coef_noequi(ione, dr_1d_rj(1,0),             &
     &      radius_1d_rj_r(1), mat_fdm_2(1,1,1))
      end if
!
      do kr = 2, nlayer_ICB-1
        call nod_r_2nd_fdm_coef_cheby                                   &
     &     (kr, dr_1d_rj(kr,0), dr_1d_rj(kr,1), mat_fdm_2(1,1,kr) )
      end do
!
      call nod_r_2nd_fdm_coef_equi(nlayer_ICB, dr_1d_rj(nlayer_ICB,0),  &
     &    mat_fdm_2(1,1,nlayer_ICB) )
!
      do kr = nlayer_ICB+1, nlayer_CMB-1
        call nod_r_2nd_fdm_coef_cheby                                   &
     &     (kr, dr_1d_rj(kr,0), dr_1d_rj(kr,1), mat_fdm_2(1,1,kr) )
      end do
!*
!* ----------  core mantle boundary --------
!*
      call nod_r_2nd_fdm_coef_equi(nlayer_CMB, dr_1d_rj(nlayer_CMB,0),  &
     &    mat_fdm_2(1,1,nlayer_CMB) )
      if (nlayer_CMB .eq. nidx_rj(1)) return
!
!* ---------- outer boundary of domains --------
!
      kr = nidx_rj(1)
      call nod_r_2nd_fdm_coef_equi(nidx_rj(1),                          &
     &    dr_1d_rj(kr,0), mat_fdm_2(1,1,kr))
!
!* ----------  external of shell --------
!*
      kst = nlayer_CMB + 1
      ked = nlayer_CMB + (nlayer_CMB - nlayer_ICB)/2 - 1
      do kr = kst, min(ked,nidx_rj(1)-1)
        call nod_r_2nd_fdm_coef_cheby                                   &
     &     (kr, dr_1d_rj(kr,0), dr_1d_rj(kr,1),  mat_fdm_2(1,1,kr) )
      end do
!
      if (ked .gt. nidx_rj(1)-1) return
!
      kst = nlayer_CMB + (nlayer_CMB - nlayer_ICB)/2
      ked = nidx_rj(1) - 1
      do kr = kst, ked
        call nod_r_2nd_fdm_coef_noequi                                  &
     &     (kr, dr_1d_rj(kr,0), dr_1d_rj(kr,1), mat_fdm_2(1,1,kr) )
      end do
!
      end subroutine nod_r_2nd_fdm_coefs_cheby
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine nod_r_2nd_fdm_coef_cheby(kr, dr_1, d2r_1, mat_fdm)
!
      use cal_inverse_small_matrix
!
      integer(kind = kint), intent(in) :: kr
      real(kind = kreal), intent(in) :: dr_1, d2r_1
      real(kind = kreal), intent(inout) :: mat_fdm(3,3)
!
      real(kind = kreal) :: mat_taylor_3(3,3)
      integer(kind = kint) :: ierr
!
!
      mat_taylor_3(1,1) = one
      mat_taylor_3(1,2) = zero
      mat_taylor_3(1,3) = zero
!
      mat_taylor_3(2,1) = one
      mat_taylor_3(2,2) = dr_1 + half * d2r_1
      mat_taylor_3(2,3) = half * dr_1*dr_1
!
      mat_taylor_3(3,1) = one
      mat_taylor_3(3,2) =-dr_1 + half * d2r_1
      mat_taylor_3(3,3) = half * dr_1*dr_1
!
      call cal_inverse_33_matrix(mat_taylor_3, mat_fdm, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix at nod_r_2nd_fdm_coef_cheby', kr
      end if
!
      end subroutine nod_r_2nd_fdm_coef_cheby
!
! -----------------------------------------------------------------------
!
      end module set_radius_func_cheby
