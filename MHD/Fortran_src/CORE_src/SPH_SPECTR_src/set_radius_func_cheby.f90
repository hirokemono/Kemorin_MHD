!>@file   set_radius_func_cheby.f90
!!@brief  module set_radius_func_cheby
!!
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n      modified by H. Matsui in Apr., 2009
!!@n      modified by H. Matsui in Apr., 2013
!!
!!@verbatim
!!      subroutine deallocate_dr_rj_cheby
!!
!!      subroutine set_dr_for_cheby                                     &
!!     &         (nlayer_ICB, nlayer_CMB, nri, radius_1d_rj_r)
!!      subroutine nod_r_2nd_fdm_coefs_cheby                            &
!!     &        (nlayer_ICB, nlayer_CMB, nri, radius_1d_rj_r, mat_fdm_2)
!!
!!**********************************************************************
!!
!!    delta r for Chebyshev grid
!!     dr_1d_rj(k)   : (dr/dn)
!!
!!     drdn_rj(k,1)   : (dr/dn)
!!     drdn_rj(k,2)   : (d^2r/dn^2)
!!     drdn_rj(k,3)   : (d^3r/dn^3)
!!     drdn_rj(k,4)   : (d^4r/dn^4)
!!
!!   Inner core (nlayer_CMB < k < nlayer_CMB)
!!     r(k,0) = ri - 0.5 * L* ( 1-cos(pi* (k-nlayer_ICB) /nele_r) )
!!   outer core (nlayer_ICB < k < nlayer_CMB)
!!     r(k,0) = ri + 0.5 * L* ( 1-cos(pi* (k-nlayer_ICB) /nele_r) )
!!   External (nlayer_CMB < k < nlayer_CMB + (nlayer_CMB-nlayer_ICB)/2 )
!!     r(k,0) = ro + 0.5 * L* ( 1-cos(pi* (k-nlayer_CMB) /nele_r) )
!!
!!**********************************************************************
!!@endverbatim
!
      module set_radius_func_cheby
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!>      1d @f$ \Delta r @f$ for @f$ f(r,j) @f$
!!@n@see  set_radius_func_cheby or set_radius_func_cheby
      real(kind = kreal), allocatable :: dr_1d_rj(:)
!
!>      1d @f$ \frac{\partial r}{\partial n} @f$ for @f$ f(r,j) @f$
      real(kind = kreal), allocatable :: drdn_rj(:,:)
!
      private :: dr_1d_rj
      private :: nod_r_2nd_fdm_coef_cheby
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine deallocate_dr_rj_cheby
!
!
      deallocate(dr_1d_rj, drdn_rj)
!
      end subroutine deallocate_dr_rj_cheby
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_dr_for_cheby                                       &
     &         (nlayer_ICB, nlayer_CMB, nri, radius_1d_rj_r)
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB, nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!
      real(kind = kreal) ::  pi
      real(kind = kreal) ::  shell
      integer(kind = kint) :: k, kst, ked, kr, nele_r
!
!
      allocate(dr_1d_rj(nri))
      allocate(drdn_rj(nri,4))
      dr_1d_rj = 0.0d0
      drdn_rj = 0.0d0
!
      nele_r = nlayer_CMB - nlayer_ICB
      shell = radius_1d_rj_r(nlayer_CMB) - radius_1d_rj_r(nlayer_ICB)
      pi = four * atan(one)
!
!* ----------  center  --------
!*
      dr_1d_rj(1) = radius_1d_rj_r(2) - radius_1d_rj_r(1)
!*
!* ----------  inner core --------
!*
      do kr = 2, nlayer_ICB-1
        k = kr - nlayer_ICB
!*
        drdn_rj(kr,1) =   (half*shell*pi/ dble(nele_r))                 &
     &                  * (-sin( pi*dble(k)/dble(nele_r) ) )
        drdn_rj(kr,2) =   (half*shell*pi*pi/ dble(nele_r*nele_r))       &
     &                  * (-cos( pi*dble(k)/dble(nele_r) ) )
        drdn_rj(kr,3) =   (half*shell*pi**3/ dble(nele_r**3))           &
     &                  * ( sin( pi*dble(k)/dble(nele_r) ) )
        drdn_rj(kr,4) =   (half*shell*pi**4/ dble(nele_r**4))           &
     &                  * ( cos( pi*dble(k)/dble(nele_r) ) )
!
        dr_1d_rj(k) = radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr)
      end do
!
!* ----------  inner core boundary --------
!*
      dr_1d_rj(nlayer_ICB) = radius_1d_rj_r(nlayer_ICB+1)               &
     &                      - radius_1d_rj_r(nlayer_ICB)
!
!* ----------  outer core --------
!*
      do kr = nlayer_ICB+1, nlayer_CMB-1
        k = kr - nlayer_ICB
!*
        drdn_rj(kr,1) =   (half*shell*pi/ dble(nele_r))                 &
     &                  * ( sin( pi*dble(k)/dble(nele_r) ) )
        drdn_rj(kr,2) =   (half*shell*pi*pi/ dble(nele_r*nele_r))       &
     &                  * ( cos( pi*dble(k)/dble(nele_r) ) )
        drdn_rj(kr,3) =   (half*shell*pi**3/ dble(nele_r**3))           &
     &                  * (-sin( pi*dble(k)/dble(nele_r) ) )
        drdn_rj(kr,4) =   (half*shell*pi**4/ dble(nele_r**4))           &
     &                  * (-cos( pi*dble(k)/dble(nele_r) ) )
!
        dr_1d_rj(k) = radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr)
      end do
!*
!* ----------  core mantle boundary --------
!*
      dr_1d_rj(nlayer_CMB) =  radius_1d_rj_r(nlayer_CMB)                &
     &                        - radius_1d_rj_r(nlayer_CMB-1)
      if (nlayer_CMB .eq. nri) go to 10
!
!* ---------- outer boundary of domain --------
!*
      kr = nri
      dr_1d_rj(kr) = radius_1d_rj_r(kr) - radius_1d_rj_r(kr-1)
!*
!* ----------  external of shell --------
!*
      kst = nlayer_CMB + 1
      ked = nlayer_CMB + (nlayer_CMB - nlayer_ICB)/2 - 1
      do kr = kst, min(ked,nri-1)
        k = kr - nlayer_CMB
!*
        drdn_rj(kr,1) =   (half*shell*pi/ dble(nele_r))                 &
     &                  * ( sin( pi*dble(k)/dble(nele_r) ) )
        drdn_rj(kr,2) =   (half*shell*pi*pi/ dble(nele_r*nele_r))       &
     &                  * ( cos( pi*dble(k)/dble(nele_r) ) )
        drdn_rj(kr,3) =   (half*shell*pi**3/ dble(nele_r**3))           &
     &                  * (-sin( pi*dble(k)/dble(nele_r) ) )
        drdn_rj(kr,4) =   (half*shell*pi**4/ dble(nele_r**4))           &
     &                  * (-cos( pi*dble(k)/dble(nele_r) ) )
!
        dr_1d_rj(k) = radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr)
      end do
      if (ked .gt. nri-1) go to 10
!*
!
      kst = nlayer_CMB + (nlayer_CMB - nlayer_ICB)/2
      ked = nri - 1
      do kr = kst, ked
        dr_1d_rj(kr) = radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr)
      end do
!
 10   continue
!
      end subroutine set_dr_for_cheby
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine nod_r_2nd_fdm_coefs_cheby                              &
     &        (nlayer_ICB, nlayer_CMB, nri, radius_1d_rj_r, mat_fdm_2)
!
      use set_radius_func_noequi
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB, nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      real(kind = kreal), intent(inout) :: mat_fdm_2(3,3,nri)
!
      integer(kind = kint) :: kr, kst, ked
!
!
      if(nlayer_ICB .gt. 1) then
        call nod_r_2nd_fdm_coef_noequi(ione, dr_1d_rj(1),               &
     &      radius_1d_rj_r(1), mat_fdm_2(1,1,1))
      end if
!
      do kr = 2, nlayer_ICB-1
        call nod_r_2nd_fdm_coef_cheby(kr, mat_fdm_2(1,1,kr) )
      end do
!
      call nod_r_2nd_fdm_coef_noequi(nlayer_ICB,                        &
     &    dr_1d_rj(nlayer_ICB), dr_1d_rj(nlayer_ICB),                   &
     &    mat_fdm_2(1,1,nlayer_ICB) )
!*
!* ----------  outer core --------
!*
      do kr = nlayer_ICB+1, nlayer_CMB-1
        call nod_r_2nd_fdm_coef_cheby(kr, mat_fdm_2(1,1,kr) )
      end do
!*
!* ----------  core mantle boundary --------
!*
      call nod_r_2nd_fdm_coef_noequi(nlayer_CMB,                        &
     &    dr_1d_rj(nlayer_CMB), dr_1d_rj(nlayer_CMB),                   &
     &    mat_fdm_2(1,1,nlayer_CMB) )
      if (nlayer_CMB .eq. nri) return
!
!* ---------- outer boundary of domains --------
!
      kr = nri
      call nod_r_2nd_fdm_coef_noequi(nri, dr_1d_rj(kr),                 &
     &    dr_1d_rj(kr), mat_fdm_2(1,1,kr))
!
!* ----------  external of shell --------
!*
      kst = nlayer_CMB + 1
      ked = nlayer_CMB + (nlayer_CMB - nlayer_ICB)/2 - 1
      do kr = kst, min(ked,nri-1)
        call nod_r_2nd_fdm_coef_cheby(kr, mat_fdm_2(1,1,kr) )
      end do
!
      if (ked .gt. nri-1) return
!
      kst = nlayer_CMB + (nlayer_CMB - nlayer_ICB)/2
      ked = nri - 1
      do kr = kst, ked
        call nod_r_2nd_fdm_coef_noequi                                  &
     &     (kr, dr_1d_rj(kr), dr_1d_rj(kr-1), mat_fdm_2(1,1,kr) )
      end do
!
      end subroutine nod_r_2nd_fdm_coefs_cheby
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine nod_r_2nd_fdm_coef_cheby(kr, mat_fdm)
!
      use m_taylor_exp_matrix
      use small_mat_mat_product
      use cal_trans_mat_dfdr_2_dfdn
      use cal_inverse_small_matrix
!
      integer(kind = kint), intent(in) :: kr
      real(kind = kreal), intent(inout) :: mat_fdm(3,3)
!
      real(kind = kreal) :: fdm2_mat_dr_to_dn(3,3)
      real(kind = kreal) :: mat_taylor_r3(3,3)
      integer(kind = kint) :: ierr
!
!
      call set_fdm2_dfdn_matrix(drdn_rj(kr,1), drdn_rj(kr,2),         &
     &    fdm2_mat_dr_to_dn)
      call mat_3x3_product(Taylor_exp_fdm2, fdm2_mat_dr_to_dn,        &
     &    mat_taylor_r3)
      call cal_inverse_33_matrix(mat_taylor_r3, mat_fdm, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix at nod_r_2nd_fdm_coef_cheby', kr
      end if
!
      if(kr .le. 5) then
        write(*,*) 'Cgebyshev matrix fdm2_mat_dr_to_dn', kr
        write(*,*) fdm2_mat_dr_to_dn(1,1:3)
        write(*,*) fdm2_mat_dr_to_dn(2,1:3)
        write(*,*) fdm2_mat_dr_to_dn(3,1:3)
        write(*,*) 'Taylor expansion matrix', kr
        write(*,*) Taylor_exp_fdm2(1,1:3)
        write(*,*) Taylor_exp_fdm2(2,1:3)
        write(*,*) Taylor_exp_fdm2(3,1:3)
        write(*,*) 'FDM source matrix', kr
        write(*,*) mat_taylor_r3(1,1:3)
        write(*,*) mat_taylor_r3(2,1:3)
        write(*,*) mat_taylor_r3(3,1:3)
        write(*,*) 'Inverse matrix', kr
        write(*,*) mat_fdm(1,1:3)
        write(*,*) mat_fdm(2,1:3)
        write(*,*) mat_fdm(3,1:3)
      end if
!
      end subroutine nod_r_2nd_fdm_coef_cheby
!
! -----------------------------------------------------------------------
!
      end module set_radius_func_cheby
