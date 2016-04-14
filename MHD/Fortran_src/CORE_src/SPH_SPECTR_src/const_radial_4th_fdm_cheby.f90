!>@file   const_radial_4th_fdm_cheby.f90
!!@brief  module const_radial_4th_fdm_cheby
!!
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n      modified by H. Matsui in Apr., 2009
!!@n      modified by H. Matsui in Apr., 2013
!!
!!@verbatim
!!      subroutine nod_r_4th_fdm_coefs_cheby                            &
!!     &         (nlayer_ICB, nlayer_CMB, nri, radius_1d_rj_r)
!!
!!**********************************************************************
!!
!!    delta r for Chebyshev grid
!!     drdn_rj(k,1)   : (dr/dn)
!!     drdn_rj(k,2)   : (d^2r/dn^2)
!!     drdn_rj(k,3)   : (d^3r/dn^3)
!!     drdn_rj(k,4)   : (d^4r/dn^4)
!!
!!   Inner core (nlayer_CMB < k < nlayer_CMB)
!!     r(k,0) = ri - 0.5 * L* ( 1-cos(pi* (k-nlayer_ICB) /nri) )
!!   outer core (nlayer_ICB < k < nlayer_CMB)
!!     r(k,0) = ri + 0.5 * L* ( 1-cos(pi* (k-nlayer_ICB) /nri) )
!!   External (nlayer_CMB < k < nlayer_CMB + (nlayer_CMB-nlayer_ICB)/2 )
!!     r(k,0) = ro + 0.5 * L* ( 1-cos(pi* (k-nlayer_CMB) /nri) )
!!
!!**********************************************************************
!!@endverbatim
!
      module const_radial_4th_fdm_cheby
!
      use m_precision
!
      use m_constants
!
      implicit none
!
      private :: nod_r_4th_fdm_coef_cheby
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine nod_r_4th_fdm_coefs_cheby                              &
     &         (nlayer_ICB, nlayer_CMB, nri, radius_1d_rj_r)
!
      use m_fdm_4th_coefs
      use const_radial_4th_fdm_noequi
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!
      integer(kind = kint) :: kr, kst, ked
      real(kind = kreal) :: dr_p1, dr_n1, dr_p2, dr_n2
!
!
      dr_p1 = radius_1d_rj_r(2) - radius_1d_rj_r(1)
      dr_p2 = radius_1d_rj_r(3) - radius_1d_rj_r(1)
      call nod_r_4th_fdm_coef_noequi(ione,                              &
     &       dr_p1, dr_p1, dr_p2, dr_p2, mat_fdm_4(1,1,1) )
!
      dr_p1 = radius_1d_rj_r(3) - radius_1d_rj_r(2)
      dr_p2 = radius_1d_rj_r(4) - radius_1d_rj_r(2)
      dr_n1 = radius_1d_rj_r(2) - radius_1d_rj_r(1)
      call nod_r_4th_fdm_coef_noequi(itwo,                              &
     &       dr_p1, dr_n1, dr_p2, dr_p2, mat_fdm_4(1,1,2) )
!
      if(nlayer_ICB .gt. 1) then
        dr_p1 = radius_1d_rj_r(2) - radius_1d_rj_r(1)
        dr_p2 = radius_1d_rj_r(3) + radius_1d_rj_r(1)
        dr_n2 = radius_1d_rj_r(1) + radius_1d_rj_r(1)
        call nod_r_4th_fdm_coef_noequi(ione, dr_p1, radius_1d_rj_r(1),  &
     &      dr_p2, dr_n2, mat_fdm_4(1,1,1))
!
        dr_p1 = radius_1d_rj_r(3) - radius_1d_rj_r(2)
        dr_p2 = radius_1d_rj_r(4) - radius_1d_rj_r(2)
        dr_n1 = radius_1d_rj_r(2) - radius_1d_rj_r(1)
        dr_n2 = radius_1d_rj_r(2)
        call nod_r_4th_fdm_coef_noequi(itwo, dr_p1, dr_n1,              &
     &      dr_p2, dr_n2, mat_fdm_4(1,1,2))
      end if
!
      do kr = 3, nlayer_ICB-2
        call nod_r_4th_fdm_coef_cheby(kr, mat_fdm_4(1,1,kr) )
      end do
!
      do kr = nlayer_ICB-1, nlayer_ICB+1
        if(kr-2 .ge. 1) then
          dr_p1 = radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr  )
          dr_n1 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-1)
          dr_p2 = radius_1d_rj_r(kr+2) - radius_1d_rj_r(kr  )
          dr_n2 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-2)
          call nod_r_4th_fdm_coef_noequi(kr,                            &
     &       dr_p1, dr_n1, dr_p2, dr_n2, mat_fdm_4(1,1,kr) )
        end if
      end do
!*
!* ----------  outer core --------
!*
      do kr = nlayer_ICB+2, nlayer_CMB-2
        call nod_r_4th_fdm_coef_cheby(kr, mat_fdm_4(1,1,kr) )
      end do
!*
!* ----------  core mantle boundary --------
!*
      do kr = nlayer_CMB-1, nlayer_ICB+1
        if(kr+2 .le. nri) then
          dr_p1 = radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr  )
          dr_n1 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-1)
          dr_p2 = radius_1d_rj_r(kr+2) - radius_1d_rj_r(kr  )
          dr_n2 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-2)
          call nod_r_4th_fdm_coef_noequi(kr,                            &
     &       dr_p1, dr_n1, dr_p2, dr_n2, mat_fdm_4(1,1,kr) )
        end if
      end do
!
!* ----------  external of shell --------
!*
      kst = nlayer_CMB + 2
      ked = nlayer_CMB + (nlayer_CMB - nlayer_ICB)/2 - 2
      do kr = kst, min(ked,nri-1)
        call nod_r_4th_fdm_coef_cheby(kr, mat_fdm_4(1,1,kr) )
      end do
!
      kst = nlayer_CMB + (nlayer_CMB - nlayer_ICB)/2 - 1
      ked = nri - 2
      do kr = kst, ked
        dr_p1 = radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr  )
        dr_n1 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-1)
        dr_p2 = radius_1d_rj_r(kr+2) - radius_1d_rj_r(kr  )
        dr_n2 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-2)
        call nod_r_4th_fdm_coef_noequi                                  &
     &     (kr, dr_p1, dr_n1, dr_p2, dr_n2, mat_fdm_4(1,1,kr) )
      end do
!
!* ---------- outer boundary of domains --------
!
      kr = nri - 1
      dr_p1 = radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr  )
      dr_n1 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-1)
      dr_n2 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-2)
      call nod_r_4th_fdm_coef_noequi(kr,                                &
     &       dr_p1, dr_n1, dr_n2, dr_n2, mat_fdm_4(1,1,kr) )
!
      kr = nri
      dr_n1 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-1)
      dr_n2 = radius_1d_rj_r(kr  ) - radius_1d_rj_r(kr-2)
      call nod_r_4th_fdm_coef_noequi(kr,                                &
     &       dr_n1, dr_n1, dr_n2, dr_n2, mat_fdm_4(1,1,kr) )
!
      end subroutine nod_r_4th_fdm_coefs_cheby
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine nod_r_4th_fdm_coef_cheby(kr, mat_fdm)
!
      use m_taylor_exp_matrix
      use small_mat_mat_product
      use cal_trans_mat_dfdr_2_dfdn
      use cal_inverse_small_matrix
      use set_radius_func_cheby
!
      integer(kind = kint), intent(in) :: kr
      real(kind = kreal), intent(inout) :: mat_fdm(5,5)
!
      real(kind = kreal) :: fdm4_mat_dr_to_dn(5,5)
      real(kind = kreal) :: mat_taylor_r5(5,5)
      integer(kind = kint) :: ierr
!
!
      call set_fdm4_dfdn_matrix(drdn_rj(kr,1), drdn_rj(kr,2),         &
     &    drdn_rj(kr,3), drdn_rj(kr,4), fdm4_mat_dr_to_dn)
      call mat_5x5_product(Taylor_exp_fdm4, fdm4_mat_dr_to_dn,        &
     &    mat_taylor_r5)
      call cal_inverse_nn_matrix(ifive, mat_taylor_r5, mat_fdm, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix at nod_r_2nd_fdm_coef_cheby', kr
      end if
!
      end subroutine nod_r_4th_fdm_coef_cheby
!
! -----------------------------------------------------------------------
!
      end module const_radial_4th_fdm_cheby
