!>@file   m_vp_coef_fdm4_nonslip_CMB.f90
!!@brief  module m_vp_coef_fdm4_nonslip_CMB
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative for non-slip at CMB
!!
!!@verbatim
!!      subroutine cal_4th_CMB_nonslip_vp_fdm(r_from_CMB)
!!      subroutine cal_4th_CMB1_nonslip_vp_fdm(r_from_CMB)
!!
!!      subroutine check_4th_CMB_nonslip_vp_fdm
!!
!!   Matrix for poloidal velocity with non-slip boundary at CMB
!!      d2fdr2 =    coef_fdm_noslip_CMB_4(-2,3) * d_rj(CMB-2)
!!                + coef_fdm_noslip_CMB_4(-1,3) * d_rj(CMB-1)
!!                + coef_fdm_noslip_CMB_4( 0,3) * d_rj(CMB  )
!!      d3fdr3 =    coef_fdm_noslip_CMB_4(-2,4) * d_rj(CMB-2)
!!                + coef_fdm_noslip_CMB_4(-1,4) * d_rj(CMB-1)
!!                + coef_fdm_noslip_CMB_4( 0,4) * d_rj(CMB  )
!!
!!   Matrix for poloidal velocity with non-slip boundary at next of CMB
!!      dfdr =      coef_fdm_noslip_CMB1_4(-2,2) * d_rj(CMB-3)
!!                + coef_fdm_noslip_CMB1_4(-1,2) * d_rj(CMB-2)
!!                + coef_fdm_noslip_CMB1_4( 0,2) * d_rj(CMB-1)
!!                + coef_fdm_noslip_CMB1_4( 1,2) * d_rj(CMB  )
!!      d2fdr2 =    coef_fdm_noslip_CMB1_4(-2,3) * d_rj(CMB-3)
!!                + coef_fdm_noslip_CMB1_4(-1,3) * d_rj(CMB-2)
!!                + coef_fdm_noslip_CMB1_4( 0,3) * d_rj(CMB-1)
!!                + coef_fdm_noslip_CMB1_4( 1,3) * d_rj(CMB  )
!!      d3fdr3 =    coef_fdm_noslip_CMB1_4(-2,4) * d_rj(CMB-3)
!!                + coef_fdm_noslip_CMB1_4(-1,4) * d_rj(CMB-2)
!!                + coef_fdm_noslip_CMB1_4( 0,4) * d_rj(CMB-1)
!!                + coef_fdm_noslip_CMB1_4( 1,4) * d_rj(CMB  )
!!      d4fdr4 =    coef_fdm_noslip_CMB1_4(-2,5) * d_rj(CMB-3)
!!                + coef_fdm_noslip_CMB1_4(-1,5) * d_rj(CMB-2)
!!                + coef_fdm_noslip_CMB1_4( 0,5) * d_rj(CMB-1)
!!                + coef_fdm_noslip_CMB1_4( 1,5) * d_rj(CMB  )
!!@endverbatim
!!
!!@n @param r_from_CMB(-3:0) radius from three next points to CMB
!!
      module m_vp_coef_fdm4_nonslip_CMB
!
      use m_precision
!
      use m_constants
      use cal_inverse_small_matrix
!
      implicit none
!
!
!>      Matrix to evaluate radial derivative at CMB with non-slip BC
      real(kind = kreal) :: coef_fdm_noslip_CMB_4(-2:0,3:4)
!
!>      Matrix to evaluate radial derivative at next of CMB
!!      with non-slip BC
      real(kind = kreal) :: coef_fdm_noslip_CMB1_4(-2:1,5)
!
!
!>      Work matrix to evaluate coef_fdm_noslip_CMB_4
!!@verbatim
!!      d2fdr2 =    mat_fdm_noslip_CMB_4(3,4) * d_rj(CMB-2)
!!                + mat_fdm_noslip_CMB_4(3,3) * d_rj(CMB-1)
!!                + mat_fdm_noslip_CMB_4(3,1) * d_rj(CMB  )
!!                + mat_fdm_noslip_CMB_4(3,2) * dfdr(CMB)
!!      d3fdr3 =    mat_fdm_noslip_CMB_4(4,4) * d_rj(CMB-2)
!!                + mat_fdm_noslip_CMB_4(4,3) * d_rj(CMB-1)
!!                + mat_fdm_noslip_CMB_4(4,1) * d_rj(CMB  )
!!                + mat_fdm_noslip_CMB_4(4,2) * dfdr(CMB)
!!      d4fdr4 =    mat_fdm_noslip_CMB_4(5,4) * d_rj(CMB-2)
!!                + mat_fdm_noslip_CMB_4(5,3) * d_rj(CMB-1)
!!                + mat_fdm_noslip_CMB_4(5,1) * d_rj(CMB  )
!!                + mat_fdm_noslip_CMB_4(5,2) * dfdr(CMB)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_noslip_CMB_4(4,4)
!
!>      Work matrix to evaluate coef_fdm_noslip_CMB1_4
!!@verbatim
!!      dfdr =      mat_fdm_noslip_CMB1_4(2,5) * d_rj(CMB-3)
!!                + mat_fdm_noslip_CMB1_4(2,4) * d_rj(CMB-2)
!!                + mat_fdm_noslip_CMB1_4(2,1) * d_rj(CMB-1)
!!                + mat_fdm_noslip_CMB1_4(2,3) * d_rj(CMB  )
!!                + mat_fdm_noslip_CMB1_4(2,2) * dfdr(CMB)
!!      d2fdr2 =    mat_fdm_noslip_CMB1_4(3,5) * d_rj(CMB-3)
!!                + mat_fdm_noslip_CMB1_4(3,4) * d_rj(CMB-2)
!!                + mat_fdm_noslip_CMB1_4(3,1) * d_rj(CMB-1)
!!                + mat_fdm_noslip_CMB1_4(3,3) * d_rj(CMB  )
!!                + mat_fdm_noslip_CMB1_4(3,2) * dfdr(CMB)
!!      d3fdr3 =    mat_fdm_noslip_CMB1_4(4,5) * d_rj(CMB-3)
!!                + mat_fdm_noslip_CMB1_4(4,4) * d_rj(CMB-2)
!!                + mat_fdm_noslip_CMB1_4(4,1) * d_rj(CMB-1)
!!                + mat_fdm_noslip_CMB1_4(4,3) * d_rj(CMB  )
!!                + mat_fdm_noslip_CMB1_4(4,2) * dfdr(CMB)
!!      d4fdr4 =    mat_fdm_noslip_CMB1_4(5,5) * d_rj(CMB-3)
!!                + mat_fdm_noslip_CMB1_4(5,4) * d_rj(CMB-2)
!!                + mat_fdm_noslip_CMB1_4(5,1) * d_rj(CMB-1)
!!                + mat_fdm_noslip_CMB1_4(5,3) * d_rj(CMB  )
!!                + mat_fdm_noslip_CMB1_4(5,2) * dfdr(CMB)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_noslip_CMB1_4(5,5)
!
      private :: mat_fdm_noslip_CMB_4, mat_fdm_noslip_CMB1_4
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_4th_CMB_nonslip_vp_fdm(r_from_CMB)
!
      real(kind = kreal) :: r_from_CMB(-3:0)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_4(4,4)
      real(kind = kreal) :: dr_n1, dr_n2, dr_n3
!
!
      dr_n1 = r_from_CMB(0) - r_from_CMB(-1)
      dr_n2 = r_from_CMB(0) - r_from_CMB(-2)
      dr_n3 = r_from_CMB(0) - r_from_CMB(-3)
!
      mat_taylor_4(1,1) = one
      mat_taylor_4(1,2) = zero
      mat_taylor_4(1,3) = zero
      mat_taylor_4(1,4) = zero
!
      mat_taylor_4(2,1) = zero
      mat_taylor_4(2,2) =  one
      mat_taylor_4(2,3) = zero
      mat_taylor_4(2,4) = zero
!
      mat_taylor_4(3,1) = one
      mat_taylor_4(3,2) =-dr_n1
      mat_taylor_4(3,3) = dr_n1*dr_n1 / two
      mat_taylor_4(3,4) =-dr_n1**3 / six
!
      mat_taylor_4(4,1) = one
      mat_taylor_4(4,2) =-dr_n2
      mat_taylor_4(4,3) = dr_n2*dr_n2 / two
      mat_taylor_4(4,4) =-dr_n2**3 / six
!
      call cal_inverse_44_matrix(mat_taylor_4,                          &
     &    mat_fdm_noslip_CMB_4, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_4th_CMB_nonslip_vp_fdm ',       &
     &            r_from_CMB(0)
      end if
!
      coef_fdm_noslip_CMB_4(-2,3:4) = mat_fdm_noslip_CMB_4(3:4,4)
      coef_fdm_noslip_CMB_4(-1,3:4) = mat_fdm_noslip_CMB_4(3:4,3)
      coef_fdm_noslip_CMB_4( 0,3:4) = mat_fdm_noslip_CMB_4(3:4,1)
!
      end subroutine cal_4th_CMB_nonslip_vp_fdm
!
! -----------------------------------------------------------------------
!
      subroutine cal_4th_CMB1_nonslip_vp_fdm(r_from_CMB)
!
      real(kind = kreal) :: r_from_CMB(-3:0)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_5(5,5)
      real(kind = kreal) :: dr_p1, dr_n1, dr_n2
!
!
      dr_p1 = r_from_CMB( 0) - r_from_CMB(-1)
      dr_n1 = r_from_CMB(-1) - r_from_CMB(-2)
      dr_n2 = r_from_CMB(-1) - r_from_CMB(-3)
!
      mat_taylor_5(1,1) = one
      mat_taylor_5(1,2) = zero
      mat_taylor_5(1,3) = zero
      mat_taylor_5(1,4) = zero
      mat_taylor_5(1,5) = zero
!
      mat_taylor_5(2,1) = zero
      mat_taylor_5(2,2) =  one
      mat_taylor_5(2,3) = dr_p1
      mat_taylor_5(2,4) = dr_p1*dr_p1 / two
      mat_taylor_5(2,5) = dr_p1**3 / six
!
      mat_taylor_5(3,1) = one
      mat_taylor_5(3,2) = dr_p1
      mat_taylor_5(3,3) = dr_p1*dr_p1 / two
      mat_taylor_5(3,4) = dr_p1**3 / six
      mat_taylor_5(3,5) = dr_p1**4 / (six*four)
!
      mat_taylor_5(4,1) = one
      mat_taylor_5(4,2) =-dr_n1
      mat_taylor_5(4,3) = dr_n1*dr_n1 / two
      mat_taylor_5(4,4) =-dr_n1**3 / six
      mat_taylor_5(4,5) = dr_n1**4 / (six*four)
!
      mat_taylor_5(5,1) = one
      mat_taylor_5(5,2) =-dr_n2
      mat_taylor_5(5,3) = dr_n2*dr_n2 / two
      mat_taylor_5(5,4) =-dr_n2**3 / six
      mat_taylor_5(5,5) = dr_n2**4 / (six*four)
!
      call cal_inverse_nn_matrix(ifive, mat_taylor_5,                   &
     &    mat_fdm_noslip_CMB1_4, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix mat_fdm_noslip_CMB1_4 ',            &
     &             r_from_CMB(0)
      end if
!
      coef_fdm_noslip_CMB1_4(-2,2:5) = mat_fdm_noslip_CMB1_4(2:5,5)
      coef_fdm_noslip_CMB1_4(-1,2:5) = mat_fdm_noslip_CMB1_4(2:5,4)
      coef_fdm_noslip_CMB1_4( 0,2:5) = mat_fdm_noslip_CMB1_4(2:5,1)
      coef_fdm_noslip_CMB1_4( 1,2:5) = mat_fdm_noslip_CMB1_4(2:5,3)
!
      end subroutine cal_4th_CMB1_nonslip_vp_fdm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_4th_CMB_nonslip_vp_fdm
!
!
      write(50,*) ' coef_fdm_noslip_CMB_4'
      write(50,*) 'mattix for d2fdr2'
      write(50,'(1p9E25.15e3)') coef_fdm_noslip_CMB_4(-2:0,3)
      write(50,*) 'mattix for d3fdr3
      write(50,'(1p9E25.15e3)') coef_fdm_noslip_CMB_4(-2:0,4)
!
      write(50,*) ' coef_fdm_noslip_CMB1_4'
      write(50,*) 'mattix for dfdr'
      write(50,'(1p9E25.15e3)') coef_fdm_noslip_CMB1_4(-2:1,2)
      write(50,*) 'mattix for d2fdr2'
      write(50,'(1p9E25.15e3)') coef_fdm_noslip_CMB1_4(-2:1,3)
      write(50,*) 'mattix for d3fdr3'
      write(50,'(1p9E25.15e3)') coef_fdm_noslip_CMB1_4(-2:1,4)
      write(50,*) 'mattix for d4fdr4'
      write(50,'(1p9E25.15e3)') coef_fdm_noslip_CMB1_4(-2:1,5)
!
      end subroutine check_4th_CMB_nonslip_vp_fdm
!
! -----------------------------------------------------------------------
!
      end module m_vp_coef_fdm4_nonslip_CMB
