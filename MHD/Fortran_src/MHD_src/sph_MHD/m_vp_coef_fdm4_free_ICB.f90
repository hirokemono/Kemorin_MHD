!>@file   m_vp_coef_fdm4_free_ICB.f90
!!@brief  module m_vp_coef_fdm4_free_ICB
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative for free-slip at ICB
!!
!!@verbatim
!!      subroutine cal_4th_ICB_free_vp_fdm(r_from_ICB)
!!      subroutine cal_4th_ICB1_free_vp_fdm(r_from_ICB)
!!
!!      subroutine check_4th_ICB_free_vp_fdm
!!
!!   Matrix for poloidal velocity with free-slip boundary at ICB
!!      dfdr =      coef_fdm_free_ICB_vp4( 2,2) * d_rj(ICB+2)
!!                + coef_fdm_free_ICB_vp4( 1,2) * d_rj(ICB+1)
!!                + coef_fdm_free_ICB_vp4( 0,2) * d_rj(ICB  )
!!      d2fdr2 =    coef_fdm_free_ICB_vp4( 2,3) * d_rj(ICB+2)
!!                + coef_fdm_free_ICB_vp4( 1,3) * d_rj(ICB+1)
!!                + coef_fdm_free_ICB_vp4( 0,3) * d_rj(ICB  )
!!      d3fdr3 =    coef_fdm_free_ICB_vp4( 2,4) * d_rj(ICB+2)
!!                + coef_fdm_free_ICB_vp4( 1,4) * d_rj(ICB+1)
!!                + coef_fdm_free_ICB_vp4( 0,4) * d_rj(ICB  )
!!
!!   Matrix for poloidal velocity with free-slip boundary at next of ICB
!!      dfdr =      coef_fdm_free_ICB1_vp4( 2,2) * d_rj(ICB+3)
!!                + coef_fdm_free_ICB1_vp4( 1,2) * d_rj(ICB+2)
!!                + coef_fdm_free_ICB1_vp4( 0,2) * d_rj(ICB+1)
!!                + coef_fdm_free_ICB1_vp4(-1,2) * d_rj(ICB  )
!!      d2fdr2 =    coef_fdm_free_ICB1_vp4( 2,3) * d_rj(ICB+3)
!!                + coef_fdm_free_ICB1_vp4( 1,3) * d_rj(ICB+2)
!!                + coef_fdm_free_ICB1_vp4( 0,3) * d_rj(ICB+1)
!!                + coef_fdm_free_ICB1_vp4(-1,3) * d_rj(ICB  )
!!      d3fdr3 =    coef_fdm_free_ICB1_vp4( 2,4) * d_rj(ICB+3)
!!                + coef_fdm_free_ICB1_vp4( 1,4) * d_rj(ICB+2)
!!                + coef_fdm_free_ICB1_vp4( 0,4) * d_rj(ICB+1)
!!                + coef_fdm_free_ICB1_vp4(-1,4) * d_rj(ICB  )
!!      d4fdr4 =    coef_fdm_free_ICB1_vp4( 2,5) * d_rj(ICB+3)
!!                + coef_fdm_free_ICB1_vp4( 1,5) * d_rj(ICB+2)
!!                + coef_fdm_free_ICB1_vp4( 0,5) * d_rj(ICB+1)
!!                + coef_fdm_free_ICB1_vp4(-1,5) * d_rj(ICB  )
!!@endverbatim
!!
!!@n @param r_from_ICB(0:3) radius to three next points from ICB
!!
      module m_vp_coef_fdm4_free_ICB
!
      use m_precision
!
      use m_constants
      use cal_inverse_small_matrix
!
      implicit none
!
!
!>      Matrix to evaluate radial derivative at ICB with non-slip BC
      real(kind = kreal) :: coef_fdm_free_ICB_vp4(0:2,4)
!
!>      Matrix to evaluate radial derivative at next of ICB
!!      with non-slip BC
      real(kind = kreal) :: coef_fdm_free_ICB1_vp4(-1:2,5)
!
!
!>      Work matrix to evaluate coef_fdm_free_ICB_vp4(0:2,3)
!!@verbatim
!!      dfdr =      mat_fdm4_ICB_free_vp(2,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB_free_vp(2,3) * d_rj(ICB+1)
!!                + mat_fdm4_ICB_free_vp(2,1) * d_rj(ICB  )
!!                + mat_fdm4_ICB_free_vp(2,2) * B.C. (=0)
!!      d2fdr2 =    mat_fdm4_ICB_free_vp(3,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB_free_vp(3,3) * d_rj(ICB+1)
!!                + mat_fdm4_ICB_free_vp(3,1) * d_rj(ICB  )
!!                + mat_fdm4_ICB_free_vp(3,2) * B.C. (=0)
!!      d3fdr3 =    mat_fdm4_ICB_free_vp(4,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB_free_vp(4,3) * d_rj(ICB+1)
!!                + mat_fdm4_ICB_free_vp(4,1) * d_rj(ICB  )
!!                + mat_fdm4_ICB_free_vp(4,2) * B.C. (=0)
!!@endverbatim
      real(kind = kreal) :: mat_fdm4_ICB_free_vp(4,4)
!
!>      Work matrix to evaluate coef_fdm_free_ICB1_vp4(-1:1,3)
!!@verbatim
!!      dfdr =      mat_fdm4_ICB1_free_vp(2,5) * d_rj(ICB+3)
!!                + mat_fdm4_ICB1_free_vp(2,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB1_free_vp(2,1) * d_rj(ICB+1)
!!                + mat_fdm4_ICB1_free_vp(2,3) * d_rj(ICB  )
!!                + mat_fdm4_ICB1_free_vp(2,2) * B.C. (=0)
!!      d2fdr2 =    mat_fdm4_ICB1_free_vp(3,5) * d_rj(ICB+3)
!!                + mat_fdm4_ICB1_free_vp(3,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB1_free_vp(3,1) * d_rj(ICB+1)
!!                + mat_fdm4_ICB1_free_vp(3,3) * d_rj(ICB  )
!!                + mat_fdm4_ICB1_free_vp(3,2) * B.C. (=0)
!!      d3fdr3 =    mat_fdm4_ICB1_free_vp(4,5) * d_rj(ICB+3)
!!                + mat_fdm4_ICB1_free_vp(4,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB1_free_vp(4,1) * d_rj(ICB+1)
!!                + mat_fdm4_ICB1_free_vp(4,3) * d_rj(ICB  )
!!                + mat_fdm4_ICB1_free_vp(4,2) * B.C. (=0)
!!      d4fdr4 =    mat_fdm4_ICB1_free_vp(5,5) * d_rj(ICB+3)
!!                + mat_fdm4_ICB1_free_vp(5,4) * d_rj(ICB+2)
!!                + mat_fdm4_ICB1_free_vp(5,1) * d_rj(ICB+1)
!!                + mat_fdm4_ICB1_free_vp(5,3) * d_rj(ICB  )
!!                + mat_fdm4_ICB1_free_vp(5,2) * B.C. (=0)
!!@endverbatim
      real(kind = kreal) :: mat_fdm4_ICB1_free_vp(5,5)
!
      private :: mat_fdm4_ICB_free_vp, mat_fdm4_ICB1_free_vp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_4th_ICB_free_vp_fdm(r_from_ICB)
!
      real(kind = kreal), intent(in) :: r_from_ICB(0:2)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_4(4,4)
      real(kind = kreal) :: dr_p1, dr_p2, r0
!
!
      r0 = r_from_ICB(0)
      dr_p1 = r_from_ICB(1) - r_from_ICB(0)
      dr_p2 = r_from_ICB(2) - r_from_ICB(0)
!
      mat_taylor_4(1,1) = one
      mat_taylor_4(1,2) = zero
      mat_taylor_4(1,3) = zero
      mat_taylor_4(1,4) = zero
!
      mat_taylor_4(2,1) =  zero
      mat_taylor_4(2,2) =  one
      mat_taylor_4(2,3) = -half * r0
      mat_taylor_4(2,4) =  zero
!
      mat_taylor_4(3,1) = one
      mat_taylor_4(3,2) = dr_p1
      mat_taylor_4(3,3) = half * dr_p1*dr_p1
      mat_taylor_4(3,4) = (one/six) * dr_p1**3
!
      mat_taylor_4(4,1) = one
      mat_taylor_4(4,2) = dr_p2
      mat_taylor_4(4,3) = half * dr_p2*dr_p2
      mat_taylor_4(4,4) = (one/six) * dr_p2**3
!
      call cal_inverse_44_matrix(mat_taylor_4, mat_fdm4_ICB_free_vp,    &
     &    ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_4th_ICB_free_vp_fdm ',          &
     &            r_from_ICB(0)
      end if
!
      coef_fdm_free_ICB_vp4(0,1) = one
      coef_fdm_free_ICB_vp4(1,1) = zero
      coef_fdm_free_ICB_vp4(2,1) = zero
      coef_fdm_free_ICB_vp4(0,2) = mat_fdm4_ICB_free_vp(2,1)
      coef_fdm_free_ICB_vp4(1,2) = mat_fdm4_ICB_free_vp(2,3)
      coef_fdm_free_ICB_vp4(2,2) = mat_fdm4_ICB_free_vp(2,4)
      coef_fdm_free_ICB_vp4(0,3) = mat_fdm4_ICB_free_vp(3,1)
      coef_fdm_free_ICB_vp4(1,3) = mat_fdm4_ICB_free_vp(3,3)
      coef_fdm_free_ICB_vp4(2,3) = mat_fdm4_ICB_free_vp(3,4)
      coef_fdm_free_ICB_vp4(0,4) = mat_fdm4_ICB_free_vp(4,1)
      coef_fdm_free_ICB_vp4(1,4) = mat_fdm4_ICB_free_vp(4,3)
      coef_fdm_free_ICB_vp4(2,4) = mat_fdm4_ICB_free_vp(4,4)
!
      end subroutine cal_4th_ICB_free_vp_fdm
!
! -----------------------------------------------------------------------
!
      subroutine cal_4th_ICB1_free_vp_fdm(r_from_ICB)
!
      real(kind = kreal), intent(in) :: r_from_ICB(0:3)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_5(5,5)
      real(kind = kreal) :: dr_p1, dr_n1, dr_p2, r0
!
!
      r0 = r_from_ICB(0)
      dr_n1 = r_from_ICB(1) - r_from_ICB(0)
      dr_p1 = r_from_ICB(2) - r_from_ICB(1)
      dr_p2 = r_from_ICB(3) - r_from_ICB(1)
!
      mat_taylor_5(1,1) = one
      mat_taylor_5(1,2) = zero
      mat_taylor_5(1,3) = zero
      mat_taylor_5(1,4) = zero
      mat_taylor_5(1,5) = zero
!
      mat_taylor_5(2,1) = zero
      mat_taylor_5(2,2) = one
      mat_taylor_5(2,3) = - (dr_n1 + half * r0)
      mat_taylor_5(2,4) = half * dr_n1 * (dr_n1 + r0)
      mat_taylor_5(2,5) = - dr_n1**2 * (dr_n1/six + r0/four)
!
      mat_taylor_5(3,1) = one
      mat_taylor_5(3,2) =-dr_n1
      mat_taylor_5(3,3) = dr_n1*dr_n1 / two
      mat_taylor_5(3,4) =-dr_n1**3 / six
      mat_taylor_5(3,5) = dr_n1**4 / (six*four)
!
      mat_taylor_5(4,1) = one
      mat_taylor_5(4,2) = dr_p1
      mat_taylor_5(4,3) = dr_p1*dr_p1 / two
      mat_taylor_5(4,4) = dr_p1**3 / six
      mat_taylor_5(4,5) = dr_p1**4 / (six*four)
!
      mat_taylor_5(5,1) = one
      mat_taylor_5(5,2) =-dr_p2
      mat_taylor_5(5,3) = dr_p2*dr_p2 / two
      mat_taylor_5(5,4) =-dr_p2**3 / six
      mat_taylor_5(5,5) = dr_p2**4 / (six*four)
!
      call cal_inverse_nn_matrix(ifive, mat_taylor_5,                   &
     &    mat_fdm4_ICB1_free_vp, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix mat_fdm4_ICB1_free_vp ',            &
     &            r_from_ICB(0)
      end if
!
      coef_fdm_free_ICB1_vp4( 2,2:5) = mat_fdm4_ICB1_free_vp(2:5,5)
      coef_fdm_free_ICB1_vp4( 1,2:5) = mat_fdm4_ICB1_free_vp(2:5,4)
      coef_fdm_free_ICB1_vp4( 0,2:5) = mat_fdm4_ICB1_free_vp(2:5,1)
      coef_fdm_free_ICB1_vp4(-1,2:5) = mat_fdm4_ICB1_free_vp(2:5,3)
!
      end subroutine cal_4th_ICB1_free_vp_fdm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_4th_ICB_free_vp_fdm
!
!
      write(50,*) ' coef_fdm_free_ICB_vp4'
      write(50,*) 'mattix for dfdr'
      write(50,'(1p9E25.15e3)') coef_fdm_free_ICB_vp4(0:2,2)
      write(50,*) 'mattix for d2fdr2'
      write(50,'(1p9E25.15e3)') coef_fdm_free_ICB_vp4(0:2,3)
      write(50,*) 'mattix for d3fdr3'
      write(50,'(1p9E25.15e3)') coef_fdm_free_ICB_vp4(0:2,4)
!
      write(50,*) ' coef_fdm_free_ICB1_vp4'
      write(50,*) 'mattix for dfdr'
      write(50,'(1p9E25.15e3)') coef_fdm_free_ICB1_vp4(-1:2,2)
      write(50,*) 'mattix for d2fdr2'
      write(50,'(1p9E25.15e3)') coef_fdm_free_ICB1_vp4(-1:2,3)
      write(50,*) 'mattix for d3fdr3'
      write(50,'(1p9E25.15e3)') coef_fdm_free_ICB1_vp4(-1:2,4)
      write(50,*) 'mattix for d4fdr4'
      write(50,'(1p9E25.15e3)') coef_fdm_free_ICB1_vp4(-1:2,5)
!
      end subroutine check_4th_ICB_free_vp_fdm
!
! -----------------------------------------------------------------------
!
      end module m_vp_coef_fdm4_free_ICB
