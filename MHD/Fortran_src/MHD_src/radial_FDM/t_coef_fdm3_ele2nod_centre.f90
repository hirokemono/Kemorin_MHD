!>@file   t_coef_fdm3_ele2nod_centre.f90
!!@brief  module t_coef_fdm3_ele2nod_centre
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative around center
!!
!!@verbatim
!!      subroutine check_3rd_e2n_CTR_vpol_fdm(fdm3_e2n_CTR)
!!        type(fdm3_e2n_center_vpol), intent(in) :: fdm3_e2n_CTR
!!
!!      subroutine cal_fdm3_e2n_mat_center1(radius, fdm3_e2n_CTR)
!!      subroutine cal_fdm3_e2n_mat_center2(radius, fdm3_e2n_CTR)
!!        real(kind = kreal), intent(in) :: radius(4)
!!        type(fdm3_e2n_center_vpol), intent(inout) :: fdm3_e2n_CTR
!!
!!   Matrix for poloidal velocity at next of center
!!      dfdr =      fdm3_e2n_CTR%dmat_pe1( 2,2) * d_ele(3)
!!                + fdm3_e2n_CTR%dmat_pe1( 1,2) * d_ele(2)
!!                + fdm3_e2n_CTR%dmat_pe1( 0,2) * d_ele(1)
!!      d2fdr2 =    fdm3_e2n_CTR%dmat_pe1( 2,3) * d_ele(3)
!!                + fdm3_e2n_CTR%dmat_pe1( 1,3) * d_ele(2)
!!                + fdm3_e2n_CTR%dmat_pe1( 0,3) * d_ele(1)
!!      d3fdr3 =    fdm3_e2n_CTR%dmat_pe1( 2,4) * d_ele(3)
!!                + fdm3_e2n_CTR%dmat_pe1( 1,4) * d_ele(2)
!!                + fdm3_e2n_CTR%dmat_pe1( 0,4) * d_ele(1)
!!      d4fdr4 =    fdm3_e2n_CTR%dmat_pe1( 2,5) * d_ele(3)
!!                + fdm3_e2n_CTR%dmat_pe1( 1,5) * d_ele(2)
!!                + fdm3_e2n_CTR%dmat_pe1( 0,5) * d_ele(1)
!!
!!   Matrix for poloidal velocity at 2nd next of center
!!      dfdr =      fdm3_e2n_CTR%dmat_pe2( 2,2) * d_ele(4)
!!                + fdm3_e2n_CTR%dmat_pe2( 1,2) * d_ele(3)
!!                + fdm3_e2n_CTR%dmat_pe2( 0,2) * d_ele(2)
!!                + fdm3_e2n_CTR%dmat_pe2(-1,2) * d_ele(1)
!!      d2fdr2 =    fdm3_e2n_CTR%dmat_pe2( 2,3) * d_ele(4)
!!                + fdm3_e2n_CTR%dmat_pe2( 1,3) * d_ele(3)
!!                + fdm3_e2n_CTR%dmat_pe2( 0,3) * d_ele(2)
!!                + fdm3_e2n_CTR%dmat_pe2(-1,3) * d_ele(1)
!!      d3fdr3 =    fdm3_e2n_CTR%dmat_pe2( 2,4) * d_ele(4)
!!                + fdm3_e2n_CTR%dmat_pe2( 1,4) * d_ele(3)
!!                + fdm3_e2n_CTR%dmat_pe2( 0,4) * d_ele(2)
!!                + fdm3_e2n_CTR%dmat_pe2(-1,4) * d_ele(1)
!!      d4fdr4 =    fdm3_e2n_CTR%dmat_pe2( 2,5) * d_ele(4)
!!                + fdm3_e2n_CTR%dmat_pe2( 1,5) * d_ele(3)
!!                + fdm3_e2n_CTR%dmat_pe2( 0,5) * d_ele(2)
!!                + fdm3_e2n_CTR%dmat_pe2(-1,5) * d_ele(1)
!!@endverbatim
!!
      module t_coef_fdm3_ele2nod_centre
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Structure for element to node FDM matrix of center
      type fdm3_e2n_center_vpol
!>        Matrix to evaluate radial derivative at center
!!       for poloidal velocity
        real(kind = kreal) :: dmat_pe1(-1:2,1:4)
!>        Matrix to evaluate radial derivative at next of center
!!       for poloidal velocity
        real(kind = kreal) :: dmat_pe2(-1:2,1:4)
      end type fdm3_e2n_center_vpol
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_3rd_e2n_CTR_vpol_fdm(fdm3_e2n_CTR)
!
      type(fdm3_e2n_center_vpol), intent(in) :: fdm3_e2n_CTR
!
!
      write(50,*) ' fdm3_e2n_CTR%dmat_vp0'
      write(50,*) 'matrix for dfdr'
      write(50,'(1p9E25.15e3)') fdm3_e2n_CTR%dmat_pe1(0:2,2)
      write(50,*) 'matrix for d2fdr2'
      write(50,'(1p9E25.15e3)') fdm3_e2n_CTR%dmat_pe1(0:2,3)
      write(50,*) 'matrix for d3fdr3'
      write(50,'(1p9E25.15e3)') fdm3_e2n_CTR%dmat_pe1(0:2,4)
!
      write(50,*) ' fdm3_e2n_CTR%dmat_pe1'
      write(50,*) 'matrix for dfdr'
      write(50,'(1p9E25.15e3)') fdm3_e2n_CTR%dmat_pe2(-1:2,2)
      write(50,*) 'matrix for d2fdr2'
      write(50,'(1p9E25.15e3)') fdm3_e2n_CTR%dmat_pe2(-1:2,3)
      write(50,*) 'matrix for d3fdr3'
      write(50,'(1p9E25.15e3)') fdm3_e2n_CTR%dmat_pe2(-1:2,4)
!
      end subroutine check_3rd_e2n_CTR_vpol_fdm
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm3_e2n_mat_center1(radius, fdm3_e2n_CTR)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: radius(4)
      type(fdm3_e2n_center_vpol), intent(inout) :: fdm3_e2n_CTR
!
!>      Work matrix to evaluate fdm3_e2n_CTR%dmat_fix_fld(-1:1,3)
!!@verbatim
!!      d_nod =     mat_fdm_ctr2_fix_4(1,1) * d_ele(1)
!!                + mat_fdm_ctr2_fix_4(1,2) * d_ele(2)
!!                + mat_fdm_ctr2_fix_4(1,3) * dfdr(0)
!!                + mat_fdm_ctr2_fix_4(1,4) * d_ele(3)
!!      dfdr =      mat_fdm_ctr1_fix_4(2,1) * d_ele(1)
!!                + mat_fdm_ctr1_fix_4(2,2) * d_ele(2)
!!                + mat_fdm_ctr1_fix_4(2,3) * dfdr(0)
!!                + mat_fdm_ctr1_fix_4(2,4) * d_ele(3)
!!      d2fdr2 =    mat_fdm_ctr1_fix_4(3,1) * d_ele(1)
!!                + mat_fdm_ctr1_fix_4(3,2) * d_ele(2)
!!                + mat_fdm_ctr1_fix_4(3,3) * dfdr(0)
!!                + mat_fdm_ctr1_fix_4(3,4) * d_ele(3)
!!      d3fdr3 =    mat_fdm_ctr1_fix_4(4,1) * d_ele(1)
!!                + mat_fdm_ctr1_fix_4(4,2) * d_ele(2)
!!                + mat_fdm_ctr1_fix_4(4,3) * dfdr(0)
!!                + mat_fdm_ctr1_fix_4(4,4) * d_ele(3)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ctr1_fix_4(4,4)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_4(4,4)
      real(kind = kreal) :: dr_n2, dr_p1, dr_n1, dr_p2
!
!
      dr_n2 = radius(1)
      dr_n1 = half * radius(1)
      dr_p1 = half * (radius(2) - radius(1))
      dr_p2 = half * (radius(3) + radius(2)) - radius(1)
!
      mat_taylor_4(1,1) =  one
      mat_taylor_4(1,2) = -dr_n1
      mat_taylor_4(1,3) =  dr_n1*dr_n1 / two
      mat_taylor_4(1,4) = -dr_n1**3 / six
!
      mat_taylor_4(2,1) =  one
      mat_taylor_4(2,2) =  dr_p1
      mat_taylor_4(2,3) =  dr_p1*dr_p1 / two
      mat_taylor_4(2,4) =  dr_p1**3 / six
!
      mat_taylor_4(3,1) =  zero
      mat_taylor_4(3,2) =  one
      mat_taylor_4(3,3) = -dr_n2
      mat_taylor_4(3,4) =  dr_n2*dr_n2 / two
!
      mat_taylor_4(4,1) =  one
      mat_taylor_4(4,2) =  dr_p2
      mat_taylor_4(4,3) =  dr_p2*dr_p2 / two
      mat_taylor_4(4,4) =  dr_p2**3 / six
!
      call cal_inverse_44_matrix(mat_taylor_4,                          &
     &                           mat_fdm_ctr1_fix_4, ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix mat_fdm_ctr1_fix_4 ',               &
     &            radius(1:2)
      end if
!
      fdm3_e2n_CTR%dmat_pe1(-1,1:4) = zero
      fdm3_e2n_CTR%dmat_pe1( 0,1:4) = mat_fdm_ctr1_fix_4(1:4,1)
      fdm3_e2n_CTR%dmat_pe1( 1,1:4) = mat_fdm_ctr1_fix_4(1:4,2)
      fdm3_e2n_CTR%dmat_pe1( 2,1:4) = mat_fdm_ctr1_fix_4(1:4,4)
!
      end subroutine cal_fdm3_e2n_mat_center1
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm3_e2n_mat_center2(radius, fdm3_e2n_CTR)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: radius(4)
      type(fdm3_e2n_center_vpol), intent(inout) :: fdm3_e2n_CTR
!
!>      Work matrix to evaluate fdm3_e2n_CTR%dmat_fix_fld(-1:1,3)
!!@verbatim
!!      d_nod =     mat_fdm_ctr2_fix_4(1,1) * d_ele(2)
!!                + mat_fdm_ctr2_fix_4(1,2) * d_ele(3)
!!                + mat_fdm_ctr2_fix_4(1,3) * d_ele(1)
!!                + mat_fdm_ctr2_fix_4(1,4) * d_ele(4)
!!      dfdr =      mat_fdm_ctr2_fix_4(2,1) * d_ele(2)
!!                + mat_fdm_ctr2_fix_4(2,2) * d_ele(3)
!!                + mat_fdm_ctr2_fix_4(2,3) * d_ele(1)
!!                + mat_fdm_ctr2_fix_4(2,4) * d_ele(4)
!!      d2fdr2 =    mat_fdm_ctr2_fix_4(3,1) * d_ele(2)
!!                + mat_fdm_ctr2_fix_4(3,2) * d_ele(3)
!!                + mat_fdm_ctr2_fix_4(3,3) * d_ele(1)
!!                + mat_fdm_ctr2_fix_4(3,4) * d_ele(4)
!!      d3fdr3 =    mat_fdm_ctr2_fix_4(4,1) * d_ele(2)
!!                + mat_fdm_ctr2_fix_4(4,2) * d_ele(3)
!!                + mat_fdm_ctr2_fix_4(4,3) * d_ele(1)
!!                + mat_fdm_ctr2_fix_4(4,4) * d_ele(4)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ctr2_fix_4(4,4)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_4(4,4)
      real(kind = kreal) :: dr_n2, dr_p1, dr_n1, dr_p2
!
!
      dr_n2 = radius(2) - half * radius(1)
      dr_n1 = half * (radius(2) - radius(1))
      dr_p1 = half * (radius(3) - radius(2))
      dr_p2 = half * (radius(4) + radius(3)) - radius(2)
!
      mat_taylor_4(1,1) =  one
      mat_taylor_4(1,2) = -dr_n1
      mat_taylor_4(1,3) =  dr_n1*dr_n1 / two
      mat_taylor_4(1,4) = -dr_n1**3 / six
!
      mat_taylor_4(2,1) =  one
      mat_taylor_4(2,2) =  dr_p1
      mat_taylor_4(2,3) =  dr_p1*dr_p1 / two
      mat_taylor_4(2,4) =  dr_p1**3 / six
!
      mat_taylor_4(3,1) =  one
      mat_taylor_4(3,2) = -dr_n2
      mat_taylor_4(3,3) =  dr_n2*dr_n2 / two
      mat_taylor_4(3,4) = -dr_n2**3 / six
!
      mat_taylor_4(4,1) =  one
      mat_taylor_4(4,2) =  dr_p2
      mat_taylor_4(4,3) =  dr_p2*dr_p2 / two
      mat_taylor_4(4,4) =  dr_p2**3 / six
!
      call cal_inverse_44_matrix(mat_taylor_4, mat_fdm_ctr2_fix_4, ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_4th_to_center_fixed_fdm ',      &
     &            radius(1:2)
      end if
!
      fdm3_e2n_CTR%dmat_pe2(-1,1:4) = mat_fdm_ctr2_fix_4(1:4,3)
      fdm3_e2n_CTR%dmat_pe2( 0,1:4) = mat_fdm_ctr2_fix_4(1:4,1)
      fdm3_e2n_CTR%dmat_pe2( 1,1:4) = mat_fdm_ctr2_fix_4(1:4,2)
      fdm3_e2n_CTR%dmat_pe2( 2,1:4) = mat_fdm_ctr2_fix_4(1:4,4)
!
      end subroutine cal_fdm3_e2n_mat_center2
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm3_ele2nod_centre
