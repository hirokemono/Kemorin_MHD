
/* invert_small_matrix_c.*/

/*
!>@file   cal_inverse_small_matrix.f90
!!@brief  module cal_inverse_small_matrix
!!
!!@date  Programmed by H.Matsui on July, 2009
!
!>@brief evaluate invert matrix for small matrix
!!
!!@verbatim
!!      subroutine cal_det_22_matrix(a, det_a)
!!      subroutine cal_det_33_matrix(a, det_a)
!!      subroutine cal_det_44_matrix(a, det_a)
!!      recursive subroutine cal_det_nn_matrix(nsize, a, det_a)
!!
!!      subroutine cal_inverse_22_matrix(a, a_inv, ierr)
!!      subroutine cal_inverse_33_matrix(a, a_inv, ierr)
!!      subroutine cal_inverse_44_matrix(a, a_inv, ierr)
!!      subroutine cal_inverse_nn_matrix(nsize, a, a_inv, ierr)
!!
!!     definition of matrix
!!            / a(1,1)  a(1,2)  a(1,3)  \
!!       A =  | a(2,1)  a(2,2)  a(2,3)  | =  a[(i-1)+N*(j-1)]
!!            \ a(3,1)  a(3,2)  a(3,3)  /
!!@endverbatim
!!
!!@n @param nsize size of matrix
!!@n @param a(nsize,nsize)      input matrix
!!@n @param a_inv(nsize,nsize)  inverse matrix
!!@n @param det_a               detarminant of matrix
!!@n @param ierr                error flag
!!
      module cal_inverse_small_matrix
!
      use m_precision
!
      implicit  none
!
      real(kind = kreal), parameter, private :: eps = 1.0e-40
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_det_22_matrix(a, det_a)
!
      real(kind= kreal), intent(in) ::    a(2,2)
      real(kind= kreal), intent(inout) :: det_a
!
!
      det_a = a(1,1)*a(2,2) - a(2,1)*a(1,2)
!
      end subroutine cal_det_22_matrix
!
!  ---------------------------------------------------------------------
!
      subroutine cal_det_33_matrix(a, det_a)
!
      real(kind= kreal), intent(in) ::    a(3,3)
      real(kind= kreal), intent(inout) :: det_a
!
      real(kind= kreal) ::  a_tmp(2,2)
      real(kind= kreal) :: det_tmp
      integer(kind = kint) :: i, k, ii
!
      det_a = 0.0d0
      do k = 1, 3
!
        ii = 0
        do i = 1, 2
          ii = ii + 1
          if(i .eq. k) ii = ii + 1
          a_tmp(1:2,i) = a(2:3,ii)
        end do
!
        call cal_det_22_matrix(a_tmp, det_tmp)
        det_a = det_a + (-1)**(k+1) * det_tmp * a(1,k)
      end do
!
      end subroutine cal_det_33_matrix
!
!  ---------------------------------------------------------------------
!
      subroutine cal_det_44_matrix(a, det_a)
!
      real(kind= kreal), intent(in) ::    a(4,4)
      real(kind= kreal), intent(inout) :: det_a
!
      real(kind= kreal) ::  a_tmp(3,3)
      real(kind= kreal) :: det_tmp
      integer(kind = kint) :: i, k, ii
!
!
      det_a = 0.0d0
      do k = 1, 4
!
        ii = 0
        do i = 1, 3
          ii = ii + 1
          if(i .eq. k) ii = ii + 1
          a_tmp(1:3,i) = a(2:4,ii)
        end do
!
        call cal_det_33_matrix(a_tmp, det_tmp)
        det_a = det_a + (-1)**(k+1) * det_tmp * a(1,k)
      end do
!
!
      end subroutine cal_det_44_matrix
!
!  ---------------------------------------------------------------------
!
      recursive subroutine cal_det_nn_matrix(nsize, a, det_a)
!
      integer(kind = kint), intent(in) :: nsize
      real(kind= kreal), intent(in) ::    a(nsize,nsize)
      real(kind= kreal), intent(inout) :: det_a
!
      real(kind= kreal) ::  a_tmp(nsize-1,nsize-1)
      real(kind= kreal) :: det_tmp
      integer(kind = kint) :: i, k, ii
!
!
      if(nsize .eq. 2) then
        call cal_det_22_matrix(a, det_a)
      else
!
        det_a = 0.0d0
        do k = 1, nsize
!
          ii = 0
          do i = 1, nsize-1
            ii = ii + 1
            if(i .eq. k) ii = ii + 1
            a_tmp(1:nsize-1,i) = a(2:nsize,ii)
          end do
!
          call cal_det_nn_matrix( (nsize-1), a_tmp, det_tmp)
          det_a = det_a + (-1)**(k+1) * det_tmp * a(1,k)
        end do
      end if
!
!
      end subroutine cal_det_nn_matrix
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_inverse_22_matrix(a, a_inv, ierr)
!
      real(kind= kreal), intent(in) ::    a(2,2)
      real(kind= kreal), intent(inout) :: a_inv(2,2)
      integer(kind = kint), intent(inout) :: ierr
!
      real(kind= kreal) :: det, a_det

      call cal_det_22_matrix(a, det)
      if ( abs(det) .lt. eps ) then
        write(*,*) '2x2 matrix is singular'
        ierr = 1
        return
      else
        a_det = 1.0d0 / det
      end if
!
      a_inv(1,1) =  a(2,2) * a_det
      a_inv(1,2) = -a(1,2) * a_det
      a_inv(2,1) = -a(2,1) * a_det
      a_inv(2,2) =  a(1,1) * a_det
      ierr = 0
!
      end subroutine cal_inverse_22_matrix
!
!  ---------------------------------------------------------------------
!
      subroutine cal_inverse_33_matrix(a, a_inv, ierr)
!
      real(kind= kreal), intent(in) ::    a(3,3)
      real(kind= kreal), intent(inout) :: a_inv(3,3)
      integer(kind = kint), intent(inout) :: ierr
!
      real(kind= kreal) :: det, a_det
      real(kind= kreal) :: a_tmp(2,2)
      integer(kind = kint) :: ki, kj, i, j, ii, jj
!
!
      call cal_det_33_matrix(a, det)
      if ( abs(det) .lt. eps ) then
        write(*,*) '3x3 matrix is singular'
        ierr = 1
        return
      else
        a_det = 1.0d0 / det
      end if
!
      do kj = 1, 3
        do ki = 1, 3
          jj = 0
          do j = 1, 2
            jj = jj + 1
            if(jj .eq. kj) jj = jj + 1
            ii = 0
            do i = 1, 2
              ii = ii + 1
              if(ii .eq. ki) ii = ii + 1
              a_tmp(i,j) = a(ii,jj)
            end do
          end do
!
          call cal_det_22_matrix(a_tmp, det)
!
          a_inv(kj,ki) = (-1)**(ki+kj) * det * a_det
        end do
      end do
      ierr = 0
!
!
      end subroutine cal_inverse_33_matrix
!
!  ---------------------------------------------------------------------
!
      subroutine cal_inverse_44_matrix(a, a_inv, ierr)
!
      real(kind= kreal), intent(in) ::    a(4,4)
      real(kind= kreal), intent(inout) :: a_inv(4,4)
      integer(kind = kint), intent(inout) :: ierr
!
      real(kind= kreal) :: det, a_det
      real(kind= kreal) ::  a_tmp(3,3)
      integer(kind = kint) :: i, j, ki, kj, ii, jj
!
      call cal_det_44_matrix(a, det)
      if ( abs(det) .lt. eps ) then
       
       write(*,*) '4x4 matrix is singular'
        ierr = 1
        return
      else
        a_det = 1.0d0 / det
      end if
!
      do kj = 1, 4
        do ki = 1, 4
          jj = 0
          do j = 1, 3
            jj = jj + 1
            if(jj .eq. kj) jj = jj + 1
            ii = 0
            do i = 1, 3
              ii = ii + 1
              if(ii .eq. ki) ii = ii + 1
              a_tmp(i,j) = a(ii,jj)
            end do
          end do
!
          call cal_det_33_matrix(a_tmp, det)
!
          a_inv(kj,ki) = (-1)**(ki+kj) * det * a_det
        end do
      end do
      ierr = 0
!
!
      end subroutine cal_inverse_44_matrix
!
!  ---------------------------------------------------------------------
!
subroutine cal_inverse_nn_matrix(nsize, a, a_inv, ierr)
!
      integer(kind = kint), intent(in) :: nsize
      real(kind= kreal), intent(in) ::    a(nsize,nsize)
      real(kind= kreal), intent(inout) :: a_inv(nsize,nsize)
      integer(kind = kint), intent(inout) :: ierr
!
      real(kind= kreal) :: det, a_det
      real(kind= kreal) :: a_tmp(nsize-1,nsize-1)
      integer(kind = kint) :: i, j, ki, kj, ii, jj
!
      call cal_det_nn_matrix(nsize, a, det)
      if ( abs(det) .lt. eps ) then
       
       write(*,*) 'nxn matrix is singular'
        ierr = 1
        return
      else
        a_det = 1.0d0 / det
      end if
!
      do kj = 1, nsize
        do ki = 1, nsize
          jj = 0
          do j = 1, nsize-1
            jj = jj + 1
            if(jj .eq. kj) jj = jj + 1
            ii = 0
            do i = 1, nsize-1
              ii = ii + 1
              if(ii .eq. ki) ii = ii + 1
              a_tmp(i,j) = a(ii,jj)
            end do
          end do
!
          call cal_det_nn_matrix( (nsize-1), a_tmp, det)
!
          a_inv(kj,ki) = (-1)**(ki+kj) * det * a_det
        end do
      end do
      ierr = 0
!
      end subroutine cal_inverse_nn_matrix
!
!  ---------------------------------------------------------------------
!
*/

#include "invert_small_matrix_c.h"

#define EPS 1.0e-40

double cal_det_22_matrix_c(const double *a){
	double det_a = a[(1-1)+2*(1-1)]*a[(2-1)+2*(2-1)] 
			- a[(2-1)+2*(1-1)]*a[(1-1)+2*(2-1)];
	return det_a;
};

double cal_det_33_matrix_c(const double *a){
	int i, k, ii;
	double a_tmp[2*2];
	double det_tmp;
	double det_a = 0.0;
	
	for(k=1;k<4;k++){
		ii = 0;
		for(i=1;i<3;i++){
			ii = ii + 1;
			if(i == k) ii = ii + 1;
			a_tmp[(1-1)+2*(i-1)] = a[(2-1)+3*(ii-1)];
			a_tmp[(2-1)+2*(i-1)] = a[(3-1)+3*(ii-1)];
		};
		det_tmp = cal_det_22_matrix_c(a_tmp);
		det_a = det_a + pow(-1,k+1) * det_tmp * a[(1-1)+3*(k-1)];
	};
	return det_a;
}

double cal_det_44_matrix_c(const double *a){
	int i, k, ii;
	double a_tmp[3*3];
	double det_tmp;
	double det_a = 0.0;
	
	for(k=1;k<5;k++){
		ii = 0;
		for(i=1;i<4;i++){
			ii = ii + 1;
			if(i == k) ii = ii + 1;
			a_tmp[(1-1)+3*(i-1)] = a[(2-1)+4*(ii-1)];
			a_tmp[(2-1)+3*(i-1)] = a[(3-1)+4*(ii-1)];
			a_tmp[(3-1)+3*(i-1)] = a[(4-1)+4*(ii-1)];
		};
		det_tmp = cal_det_33_matrix_c(a_tmp);
		det_a = det_a + pow(-1,k+1) * det_tmp * a[(1-1)+4*(k-1)];
	};
	return det_a;
}

double cal_det_nn_matrix_c(int nsize, const double *a){
	int i, j, k, ii;
	double a_tmp[(nsize-1)*(nsize-1)];
	double det_tmp;
	double det_a = 0.0;
	
	if(nsize == 2){
		det_a = cal_det_22_matrix_c(a);
		return det_a;
	};
	
	for(k=1;k<nsize+1;k++){
		ii = 0;
		for(i=1;i<nsize;i++){
			ii = ii + 1;
			if(i == k) ii = ii + 1;
			for(j=1;j<nsize;j++){
				a_tmp[(j-1)+(nsize-1)*(i-1)] = a[(j)+nsize*(ii-1)];
			};
		};
		det_tmp = cal_det_nn_matrix_c(nsize-1, a_tmp);
		det_a = det_a + pow(-1,k+1) * det_tmp * a[(1-1)+nsize*(k-1)];
	};
	return det_a;
};


int cal_inverse_22_matrix_c(const double *a, double *a_inv){
	double det, a_det22;
	int ierr = 0;
	
	det = cal_det_22_matrix_c(a);
	if(fabs(det) < EPS){
		printf("2x2 matrix is singular\n");
		ierr = 1;
		return ierr;
	};
	a_det22 = 1.0 / det;
	
	a_inv[(1-1)+2*(1-1)] =  a[(2-1)+2*(2-1)] * a_det22;
	a_inv[(1-1)+2*(2-1)] = -a[(1-1)+2*(2-1)] * a_det22;
	a_inv[(2-1)+2*(1-1)] = -a[(2-1)+2*(1-1)] * a_det22;
	a_inv[(2-1)+2*(2-1)] =  a[(1-1)+2*(1-1)] * a_det22;
	return ierr;
};

int cal_inverse_33_matrix_c(const double *a, double *a_inv){
	double a_tmp[2*2];
	double det, a_det33;
	int ki, kj, i, j, ii, jj;
	int ierr = 0;
	
	det = cal_det_33_matrix_c(a);
	if(fabs(det) < EPS){
		printf("3x3 matrix is singular\n");
		ierr = 1;
		return ierr;
	};
	a_det33 = 1.0 / det;
	
	for(kj=1;kj<4;kj++){
		for(ki=1;ki<4;ki++){
			jj = 0;
			for(j=1;j<3;j++){
				jj = jj + 1;
				if(jj == kj) jj = jj + 1;
				ii = 0;
				for(i=1;i<3;i++){
					ii = ii + 1;
					if(ii == ki) ii = ii + 1;
					a_tmp[(i-1)+2*(j-1)] = a[(ii-1)+3*(jj-1)];
				};
			};
			det = cal_det_22_matrix_c(a_tmp);
			a_inv[(kj-1)+3*(ki-1)] = pow(-1,ki+kj) * det * a_det33;
		};
	};
	
	return ierr;
};

int cal_inverse_44_matrix_c(const double *a, double *a_inv){
	double a_tmp[3*3];
	double det, a_det44;
	int ki, kj, i, j, ii, jj;
	int ierr = 0;
	
	det = cal_det_44_matrix_c(a);
	if(fabs(det) < EPS){
		printf("4x4 matrix is singular\n");
		ierr = 1;
		return ierr;
	};
	a_det44 = 1.0 / det;
	
	for(kj=1;kj<5;kj++){
		for(ki=1;ki<5;ki++){
			jj = 0;
			for(j=1;j<4;j++){
				jj = jj + 1;
				if(jj == kj) jj = jj + 1;
				ii = 0;
				for(i=1;i<4;i++){
					ii = ii + 1;
					if(ii == ki) ii = ii + 1;
					a_tmp[(i-1)+3*(j-1)] = a[(ii-1)+4*(jj-1)];
				};
			};
			det = cal_det_33_matrix_c(a_tmp);
			a_inv[(kj-1)+4*(ki-1)] = pow(-1,ki+kj) * det * a_det44;
		};
	};
	
	return ierr;
};

int cal_inverse_nn_matrix_c(int nsize, const double *a, double *a_inv){
	double det, a_detnn;
	double a_tmp[(nsize-1)*(nsize-1)];
	int i, j, ki, kj, ii, jj;
	int ierr = 0;
	
	det = cal_det_nn_matrix_c(nsize, a);
	if(fabs(det) < EPS){
		printf("NxN matrix is singular\n");
		ierr = 1;
		return ierr;
	};
	a_detnn = 1.0 / det;
	
	for(kj=1;kj<nsize+1;kj++){
		for(ki=1;ki<nsize+1;ki++){
			jj = 0;
			for(j=1;j<nsize;j++){
				jj = jj + 1;
				if(jj == kj) jj = jj + 1;
				ii = 0;
				for(i=1;i<nsize;i++){
					ii = ii + 1;
					if(ii == ki) ii = ii + 1;
					a_tmp[(i-1)+(nsize-1)*(j-1)] = a[(ii-1)+nsize*(jj-1)];
				};
			};
			det = cal_det_nn_matrix_c((nsize-1), a_tmp);
			a_inv[(kj-1)+nsize*(ki-1)] = pow(-1,ki+kj) * det * a_detnn;
		};
	};
	
	return ierr;
};
