!>@file   m_ludcmp.f90
!!@brief  module m_ludcmp
!!
!!@author H. Matsui
!!@date Modified on 2007
!
!
!>@brief  linear solver using LU decompotion
!!
!!@verbatim
!!---------------------------------------------------------------------
!!
!!      subroutine ludcmp(a,n,np,indx,d)
!!
!! Given a matrix a(1:n,1:n), with physical dimension np by np,
!! this routine replaces it by the LU decomposition of a rowwise
!! permutation of itself. a and n are input.
!! a is output, arranged as in equation (2.3.14) above; index(1:n)
!! is an output vector that records the row permutation effected by
!! partial povoting; dis output as +/- 1 depending on whether
!! the number of row interchanges was even or odd, respectively.
!! This routine is used in combination with lubksb to solve
!! linear equations or ivert a matrix.
!!
!!
!!      subroutine lubksb(a,n,np,indx,b)
!!
!! solve the set of n linear equations Ax=b. Here is a input,
!! not at the matrix A but rather as its LU decompsition,
!! determined by the routine ludcmp. indx is input as the
!! permutation vector returned by ludcmp. b(1:n) is input
!! as the right-hand side vectror b, and returns with the
!! solution vector x.
!! a, n, np and indx are not modified by this routine
!! and be left in place for successive calls with different
!! right hand sides b. This routine takes into account
!! the possibility that b will begin with many zero elements,
!! so it is efficient for use in matrix inversion.
!!
!!      subroutine luinv(a,n,np,indx,d,a_inv)
!!
!! Evaluate invese matrix of a. Solution is stored in a_inv.
!!@endverbatim
!
      module m_ludcmp
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine ludcmp(a,n,np,indx,d)
!
      integer (kind = kint), intent(in) :: n,np
!
      integer (kind = kint), intent(inout) :: indx(n)
      real(kind = kreal), intent(inout) :: a(np,np)
      real(kind = kreal), intent(inout) :: d
!
      integer  (kind = kint) :: i,imax,j,k
      real(kind = kreal) :: aamax,dum,sum
      real(kind = kreal), allocatable :: vv(:)
 
!
      allocate( vv(np) )
      vv = 0.0d0
!
      d = 1.0d0
      do i = 1, n
        aamax = abs(a(i,1))
        do j = 2, n
          aamax = max(aamax, abs(a(i,j)))
        end do
        if (aamax.eq.0.0d0) stop 'singular matrix in ludcmp'
        vv(i)=1.0d0 / aamax
      end do
       
      do j=1,n
         do i=1,j-1
           sum=a(i,j)
           do k=1,i-1
              sum=sum-a(i,k)*a(k,j)
           enddo
           a(i,j)=sum
         enddo
         aamax=0.0d0
         do i=j,n
           sum=a(i,j)
           do k=1,j-1
              sum=sum-a(i,k)*a(k,j)
           enddo
           a(i,j)=sum
           dum=vv(i)*abs(sum)
           if (dum.ge.aamax) then
               imax=i
               aamax=dum
           endif
         enddo
         if (j.ne.imax) then
             do k=1,n
               dum=a(imax,k)
               a(imax,k)=a(j,k)
               a(j,k)=dum
             enddo
             d=-d
             vv(imax)=vv(j)
         endif
         indx(j)=imax
         if(a(j,j).eq.0.0d0) a(j,j)=TINY*TINY
         if(j.ne.n) then
            dum=1./a(j,j)
            do i=j+1,n
               a(i,j)=a(i,j)*dum
            enddo
         endif
      enddo
!
      deallocate( vv )
!
      END subroutine ludcmp
!
! ----------------------------------------------------------------------
!
      subroutine lubksb(a,n,np,indx,b)
!
      integer (kind = kint), intent(in) :: n,np
      integer (kind = kint), intent(in) :: indx(n)
      real(kind = kreal), intent(in) :: a(np,np)
      real(kind = kreal), intent(inout) :: b(n)
!
      integer(kind = kint) :: i,ii,j,ll
      real(kind = kreal) :: sum
!
!
      do i = 1, n
        ll = indx(i)
        sum = b(ll)
        b(ll) = b(i)
        b(i) =  sum
      end do
!
      ii=0
      do i=1,n
        sum = b(i)
        if (ii.ne.0) then
          do j = ii,i-1
            sum = sum - a(i,j)*b(j)
          end do
        else if (sum.ne.0) then
          ii = i
        end if
        b(i) = sum
      end do
!
      do i = n,1,-1
        sum = b(i)
        do j = i+1, N
          sum = sum - a(i,j)*b(j)
        end do
        b(i) = sum / a(i,i)
      end do
!
      END subroutine lubksb
!
! ----------------------------------------------------------------------
!
      subroutine luinv(a,n,np,indx,d,a_inv)
!
      integer (kind = kint), intent(in) :: n,np
      integer (kind = kint), intent(in) :: indx(n)
      real(kind = kreal), intent(in) :: d
      real(kind = kreal), intent(in) :: a(np,np)
      real(kind = kreal), intent(inout) :: a_inv(np,np)
!
      real(kind = kreal) :: det
      integer(kind = kint) :: i, j
!
!
      det = d
      do j = 1, np
        det = det*a(j,j)
        do i = 1, np
          a_inv(i,j) = 0.0d0
        end do
      end do
!
      do j = 1, np
        a_inv(j,j) = 1.0d0
        call lubksb(a, n, np, indx, a_inv(1,j) )
      end do
!
      end subroutine luinv
!
! ----------------------------------------------------------------------
!
      end module m_ludcmp
