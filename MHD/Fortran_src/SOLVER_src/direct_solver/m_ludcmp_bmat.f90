!
!      module m_ludcmp_bmat
!
!      modified by Kemorin
!
!  ---------------------------------------------------------------------
!
!      subroutine ludcmp_bmat(a,n,np,nband,indx,d)
!
!c Given a matrix a(1:n,1:n), with physical dimension np by np,
!c this routine replaces it by the LU decomposition of a rowwise
!c permutation of itself. a and n are input.
!c a is output, arranged as in equation (2.3.14) above; index(1:n)
!c is an output vector that records the row permutation effected by
!c partial povoting; dis output as +/- 1 depending on whether
!c the number of row interchanges was even or odd, respectively.
!c This routine is used in combination with lubksb to solve
!c linear equations or ivert a matrix.
!
!
!      SUBROUTINE lubksb_bmat(a,n,np,nband,indx,b)
!      SUBROUTINE lubksb_3b(a,n,np,indx,b)
!      SUBROUTINE lubksb_5b(a,n,np,indx,b)
!      SUBROUTINE lubksb_7b(a,n,np,indx,b)
!
!c solve the set of n linear equations Ax=b. Here is a input,
!c not at the matrix A but rather as its LU decompsition,
!c determined by the routine ludcmp. indx is input as the
!c permutation vector returned by ludcmp. b(1:n) is input
!c as the right-hand side vectror b, and returns with the
!c solution vector x.
!c a, n, np and indx are not modified by this routine
!c and be left in place for successive calls with different
!c right hand sides b. This routine takes into account
!c the possibility that b will begin with many zero elements,
!c so it is efficient for use in matrix inversion.
! 
      module m_ludcmp_bmat
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
      SUBROUTINE ludcmp_bmat(a,n,np,nband, indx,d)
!
      integer (kind = kint), intent(in) :: n,np, nband
!
      integer (kind = kint), intent(inout) :: indx(n)
      real(kind = kreal), intent(inout) :: a(np,np)
      real(kind = kreal), intent(inout) :: d
!
      real(kind = kreal), parameter :: TINY = 1.d-30
      real(kind = kreal) :: aamax,dum,sum
      integer(kind = kint), allocatable :: idx_org(:)
      real(kind = kreal), allocatable :: vv(:)
      integer(kind = kint) :: nb
      integer(kind = kint) :: i, imax, j, k
      integer(kind = kint) :: ist, ied, jst, jed, kst, ked
!
!
      nb = (nband-1) / 2
      allocate( vv(np), idx_org(np) )
      vv = 0.0d0
!
      d = 1.0d0
      do i = 1,n
        jst = max(i-nb,ione)
        jed = min(i+nb,n)
        aamax = abs(a(i,jst))
        do j = jst+1, jed
          aamax = max(aamax, abs(a(i,j)))
        end do
        if (aamax.eq.0.0d0) pause 'singular matrix in ludcmp'
        vv(i)=1.0d0 / aamax
        idx_org(i) = i
      end do
!
      do j = 1,n
        ist = max((j-itwo*nb),ione)
        ied = min((j+nb),n)
!
        do i = ist, j-1
          do k = ist, i-1
            a(i,j) = a(i,j) - a(i,k) * a(k,j)
          end do
        end do
!
        aamax = 0.0d0
        do i = j, ied
          kst = max((i-itwo*nb),ione)
          do k = kst, j-1
            a(i,j) = a(i,j) - a(i,k)*a(k,j)
          end do
          dum = vv(i)*abs( a(i,j) )
          if (dum.ge.aamax .and. i.le.(idx_org(j)+nb)) then
            imax = i
            aamax = dum
          end if
        end do
!
        if (j.ne.imax) then
          k =             idx_org(imax)
          idx_org(imax) = idx_org(j)
          idx_org(j) = k
!
          kst = max((j-itwo*nb),ione)
          ked = min((imax+itwo*nb),n ) 
          do k = kst, ked
            dum = a(imax,k)
            a(imax,k) = a(j,k)
            a(j,k) = dum
          end do
          d = -d
          vv(imax) = vv(j)
        end if
        indx(j) = imax
!
        if(a(j,j).eq.0.0d0) a(j,j) = TINY
        if(j .ne. n) then
          dum = 1.0d0 / a(j,j)
          ied = min( (j+itwo*nb),n )
          do i = j+1, ied
            a(i,j) = a(i,j) * dum
          end do
        end if
      end do
!
!      write(*,*) 'idx_org',  idx_org
      deallocate( vv, idx_org )
!
      END subroutine ludcmp_bmat
!
! ----------------------------------------------------------------------
!
      SUBROUTINE lubksb_bmat(a,n,np,nband,indx,b)
!
      integer (kind = kint), intent(in) :: n,np, nband
      integer (kind = kint), intent(in) :: indx(n)
      real(kind = kreal), intent(in) :: a(np,np)
      real(kind = kreal), intent(inout) :: b(n)
!
      integer(kind = kint) :: i,ii,j,ll, jst, jed, nb
      real(kind = kreal) :: sum
!
!
      nb = (nband-1) / 2
!
      do i = 1, n
        ll = indx(i)
        sum = b(ll)
        b(ll) = b(i)
        b(i) =  sum
      end do
!
      do i = 1, n
        if (b(i) .ne. 0.0d0) then
          ii = i
          exit
        end if
      end do
!
      do i = ii, n
        jst = max((i-itwo*nb),ione)
        do j = jst,i-1
          b(i) = b(i) - a(i,j)*b(j)
        end do
      end do
!
      do i = n, 1, -1
        jed = min( (i+itwo*nb),n )
        do j = i+1, jed
          b(i) = b(i) - a(i,j)*b(j)
        end do
        b(i) = b(i) / a(i,i)
      end do
!
      END subroutine lubksb_bmat
!
! ----------------------------------------------------------------------
!
      SUBROUTINE lubksb_3b(a,n,np,indx,b)
!
      INTEGER (kind = kint) :: n,np
      INTEGER (kind = kint), intent(in) :: indx(n)
      REAL(kind = kreal), intent(in) :: a(np,np)
      REAL(kind = kreal), intent(inout) :: b(n)
!
      INTEGER(kind = kint) :: i, ll
      REAL(kind = kreal) :: sum
! 
!
      do i = 1, n
        ll = indx(i)
        sum =   b(ll)
        b(ll) = b(i)
        b(i) =  sum
      end do
!
      b(2) = b(2) - a(2,1)*b(1)
      do i = 3, n
        b(i) = b(i) - a(i,i-2)*b(i-2) - a(i,i-1)*b(i-1)
      end do
!
      b(n) =   b(n) / a(n,n)
      b(n-1) = (b(n-1) - a(n-1,n) * b(n)) / a(n-1,n-1)
      do i = n-2, 1, -1
        b(i) = (b(i) - a(i,i+1) * b(i+1) - a(i,i+2) * b(i+2)) / a(i,i)
      end do
!
      end subroutine lubksb_3b
!
! ----------------------------------------------------------------------
!
      SUBROUTINE lubksb_5b(a,n,np,indx,b)
!
      INTEGER (kind = kint) :: n,np
      INTEGER (kind = kint), intent(in) :: indx(n)
      REAL(kind = kreal), intent(in) :: a(np,np)
      REAL(kind = kreal), intent(inout) :: b(n)
!
      INTEGER(kind = kint) :: i, ll
      REAL(kind = kreal) :: sum
! 
!
      do i = 1, n
        ll = indx(i)
        sum =   b(ll)
        b(ll) = b(i)
        b(i) =  sum
      end do
!
      b(2) = b(2) - a(2,1)*b(1)
      b(3) = b(3) - a(3,1)*b(1) - a(3,2)*b(2)
      b(4) = b(4) - a(4,1)*b(1) - a(4,2)*b(2) - a(4,3)*b(3)
      do i = 5, n
        b(i) = b(i) - a(i,i-4)*b(i-4) - a(i,i-3)*b(i-3)                 &
     &              - a(i,i-2)*b(i-2) - a(i,i-1)*b(i-1)
      end do
!
      b(n) =   b(n) / a(n,n)
      b(n-1) = (b(n-1) - a(n-1,n) * b(n)) / a(n-1,n-1)
      b(n-2) = (b(n-2) - a(n-2,n-1) * b(n-1)                            &
     &                 - a(n-2,n) * b(n) ) / a(n-2,n-2)
      b(n-3) = (b(n-3) - a(n-3,n-2) * b(n-2) - a(n-3,n-1) * b(n-1)      &
     &                 - a(n-3,n) * b(n) ) / a(n-3,n-3)
      do i = n-4, 1, -1
        b(i) = (b(i) - a(i,i+1) * b(i+1) - a(i,i+2) * b(i+2)            &
     &               - a(i,i+3) * b(i+3) - a(i,i+4) * b(i+4) ) / a(i,i)
      end do
!
      end subroutine lubksb_5b
!
! ----------------------------------------------------------------------
!
      SUBROUTINE lubksb_7b(a,n,np,indx,b)
!
      INTEGER (kind = kint) :: n,np
      INTEGER (kind = kint), intent(in) :: indx(n)
      REAL(kind = kreal), intent(in) :: a(np,np)
      REAL(kind = kreal), intent(inout) :: b(n)
!
      INTEGER(kind = kint) :: i, ll
      REAL(kind = kreal) :: sum
! 
!
      do i = 1, n
        ll = indx(i)
        sum =   b(ll)
        b(ll) = b(i)
        b(i) =  sum
      end do
!
      b(2) = b(2) - a(2,1)*b(1)
      b(3) = b(3) - a(3,1)*b(1) - a(3,2)*b(2)
      b(4) = b(4) - a(4,1)*b(1) - a(4,2)*b(2) - a(4,3)*b(3)
      b(5) = b(5) - a(5,1)*b(1) - a(5,2)*b(2) - a(5,3)*b(3)             &
     &            - a(5,4)*b(4)
      b(6) = b(6) - a(6,1)*b(1) - a(6,2)*b(2) - a(6,3)*b(3)             &
     &            - a(5,4)*b(4) - a(6,5)*b(5)
      do i = 7, n
        b(i) = b(i) - a(i,i-6)*b(i-6) - a(i,i-5)*b(i-5)                 &
     &              - a(i,i-4)*b(i-4) - a(i,i-3)*b(i-3)                 &
     &              - a(i,i-2)*b(i-2) - a(i,i-1)*b(i-1)
      end do
!
      b(n) =   b(n) / a(n,n)
      b(n-1) = (b(n-1) - a(n-1,n) * b(n)) / a(n-1,n-1)
      b(n-2) = (b(n-2) - a(n-2,n-1) * b(n-1)                            &
     &                 - a(n-2,n) * b(n) ) / a(n-2,n-2)
      b(n-3) = (b(n-3) - a(n-3,n-2) * b(n-2) - a(n-3,n-1) * b(n-1)      &
     &                 - a(n-3,n) * b(n) ) / a(n-3,n-3)
      b(n-4) = (b(n-4) - a(n-4,n-3) * b(n-3)                            &
     &                 - a(n-4,n-2) * b(n-2) - a(n-4,n-1) * b(n-1)      &
     &                 - a(n-4,n) * b(n) ) / a(n-4,n-4)
      b(n-5) = (b(n-5) - a(n-5,n-4) * b(n-4) - a(n-5,n-3) * b(n-3)      &
     &                 - a(n-5,n-2) * b(n-2) - a(n-5,n-1) * b(n-1)      &
     &                 - a(n-5,n) * b(n) ) / a(n-5,n-5)
      do i = n-6, 1, -1
        b(i) = (b(i) - a(i,i+1) * b(i+1) - a(i,i+2) * b(i+2)            &
     &               - a(i,i+3) * b(i+3) - a(i,i+4) * b(i+4)            &
     &               - a(i,i+5) * b(i+5) - a(i,i+6) * b(i+6) ) / a(i,i)
      end do
!
      end subroutine lubksb_7b
!
! ----------------------------------------------------------------------
!
      end module m_ludcmp_bmat
