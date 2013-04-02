!
!      module m_ludcmp_5mat
!
!      modified by Kemorin
!
!  ---------------------------------------------------------------------
!
!      subroutine ludcmp_5b(a,n,np,indx,d)
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
!      SUBROUTINE lubksb_5b(a,n,np,indx,b)
!
!c solve the set of n linear equations Ax=b. Here is a input,
!c not at the matrix A but rather as its LU decompsition,
!c determined by the routine ludcmp_5b. indx is input as the
!c permutation vector returned by ludcmp_5b. b(1:n) is input
!c as the right-hand side vectror b, and returns with the
!c solution vector x.
!c a, n, np and indx are not modified by this routine
!c and be left in place for successive calls with different
!c right hand sides b. This routine takes into account
!c the possibility that b will begin with many zero elements,
!c so it is efficient for use in matrix inversion.
! 
      module m_ludcmp_5mat
!
      use m_precision
!
      implicit none
!
      private :: swap_bmat, swap_int, swap_real
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      SUBROUTINE ludcmp_5b(a,n,np,indx,d)
!
      INTEGER (kind = kint), intent(in) :: n,np
!
      INTEGER (kind = kint), intent(inout) :: indx(n)
      REAL(kind = kreal), intent(inout) :: a(np,np)
      REAL(kind = kreal), intent(inout) :: d
!
      REAL(kind = kreal), parameter :: TINY = 1.d-30
      INTEGER  (kind = kint) :: i,imax,j,k, num, iflag
      INTEGER  (kind = kint), parameter :: nb1 = 2
      REAL(kind = kreal) :: aamax,dum,sum
      INTEGER(kind = kint), allocatable :: idx_org(:)
      REAL(kind = kreal),   allocatable :: vv(:)
 
!
      allocate( vv(np), idx_org(np) )
      vv = 0.0d0
      iflag = 0
!
      d = 1.0d0
      do i = 1, n
        aamax =abs(a(i,i))
        if(i.gt.1)   aamax = max( abs(a(i,i-1)), aamax)
        if(i.gt.2)   aamax = max( abs(a(i,i-2)), aamax)
        if(i.gt.n-2) aamax = max( abs(a(i,i+2)), aamax)
        if(i.gt.n-1) aamax = max( abs(a(i,i+1)), aamax)
!
        if (aamax.eq.0.0d0) pause 'singular matrix in ludcmp'
        vv(i) = 1.0d0 / aamax
        idx_org(i) = i
      end do
!
!
      j = 1
!
      aamax = vv(j)*abs( a(j,j) )
      imax = j
!
      dum = vv(j+1)*abs( a(j+1,j) )
      if (dum.ge.aamax .and. (j+1).le.(idx_org(j)+2)) then
        imax = j+1
        aamax = dum
      end if
!
      dum = vv(j+2)*abs( a(j+2,j) )
      if (dum.ge.aamax .and. (j+2).le.(idx_org(j)+2)) imax = j+2
!
      if (j .ne. imax) then
        call swap_bmat(n, np, nb1, j, imax, idx_org, a, d, vv)
      end if
      indx(j) = imax
!
      if(a(j,j) .eq. 0.0d0) a(j,j) = TINY
      dum = 1.0d0 / a(j,j)
      a(j+1:j+2,j) = a(j+1:j+2,j)*dum
!
!
      j = 2
!
      a(j,j) =  a(j,j) - a(j,j-1)*a(j-1,j)
      aamax = vv(j)*abs( a(j,j) )
      imax = j
!
      a(j+1,j) =  a(j+1,j) - a(j+1,j-1)*a(j-1,j)
      dum = vv(j+1)*abs( a(j+1,j) )
      if (dum.ge.aamax .and. (j+1).le.(idx_org(j)+2)) then
        imax = j+1
        aamax = dum
      end if
!
      a(j+2,j) =  a(j+2,j) - a(j+2,j-1)*a(j-1,j)
      dum = vv(j+2)*abs( a(j+2,j) )
      if (dum.ge.aamax .and. (j+2).le.(idx_org(j)+2)) imax = j+2
!
      if (j .ne. imax) then
        call swap_bmat(n, np, nb1, j, imax, idx_org, a, d, vv)
      end if
!
      indx(j) = imax
      if(a(j,j) .eq. 0.0d0) a(j,j) = TINY
      dum = 1.0d0 / a(j,j)
      a(j+1:j+2,j) = a(j+1:j+2,j)*dum
!
!
      j = 3
      a(j-1,j) = a(j-1,j) - a(j-1,j-2)*a(j-2,j)
!
      a(j,  j) = a(j,  j) - a(j,  j-2)*a(j-2,j) - a(j,  j-1)*a(j-1,j)
      aamax = vv(j)*abs( a(j,j) )
      imax = j
!
      a(j+1,j) = a(j+1,j) - a(j+1,j-2)*a(j-2,j) - a(j+1,j-1)*a(j-1,j)
      dum = vv(j+1)*abs( a(j+1,j) )
      if (dum.ge.aamax .and. (j+1).le.(idx_org(j)+2)) then
        imax = j+1
        aamax = dum
      end if
!
      a(j+2,j) = a(j+2,j) - a(j+2,j-2)*a(j-2,j) - a(j+2,j-1)*a(j-1,j)
      dum = vv(j+2)*abs( a(j+2,j) )
      if (dum.ge.aamax .and. (j+2).le.(idx_org(j)+2)) imax = j+2
!
      if (j .ne. imax) then
        call swap_bmat(n, np, nb1, j, imax, idx_org, a, d, vv)
      end if
!
      indx(j) = imax
      if(a(j,j) .eq. 0.0d0) a(j,j) = TINY
      dum = 1.0d0 / a(j,j)
      a(j+1:j+2,j) = a(j+1:j+2,j)*dum
!
!
      j = 4
      a(j-2,j) = a(j-2,j) - a(j-2,j-3)*a(j-3,j)
      a(j-1,j) = a(j-1,j) - a(j-1,j-3)*a(j-3,j) - a(j-1,j-2)*a(j-2,j)
!
!
      a(j,  j) = a(j,  j) - a(j,  j-3)*a(j-3,j)                         &
     &                    - a(j,  j-2)*a(j-2,j) - a(j,  j-1)*a(j-1,j)
      aamax = vv(j)*abs( a(j,j) )
      imax = j
!
      a(j+1,j) = a(j+1,j) - a(j+1,j-3)*a(j-3,j)                         &
     &                    - a(j+1,j-2)*a(j-2,j) - a(j+1,j-1)*a(j-1,j)
      dum = vv(j+1)*abs( a(j+1,j) )
      if (dum.ge.aamax .and. (j+1).le.(idx_org(j)+2)) then
        imax = j+1
        aamax = dum
      end if
!
      a(j+2,j) = a(j+2,j) - a(j+2,j-2)*a(j-2,j) - a(j+2,j-1)*a(j-1,j)
      dum = vv(j+2)*abs( a(j+2,j) )
      if (dum.ge.aamax .and. (j+2).le.(idx_org(j)+2)) imax = j+2
!
      if (j .ne. imax) then
        call swap_bmat(n, np, nb1, j, imax, idx_org, a, d, vv)
      end if
!
      indx(j) = imax
      if(a(j,j) .eq. 0.0d0) a(j,j) = TINY
      dum = 1.0d0 / a(j,j)
      if (j .le. (n-1)) a(j+1,j) = a(j+1,j)*dum
      if (j .le. (n-2)) a(j+2,j) = a(j+2,j)*dum
!
!
      do j = 5, n
        a(j-3,j) = a(j-3,j) - a(j-3,j-4)*a(j-4,j)
        a(j-2,j) = a(j-2,j) - a(j-2,j-4)*a(j-4,j)                       &
     &                      - a(j-2,j-3)*a(j-3,j)
        a(j-1,j) = a(j-1,j) - a(j-1,j-4)*a(j-4,j)                       &
     &                      - a(j-1,j-3)*a(j-3,j)                       &
     &                      - a(j-1,j-2)*a(j-2,j)
!
        a(j,  j) = a(j,  j) - a(j,  j-4)*a(j-4,j)                       &
     &                      - a(j,  j-3)*a(j-3,j)                       &
     &                      - a(j,  j-2)*a(j-2,j)                       &
     &                      - a(j,  j-1)*a(j-1,j)
        imax = j
!
        if(j .lt. n) then
          aamax = vv(j)*abs( a(j,j) )
!
          a(j+1,j) = a(j+1,j) - a(j+1,j-3)*a(j-3,j)                     &
     &                        - a(j+1,j-2)*a(j-2,j)                     &
     &                        - a(j+1,j-1)*a(j-1,j)
          dum = vv(j+1)*abs( a(j+1,j) )
!
          if (dum.gt.aamax .and. (j+1).le.(idx_org(j)+2)) then
            imax = j+1
            aamax = dum
          end if
        end if
!
        if(j .lt. n-1) then
          a(j+2,j) = a(j+2,j) - a(j+2,j-2)*a(j-2,j)                     &
     &                        - a(j+2,j-1)*a(j-1,j)
          dum = vv(j+2)*abs( a(j+2,j) )
          if (dum.ge.aamax .and. (j+2).le.(idx_org(j)+2)) imax = j+2
        end if
!
        if (j .ne. imax) then
          call swap_bmat(n, np, nb1, j, imax, idx_org, a, d, vv)
        end if
!
        indx(j) = imax
        if(a(j,j) .eq. 0.0d0) a(j,j) = TINY
        dum = 1.0d0 / a(j,j)
        if (j .le. (n-1)) a(j+1,j) = a(j+1,j)*dum
        if (j .le. (n-2)) a(j+2,j) = a(j+2,j)*dum
      end do
!
      deallocate(vv, idx_org)
!
      END subroutine ludcmp_5b
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine swap_bmat(n, np, nb1, j, imax, idx, a, d, vv)
!
      integer(kind = kint), intent(in) :: n, np, nb1
      integer(kind = kint), intent(in) :: j, imax
      integer(kind = kint), intent(inout) :: idx(np)
      real(kind = kreal), intent(inout) :: d
      real(kind = kreal), intent(inout) :: vv(np)
      real(kind = kreal), intent(inout) :: a(np,np)
!
      integer(kind = kint) :: k
!
      call swap_int(idx(j), idx(imax))
      do k = max(j-4,1), min(j+4,n)
        call swap_real(a(j,k), a(imax,k))
      end do
      d = -d
      vv(imax) = vv(j)
!
      end subroutine swap_bmat
!
! ----------------------------------------------------------------------
!
      subroutine swap_int(ia,ib)
!
      integer(kind = kint), intent(inout) :: ia,ib
      integer(kind = kint) :: iw
!
      iw = ia
      ia = ib
      ib = iw
!
      end subroutine swap_int
!
!-----------------------------------------------------------------------
!
      subroutine swap_real(a,b)
!
      real(kind = kreal), intent(inout) :: a, b
      real(kind = kreal) :: w
!
      w = a
      a = b
      b = w
!
      end subroutine swap_real
!
!-----------------------------------------------------------------------
!
      end module m_ludcmp_5mat
