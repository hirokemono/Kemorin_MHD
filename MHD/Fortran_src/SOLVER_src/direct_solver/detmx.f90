!
!     program test_det
!
      use m_precision
!
      use m_ludcmp
!
      implicit none
!
      real(kind = kreal), dimension(3,3) :: a
      integer(kind = kint), dimension(3) :: indx
      integer(kind = kint) :: np, n
!
      integer(kind = kint) :: j
      real(kind = kreal) :: d
!

      np=3
      n=3
      a(1,1)=2.0
      a(1,2)=4.0
      a(1,3)=6.0
      a(2,1)=3.0
      a(2,2)=8.0
      a(2,3)=7.0
      a(3,1)=5.0
      a(3,2)=7.0
      a(3,3)=21.0
!c decompose A = LU
      call ludcmp(a,n,np,indx,d)
!c This returns as d= +-1
!c get the determinant of A
      do j=1,n
        d=d*a(j,j)
      enddo
      write(6,*)d
      end
