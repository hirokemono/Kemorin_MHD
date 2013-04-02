!
      program lae
!
      use m_precision
!
      use m_ludcmp
!
      implicit none
!
      real(kind = kreal), dimension(3,3) :: a
      real(kind = kreal), dimension(3) :: b
      integer(kind = kint), dimension(3) :: indx
      integer(kind = kint) :: np, n
!
      real(kind = kreal) :: d
!
      a(1,1)=2.0
      a(1,2)=4.0
      a(1,3)=6.0
      a(2,1)=3.0
      a(2,2)=8.0
      a(2,3)=7.0
      a(3,1)=5.0
      a(3,2)=7.0
      a(3,3)=21.0
      b(1)=6.0
      b(2)=15.0
      b(3)=24.0
      n=3
      np=3
!c decompose A = LU
      call ludcmp(a,n,np,indx,d)
!c solve Ax=LUx=b
      call lubksb(a,n,np,indx,b)
!c
      write(6,*)b(1),b(2),b(3)
      end
 