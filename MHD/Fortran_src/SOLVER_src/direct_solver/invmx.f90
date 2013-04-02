!
      program invmx
!
      use m_precision
!
      use m_ludcmp
!
      implicit none
!
      real(kind = kreal), dimension(3,3) :: a, aa, y
      integer(kind = kint), dimension(3) :: indx
      integer(kind = kint) :: np, n
!
      integer(kind = kint) :: i, j, k
      real(kind = kreal) :: d, s
!
!
!c size of dimension and matrix
      np=3
      n=3
!c defined matrix A
      a(1,1)=2.0
      a(1,2)=4.0
      a(1,3)=6.0
      a(2,1)=3.0
      a(2,2)=8.0
      a(2,3)=7.0
      a(3,1)=5.0
      a(3,2)=7.0
      a(3,3)=21.0
!c save the matrix in aa(3,3)
!c because the original a(3,3) will be
!c destroyed after LU-decomposition.
      do i=1,n
         do j=1,n
            y(i,j)=0.0
            aa(i,j)=a(i,j)
         enddo
         y(i,i)=1.0
      enddo
!c print out the matrix A
      write(6,*)" matrix A"
      do i=1,n
        write(6,600)(a(i,j),j=1,n)
      enddo
!c---------- begin to inverse A -------- 
      do i=1,n
         do j=1,n
            y(i,j)=0.0
         enddo
         y(i,i)=1.0
      enddo
!c decompose A = LU
      call ludcmp(a,n,np,indx,d)
!c inverse matrix A
      do j=1,n
         call lubksb(a,n,np,indx,y(1,j))
!c        Note that FORTRAN stores two-dimensional matricses
!c        by column, so y(1,j) is the address of the jth 
!c        column of y.
      enddo
!c write up the result
      write(6,*)" inversed A"
      do i=1,n
        write(6,600)(y(i,j),j=1,n)
      enddo
!c check the program; calcurate A*A^-1=I
      do i=1,n
        do j=1,n
          s=0.0
           do k=1,n
             s=s+aa(i,k)*y(k,j)
           enddo
          write(6,*)i,j,s
        enddo
      enddo
  600 format(10f10.4)
      end
