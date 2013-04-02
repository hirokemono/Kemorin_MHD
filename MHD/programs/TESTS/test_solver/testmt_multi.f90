      program testmt
!***************************************
!*
!*   test program for solve matrix
!*
!***************************************
!*
!***************************************
!*
!*        |  1  3  0                |
!*        | -4  2  6  0             |
!*        |  0  1  4  2  0          |
!*    A = |     0  3 -2 -9  0       |
!*        |        0  1  3 -6  0    |
!*        |           0  2  6  1  0 |
!*        |              0  4 -1  3 |
!*        |                 0 -1  5 |
!*
!*        |  1 -4  0                |
!*        |  3  2  1  0             |
!*     t  |  0  6  4  3  0          |
!*    A = |     0  2 -2  1  0       |
!*        |        0  9  3  2  0    |
!*        |           0 -6  6  4  0 |
!*        |              0  1 -1 -1 |
!*        |                 0  3  5 |
!*
!***************************************
!*
!*
!*        |   3 |            | -18 |             | -6 |
!*        |  42 |            | -10 |             |  3 |
!*        |  13 |            |  27 |             |  2 |
!*    b = | -32 |       c =  |   6 |        x =  |  1 |
!*        |  31 |            | - 3 |             |  4 |
!*        | - 5 |            | -22 |             | -3 |
!*        |   1 |            | -14 |             |  5 |
!*        |  25 |            |  45 |             |  6 |
!*
!*
!***************************************
!*
!**********************************************************************
!*
!*       define of a(i,j)
!*
!*               | a(1,1)  a(1,2)  ........  a(1,n)  |
!*               | a(2,1)  a(2,2)  ........  a(2,n)  |
!*          A =  |   .       .      a(i,j)      .    |
!*               | a(n,1)  a(n,2)  ........  a(n,n)  |
!*
!*       subroutine
!*
!      call ludcmp(a,N,NP,ip,ep)      : LU factrization
!      call lubksb(a,N,NP,ip,x)     :  solve
!
!**********************************************************************
!*
!
!**********************************************************************
!     Kemo's 3-band solver
!
!       a(i,i) =   dg(i)
!       a(i+1,i) = al(i)
!       a(i,i+1) = au(i)
!
!*               | dg(1)  au(1)  ........     0         0     |
!*               | al(1)  dg(2)  ........     .         .     |
!*               |   0    al(2)  ........     .         .     |
!*    a(i,j)  =  |   .       0   ........     0         .     |
!*               | ...... al(k-1)  dg(k)     au(k) .......... |
!*               |   .       .   ........  au(N-2)     0      |
!*               |   .       .   ........  dg(N-1)  au(N-1)   |
!*               |   0       0   ........  al(N-1)  dg(N)     |
!
!      SUBROUTINE ludcmp_3band(n, dg, al, au)
!      SUBROUTINE lubksb_3band(n, dg, al, au, x)
!
!**********************************************************************
!*
!*
!**********************************************************************
!*
!*     band matrix for IMSL subroutine 
!*
!*               | a(2,1)  a(1,2)  ........     0         0     |
!*               | a(3,1)  a(2,2)  ........     .         .     |
!*               |   0     a(3,2)  ........     .         .     |
!*    a(i,j)  =  |   .       0     ........     0         .     |
!*               | ...... a(3,k-1)  a(2,k)  a(1,k+1) .......... |
!*               |   .       .     ........  a(1,N-2)     0     |
!*               |   .       .     ........  a(2,N-2)  a(1,N-1) |
!*               |   0       0     ........  a(3,N-2)  a(2,N-1) |
!*
!*    a(1,1) = a(3,N-1) = a(m,j) = 0.0    { m > 4 }
!*
!*       subroutine
!*
!*    dlftrb ( N-1 ,a ,N-1 ,1 ,1 ,a ,N-1 ,ip )       : LU factrization
!*    dlfsrb ( N-1 ,a ,N-1 ,1 ,1 ,ip ,b ,1 ,x )      :  solve
!*
!**********************************************************************
!*
!**********************************************************************
!*
!*     band matrix for SX3r super computer
!*
!*               | a(2,1)  a(3,1)  ........     0         0     |
!*               | a(1,2)  a(2,2)  ........     .         .     |
!*               |   0     a(1,3)  ........     .         .     |
!*    a(i,j)  =  |   .       0     ........     0         .     |
!*               | ........ a(1,k)  a(2,k)  a(3,k) ............ |
!*               |   .       .     ........  a(3,N-3)     0     |
!*               |   .       .     ........  a(2,N-2)  a(3,N-2) |
!*               |   0       0     ........  a(1,N-1)  a(2,N-1) |
!*
!*    a(1,1) = a(3,N-1) = a(m,j) = 0.0    { m > 4 }
!*
!*       subroutine
!*
!*    dbbdlu ( a ,N-1 ,N-1 ,1 ,1 ,ip ,ic )       : LU factrization
!*    dbbdls ( a ,N-1 ,N-1 ,1 ,1 ,b ,ip ,ic )    :  solve
!*
!**********************************************************************
!*
      use m_ludcmp_3band
      use lubksb_357band
      use lubksb_357band_mul
!*
!* ------  define  --------------
!*
      integer, parameter :: ismp = 4, nvect = 8, ncp = 8
      integer :: istack_smp(0:ismp) = (/0,2,4,6,8/)
!
       real*8 a(ncp,ncp)
       real*8 b(ncp), x(ncp)
       real*8 ep, dm(nvect), d1
!
       real*8 :: band_am(3,ncp,nvect) , band_lum(5,ncp,nvect)
       real*8 :: bm(nvect,ncp) , xm(nvect,ncp)
       real*8 :: diff(nvect,ncp) , xm2(nvect,ncp)
!*
       integer ip(ncp) ,iopt(2), idxm(ncp,nvect)
       integer :: i, j
!*
       data a/ 1.0 ,-4.0,0.0 ,0.0 ,0.0 ,0.0 ,0.0 ,0.0                   &
     &        ,3.0 ,2.0 ,1.0 ,0.0 ,0.0 ,0.0 ,0.0 ,0.0                   &
     &        ,0.0 ,6.0 ,4.0 ,3.0 ,0.0 ,0.0 ,0.0 ,0.0                   &
     &        ,0.0 ,0.0 ,2.0 ,-2.0,1.0 ,0.0 ,0.0 ,0.0                   &
     &        ,0.0 ,0.0 ,0.0 ,-9.0,3.0 ,2.0 ,0.0 ,0.0                   &
     &        ,0.0 ,0.0 ,0.0 ,0.0 ,-6.0,6.0 ,4.0 ,0.0                   &
     &        ,0.0 ,0.0 ,0.0 ,0.0 ,0.0 ,1.0 ,-1.0,-1.0                  &
     &        ,0.0 ,0.0 ,0.0 ,0.0 ,0.0 ,0.0 ,3.0 ,5.0 /
       data b/ 3.0 , 42.0 , 13.0 ,-32.0 , 31.0 ,-5.0 , 1.0 , 25.0/
!*
!*  -------    set band matrix ab ----------
!*
      do 9 i = 1 ,ncp
        write(6,600) (a(i,j) ,j=1,ncp)
 600    format(8f3.0)
   9  continue
!*
!*
!*
      band_am = 0.0d0
!
      do j = 1, 4
        band_am(2,1,j) = a(1,1)*j
        band_am(3,1,j) = a(2,1)*j
        do i = 2, ncp-1
          band_am(1,i,j) = a(i-1,i)*j
          band_am(2,i,j) = a(i,  i)*j
          band_am(3,i,j) = a(i+1,i)*j
        end do
        band_am(1,ncp,j) = a(ncp-1,ncp)*j
        band_am(2,ncp,j) = a(ncp,  ncp)*j
      end do
!*
      do j = 5, 8
        band_am(2,1,j) = a(1,1)
        band_am(3,1,j) = a(2,1)
        do i = 2, ncp-1
          band_am(1,i,j) = a(i-1,i)
          band_am(2,i,j) = a(i,  i)
          band_am(3,i,j) = a(i+1,i)
        end do
        band_am(1,ncp,j) = a(ncp-1,ncp)
        band_am(2,ncp,j) = a(ncp,  ncp)
      end do
!*
      do j = 1, 8
        write(6,*) 'band matrix',j
        do 30 i = 1, ncp
          write(6,*) band_am(1:3,i,j)
  30    continue
      end do
!*
      do j = 1, 4
        bm(j,1:ncp) = b(1:ncp)
        xm(j,1:ncp) = b(1:ncp)
      end do
      do j = 5, 8
        bm(j,1:ncp) = b(1:ncp)*(j-3)
        xm(j,1:ncp) = b(1:ncp)*(j-3)
      end do
!
!*  --------  solve vector by 3band -------------
!*
      do j = 1, nvect
        call ludcmp_3band(ncp, band_am(1,1,j), idxm(1,j), ierr,         &
     &      band_lum(1,1,j), dm(j))
      end do
!
      xm2 = bm
      call lubksb_3band_mul(ismp, istack_smp, nvect, ncp,               &
     &    band_lum, idxm, xm2)
!
      do j = 1, 8
        x(1:ncp) = bm(j,1:ncp)
        call lubksb_3band(ncp, band_lum(1,1,j), idxm(1,j), x)
        xm(j,1:ncp) = x(1:ncp)
      end do
!
      do j = 1, 8
        write(6,*) 'RHS and soultion by band matrix', j, ier
        do 20 k = 1 ,ncp
          write(6,*) bm(j,k) ,xm(j,k), xm2(j,k)
  20    continue
      end do
!*
      stop
      end
