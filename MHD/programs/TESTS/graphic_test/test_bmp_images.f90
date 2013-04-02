!* --------------------------------------------
!* Sample FORTRAN 90 program to make PPM / BMP
!*
!*  1998-Apr-03 (long long ago)
!*      :
!*  2005-Jul-22 (added P3)
!*      :
!*  2006 Dec-22 (added BMP)
!*
!*  Image array rgb(3,*,*) is filled in subroutine mkbitmap()
!*  and
!*  saved in PPM or BMP format in subroutine pixout().
!*
!*                                   K. Hayashi
!* --------------------------------------------
!*
      program pixelout
!
      use m_precision
!
      use write_bmp_image
!
      implicit none
!
      integer ihpixf, jvpixf
      parameter(ihpixf = 128, jvpixf = 128) ! pixel size
      character(len=1) rgb(3,ihpixf,jvpixf) ! RGB image array
      integer nframe
      character(len=kchara) ::  fheadout
      integer ipixout
!
       write(*,*) 'data format...1:binary ppm, 2: ascii ppm, other: BMP'
       read(*,*) ipixout
!
!
      do nframe = 1, 50
        call mkbitmap(rgb,nframe,ihpixf, jvpixf)
        write(fheadout,'(''smpl'',i3.3)') nframe
!
        if (ipixout .EQ. 1) then
          call pixout_ppm_p6(fheadout, ihpixf, jvpixf, rgb)
        else if (ipixout .EQ. 2) then
          call pixout_ppm_p3(fheadout, ihpixf, jvpixf, rgb)
        else
          call pixout_BMP(fheadout, ihpixf, jvpixf, rgb)
        end if
!
      enddo
!
      end program pixelout
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
!* --------------------------------------------
!*   fill rgb data array with something
!* --------------------------------------------

       subroutine mkbitmap(rgb,nframe, ihpixf, jvpixf)
       implicit none
       integer nframe
       integer ::  ihpixf, jvpixf
!      RGB pixel data array
       character(len=1) :: rgb(3,ihpixf,jvpixf)
!* local
       real*8  red, gre, blu
       integer ired, igre, iblu
       real*8  ofst
       parameter(ofst = 0.7D+00)
       integer i, j, itmp
       real*8   aa, bb, cc, rr, xx, yy, tt
       integer ichoice
       parameter(ichoice = 1) ! .... choice
       real*8  pi
       parameter(pi = 3.14159265358979D+00)

       if (ichoice .EQ. 0) then
         do 100 j = 1, jvpixf
         do 100 i = 1, ihpixf
           itmp = i*3*nframe + j*2
           itmp = mod(itmp,256)  ! assuming color depth ranges 0--255
           rgb(1,i,j) = char(itmp) ! red
           itmp = i*1*nframe + j*3
           itmp = mod(itmp,256)
           rgb(2,i,j) = char(itmp) ! green
           itmp = i*5*nframe + j*7
           itmp = mod(itmp,256)
           rgb(3,i,j) = char(itmp) ! blue
 100     continue
       else
         do 101 j = 1, jvpixf
         do 101 i = 1, ihpixf
!* red-ball
           tt = dfloat(nframe) / 25.0D+00 !                  time/period
           xx = dfloat(i) / dfloat(ihpixf) - 0.33D+00  !     center x
           yy = dfloat(j) / dfloat(jvpixf) - 0.25D+00  !     center y
           rr = dsqrt(xx**2 + yy**2 + 1.0D-30)
           aa = rr / 0.25D+00 !                              half-width
           bb =(tt - rr) * pi
           cc = dexp(-aa**2) * (dcos(bb))**2
           if (cc .LT. ofst) then
             red = cc / ofst
             gre = 0.0D+00
             blu = 0.0D+00
           else
             red = 1.0D+00
             gre =(cc - ofst) / (1.0D+00 - ofst)
             blu =(cc - ofst) / (1.0D+00 - ofst)
           endif
!* green-ball
           tt = dfloat(nframe) / 50.0D+00
           xx = dfloat(i) / dfloat(ihpixf) - 0.40D+00
           yy = dfloat(j) / dfloat(jvpixf) - 0.65D+00
           rr = dsqrt(xx**2 + yy**2 + 1.0D-30)
           aa = rr / 0.40D+00
           bb =(tt - rr) * pi
           cc = dexp(-aa**2) * (dcos(bb))**2
           if (cc .LT. ofst) then ! here and hereafter, additive rgb color is simply added.
             gre = gre + cc / ofst
           else
             red = red +(cc - ofst) / (1.0D+00 - ofst)
             gre = gre + 1.0D+00
             blu = blu +(cc - ofst) / (1.0D+00 - ofst)
           endif
!* blue-ball
           tt = dfloat(nframe) / 12.5D+00
           xx = dfloat(i) / dfloat(ihpixf) - 0.75D+00
           yy = dfloat(j) / dfloat(jvpixf) - 0.70D+00
           rr = dsqrt(xx**2 + yy**2 + 1.0D-30)
           aa = rr / 0.30D+00
           bb =(tt - rr) * pi
           cc = dexp(-aa**2) * (dcos(bb))**2
           if (cc .LT. ofst) then
             blu = blu + cc / ofst
           else
             red = red +(cc - ofst) / (1.0D+00 - ofst)
             gre = gre +(cc - ofst) / (1.0D+00 - ofst)
             blu = blu + 1.0D+00
           endif
!* yellow-ball
           tt = dfloat(nframe) / 16.66666666666D+00
           xx = dfloat(i) / dfloat(ihpixf) - 0.75D+00
           yy = dfloat(j) / dfloat(jvpixf) - 0.30D+00
           rr = dsqrt(xx**2 + yy**2 + 1.0D-30)
           aa = rr / 0.25D+00
           bb =(tt - rr) * pi
           cc = dexp(-aa**2) * (dcos(bb))**2
           if (cc .LT. ofst) then
             red = red + cc / ofst
             gre = gre + cc / ofst
           else
             red = red + 1.0D+00
             gre = gre + 1.0D+00
             blu = blu +(cc - ofst) / (1.0D+00 - ofst)
           endif

           ired = int(red * 255.0D+00)
           igre = int(gre * 255.0D+00)
           iblu = int(blu * 255.0D+00)
           if (ired .GT. 255) ired = 255
           if (igre .GT. 255) igre = 255
           if (iblu .GT. 255) iblu = 255
           if (ired .LT.   0) ired =   0
           if (igre .LT.   0) igre =   0
           if (iblu .LT.   0) iblu =   0
           rgb(1,i,j) = char(ired)
           rgb(2,i,j) = char(igre)
           rgb(3,i,j) = char(iblu)
 101     continue
       endif
!
       end subroutine mkbitmap

