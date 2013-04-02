!set_corner_quad_connect.f90
!      module set_corner_quad_connect
!
!     Written by H. Matsui on Apr., 2003
!     Modified by H. Matsui on Oct., 2007
!     Modified by H. Matsui on Dec., 2011
!
      module set_corner_quad_connect
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_corner_quad_connect_1(iele, ifile, ifile_20,       &
     &          numnod, num_h, num_v)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
!  corner (x = y = z =-cube_size)
!
      ie20(1) = ncube_c + 1
      ie20(5) = ncube_c + (num_h+1)**2 + 1
      ie20(7) = 1
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 1 + (num_h+1)
      ie20(4) = ie20(1)     + (num_h+1)
      ie20(6) = ie20(5) + 1
      ie20(8) = ie20(5) + 4*num_h - 1
!
      ie20( 9)   = numnod + numedge_cube + 1
      ie20(12)   = numnod + numedge_cube + (num_h+1)*num_h + 1
      ie20(13)   = numnod + numedge_cube                                &
     &            + (2*num_h+4*num_v+2)*num_h + 1
      ie20(14)   = numnod                                               &
     &            + (3*num_h-5)*(num_h-1)*(num_v-1) + 1
      ie20(17)   = numnod + numedge_cube + 2*(num_h+1)*num_h + 1
      ie20(19)   = numnod +  (3*num_h-5)*(num_h-1)*(num_v-1)            &
     &                    - (num_h-1)*(num_h-1) + 1
!
      ie20(10) = ie20(12) + 1
      ie20(11) = ie20( 9) + num_h
!
      ie20(15) = ie20(14) + 4*(num_h-1) - 1
      ie20(16) = ie20(13) + 4*num_h - 1
!
      ie20(18) = ie20(17) + 1
      ie20(20) = ie20(17) + 4*num_h - 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:8)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:20)
!
      end subroutine set_corner_quad_connect_1
!
!   --------------------------------------------------------------------
!
      subroutine set_corner_quad_connect_2(iele, ifile, ifile_20,       &
     &          numnod, num_h, num_v)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
!  corner (x = cube_size, y = z = -cube_size)
!
      ie20(1) = ncube_c + num_h
      ie20(5) = ncube_c + (num_h+1)**2 + num_h
      ie20(8) = num_h-1
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 1 + (num_h+1)
      ie20(4) = ie20(1)     + (num_h+1)
      ie20(6) = ie20(5) + 1
      ie20(7) = ie20(5) + 2
!
      ie20( 9)   = numnod + numedge_cube + num_h
      ie20(12)   = numnod + numedge_cube + (num_h+1)*num_h + num_h
      ie20(13)   = numnod + numedge_cube                                &
     &             + (2*num_h+4*num_v+2)*num_h + num_h
      ie20(16)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1) + num_h-1
      ie20(17)   = numnod + numedge_cube + 2*(num_h+1)*num_h + num_h
      ie20(20)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &                    - (num_h-1)*(num_h-1) + num_h-1
!
      ie20(10) = ie20(12) + 1
      ie20(11) = ie20( 9)      + num_h
!
      ie20(14) = ie20(13) + 1
      ie20(15) = ie20(16) + 1
!
      ie20(18) = ie20(17) + 1
      ie20(19) = ie20(17) + 2
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:8)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:20)
!
      end subroutine set_corner_quad_connect_2
!
!   --------------------------------------------------------------------
!
      subroutine set_corner_quad_connect_4(iele, ifile, ifile_20,       &
     &          numnod, num_h, num_v)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
!  corner (x=-cube_size, y=cube_size, z=-cube_size)
!
      ie20(1) = ncube_c + (num_h+1) * (num_h-1) + 1
      ie20(7) = ncube_c + (num_h+1)**2 + 3*num_h
      ie20(6) = (num_h-1)*(num_h-2) + 1
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 1 + (num_h+1)
      ie20(4) = ie20(1)     + (num_h+1)
      ie20(5) = ie20(7) + 2
      ie20(8) = ie20(7) + 1
!
      ie20( 9)   = numnod + numedge_cube + num_h*(num_h-1) + 1
      ie20(12)   = numnod + numedge_cube + (num_h+1)*num_h              &
     &            + (num_h+1)*(num_h-1) + 1
      ie20(14)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &            + 3*(num_h-1)
      ie20(15)   = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h    &
     &            + 3*num_h
      ie20(18)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &            - (num_h-1) + 1
      ie20(19)   = numnod + numedge_cube + 2*(num_h+1)*num_h &
     &          + 3*num_h
!
      ie20(10) = ie20(12) + 1
      ie20(11) = ie20( 9)      + num_h
!
      ie20(13) = ie20(14) + 1
      ie20(16) = ie20(15) + 1
!
      ie20(17) = ie20(19) + 2
      ie20(20) = ie20(19) + 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:8)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:20)
!
      end subroutine set_corner_quad_connect_4
!
!   --------------------------------------------------------------------
!
      subroutine set_corner_quad_connect_3(iele, ifile, ifile_20,       &
     &          numnod, num_h, num_v)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
!  corner (x=y=cube_size, z = -cube_size)
!
      ie20(1) = ncube_c + (num_h+1)*num_h - 1
      ie20(6) = ncube_c + (num_h+1)**2 + 2*num_h
      ie20(5) = (num_h-1)**2
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 1 + (num_h+1)
      ie20(4) = ie20(1)     + (num_h+1)
      ie20(7) = ie20(6) + 1
      ie20(8) = ie20(6) + 2
!
      ie20( 9)   = numnod + numedge_cube + num_h**2
      ie20(12)   = numnod + numedge_cube + (num_h+1)*num_h              &
     &            + (num_h+1)*(num_h-1) + num_h
      ie20(13)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &            + 2*(num_h-1)
      ie20(14)   = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h    &
     &            + 2*num_h
      ie20(18)   = numnod + numedge_cube + 2*(num_h+1)*num_h + 2*num_h
      ie20(17)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)
!
      ie20(10) = ie20(12) + 1
      ie20(11) = ie20( 9)      + num_h
!
      ie20(15) = ie20(14) + 1
      ie20(16) = ie20(13) + 1
!
      ie20(19) = ie20(18) + 1
      ie20(20) = ie20(18) + 2
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:8)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:20)
!
      end subroutine set_corner_quad_connect_3
!
!   --------------------------------------------------------------------
!
      subroutine set_corner_quad_connect_5(iele, ifile, ifile_20,       &
     &          numnod, num_h, num_v)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube, inod_c, inod_o
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
      inod_c = (num_h-1)**2*(num_v-2)
      inod_o = ncube_c + (num_h+1)**2 + 4*num_h*(num_v-1)
!
!  corner (x = y = -cube_size, z =cube_size)
!
      ie20(3) = inod_c + 1
      ie20(1) = inod_o - 4*num_h + 1
      ie20(5) = inod_o + 1
!
      ie20(2) = ie20(1) + 1
      ie20(4) = ie20(1) + 4*num_h - 1
      ie20(6) = ie20(5) + 1
      ie20(7) = ie20(5) + 1 + (num_h+1)
      ie20(8) = ie20(5)     + (num_h+1)
!
      ie20( 9) = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h      &
     &            + 4*num_h*(num_v-2) + 1
      ie20(10) = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)               &
     &            + 4*(num_h-1)*(num_v-2) + 1
      ie20(13) = numnod + numedge_cube + (2*num_h+8*num_v-2)*num_h + 1
      ie20(16) = numnod + numedge_cube + (3*num_h+8*num_v-1)*num_h + 1
      ie20(17) = numnod + numedge_cube + (2*num_h+4*num_v-2)*num_h + 1
      ie20(19) = numnod + (3*num_h-1)*(num_h-1)*(num_v-1) + 1
!
      ie20(11) = ie20(10) + 4*(num_h-1)   - 1
      ie20(12) = ie20( 9)  + 4*num_h - 1
!
      ie20(14) = ie20(16) + 1
      ie20(15) = ie20(13)     + num_h
!
      ie20(18) = ie20(17) + 1
      ie20(20) = ie20(17) + 4*num_h - 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:8)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:20)
!
      end subroutine set_corner_quad_connect_5
!
!   --------------------------------------------------------------------
!
      subroutine set_corner_quad_connect_6(iele, ifile, ifile_20,       &
     &          numnod, num_h, num_v)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube, inod_c, inod_o
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
      inod_c = (num_h-1)**2*(num_v-2)
      inod_o = ncube_c + (num_h+1)**2 + 4*num_h*(num_v-1)
!
!  corner (x = cube_size, y = -cube_size, z = cube_size)
!
      ie20(4) = inod_c + (num_h-1)
      ie20(1) = inod_o - 3*num_h
      ie20(5) = inod_o + num_h
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 2
      ie20(6) = ie20(5) + 1
      ie20(7) = ie20(5) + 1 + (num_h+1)
      ie20(8) = ie20(5)     + (num_h+1)
!
      ie20( 9)    = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h   &
     &             + 4*num_h*(num_v-2) + num_h
      ie20(12)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &            + 4*(num_h-1)*(num_v-2) + (num_h-1)
      ie20(13)   = numnod + numedge_cube                                &
     &            + (2*num_h+8*num_v-2)*num_h + num_h
      ie20(16)   = numnod + numedge_cube                                &
     &            + (3*num_h+8*num_v-1)*num_h + num_h
      ie20(17)   = numnod + numedge_cube                                &
     &            + (2*num_h+4*num_v-2)*num_h + num_h
      ie20(20)   = numnod + (3*num_h-1)*(num_h-1)*(num_v-1) + (num_h-1)
!
      ie20(10) = ie20( 9)  + 1
      ie20(11) = ie20(12) + 1
!
      ie20(14) = ie20(16) + 1
      ie20(15) = ie20(13)     + num_h
!
      ie20(18) = ie20(17) + 1
      ie20(19) = ie20(17) + 2
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:8)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:20)
!
      end subroutine set_corner_quad_connect_6
!
!   --------------------------------------------------------------------
!
      subroutine set_corner_quad_connect_8(iele, ifile, ifile_20,       &
     &          numnod, num_h, num_v)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube, inod_c, inod_o
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
      inod_c = (num_h-1)**2*(num_v-2)
      inod_o = ncube_c + (num_h+1)**2 + 4*num_h*(num_v-1)
!
!  corner (x=-cube_size, y=z=cube_size)
!
      ie20(2) = inod_c + (num_h-1)*(num_h-2) + 1
      ie20(3) = inod_o - num_h
      ie20(5) = inod_o + (num_h+1) * (num_h-1) + 1 
!
      ie20(1) = ie20(3) + 2
      ie20(4) = ie20(3) + 1
      ie20(6) = ie20(5) + 1
      ie20(7) = ie20(5) + 1 + (num_h+1)
      ie20(8) = ie20(5)     + (num_h+1)
!
      ie20(10)  = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)              &
     &           + 4*(num_h-1)*(num_v-2) + 3*(num_h-1)
      ie20(11)  = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h     &
     &           + 4*num_h*(num_v-2) + 3*num_h
      ie20(13)  = numnod + numedge_cube + (2*num_h+8*num_v-2)*num_h     &
     &           + num_h*(num_h-1) + 1
      ie20(16)  = numnod + numedge_cube + (3*num_h+8*num_v-1)*num_h     &
     &           + (num_h+1)*(num_h-1) + 1
      ie20(18)  = numnod + (3*num_h-1)*(num_h-1)*(num_v-1)              &
     &           + (num_h-1)*(num_h-2) + 1
      ie20(19)  = numnod + numedge_cube + (2*num_h+4*num_v-2)*num_h     &
     &           + 3*num_h
!
      ie20( 9) = ie20(10) + 1
      ie20(12) = ie20(11) + 1
!
      ie20(14) = ie20(16) + 1
      ie20(15) = ie20(13)     + num_h
!
      ie20(17) = ie20(19) + 2
      ie20(20) = ie20(19) + 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:8)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:20)
!
      end subroutine set_corner_quad_connect_8
!
!   --------------------------------------------------------------------
!
      subroutine set_corner_quad_connect_7(iele, ifile, ifile_20,       &
     &          numnod, num_h, num_v)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube, inod_c, inod_o
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
      inod_c = (num_h-1)**2*(num_v-2)
      inod_o = ncube_c + (num_h+1)**2 + 4*num_h*(num_v-1)
!
!  corner (x=y=z=cube_size)
!
      ie20(1) = inod_c + (num_h-1)**2
      ie20(2) = inod_o - 2*num_h
      ie20(5) = inod_o + (num_h+1) * num_h - 1
!
      ie20(3) = ie20(2) + 1
      ie20(4) = ie20(2) + 2
      ie20(6) = ie20(5) + 1
      ie20(7) = ie20(5) + 1 + (num_h+1)
      ie20(8) = ie20(5)     + (num_h+1)
!
      ie20( 9)  = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)              &
     &           + 4*(num_h-1)*(num_v-2) + 2*(num_h-1)
      ie20(10)  = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h     &
     &           + 4*num_h*(num_v-2) + 2*num_h
      ie20(13)  = numnod + numedge_cube + (2*num_h+8*num_v-2)*num_h     &
     &           + num_h**2
      ie20(16)  = numnod + numedge_cube + (3*num_h+8*num_v-1)*num_h     &
     &           + (num_h+1)*(num_h-1) + num_h
      ie20(17)  = numnod + (3*num_h-1)*(num_h-1)*(num_v-1)              &
     &           + (num_h-1)**2
      ie20(18)  = numnod + numedge_cube + (2*num_h+4*num_v-2)*num_h     &
     &           + 2*num_h
!
      ie20(11) = ie20(10) + 1
      ie20(12) = ie20( 9)  + 1
!
      ie20(14) = ie20(16) + 1
      ie20(15) = ie20(13)     + num_h
!
      ie20(19) = ie20(18) + 1
      ie20(20) = ie20(18) + 2
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:8)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:20)
!
      end subroutine set_corner_quad_connect_7
!
!   --------------------------------------------------------------------
!
      end module set_corner_quad_connect
