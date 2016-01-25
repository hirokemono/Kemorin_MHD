!set_side_quad_connect.f90
!      module set_side_quad_connect
!
!     Written by H. Matsui on Apr., 2003
!     Modified by H. Matsui on Oct., 2007
!     Modified by H. Matsui on Dec., 2011
!
      module set_side_quad_connect
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
      subroutine set_side_quad_connect_1(iele, ifile, ifile_20,         &
     &          numnod, num_h, num_v, ix)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: ix
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
!  edge (y = z = -cube_size)
!
      ie20(1) = ncube_c + ix + 1
      ie20(5) = ncube_c + (num_h+1)*(num_h+1) + ix + 1
      ie20(8) = ix
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 1 + (num_h+1)
      ie20(4) = ie20(1)     + (num_h+1)
      ie20(6) = ie20(5) + 1
      ie20(7) = ie20(8) + 1
!
      ie20( 9)   = numnod + numedge_cube + ix+1
      ie20(12)   = numnod + numedge_cube + (num_h+1)*num_h + ix+1
      ie20(13)   = numnod + numedge_cube                                &
     &            + (2*num_h+4*num_v+2)*num_h + ix+1
      ie20(15)   = numnod + ix
      ie20(16)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1) + ix
      ie20(17)   = numnod + numedge_cube + 2*(num_h+1)*num_h + ix + 1
      ie20(20)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &                    - (num_h-1)*(num_h-1) + ix
!
      ie20(10) = ie20(12) + 1
      ie20(11) = ie20( 9)      + num_h
!
      ie20(14) = ie20(16) + 1
!
      ie20(18) = ie20(17) + 1
      ie20(19) = ie20(20) + 1
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_1
!
!   --------------------------------------------------------------------
!
      subroutine set_side_quad_connect_4(iele, ifile, ifile_20,         &
     &          numnod, num_h, num_v, iy)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: iy
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c, numedge_cube
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
!  edge (x=-cube_size, z = -cube_size)
!
      ie20(1) = ncube_c + (num_h+1) * iy + 1
      ie20(8) = ncube_c + (num_h+1)**2 + 4*num_h - iy
      ie20(6) = (num_h-1)*(iy-1) + 1
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 1 + (num_h+1)
      ie20(4) = ie20(1)     + (num_h+1)
      ie20(5) = ie20(8) + 1
      ie20(7) = ie20(6) + (num_h-1)
!
      ie20( 9)   = numnod + numedge_cube + num_h*iy + 1
      ie20(12)   = numnod + numedge_cube + (num_h+1)*num_h              &
     &            + (num_h+1)*iy + 1
      ie20(15)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &            + 4*(num_h-1) - iy
      ie20(16)   = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h    &
     &            + 4*num_h - iy
      ie20(14)   = numnod + (num_h-2)*(num_h-1)*(num_v-1)               &
     &            + (num_h-1)*(iy-1) + 1
      ie20(18)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &            - (num_h-1)*(num_h-1) + (num_h-1)*(iy-1) + 1
      ie20(20)   = numnod + numedge_cube + 2*(num_h+1)*num_h &
     &            + 4*num_h - iy
!
      ie20(10) = ie20(12) + 1
      ie20(11) = ie20( 9) + num_h
!
      ie20(13) = ie20(15) + 1
!
      ie20(17) = ie20(20) + 1
      ie20(19) = ie20(18) + (num_h-1)
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_4
!
!   --------------------------------------------------------------------
!
      subroutine set_side_quad_connect_2(iele, ifile, ifile_20,         &
     &          numnod, num_h, num_v, iy)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: iy
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c, numedge_cube
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
!  edge (x=cube_size, z = -cube_size)
!
       ie20(1) = ncube_c + (num_h+1) * (iy+1) - 1
       ie20(6) = ncube_c + (num_h+1)**2 + num_h + iy + 1
       ie20(5) = (num_h-1) * iy
!
       ie20(2) = ie20(1) + 1
       ie20(3) = ie20(1) + 1 + (num_h+1)
       ie20(4) = ie20(1)     + (num_h+1)
       ie20(7) = ie20(6) + 1
       ie20(8) = ie20(5)     + (num_h-1)
!
       ie20( 9)   = numnod + numedge_cube + num_h*(iy+1)
       ie20(12)   = numnod + numedge_cube + (num_h+1)*num_h             &
     &             + (num_h+1)*iy + num_h
       ie20(13)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)            &
     &             + num_h-1 + iy
       ie20(14)   = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h   &
     &             + num_h + iy+1
       ie20(16)   = numnod + (num_h-2)*(num_h-1)*(num_v-1)              &
     &             + (num_h-1)*iy
       ie20(18)   = numnod + numedge_cube + 2*(num_h+1)*num_h &
     &             + num_h + iy+1
       ie20(17)   = numnod +  (3*num_h-5)*(num_h-1)*(num_v-1)           &
     &             - (num_h-1)*(num_h-1) + (num_h-1)*iy
!
       ie20(10) = ie20(12) + 1
       ie20(11) = ie20( 9)      + num_h
!
       ie20(15) = ie20(13) + 1
!
       ie20(19) = ie20(18) + 1
       ie20(20) = ie20(17)     + (num_h-1)
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_2
!
!   --------------------------------------------------------------------
!
      subroutine set_side_quad_connect_3(iele, ifile, ifile_20,         &
     &          numnod, num_h, num_v, ix)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: ix
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
!  edge (y = z = -cube_size)
!
      ie20(1) = ncube_c +  num_h**2 + ix
      ie20(7) = ncube_c + (num_h+1)**2 + 3*num_h - ix
      ie20(5) = (num_h-2)*(num_h-1) + ix
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 1 + (num_h+1)
      ie20(4) = ie20(1)     + (num_h+1)
      ie20(6) = ie20(5) + 1
      ie20(8) = ie20(7) + 1
!
      ie20( 9)   = numnod + numedge_cube + num_h*(num_h-1) + ix + 1
      ie20(12)   = numnod + numedge_cube + (num_h+1)*num_h              &
     &            + (num_h+1)*(num_h-1) + ix + 1
      ie20(13)   = numnod + (num_h-3)*(num_h-1) + ix + 1
      ie20(14)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &            + 3*(num_h-1) - ix
      ie20(15)   = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h    &
     &            + 3*num_h - ix
      ie20(17)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &            - (num_h-1) + ix
      ie20(19)   = numnod + numedge_cube + 2*(num_h+1)*num_h            &
     &          + 3*num_h -ix
!
      ie20(10) = ie20(12) + 1
      ie20(11) = ie20( 9)      + num_h
!
      ie20(16) = ie20(14) + 1
!
      ie20(18) = ie20(17) + 1
      ie20(20) = ie20(19) + 1
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_3
!
!   --------------------------------------------------------------------
!
      subroutine set_side_quad_connect_9(iele, ifile, ifile_20,         &
     &          numnod, num_h, num_v, iz)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: iz
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube, inod_c, inod0
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
!  edge (x = y = -cube_size)
!
      inod0 = ncube_c + (num_h+1)**2 + 4*num_h*(iz-1)
      inod_c = (num_h-1)**2*(iz-1)
!
      ie20(1) = inod0 + 1
      ie20(3) = inod_c + 1
!
      ie20(2) = ie20(1) + 1
      ie20(4) = inod0      + 4*num_h
      ie20(5) = ie20(1)      + 4*num_h
      ie20(6) = ie20(1)  + 1 + 4*num_h
      ie20(7) = ie20(3)      + (num_h-1)**2
      ie20(8) = inod0      + 4*num_h*2
!
      ie20( 9)   = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h    &
     &            + 4*num_h*(iz-1) + 1
      ie20(10)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &            + 4*(num_h-1)*(iz-1) + 1
      ie20(17)   = numnod + numedge_cube + 2*(num_h+1)*num_h            &
     &            + 4*num_h*iz + 1
      ie20(19)   = numnod + 2*(num_h-2)*(num_h-1)*(num_v-1)             &
     &            + (num_h-1)**2*(iz-1) + 1
!
      ie20(11) = ie20(10) + 4*(num_h-1)   - 1
      ie20(12) = ie20( 9)  + 4*num_h - 1
!
      ie20(13) = ie20( 9)  + 4*num_h
      ie20(14) = ie20(10) + 4*(num_h-1)
      ie20(15) = ie20(10) + 8*(num_h-1)   - 1
      ie20(16) = ie20( 9)  + 8*num_h - 1
!
      ie20(18) = ie20(17) + 1
      ie20(20) = ie20(17) + 4*num_h - 1
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_9
!
!   --------------------------------------------------------------------
!
      subroutine set_side_quad_connect_10(iele, ifile, ifile_20,        &
     &          numnod, num_h, num_v, iz)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: iz
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube, inod_c, inod0
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
!  edge (x = cube_size, y = -cube_size)
!
      inod0 = ncube_c + (num_h+1)**2 + 4*num_h*(iz-1)
      inod_c = (num_h-1)**2*(iz-1)
!
      ie20(1) = inod0 + num_h
      ie20(4) = inod_c + (num_h-1)
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 2
      ie20(5) = ie20(1)     + 4*num_h
      ie20(6) = ie20(1) + 1 + 4*num_h
      ie20(7) = ie20(1) + 2 + 4*num_h
      ie20(8) = ie20(4)     + (num_h-1)**2
!
      ie20( 9)   = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h    &
     &            + 4*num_h*(iz-1) + num_h
      ie20(12)   = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)             &
     &            + 4*(num_h-1)*(iz-1) + (num_h-1)
      ie20(17)   = numnod + numedge_cube + 2*(num_h+1)*num_h            &
     &            + 4*num_h*iz + num_h
      ie20(20)   = numnod + 2*(num_h-2)*(num_h-1)*(num_v-1)             &
     &            + (num_h-1)**2*(iz-1) + (num_h-1)
!
      ie20(10) = ie20( 9)  + 1
      ie20(11) = ie20(12) + 1
!
      ie20(13) = ie20( 9)      + 4*num_h
      ie20(14) = ie20( 9)  + 1 + 4*num_h
      ie20(15) = ie20(12) + 1 + 4*(num_h-1)
      ie20(16) = ie20(12)     + 4*(num_h-1)
!
      ie20(18) = ie20(17) + 1
      ie20(19) = ie20(17) + 2
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_10
!
!   --------------------------------------------------------------------
!
      subroutine set_side_quad_connect_12(iele, ifile, ifile_20,        &
     &          numnod, num_h, num_v, iz)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: iz
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube, inod_c, inod0
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
!  edge (x = cube_size, y = -cube_size)
!
      inod0 = ncube_c + (num_h+1)**2 + 4*num_h*(iz-1)
      inod_c = (num_h-1)**2*(iz-1)
!
      ie20(3) = inod0 + 3 * num_h
      ie20(2) = inod_c + (num_h-1)*(num_h-2) + 1
!
      ie20(1) = ie20(3) + 2
      ie20(4) = ie20(3) + 1
      ie20(5) = ie20(3) + 2 + 4*num_h
      ie20(6) = ie20(2)               + (num_h-1)**2
      ie20(7) = ie20(3)     + 4*num_h
      ie20(8) = ie20(3) + 1 + 4*num_h
!
      ie20(10)  = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)              &
     &           + 4*(num_h-1)*(iz-1) + 3*(num_h-1)
      ie20(11)  = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h     &
     &           + 4*num_h*(iz-1)+ 3*num_h
      ie20(18)  = numnod + 2*(num_h-2)*(num_h-1)*(num_v-1)              &
     &           + (num_h-1)**2*(iz-1) + (num_h-1)*(num_h-2) + 1
      ie20(19)  = numnod + numedge_cube + 2*(num_h+1)*num_h             &
     &           + 4*num_h*iz + 3*num_h
!
      ie20( 9) = ie20(10) + 1
      ie20(12) = ie20(11) + 1
!
      ie20(13) = ie20(10) + 1 + 4*(num_h-1)
      ie20(14) = ie20(10)     + 4*(num_h-1)
      ie20(15) = ie20(11)     + 4*num_h
      ie20(16) = ie20(11) + 1 + 4*num_h
!
      ie20(17) = ie20(19) + 2
      ie20(20) = ie20(19) + 1
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_12
!
!   --------------------------------------------------------------------
!
      subroutine set_side_quad_connect_11(iele, ifile, ifile_20,        &
     &          numnod, num_h, num_v, iz)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: iz
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: ncube_c,  numedge_cube, inod_c, inod0
      integer(kind = kint) :: ie20(20)
!
!
      ncube_c  = (num_h-1)*(num_h-1)*(num_v-1)
      numedge_cube = (3*num_h-1)*(num_h-1)*(num_v-1)                    &
     &              + (num_h-1)*(num_h-1)
!
!  edge (x=y=cube_size)
!
      inod0 = ncube_c + (num_h+1)**2 + 4*num_h*(iz-1)
      inod_c = (num_h-1)**2*(iz-1)
!
      ie20(2) = inod0 + 2 * num_h
      ie20(1) = inod_c + (num_h-1)**2
!
      ie20(3) = ie20(2) + 1
      ie20(4) = ie20(2) + 2
      ie20(5) = ie20(1)     + (num_h-1)**2
      ie20(6) = ie20(2)     + 4*num_h
      ie20(7) = ie20(2) + 1 + 4*num_h
      ie20(8) = ie20(2) + 2 + 4*num_h
!
      ie20( 9)  = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)              &
     &           + 4*(num_h-1)*(iz-1) + 2*(num_h-1)
      ie20(10)  = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h     &
     &           + 4*num_h*(iz-1) + 2*num_h
      ie20(17)  = numnod + 2*(num_h-2)*(num_h-1)*(num_v-1)              &
     &           + (num_h-1)**2*(iz-1) + (num_h-1)**2
      ie20(18)  = numnod + numedge_cube + 2*(num_h+1)*num_h             &
     &           + 4*num_h*iz + 2*num_h
!
      ie20(11) = ie20(10) + 1
      ie20(12) = ie20( 9)  + 1
!
      ie20(13) = ie20( 9)      + 4*(num_h-1)
      ie20(14) = ie20(10)     + 4*num_h
      ie20(15) = ie20(10) + 1 + 4*num_h
      ie20(16) = ie20( 9)  + 1 + 4*(num_h-1)
!
      ie20(19) = ie20(18) + 1
      ie20(20) = ie20(18) + 2
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_11
!
!   --------------------------------------------------------------------
!
      subroutine set_side_quad_connect_5(iele, ifile, ifile_20,         &
     &          numnod, num_h, num_v, ix)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: ix
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
!  edge ( y = -cube_size, z = cube_size)
!
      ie20(4) = inod_c + ix
      ie20(1) = inod_o - 4*num_h + ix + 1
      ie20(5) = inod_o + ix + 1
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(4) + 1
      ie20(6) = ie20(5) + 1
      ie20(7) = ie20(5) + 1 + (num_h+1)
      ie20(8) = ie20(5)     + (num_h+1)
!
      ie20( 9) = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h      &
     &          + 4*num_h*(num_v-2) + ix + 1
      ie20(11) = numnod + (num_h-1)*(num_h-2)*(num_v-2) + ix
      ie20(12) = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)               &
     &          + 4*(num_h-1)*(num_v-2) + ix
      ie20(13) = numnod + numedge_cube                                  &
     &          + (2*num_h+8*num_v-2)*num_h + ix + 1
      ie20(16) = numnod + numedge_cube                                  &
     &          + (3*num_h+8*num_v-1)*num_h + ix + 1
      ie20(17) = numnod + numedge_cube                                  &
     &          + (2*num_h+4*num_v-2)*num_h + ix + 1
      ie20(20) = numnod + (3*num_h-1)*(num_h-1)*(num_v-1) + ix
!
      ie20(10) = ie20(12) + 1
!
      ie20(14) = ie20(16) + 1
      ie20(15) = ie20(13)     + num_h
!
      ie20(18) = ie20(17) + 1
      ie20(19) = ie20(20) + 1
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_5
!
!   --------------------------------------------------------------------
!
      subroutine set_side_quad_connect_8(iele, ifile, ifile_20,         &
     &          numnod, num_h, num_v, iy)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: iy
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
!  edge (x=-cube_size, z = cube_size)
!
      ie20(2) = inod_c + (num_h-1)*(iy-1) + 1
      ie20(4) = inod_o - iy
      ie20(5) = inod_o + (num_h+1) * iy + 1 
!
      ie20(1) = ie20(4) + 1
      ie20(3) = ie20(2)     + (num_h-1)
      ie20(6) = ie20(5) + 1
      ie20(7) = ie20(5) + 1 + (num_h+1)
      ie20(8) = ie20(5)     + (num_h+1)
!
      ie20(10)  = numnod + (num_h-2)*(num_h-1)*(num_v-1)                &
     &           + (num_h-1)*(num_h-2)*(num_v-2) + (num_h-1)*(iy-1) + 1
      ie20(11)  = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)              &
     &           + 4*(num_h-1)*(num_v-1) - iy
      ie20(12)  = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h     &
     &           + 4*num_h*(num_v-1) - iy
      ie20(13)  = numnod + numedge_cube + (2*num_h+8*num_v-2)*num_h     &
     &           + num_h*iy + 1
      ie20(16)  = numnod + numedge_cube + (3*num_h+8*num_v-1)*num_h     &
     &           + (num_h+1)*iy + 1
      ie20(18)  = numnod + (3*num_h-1)*(num_h-1)*(num_v-1)              &
     &           + (num_h-1)*(iy-1) + 1
      ie20(20)  = numnod + numedge_cube + (2*num_h+4*num_v-2)*num_h     &
     &           + 4*num_h - iy
!
      ie20( 9) = ie20(11) + 1
!
      ie20(14) = ie20(16) + 1
      ie20(15) = ie20(13)     + num_h
!
      ie20(17) = ie20(20) + 1
      ie20(19) = ie20(18) + (num_h-1)
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_8
!
!   --------------------------------------------------------------------
!
      subroutine set_side_quad_connect_6(iele, ifile, ifile_20,         &
     &          numnod, num_h, num_v, iy)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: iy
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
!  edge (x=cube_size, z = cube_size)
!
      ie20(1) = inod_c + (num_h-1)*iy
      ie20(2) = inod_o - 3*num_h + iy + 1
      ie20(5) = inod_o + (num_h+1) * (iy+1) - 1
!
      ie20(3) = ie20(2) + 1
      ie20(4) = ie20(1)     + (num_h-1)
      ie20(6) = ie20(5) + 1
      ie20(7) = ie20(5) + 1 + (num_h+1)
      ie20(8) = ie20(5)     + (num_h+1)
!
      ie20( 9)  = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)              &
     &           + 4*(num_h-1)*(num_v-2) + (num_h-1) + iy
      ie20(10)  = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h     &
     &           + 4*num_h*(num_v-2) + num_h + iy+1
      ie20(12)  = numnod + (num_h-2)*(num_h-1)*(num_v-1)                &
     &           + (num_h-1)*(num_h-2)*(num_v-2) + (num_h-1)*iy
      ie20(13)  = numnod + numedge_cube + (2*num_h+8*num_v-2)*num_h     &
     &            + num_h*iy + num_h
      ie20(16)  = numnod + numedge_cube + (3*num_h+8*num_v-1)*num_h     &
     &            + (num_h+1)*iy + num_h
      ie20(17)  = numnod + (3*num_h-1)*(num_h-1)*(num_v-1)              &
     &           + (num_h-1)*iy
      ie20(18)  = numnod + numedge_cube + (2*num_h+4*num_v-2)*num_h     &
     &           + num_h + iy + 1
!
      ie20(11) = ie20( 9) + 1
!
      ie20(14) = ie20(16) + 1
      ie20(15) = ie20(13)     + num_h
!
      ie20(19) = ie20(18) + 1
      ie20(20) = ie20(17)     + (num_h-1)
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_6
!
!   --------------------------------------------------------------------
!
      subroutine set_side_quad_connect_7(iele, ifile, ifile_20,         &
     &          numnod, num_h, num_v, ix)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h, num_v
      integer(kind = kint), intent(in) :: ix
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
!  edge (y=z=cube_size)
!
      ie20(1) = inod_c + (num_h-2)*(num_h-1) + ix
      ie20(3) = inod_o - num_h - ix
      ie20(5) = inod_o + num_h * num_h + ix
!
      ie20(2) = ie20(1) + 1
      ie20(4) = ie20(3) + 1
      ie20(6) = ie20(5) + 1
      ie20(7) = ie20(5) + 1 + (num_h+1)
      ie20(8) = ie20(5)     + (num_h+1)
!
      ie20( 9)  = numnod + (num_h-2)*(num_h-1)*(num_v-1)                &
     &           - (num_h-1) + ix + 1
      ie20(10)  = numnod + (3*num_h-5)*(num_h-1)*(num_v-1)              &
     &           + 4*(num_h-1)*(num_v-2) + 3*(num_h-1) - ix
      ie20(11)  = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h     &
     &           + 4*num_h*(num_v-1) - num_h - ix
      ie20(13)  = numnod + numedge_cube + (2*num_h+8*num_v-2)*num_h     &
     &           + num_h*(num_h-1) + ix + 1
      ie20(16)  = numnod + numedge_cube + (3*num_h+8*num_v-1)*num_h     &
     &           + (num_h+1)*(num_h-1)     &
     &           + ix + 1
      ie20(17)  = numnod + (3*num_h-1)*(num_h-1)*(num_v-1)              &
     &           + (num_h-2)*(num_h-1) + ix
      ie20(19)  = numnod + numedge_cube + (2*num_h+4*num_v+2)*num_h     &
     &           - num_h - ix
!
      ie20(12) = ie20(10) + 1
!
      ie20(14) = ie20(16) + 1
      ie20(15) = ie20(13)     + num_h
!
      ie20(18) = ie20(17) + 1
      ie20(20) = ie20(19) + 1
!
      iele = iele + 1
      write(ifile,'(21i16)') iele, ie20(1:8)
      if(ifile_20 .gt. 0) write(ifile_20, '(21i16)') iele, ie20(1:20)
!
      end subroutine set_side_quad_connect_7
!
!   --------------------------------------------------------------------
!
      end module set_side_quad_connect
