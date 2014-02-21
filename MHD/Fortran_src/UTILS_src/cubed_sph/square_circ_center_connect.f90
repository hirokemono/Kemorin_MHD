!square_circ_center_connect.f90
!      module square_circ_center_connect
!
!     Written by H. Matsui on Apr., 2003
!     Modified by H. Matsui on Oct., 2007
!
!      subroutine set_center_connect_quad(iele, ifile, ifile_20)
!      subroutine set_center_rect_connect_quad(iele, ifile, ifile_20)
!
      module square_circ_center_connect
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
      subroutine square_center_connect_quad(iele, ifile, ifile_20,      &
     &          numnod, num_h)
!
!      use set_corner_quad_connect
!      use set_side_quad_connect
!      use set_surface_quad_connect
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h
      integer(kind = kint), intent(inout) :: iele
!
      integer(kind = kint) :: ix, iy
!
!  corner (x = y =-cube_size)
      call square_corner_quad_connect_1(iele, ifile, ifile_20,          &
     &    numnod, num_h)
!
!  edge (y = -cube_size)
      do ix = 1, num_h-2
        call square_side_quad_connect_1(iele, ifile, ifile_20,          &
     &      numnod, num_h, ix)
      end do
!
!  corner (x = cube_size, y = -cube_size)
      call square_corner_quad_connect_2(iele, ifile, ifile_20,          &
     &    numnod, num_h)
!
      do iy = 1, num_h-2
!  edge (x = -cube_size)
        call square_side_quad_connect_4(iele, ifile, ifile_20,          &
     &      numnod, num_h, iy)
!
        do ix = 1, num_h-2
          call square_surf_quad_connect(iele, ifile, ifile_20,          &
     &        numnod, num_h, ix, iy)
        end do
!
!  edge (x = cube_size)
        call square_side_quad_connect_2(iele, ifile, ifile_20,          &
     &      numnod, num_h, iy)
      end do
!
!  corner (x = -cube_size, y = cube_size)
      call square_corner_quad_connect_4(iele, ifile, ifile_20,          &
     &    numnod, num_h)
!
!  edge (y = -cube_size)
      do ix = 1, num_h-2
        call square_side_quad_connect_3(iele, ifile, ifile_20,          &
     &      numnod, num_h, ix)
      end do
!
!  corner (x = y = cube_size)
      call square_corner_quad_connect_3(iele, ifile, ifile_20,          &
     &    numnod, num_h)
!
!
      end subroutine square_center_connect_quad
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine square_corner_quad_connect_1(iele, ifile, ifile_20,    &
     &          numnod, num_h)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: numedge_cube, inod0
      integer(kind = kint) :: ie20(20)
!
!
      numedge_cube = 2*(num_h-2)*(num_h-1)
!
!  edge (x = y = -cube_size)
!
      inod0 = (num_h-1)**2
!
      ie20(1) = inod0 + 1
      ie20(3) = 1
!
      ie20(2) = ie20(1) + 1
      ie20(4) = inod0      + 4*num_h
!
      ie20( 5)   = numnod + numedge_cube + 4*(num_h-1) + 1
      ie20( 6)   = numnod + numedge_cube + 1
!
      ie20( 7) = ie20( 6) + 4*(num_h-1) - 1
      ie20( 8) = ie20( 5)  + 4*num_h - 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:4)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:8)
!
      end subroutine square_corner_quad_connect_1
!
!   --------------------------------------------------------------------
!
      subroutine square_corner_quad_connect_2(iele, ifile, ifile_20,    &
     &          numnod, num_h)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: numedge_cube, inod0
      integer(kind = kint) :: ie20(20)
!
!
      numedge_cube = 2*(num_h-2)*(num_h-1)
!
      inod0 = (num_h-1)**2
!
      ie20(1) = inod0 + num_h
      ie20(4) = num_h-1
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 2
!
      ie20( 5) = numnod + numedge_cube + 4*(num_h-1) + 1 + num_h-1
      ie20( 8) = numnod + numedge_cube + num_h-1
!
      ie20( 6) = ie20( 5) + 1
      ie20( 7) = ie20( 8) + 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:4)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:8)
!
      end subroutine square_corner_quad_connect_2
!
!   --------------------------------------------------------------------
!
      subroutine square_corner_quad_connect_4(iele, ifile, ifile_20,    &
     &          numnod, num_h)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: numedge_cube, inod0
      integer(kind = kint) :: ie20(20)
!
!
      numedge_cube = 2*(num_h-2)*(num_h-1)
!
!  edge (x = -cube_size)
!
      inod0 = (num_h-1)**2
!
      ie20(4) = inod0      + 3*num_h + 1
      ie20(2) = (num_h-1)*(num_h-2) + 1
!
      ie20(1) = ie20(4) + 1
      ie20(3) = ie20(4) - 1
!
      ie20( 5) = numnod + numedge_cube + 3*(num_h-1) + 1
      ie20( 8) = numnod + numedge_cube + 3*(num_h-1) + 4*num_h
!
      ie20( 6) = ie20( 5) - 1
      ie20( 7) = ie20( 8) - 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:4)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:8)
!
      end subroutine square_corner_quad_connect_4
!
!   --------------------------------------------------------------------
!
      subroutine square_corner_quad_connect_3(iele, ifile, ifile_20,    &
     &          numnod, num_h)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: numedge_cube, inod0
      integer(kind = kint) :: ie20(20)
!
!
      numedge_cube = 2*(num_h-2)*(num_h-1)
!
!  edge (x = -cube_size)
!
      inod0 = (num_h-1)**2
!
      ie20(2) = inod0      + 2*num_h
      ie20(1) = (num_h-1)*(num_h-1)
!
      ie20(3) = ie20(2) + 1
      ie20(4) = ie20(2) + 2
!
      ie20( 7) = numnod + numedge_cube + 3*(num_h-1) + 3*num_h
      ie20( 8) = numnod + numedge_cube + 2*num_h - 1
!
      ie20( 6) = ie20( 7) - 1
      ie20( 5) = ie20( 8) - 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:4)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:8)
!
      end subroutine square_corner_quad_connect_3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine square_side_quad_connect_1(iele, ifile, ifile_20,      &
     &          numnod, num_h, ix)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h
      integer(kind = kint), intent(in) :: ix
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: numedge_cube, inod0
      integer(kind = kint) :: ie20(20)
!
!
      numedge_cube = 2*(num_h-2)*(num_h-1)
!
!  edge (x = y = -cube_size)
!
      inod0 = (num_h-1)**2
!
      ie20(1) = inod0 + 1 + ix
      ie20(4) = ix
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(4) + 1
!
      ie20( 5) = numnod + numedge_cube + 4*(num_h-1) + 1 + ix
      ie20( 7) = numnod + ix
      ie20( 8) = numnod + numedge_cube + ix
!
      ie20( 6) = ie20( 8) + 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:4)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:8)
!
      end subroutine square_side_quad_connect_1
!
!   --------------------------------------------------------------------
!
      subroutine square_side_quad_connect_4(iele, ifile, ifile_20,      &
     &          numnod, num_h, iy)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h
      integer(kind = kint), intent(in) :: iy
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: numedge_cube, inod0
      integer(kind = kint) :: ie20(20)
!
!
      numedge_cube = 2*(num_h-2)*(num_h-1)
!
!  edge (x = -cube_size)
!
      inod0 = (num_h-1)**2
!
      ie20(4) = inod0      + 4*num_h - iy
      ie20(2) = (num_h-1)*(iy-1) + 1
!
      ie20(1) = ie20(4) + 1
      ie20(3) = ie20(2) + (num_h-1)
!
      ie20( 5) = numnod + numedge_cube + 4*(num_h-1) + 1 - iy
      ie20( 6) = numnod + (num_h-2)*(num_h-1) + (num_h-1) * (iy-1) + 1
      ie20( 8) = numnod + numedge_cube + 4*(num_h-1) + 4*num_h - iy
!
      ie20( 7) = ie20( 5) - 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:4)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:8)
!
      end subroutine square_side_quad_connect_4
!
!   --------------------------------------------------------------------
!
      subroutine square_side_quad_connect_2(iele, ifile, ifile_20,      &
     &          numnod, num_h, iy)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h
      integer(kind = kint), intent(in) :: iy
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: numedge_cube, inod0
      integer(kind = kint) :: ie20(20)
!
!
      numedge_cube = 2*(num_h-2)*(num_h-1)
!
!  edge (x = cube_size)
!
      inod0 = (num_h-1)**2
!
      ie20(1) = (num_h-1) * iy
      ie20(2) = inod0 + num_h + iy + 1
!
      ie20(3) = ie20(2) + 1
      ie20(4) = ie20(1) + (num_h-1)
!
      ie20( 5) = numnod + numedge_cube + num_h-1 + iy
      ie20( 6) = numnod + numedge_cube + 4*(num_h-1) + (num_h+1) + iy
      ie20( 8) = numnod + (num_h-2)*(num_h-1) + (num_h-1) * iy
!
      ie20( 7) = ie20(5) + 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:4)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:8)
!
      end subroutine square_side_quad_connect_2
!
!   --------------------------------------------------------------------
!
      subroutine square_side_quad_connect_3(iele, ifile, ifile_20,      &
     &          numnod, num_h, ix)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h
      integer(kind = kint), intent(in) :: ix
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: numedge_cube, inod0
      integer(kind = kint) :: ie20(20)
!
!
      numedge_cube = 2*(num_h-2)*(num_h-1)
!
!  edge (x = -cube_size)
!
      inod0 = (num_h-1)**2
!
      ie20(4) = inod0      + 3*num_h + 1 - ix
      ie20(1) = (num_h-1)*(num_h-2) + ix
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(4) - 1
!
      ie20( 5) = numnod + (num_h-2)*(num_h-2) + ix
      ie20( 6) = numnod + numedge_cube + 3*(num_h-1) - ix
      ie20( 7) = numnod + numedge_cube + 3*(num_h-1) + 4*num_h - 1 - ix
!
      ie20( 8) = ie20( 6) + 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:4)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:8)
!
      end subroutine square_side_quad_connect_3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine square_surf_quad_connect(iele, ifile, ifile_20,        &
     &          numnod, num_h, ix, iy)
!
      integer(kind = kint), intent(in) :: ifile, ifile_20
      integer(kind = kint), intent(in) :: numnod, num_h
      integer(kind = kint), intent(in) :: ix, iy
!
      integer(kind = kint), intent(inout) :: iele
      integer(kind = kint) :: numedge_cube, inod0
      integer(kind = kint) :: ie20(20)
!
!
      numedge_cube = 2*(num_h-2)*(num_h-1)
!
!  edge (x = y = -cube_size)
!
      inod0 = (num_h-1)**2
!
      ie20(1) = ix + (num_h-1) * (iy-1)
!
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + (num_h-1) + 1
      ie20(4) = ie20(1) + (num_h-1)
!
      ie20( 5) = numnod + ix + (num_h-2) * (iy-1)
      ie20( 8) = numnod + (num_h-2)*(num_h-1) + ix + (num_h-1) * (iy-1)
!
      ie20( 7) = ie20( 5) + (num_h-2)
      ie20( 6) = ie20( 8) + 1
!
      iele = iele + 1
      write(ifile,    '(21i10)') iele, ie20(1:4)
      if (ifile_20 .gt. 0) write(ifile_20, '(21i10)') iele, ie20(1:8)
!
      end subroutine square_surf_quad_connect
!
!   --------------------------------------------------------------------
!
      end module square_circ_center_connect
