!cubed_circle_center_connect.f90
!      module cubed_circle_center_connect
!
!     Written by H. Matsui on Apr., 2003
!     Modified by H. Matsui on Oct., 2007
!
!      subroutine set_center_square_connect(iele, ifile)
!
      module cubed_circle_center_connect
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
      subroutine set_center_square_connect(iele, ifile)
!
      use m_cubed_sph_mesh
      use m_numref_cubed_sph
!
      integer(kind = kint), intent(in) :: ifile
!
      integer(kind = kint), intent(inout) :: iele
!
      integer(kind = kint) :: ix, iy
!
!
!  corner (x = y =-cube_size)
!
      ie20(1) = 1
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 1 + (num_hemi+1)
      ie20(4) = ie20(1)     + (num_hemi+1)
!
      iele = iele + 1
      write(ifile,    '(21i16)') iele, ie20(1:4)
!
!  edge (y = -cube_size)
!
      do ix = 1, num_hemi-2
        ie20(1) = ix + 1
        ie20(2) = ie20(1) + 1
        ie20(3) = ie20(1) + 1 + (num_hemi+1)
        ie20(4) = ie20(1)     + (num_hemi+1)
!
        iele = iele + 1
        write(ifile,    '(21i16)') iele, ie20(1:4)
      end do
!
!  corner (x = cube_size, y = -cube_size)
!
      ie20(1) = num_hemi
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 1 + (num_hemi+1)
      ie20(4) = ie20(1)     + (num_hemi+1)
!
      iele = iele + 1
      write(ifile,    '(21i16)') iele, ie20(1:4)
!
!
!  edge (x=-cube_size)
!
      do iy = 1, num_hemi-2
        ie20(1) =  (num_hemi+1) * iy + 1
        ie20(2) = ie20(1) + 1
        ie20(3) = ie20(1) + 1 + (num_hemi+1)
        ie20(4) = ie20(1)     + (num_hemi+1)
!
        iele = iele + 1
        write(ifile,    '(21i16)') iele, ie20(1:4)
!
!  bottom surface
!
        do ix = 1, num_hemi-2
          ie20(1) = (num_hemi+1)*iy + ix + 1
          ie20(2) = ie20(1) + 1
          ie20(3) = ie20(1) + 1 + (num_hemi+1)
          ie20(4) = ie20(1)     + (num_hemi+1)
!
          iele = iele + 1
          write(ifile,    '(21i16)') iele, ie20(1:4)
        end do
!
!  edge (x=cube_size)
!
        ie20(1) = (num_hemi+1) * (iy+1) - 1
        ie20(2) = ie20(1) + 1
        ie20(3) = ie20(1) + 1 + (num_hemi+1)
        ie20(4) = ie20(1)     + (num_hemi+1)
!
        iele = iele + 1
        write(ifile,    '(21i16)') iele, ie20(1:4)
      end do
!
!  corner (x=-cube_size, y=cube_size)
!
      ie20(1) = (num_hemi+1) * (num_hemi-1) + 1
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 1 + (num_hemi+1)
      ie20(4) = ie20(1)     + (num_hemi+1)
!
      iele = iele + 1
      write(ifile,    '(21i16)') iele, ie20(1:4)
!
!  edge (y=cube_size)
!
      do ix = 1, num_hemi-2
        ie20(1) = num_hemi**2 + ix
        ie20(2) = ie20(1) + 1
        ie20(3) = ie20(1) + 1 + (num_hemi+1)
        ie20(4) = ie20(1)     + (num_hemi+1)
!
        iele = iele + 1
        write(ifile,    '(21i16)') iele, ie20(1:4)
      end do
!
!  corner (x=y=cube_size)
!
      ie20(1) = (num_hemi+1)*num_hemi - 1
      ie20(2) = ie20(1) + 1
      ie20(3) = ie20(1) + 1 + (num_hemi+1)
      ie20(4) = ie20(1)     + (num_hemi+1)
!
      iele = iele + 1
      write(ifile,    '(21i16)') iele, ie20(1:4)
!
      end subroutine set_center_square_connect
!
!   --------------------------------------------------------------------
!
      end module cubed_circle_center_connect
