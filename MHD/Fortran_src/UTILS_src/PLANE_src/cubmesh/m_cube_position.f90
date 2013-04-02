!
!      module m_cube_position
!
      module m_cube_position
!
!      written by Kemorin
!
      use m_precision
!
      implicit none
!
      real   (kind=kreal)  ::  xsize  , ysize  , zsize
      real   (kind=kreal)  ::  xoff   , yoff   , zoff
!
      real   (kind=kreal), dimension(:), allocatable :: zz
      real   (kind=kreal), dimension(:), allocatable :: zz_edge
!
      real   (kind=kreal) :: pi
!
      integer(kind=kint ) :: iradi
!  flag for the vertical pattern
!
!      subroutine set_offset_of_domain(ipe, jpe, kpe)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_offset_of_domain(ipe, jpe, kpe)
!
! *****  initialization to construct node information
!
      use m_size_of_cube
      use m_size_4_plane
!
      integer(kind=kint)  ::  ipe, jpe, kpe
!
! ***** set coordinate off set (starting corner for pe node)
!
                        xoff = xmin + (ipe-1)*xsize*nxi /(nx_all)
!
                        yoff = ymin + (jpe-1)*ysize*nyi /(ny_all)
!
!                        zoff = zmin + (kpe-1)*zsize*nzi /(nz_all)
!            if (kpe/=1) zoff =  zoff - zsize/(nz_all-1)*ndepth
!
!
       end subroutine set_offset_of_domain
!
! ----------------------------------------------------------------------
!
      end module m_cube_position
