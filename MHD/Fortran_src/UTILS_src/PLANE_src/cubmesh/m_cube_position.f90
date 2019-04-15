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
!
      real   (kind=kreal), dimension(:), allocatable :: zz
      real   (kind=kreal), dimension(:), allocatable :: zz_edge
!
      real   (kind=kreal) :: pi
!
      integer(kind=kint ) :: iradi
!
      end module m_cube_position
