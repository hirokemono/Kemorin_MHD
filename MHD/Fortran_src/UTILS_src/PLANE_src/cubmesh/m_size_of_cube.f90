!
!     module m_size_of_cube
!
!     modified by Kemorin
!
      module m_size_of_cube
!
      use m_precision
      use t_size_of_cube
!
      implicit none
!
      type(size_of_cube) :: c_size1
      type(size_of_each_cube) :: c_each1
!
! ......................................................................
!  * parameters for depth of sleeve area
!      ndepth     : depth of sleeve area
      integer(kind=kint )   ::  ndepth = 1
!
!
      integer(kind = kint) :: iflag_filter = -1
!
      end module m_size_of_cube
