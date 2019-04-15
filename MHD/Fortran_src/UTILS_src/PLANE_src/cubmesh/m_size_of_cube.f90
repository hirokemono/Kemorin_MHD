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
!  * variables

      integer(kind=kint )  ::  nxi, nyi, nzi
! \beginLOCALVARS[nxi]     internal node count at x direction line
!       \LOCALVAR[nyi]     internal node count at y direction line
!       \LOCALVAR[nzi]     internal node count at z direction line

      integer(kind=kint )  ::  nx , ny , nz 
! \beginLOCALVARS[nx]      nodal count for x direction
!       \LOCALVAR[ny]      nodal count for y direction
!       \LOCALVAR[nz]      nodal count for z direction
!
!
      integer(kind = kint) :: iflag_filter = -1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine set_size_of_domain(elm_type, kpe)
!
       use m_size_4_plane
!
       integer (kind = kint) :: elm_type, kpe
!
      call set_each_cube_resolution(elm_type, kpe, c_size1, c_each1)
      call copy_each_cube_resolution(c_each1)
!
       end subroutine set_size_of_domain
!
! ----------------------------------------------------------------------
!
      subroutine copy_plane_resolution(c_size)
!
      type(size_of_cube), intent(in) :: c_size
!
!      ndepth = c_size%ndepth
!
      nxi = c_size%nxi
      nyi = c_size%nyi
      nzi = c_size%nzi
!
!      iflag_filter = c_size%iflag_filter
!
      end subroutine copy_plane_resolution
!
! ----------------------------------------------------------------------
!
      subroutine copy_each_cube_resolution(c_each)
!
      type(size_of_each_cube), intent(in) :: c_each
!
      nx  = c_each%nx
      ny  = c_each%ny
      nz  = c_each%nz
!
      end subroutine copy_each_cube_resolution
!
! ----------------------------------------------------------------------
!
      end module m_size_of_cube
