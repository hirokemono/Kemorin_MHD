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
      integer(kind=kint )   ::  ndep_1 = 3
      integer(kind=kint )   ::  ndep_2 = 9
      integer(kind=kint )   ::  ndep_3 = 27
!  * variables

      integer(kind=kint )  ::  nxi, nyi, nzi
! \beginLOCALVARS[nxi]     internal node count at x direction line
!       \LOCALVAR[nyi]     internal node count at y direction line
!       \LOCALVAR[nzi]     internal node count at z direction line

      integer(kind=kint )  ::  nx , ny , nz 
! \beginLOCALVARS[nx]      nodal count for x direction
!       \LOCALVAR[ny]      nodal count for y direction
!       \LOCALVAR[nz]      nodal count for z direction

      integer(kind=kint )  ::  nx1 , ny1, nz1 
! \beginLOCALVARS[nx1]      nodal count with 1 fill-in for x direction
!       \LOCALVAR[ny1]      nodal count with 1 fill-in for y direction
!       \LOCALVAR[nz1]      nodal count with 1 fill-in for z direction

      integer(kind=kint )  ::  numnod_x , numnod_y , numnod_z 
      integer(kind=kint )  ::  nnod_cubmesh
! \beginLOCALVARS[nx]      array size for x direction
!       \LOCALVAR[ny]      array size for y direction
!       \LOCALVAR[nz]      array size for z direction

      integer(kind=kint )  ::  nod_gltot
!   number of node in the domain without sleeves
      integer(kind=kint )  ::  edge_gltot
!   number of edge in the domain without sleeves
!
      integer(kind=kint )  ::  nodtot, intnodtot
      integer(kind=kint )  ::  elmtot
      integer(kind=kint )  ::  edgetot
!
      integer(kind=kint )  ::  elm_fil1_tot
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
      ndep_1 = c_size%ndep_1
      ndep_2 = c_size%ndep_2
      ndep_3 = c_size%ndep_3
!
      nod_gltot = c_size%nod_gltot
      edge_gltot = c_size%edge_gltot
!
      nxi = c_size%nxi
      nyi = c_size%nyi
      nzi = c_size%nzi

      numnod_x  = c_size%numnod_x
      numnod_y  = c_size%numnod_y
      numnod_z  = c_size%numnod_z
!
      nnod_cubmesh = c_size%nnod_cubmesh
!
!      elm_fil1_tot = c_size%elm_fil1_tot
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

      nx1 = c_each%nx1
      ny1 = c_each%ny1
      nz1 = c_each%nz1

      nodtot = c_each%nodtot
      intnodtot = c_each%intnodtot
      elmtot = c_each%elmtot
      edgetot = c_each%edgetot
!
      end subroutine copy_each_cube_resolution
!
! ----------------------------------------------------------------------
!
      end module m_size_of_cube
