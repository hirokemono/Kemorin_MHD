!
!     module t_size_of_cube
!
!     modified by Kemorin
!
!!      subroutine set_plane_size(c_size)
!!      subroutine set_plane_resolution(c_size)
!!      subroutine set_each_cube_resolution(elm_type, kpe, c_size)
!!        type(size_of_each_cube), intent(inout) :: c_size
!
      module t_size_of_cube
!
      use m_precision
      use m_constants
!
      implicit none
!
      type size_of_cube
!>  * parameters for depth of sleeve area
!!      ndepth     : depth of sleeve area
        integer(kind=kint )   ::  ndepth = 1
        integer(kind=kint )   ::  ndep_1 = 3
!
!>        nodal count at x direction line for all model
        integer(kind=kint ) :: nx_all
!>        nodal count at y direction line for all model
        integer(kind=kint ) :: ny_all
!>        nodal count at z direction line for all model
        integer(kind=kint ) :: nz_all
!
!>        subdomain division count at x direction line
        integer(kind=kint ) :: ndx
!>        subdomain division count at y direction line
        integer(kind=kint ) :: ndy
!>        subdomain division count at z direction line
        integer(kind=kint ) :: ndz
!
!>        number of node in the domain without sleeves
        integer(kind=kint )  ::  nod_gltot
!>        number of edge in the domain without sleeves
        integer(kind=kint )  ::  edge_gltot
!
!>        nxi:     internal node count at x direction line
        integer(kind=kint )  ::  nxi
!>        nyi:     internal node count at y direction line
        integer(kind=kint )  ::  nyi
!>        nzi:     internal node count at z direction line
        integer(kind=kint )  ::  nzi

!>        numnod_x:      array size for x direction
        integer(kind=kint )  ::  numnod_x
!>        numnod_y:      array size for y direction
        integer(kind=kint )  ::  numnod_y 
!>        numnod_z:      array size for z direction
        integer(kind=kint )  ::  numnod_z 
!
!>        nnod_cubmesh:      array size direction
        integer(kind=kint )  ::  nnod_cubmesh
!
        real(kind = kreal)  ::  xsize
        real(kind = kreal)  ::  ysize
        real(kind = kreal)  ::  zsize
!
        real(kind=kreal) :: xmin
        real(kind=kreal) :: xmax
!
        real(kind=kreal) :: ymin
        real(kind=kreal) :: ymax
!
        real(kind=kreal) :: zmin
        real(kind=kreal) :: zmax
      end type size_of_cube
!
      type size_of_each_cube
!>        nx:      nodal count for x direction
        integer(kind=kint )  ::  nx
!>        ny:      nodal count for y direction
        integer(kind=kint )  ::  ny 
!>        nz:      nodal count for z direction
        integer(kind=kint )  ::  nz 

!>        Total number node
        integer(kind=kint )  ::  nodtot
!>        Total number internal node
        integer(kind=kint )  ::  intnodtot
!>        Total number element
        integer(kind=kint )  ::  elmtot
!>        Total number internal element
        integer(kind=kint )  ::  intelmtot
!>        Total number edge
        integer(kind=kint )  ::  edgetot
!>        Total number internal edge
        integer(kind=kint )  ::  intedgetot
!
!>        Total number elmement for filtering
        integer(kind=kint )  ::  elm_fil1_tot
      end type size_of_each_cube
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_plane_size(c_size)
!
      type(size_of_cube), intent(inout) :: c_size
!
      c_size%xmax = c_size%xsize / two
      c_size%ymax = c_size%ysize / two
      c_size%zmax = c_size%zsize / two
      c_size%xmin = -c_size%xmax
      c_size%ymin = -c_size%ymax
      c_size%zmin = -c_size%zmax
!
      end subroutine set_plane_size
!
! ----------------------------------------------------------------------
!
      subroutine set_plane_resolution(c_size)
!
      type(size_of_cube), intent(inout) :: c_size
!
!
      c_size%ndep_1 = (2*c_size%ndepth+1)
!
      c_size%nod_gltot  = c_size%nx_all * c_size%ny_all * c_size%nz_all
      c_size%edge_gltot = c_size%nx_all * c_size%ny_all                 &
     &                   * (3*c_size%nz_all - 1)
!
! ***** set internal node count
!
      c_size%nxi = c_size%nx_all / c_size%ndx
      c_size%nyi = c_size%ny_all / c_size%ndy
      c_size%nzi = c_size%nz_all / c_size%ndz
!
      c_size%numnod_x = c_size%nxi + 2*c_size%ndepth
      c_size%numnod_y = c_size%nyi + 2*c_size%ndepth
      c_size%numnod_z = c_size%nzi + 2*c_size%ndepth
!
      c_size%nnod_cubmesh                                               &
     &         = c_size%numnod_x * c_size%numnod_y * c_size%numnod_z
!
      end subroutine set_plane_resolution
!
! ----------------------------------------------------------------------
!
      subroutine set_each_cube_resolution                               &
     &         (elm_type, kpe, c_size, c_each)
!
      integer (kind = kint), intent(in) :: elm_type, kpe
      type(size_of_cube), intent(in) :: c_size
      type(size_of_each_cube), intent(inout) :: c_each
!
!>      nx1:      nodal count with 1 overlap
      integer(kind=kint )  ::  nx1, ny1, nz1

!
!                                       .. pe nod per 1 line

                       c_each%nx  = c_size%nxi + 2*c_size%ndepth

                       c_each%ny  = c_size%nyi + 2*c_size%ndepth

                            c_each%nz  = c_size%nzi
      if(kpe .ne.          1) c_each%nz = c_each%nz + c_size%ndepth
      if(kpe .ne. c_size%ndz) c_each%nz = c_each%nz + c_size%ndepth
!
!                                       .. total node
      c_each%nodtot    = c_each%nx  * c_each%ny  * c_each%nz
!                                       .. internal nodes
      c_each%intnodtot = c_size%nxi * c_size%nyi * c_size%nzi
!
      c_each%elmtot =       (c_each%nx-1)*(c_each%ny-1)*(c_each%nz-1)
!
!
                                 nx1 = c_size%nxi + 2
                                 ny1 = c_size%nyi + 2
                                 nz1 = c_size%nzi
      if (kpe .ne.          1)  nz1 = nz1 + 1
      if (kpe .ne. c_size%ndz)  nz1 = nz1 + 1
      c_each%elm_fil1_tot = (nx1-1)*(ny1-1)*(nz1-1)
!
!
      c_each%edgetot   = (c_each%nx-1) * c_each%ny  * c_each%nz
      c_each%edgetot   = c_each%edgetot                                 &
     &                  + c_each%nx * (c_each%ny-1)  * c_each%nz
      c_each%edgetot   = c_each%edgetot                                 &
     &                  + c_each%nx * c_each%ny  * (c_each%nz-1)
!
      c_each%intedgetot = 3 * c_size%nxi * c_size%nyi * c_size%nzi
      if (kpe == c_size%ndz) then
        c_each%intedgetot = c_each%intedgetot - c_size%nxi * c_size%nyi
      end if
!
      if (elm_type .eq. 332) then
        c_each%nodtot    = c_each%nodtot + c_each%edgetot
        c_each%intnodtot = c_each%intnodtot + c_each%intedgetot
      end if
!
      end subroutine set_each_cube_resolution
!
! ----------------------------------------------------------------------
!
      end module t_size_of_cube
