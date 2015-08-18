!>@file   m_geometry_data.f90
!!@brief  module m_geometry_data
!!
!!@author H. Okuda and H. Matsui
!!@date Programmed in 2001
!!@date Modified in 2008
!
!> @brief geometry data for FEM mesh
!!   including node and element position, all connectivities
!!
!!@verbatim
!!      subroutine allocate_numnod_stack(nprocs)
!!      subroutine allocate_numele_stack(nprocs)
!!      subroutine allocate_numsurf_stack(nprocs)
!!      subroutine allocate_numedge_stack(nprocs)
!!
!!      subroutine deallocate_numnod_stack
!!      subroutine deallocate_numele_stack
!!      subroutine deallocate_numsurf_stack
!!      subroutine deallocate_numedge_stack
!!@endverbatim
!!
!>
!>@n@image html 1ele_node.png Node connectivity \p ie(iele,j)
!>                            for each element
!>@n@image html 1ele_edge.png Edge connectivity \p isurf_4_ele(iele,j)
!>                            for each element
!>@n@image html 1ele_surf.png Surface connectivity \p iedge_4_ele(iele,j)
!>                            for each element
!>@n@image html 1quad_ele_img.png Node connectivity \p ie(iele,j)
!>                            for quadrature element
!
!
      module   m_geometry_data
!
      use m_precision
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit  none
!
!>  structure for node data (position)
      type(node_data), save :: node1
!    node1%a_s
!
!>  structure for element data (position and connectivity)
      type(element_data), save :: ele1
!    ele1%a_vol
!
!>      structure of surface data (geometry and connectivity)
      type(surface_data), save :: surf1
!  surf1%isf_isolate
!
!>     Structure for edge data
      type(edge_data), save :: edge1
!  edge1%isurf_4_edge
!
!>   Stack list of number of node
      integer(kind=kint_gl), allocatable, target  :: istack_numnod(:)
!>   Stack list of number of internal node
      integer(kind=kint_gl), allocatable, target  :: istack_internod(:)
!
!>   Stack list of number of element
      integer(kind=kint_gl), allocatable, target  :: istack_numele(:)
!>   Stack list of number of internal element
      integer(kind=kint_gl), allocatable, target  :: istack_interele(:)
!
!>   Stack list of number of surface
      integer(kind=kint_gl), allocatable, target  :: istack_numsurf(:)
!>   Stack list of number of internal surface
      integer(kind=kint_gl), allocatable, target  :: istack_intersurf(:)
!>   Stack list of number of edge
      integer(kind=kint_gl), allocatable, target  :: istack_numedge(:)
!>   Stack list of number of internal edge
      integer(kind=kint_gl), allocatable, target  :: istack_interedge(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_numnod_stack(nprocs)
!
      integer(kind = kint), intent(in) :: nprocs
!
!
      allocate(istack_numnod(0:nprocs))
      allocate(istack_internod(0:nprocs))
      istack_numnod =   0
      istack_internod = 0
!
      end subroutine allocate_numnod_stack
!
! ------------------------------------------------------
!
      subroutine allocate_numele_stack(nprocs)
!
      integer(kind = kint), intent(in) :: nprocs
!
!
      allocate(istack_numele(0:nprocs))
      allocate(istack_interele(0:nprocs))
      istack_numele =   0
      istack_interele = 0
!
      end subroutine allocate_numele_stack
!
! ------------------------------------------------------
!
      subroutine allocate_numsurf_stack(nprocs)
!
      integer(kind = kint), intent(in) :: nprocs
!
!
      allocate(istack_numsurf(0:nprocs))
      allocate(istack_intersurf(0:nprocs))
      istack_numsurf =   0
      istack_intersurf = 0
!
      end subroutine allocate_numsurf_stack
!
! ------------------------------------------------------
!
      subroutine allocate_numedge_stack(nprocs)
!
      integer(kind = kint), intent(in) :: nprocs
!
!
      allocate(istack_numedge(0:nprocs))
      allocate(istack_interedge(0:nprocs))
      istack_numedge =   0
      istack_interedge = 0
!
      end subroutine allocate_numedge_stack
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_numnod_stack
!
!
      deallocate(istack_numnod, istack_internod)
!
      end subroutine deallocate_numnod_stack
!
! ------------------------------------------------------
!
      subroutine deallocate_numele_stack
!
!
      deallocate(istack_numele, istack_interele)
!
      end subroutine deallocate_numele_stack
!
! ------------------------------------------------------
!
      subroutine deallocate_numsurf_stack
!
!
      deallocate(istack_numsurf, istack_intersurf)
!
      end subroutine deallocate_numsurf_stack
!
! ------------------------------------------------------
!
      subroutine deallocate_numedge_stack
!
!
      deallocate(istack_numedge, istack_interedge)
!
      end subroutine deallocate_numedge_stack
!
! ------------------------------------------------------
!
      end module m_geometry_data
