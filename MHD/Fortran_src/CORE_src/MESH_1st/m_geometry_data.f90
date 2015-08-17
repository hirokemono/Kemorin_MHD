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
!!
!!      subroutine allocate_element_geometry
!!
!!      subroutine allocate_surface_connect
!!
!!      subroutine allocate_edge_connect
!!      subroutine allocate_edge_4_ele
!!
!!      subroutine allocate_ele_4_surf
!!      subroutine allocate_ele_4_edge_num
!!      subroutine allocate_ele_4_edge_item
!!      subroutine allocate_surf_4_edge_num
!!      subroutine allocate_surf_4_edge_item
!!
!!      subroutine deallocate_surface_connect
!!
!!      subroutine deallocate_edge_connect
!!      subroutine deallocate_edge_4_ele
!!      subroutine deallocate_ele_4_surf
!!      subroutine deallocate_ele_4_edge_item
!!      subroutine deallocate_surf_4_edge_item
!!
!!      subroutine allocate_ext_surface
!!      subroutine allocate_iso_surface
!!      subroutine allocate_iso_edge
!!
!!      subroutine deallocate_ext_surface
!!      subroutine deallocate_iso_surface
!!      subroutine deallocate_iso_edge
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
!    ele1%ie
!
!>      structure of surface data (geometry and connectivity)
      type(surface_data), save :: surf1
!  surf1%isurf_global
!
!>     Structure for edge data
      type(edge_data), save :: edge1
!  edge1%istack_edge_smp
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
!>   position of nodes (i:direction, j:node ID)
!      real(kind=kreal)  , pointer  :: xx(:,:)
!>   element connectivity ie(i:element ID,j:element index)
!      integer(kind=kint), pointer  :: ie(:,:)
!>   element type defined by the first element
!      integer(kind=kint) ::  first_ele_type
!
!
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
!
!>   surface connectivity ie_surf(i:surface ID,j:surface index)
!      integer(kind=kint), pointer  :: ie_surf(:,:)
!>   edge connectivity ie_edge(i:edge ID,j:surface index)
!      integer(kind=kint), pointer  :: ie_edge(:,:)
!
!
!>   surface ID for element surface isf_4_ele(:,:)
!>          ...i:element ID, j:surface ID
!>@n          Positive: normal direction negative: reverse direction
!      integer(kind=kint), allocatable, target  :: isf_4_ele(:,:)
!>   rotation ID for element surface isf_rot_ele(:,:)
!>          ...i:element ID, j:surface ID
!>@n          0: normal direction  1-4: rotation flag for reverse surface
!      integer(kind=kint), allocatable, target  :: isf_rot_ele(:,:)
!>   edge ID for each surface
      integer(kind=kint), allocatable, target  :: iedge_4_sf(:,:)
!>   edge ID for each element
      integer(kind=kint), allocatable, target  :: iedge_4_ele(:,:)
!>   number of isolated edges
      integer(kind=kint) ::  numedge_iso
!>     isolated edge list
      integer(kind=kint), allocatable  ::  iedge_isolate(:)
!
!
!>   belonged element for surface(surface#,face#,
!>                                1:element or 2:local surface)
      integer(kind=kint), allocatable, target :: iele_4_surf(:,:,:)
!
!>   total number of element list for edge
      integer(kind=kint) :: ntot_iele_4_edge
!>   number of element list for each edge
      integer(kind=kint), allocatable, target :: num_iele_4_edge(:)
!>   end address of element list for each edge
      integer(kind=kint), allocatable, target :: istack_iele_4_edge(:)
!>   element id list for each edge (negative: opposite direction)
      integer(kind=kint), allocatable, target :: iele_4_edge(:,:)
!
!>   total number of surface list for edge
      integer(kind=kint) :: ntot_isurf_4_edge
!>   number of surface list for each edge
      integer(kind=kint), allocatable, target :: num_isurf_4_edge(:)
!>   end address of surface list for each edge
      integer(kind=kint), allocatable, target :: istack_isurf_4_edge(:)
!>   surafce id list for each edge (negative: opposite direction)
      integer(kind=kint), allocatable, target :: isurf_4_edge(:,:)
!
!>     number of external surface
      integer(kind=kint) ::  numsurf_ext
!>     number of isolated surface
      integer(kind=kint) ::  numsurf_iso
!>     external surface list
      integer(kind=kint), allocatable  ::  isf_external(:)
!>     isolated surface list
      integer(kind=kint), allocatable  ::  isf_isolate(:)
!

!>     element type id   (where i:element id)
!      integer(kind=kint), allocatable, target  ::  elmtyp(:)
!>     element type id   (where i:element id)
!      integer(kind=kint), allocatable, target  ::  nodelm(:)
!>     global node    id (where i:node id)
!      integer(kind=kint_gl), pointer  ::  inod_global(:)
!>     global element id (where i:element id)
!      integer(kind=kint_gl), allocatable, target  ::  iele_global(:)
!>     global surface id (where i:surface id)
      integer(kind=kint_gl), allocatable, target  ::  isurf_global(:)
!>     global edge id (where i:edge id)
      integer(kind=kint_gl), allocatable, target  ::  iedge_global(:)
!
!>   distance from the center
!      real(kind=kreal)  , pointer  :: radius(:)
!>   1/radius
!      real(kind=kreal)  , pointer  :: a_radius(:)
!>   longitude of node
!      real(kind=kreal)  , pointer  :: longitude(:)
!>   colatitude of node
!      real(kind=kreal)  , pointer  :: colatitude(:)
!>   cylindorical radius of node
!      real(kind=kreal)  , pointer  :: s_cylinder(:)
!>   1 / (cylindorical radius)
!      real(kind=kreal)  , pointer  :: a_s_cylinder(:)
!
!>  integer flag for interior element 1...interior, 0...exterior
      integer(kind = kint), allocatable, target :: interior_ele(:)
!>  double flag for interior element  1.0...interior, 0.0...exterior
      real(kind=kreal)  , allocatable, target  :: e_multi(:)
!
!>  integer flag for interior surface 1...interior, 0...exterior
      integer(kind = kint), allocatable, target :: interior_surf(:)
!>  integer flag for interior edge 1...interior, 0...exterior
      integer(kind = kint), allocatable, target :: interior_edge(:)
!
!
!>   position of centre of element
      real(kind=kreal)  , allocatable, target :: x_ele(:,:)
!>   distance from the centre of element
      real(kind=kreal)  , allocatable, target :: r_ele(:)
!>   1/r_ele
      real(kind=kreal)  , allocatable, target :: ar_ele(:)
!>   longitude of element
      real(kind=kreal)  , allocatable, target :: phi_ele(:)
!>  colatitude of element
      real(kind=kreal)  , allocatable, target :: theta_ele(:)
!>  cylindorical radius of element
      real(kind=kreal)  , allocatable, target :: s_ele(:)
!>  1 / s_ele
      real(kind=kreal)  , allocatable, target :: as_ele(:)
!
!>  Volume of each element
      real (kind=kreal), allocatable, target :: volume_ele(:)
!>  1 / volume of each element
      real (kind=kreal), allocatable, target :: a_vol_ele(:)
!
!
!>  Total volume of domain
      real(kind=kreal) :: volume
!>  1 / volume of domain
      real(kind=kreal) :: a_vol
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
! ------------------------------------------------------
!
       subroutine allocate_element_geometry
!
!
        allocate(x_ele(ele1%numele,3))
        allocate(r_ele(ele1%numele))
        allocate(ar_ele(ele1%numele))
        allocate(phi_ele(ele1%numele))
        allocate(theta_ele(ele1%numele))
        allocate(s_ele(ele1%numele))
        allocate(as_ele(ele1%numele))
!
        allocate ( interior_ele(ele1%numele) )
        allocate ( e_multi(ele1%numele) )
!
        allocate( volume_ele (ele1%numele))
        allocate( a_vol_ele (ele1%numele))
!
       interior_ele = 1
       e_multi = 1.0d0
!
       x_ele = 0.0d0
!
       r_ele = 0.0d0
       ar_ele = 0.0d0
       phi_ele = 0.0d0
       theta_ele = 0.0d0
       s_ele = 0.0d0
       as_ele = 0.0d0
!
       volume_ele = 0.0d0
       a_vol_ele = 0.0d0
!
       end subroutine allocate_element_geometry
!
! ------------------------------------------------------
!
       subroutine deallocate_element_geometry
!
        deallocate(x_ele)
        deallocate(r_ele)
        deallocate(ar_ele)
        deallocate(phi_ele)
        deallocate(theta_ele)
        deallocate(s_ele)
        deallocate(as_ele)
!
        deallocate ( interior_ele )
        deallocate ( e_multi )
!
        deallocate( volume_ele )
        deallocate( a_vol_ele )
!
       end subroutine deallocate_element_geometry
!
! ------------------------------------------------------
!
      subroutine allocate_surface_connect
!
      use m_geometry_constants
!
      call allocate_surface_connect_type(surf1, ele1%numele)
!
!      allocate( isf_4_ele(ele1%numele,nsurf_4_ele) )
!      allocate( isf_rot_ele(ele1%numele,nsurf_4_ele) )
!      allocate( ie_surf(surf1%numsurf,surf1%nnod_4_surf) )
      allocate( isurf_global(surf1%numsurf) )
!      allocate( interior_surf(surf1%numsurf) )
!
      end subroutine allocate_surface_connect
!
! ------------------------------------------------------
!
      subroutine allocate_edge_connect
!
      use m_geometry_constants
!
      call allocate_edge_connect_type(edge1, surf1%numsurf)
!
      allocate( iedge_4_sf(surf1%numsurf,nedge_4_surf) )
!      allocate( ie_edge(edge1%numedge,edge1%nnod_4_edge) )
      allocate( iedge_global(edge1%numedge) )
      allocate( interior_edge(edge1%numedge) )
!
      iedge_4_sf = 0
      iedge_global = 0
      interior_edge = 0
!
      end subroutine allocate_edge_connect
!
! ------------------------------------------------------
!
      subroutine allocate_edge_4_ele
!
      use m_geometry_constants
!
      allocate( iedge_4_ele(ele1%numele,nedge_4_ele) )
      iedge_4_ele = 0
!
      end subroutine allocate_edge_4_ele
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine allocate_ele_4_surf
!
!
      allocate( iele_4_surf(surf1%numsurf,2,2) )
      iele_4_surf = 0
!
      end subroutine allocate_ele_4_surf
!
! ------------------------------------------------------
!
      subroutine allocate_ele_4_edge_num
!
!
      allocate( num_iele_4_edge(edge1%numedge) )
      allocate( istack_iele_4_edge(0:edge1%numedge) )
      num_iele_4_edge =    0
      istack_iele_4_edge = 0
!
      end subroutine allocate_ele_4_edge_num
!
! ------------------------------------------------------
!
      subroutine allocate_ele_4_edge_item
!
!
      ntot_iele_4_edge = istack_iele_4_edge(edge1%numedge)
      allocate( iele_4_edge(ntot_iele_4_edge,2) )
      iele_4_edge = 0
!
      end subroutine allocate_ele_4_edge_item
!
! ------------------------------------------------------
!
      subroutine allocate_surf_4_edge_num
!
!
      allocate( num_isurf_4_edge(edge1%numedge) )
      allocate( istack_isurf_4_edge(0:edge1%numedge) )
      num_isurf_4_edge =    0
      istack_isurf_4_edge = 0
!
      end subroutine allocate_surf_4_edge_num
!
! ------------------------------------------------------
!
      subroutine allocate_surf_4_edge_item
!
!
      ntot_isurf_4_edge = istack_isurf_4_edge(edge1%numedge)
      allocate( isurf_4_edge(ntot_isurf_4_edge,2) )
      isurf_4_edge = 0
!
      end subroutine allocate_surf_4_edge_item
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_surface_connect
!
      call deallocate_surface_connect_type(surf1)
!
      deallocate( isurf_global )
!
      end subroutine deallocate_surface_connect
!
! ------------------------------------------------------
!
      subroutine deallocate_edge_connect
!
      deallocate( iedge_4_sf )
      deallocate( iedge_global )
      deallocate( interior_edge )
!
      end subroutine deallocate_edge_connect
!
! ------------------------------------------------------
!
      subroutine deallocate_edge_4_ele
!
      deallocate( iedge_4_ele )
!
      end subroutine deallocate_edge_4_ele
!
! ------------------------------------------------------
!
      subroutine deallocate_ele_4_surf
!
!
      deallocate( iele_4_surf )
!
      end subroutine deallocate_ele_4_surf
!
! ------------------------------------------------------
!
      subroutine deallocate_ele_4_edge_item
!
!
      deallocate(num_iele_4_edge, istack_iele_4_edge, iele_4_edge)
!
      end subroutine deallocate_ele_4_edge_item
!
! ------------------------------------------------------
!
      subroutine deallocate_surf_4_edge_item
!
!
      deallocate( isurf_4_edge, num_isurf_4_edge, istack_isurf_4_edge)
!
      end subroutine deallocate_surf_4_edge_item
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine allocate_ext_surface
!
      allocate( isf_external(numsurf_ext) )
      isf_external = 0
!
      end subroutine allocate_ext_surface
!
! ------------------------------------------------------
!
      subroutine allocate_iso_surface
!
      allocate( isf_isolate(numsurf_iso) )
      isf_isolate = 0
!
      end subroutine allocate_iso_surface
!
! ------------------------------------------------------
!
      subroutine allocate_iso_edge
!
      allocate( iedge_isolate(numedge_iso) )
      iedge_isolate = 0
!
      end subroutine allocate_iso_edge
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_ext_surface
!
      deallocate( isf_external )
!
      end subroutine deallocate_ext_surface
!
! ------------------------------------------------------
!
      subroutine deallocate_iso_surface
!
      deallocate( isf_isolate )
!
      end subroutine deallocate_iso_surface
!
! ------------------------------------------------------
!
      subroutine deallocate_iso_edge
!
      deallocate( iedge_isolate )
!
      end subroutine deallocate_iso_edge
!
! ------------------------------------------------------
!
      end module m_geometry_data
