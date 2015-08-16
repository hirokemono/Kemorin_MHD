!
!   module   m_geometry_parameter
!>   number of node, element, surface, and edge data
!>   and SMP stack
!
!   Written by H. Matsui & H. Okuda
!
!
      module m_geometry_parameter
!
      use m_precision
!
      implicit  none
!
!>     number of node on local PE (include external node)
!      integer( kind=kint )  ::  numnod
!>     number of node on local PE
!      integer( kind=kint )  ::  internal_node
!>     number of element on local PE
!      integer( kind=kint )  ::  numele
!>     number of internal element on local PE
!      integer( kind=kint )  ::  internal_ele
!>     number of surface on local PE
!      integer( kind=kint )  ::  numsurf
!>     number of internal surface on local PE
!      integer( kind=kint )  ::  internal_surf
!>     number of edge on local PE
!      integer( kind=kint )  ::  numedge
!>     number of internal edge on local PE
!      integer( kind=kint )  ::  internal_edge
!
!
!   number of nodes in each edge
!
!>   number of nodes in each element
!      integer(kind=kint) :: nnod_4_ele
!>   number of nodes in each surface
!      integer(kind=kint) :: nnod_4_surf
!>   number of nodes in each edge
!      integer(kind=kint) :: nnod_4_edge
!
!>   local index for surface on each element
!      integer (kind=kint), allocatable, target :: node_on_sf(:,:)
!>   local index for opposite surface on each element
!      integer (kind=kint), allocatable, target :: node_on_sf_n(:,:)
!
!>   local index for edge on each element
!      integer (kind=kint), allocatable, target :: node_on_edge(:,:)
!>   local index for edge on each surface
!      integer (kind=kint), allocatable, target :: node_on_edge_sf(:,:)
!
!
!>     smp stack for total node on  local PE
!      integer( kind=kint ), pointer :: inod_smp_stack(:)
!>     smp stack for internal node on  local PE
!      integer( kind=kint ), allocatable, target :: inter_smp_stack(:)
!>     maximum number of smp node on local PE
      integer( kind=kint )  ::  maxnod_4_smp = 0
!>     maximum number of smp internal node on local PE
      integer( kind=kint )  ::  max_in_nod_4_smp = 0
!
!
!>     smp stack for element on  local PE
!      integer( kind=kint ), allocatable, target :: iele_smp_stack(:)
!>     maximum number of smp element on local PE
      integer( kind=kint )  ::  maxele_4_smp = 0
!
!>     smp stack for surface on  local PE
!a      integer( kind=kint ), allocatable, target :: isurf_smp_stack(:)
!>     maximum number of smp surface on local PE
      integer( kind=kint )  ::  maxsurf_4_smp = 0
!
!>     smp stack for edge on  local PE
!      integer( kind=kint ), allocatable, target :: iedge_smp_stack(:)
!>     maximum number of smp edge on local PE
      integer(kind = kint)  ::  maxedge_4_smp = 0
!
      end module   m_geometry_parameter
