!
!   module   m_2nd_geometry_param
!>   number of node, element, surface, and edge data
!>   and SMP stack for second mesh
!
!   Written by H. Matsui on Aug., 2006
!
!      subroutine allocate_inod_in_surf_2nd
!      subroutine allocate_inod_in_edge_2nd
!      subroutine allocate_2nd_geomet_param_smp
!      subroutine allocate_2nd_surf_param_smp
!      subroutine allocate_2nd_edge_param_smp
!
!      subroutine deallocate_inod_in_surf_2nd
!      subroutine deallocate_inod_in_edge_2nd
!      subroutine deallocate_2nd_geomet_param_smp
!      subroutine deallocate_2nd_surf_param_smp
!      subroutine deallocate_2nd_edge_param_smp
!
!      subroutine check_smp_size_2nd(my_rank)
!      subroutine check_smp_size_2nd_surf_edge
!
!
      module m_2nd_geometry_param
!
      use m_precision
!
      implicit  none
!
!>     number of node on local PE (include external node)
      integer( kind=kint )  ::  nnod_2nd
!>     number of node on local PE
      integer( kind=kint )  ::  internal_nod_2nd
!>     number of element on local PE
      integer( kind=kint )  ::  nele_2nd
!>     number of internal element on local PE
      integer( kind=kint )  ::  internal_ele_2nd
!>     number of surface on local PE
      integer( kind=kint )  ::  nsurf_2nd
!>     number of internal surface on local PE
      integer( kind=kint )  ::  internal_surf_2nd
!>     number of edge on local PE
      integer( kind=kint )  ::  nedge_2nd
!>     number of internal edge on local PE
      integer( kind=kint )  ::  internal_edge_2nd
!
!>   number of nodes in each element
      integer(kind=kint) :: nnod_4_ele_2nd =   8
!>   number of nodes in each surface
      integer(kind=kint) :: nnod_4_surf_2nd =  4
!>   number of nodes in each edge
      integer(kind=kint) :: nnod_4_edge_2nd =  2
!
!
!>   local index for surface on each element
      integer (kind=kint), pointer :: node_on_sf_2nd(:,:)
!>   local index for opposite surface on each element
      integer (kind=kint), pointer :: node_on_sf_n_2nd(:,:)
!
!>   local index for edge on each element
      integer (kind=kint), pointer :: node_on_edge_2nd(:,:)
!>   local index for edge on each surface
      integer (kind=kint), pointer :: node_on_edge_sf_2nd(:,:)
!
!
!>     smp stack for total node on  local PE
      integer( kind=kint ), pointer :: inod_smp_stack_2nd(:)
!>     smp stack for internal node on  local PE
      integer( kind=kint ), pointer :: inter_smp_stack_2nd(:)
!>     maximum number of smp node on local PE
      integer( kind=kint )  ::  maxnod_4_smp_2nd = 0
!>     maximum number of smp internal node on local PE
      integer( kind=kint )  ::  max_in_nod_4_smp_2nd = 0
!
!>     smp stack for element on  local PE
      integer( kind=kint ), pointer :: iele_smp_stack_2nd(:)
!>     maximum number of smp element on local PE
      integer( kind=kint )  ::  maxele_4_smp_2nd = 0
!
!>     smp stack for surface on  local PE
      integer( kind=kint ), pointer :: isurf_smp_stack_2nd(:)
!     number of surf on this PE
!>     maximum number of smp surface on local PE
      integer( kind=kint )  ::  maxsurf_4_smp_2nd = 0
!
!>     smp stack for edge on  local PE
      integer( kind=kint ), pointer :: iedge_smp_stack_2nd(:)
!>     maximum number of smp edge on local PE
      integer( kind=kint )  ::  maxedge_4_smp_2nd = 0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine allocate_inod_in_surf_2nd
!
       use m_geometry_constants
!
!
       allocate ( node_on_sf_2nd  (nnod_4_surf_2nd,nsurf_4_ele) )
       allocate ( node_on_sf_n_2nd(nnod_4_surf_2nd,nsurf_4_ele) )
!
       node_on_sf_2nd =   0
       node_on_sf_n_2nd = 0
!
       end subroutine allocate_inod_in_surf_2nd
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_inod_in_edge_2nd
!
       use m_geometry_constants
!
!
       allocate (node_on_edge_2nd(nnod_4_edge_2nd,nedge_4_ele) )
       allocate (node_on_edge_sf_2nd(nnod_4_edge_2nd,nedge_4_surf) )
!
       node_on_edge_2nd =    0
       node_on_edge_sf_2nd = 0
!
       end subroutine allocate_inod_in_edge_2nd
!
!-----------------------------------------------------------------------
!
       subroutine allocate_2nd_geomet_param_smp
!
       use m_machine_parameter
!
       allocate( iele_smp_stack_2nd(0:np_smp))
       allocate( inod_smp_stack_2nd(0:np_smp))
       allocate( inter_smp_stack_2nd(0:np_smp))
!
       iele_smp_stack_2nd = 0
       inod_smp_stack_2nd = 0
       inter_smp_stack_2nd = 0
!
       end subroutine allocate_2nd_geomet_param_smp
!
!-----------------------------------------------------------------------
!
       subroutine allocate_2nd_surf_param_smp
!
       use m_machine_parameter
!
       allocate( isurf_smp_stack_2nd(0:np_smp))
       isurf_smp_stack_2nd = 0
!
       end subroutine allocate_2nd_surf_param_smp
!
!-----------------------------------------------------------------------
!
       subroutine allocate_2nd_edge_param_smp
!
       use m_machine_parameter
!
       allocate( iedge_smp_stack_2nd(0:np_smp))
       iedge_smp_stack_2nd = 0
!
       end subroutine allocate_2nd_edge_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine deallocate_inod_in_surf_2nd
!
!
       deallocate (node_on_sf_2nd, node_on_sf_n_2nd)
!
       end subroutine deallocate_inod_in_surf_2nd
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_inod_in_edge_2nd
!
!
       deallocate (node_on_edge_2nd, node_on_edge_sf_2nd)
!
       end subroutine deallocate_inod_in_edge_2nd
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_2nd_geomet_param_smp
!
       deallocate( iele_smp_stack_2nd)
       deallocate( inod_smp_stack_2nd)
       deallocate( inter_smp_stack_2nd)
!
       end subroutine deallocate_2nd_geomet_param_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_2nd_surf_param_smp
!
       deallocate( isurf_smp_stack_2nd)
!
       end subroutine deallocate_2nd_surf_param_smp
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_2nd_edge_param_smp
!
       deallocate( iedge_smp_stack_2nd)
!
       end subroutine deallocate_2nd_edge_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_smp_size_2nd(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inod_smp_stack_2nd ', inod_smp_stack_2nd
       write(*,*) 'PE: ', my_rank,                                      &
     &           'inter_smp_stack_2nd ', inter_smp_stack_2nd
       write(*,*) 'PE: ', my_rank,                                      &
     &           'iele_smp_stack_2nd ', iele_smp_stack_2nd
!
      end subroutine check_smp_size_2nd
!
!-----------------------------------------------------------------------
!
      subroutine check_smp_size_2nd_surf_edge
!
        write(*,*) 'isurf_smp_stack_2nd ', isurf_smp_stack_2nd
        write(*,*) 'iedge_smp_stack_2nd ', iedge_smp_stack_2nd
!
      end subroutine check_smp_size_2nd_surf_edge
!
!-----------------------------------------------------------------------
!
      end module m_2nd_geometry_param
