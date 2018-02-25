!
!      module m_surface_mesh_4_merge
!
!      Written by Kemorin
!
!      subroutine allocate_num_mesh_sf
!      subroutine allocate_nod_position_viewer
!      subroutine allocate_surf_type_viewer
!      subroutine allocate_surf_connect_viewer(nnod_4_surf)
!      subroutine allocate_edge_data_4_sf
!      subroutine allocate_edge_type_viewer
!      subroutine allocate_domain_stack_4_surf
!      subroutine allocate_domain_surf_item_sf
!      subroutine allocate_domain_nod_item_sf
!      subroutine allocate_domain_edge_item_sf
!      subroutine allocate_nod_grp_item_4_surf
!      subroutine allocate_ele_grp_item_4_surf
!      subroutine allocate_ele_gp_nod_item_sf
!      subroutine allocate_ele_grp_edge_item_sf
!      subroutine allocate_surf_grp_item_4_surf
!      subroutine allocate_sf_gp_nod_item_sf
!      subroutine allocate_sf_grp_edge_item_sf
!
!      subroutine deallocate_nod_position_viewer
!      subroutine deallocate_surf_type_viewer
!      subroutine deallocate_surf_connect_viewer
!      subroutine deallocate_ele_gp_nod_item_sf
!      subroutine deallocate_ele_grp_edge_item_sf
!      subroutine deallocate_sf_grp_edge_item_sf
!
!      subroutine check_edge_connent_viewer(nnod_4_edge)
!
      module m_surface_mesh_4_merge
!
      use m_precision
      use m_constants
      use t_surface_mesh_4_merge
!
      implicit none
!
!
      integer(kind=kint )  :: num_pe_sf
!
      integer(kind=kint ), allocatable :: inod_sf_stack(:)
      integer(kind=kint ), allocatable :: iedge_sf_stack(:)
      integer(kind=kint ), allocatable :: isurf_sf_stack(:)
!
      integer(kind=kint )  ::  surfpetot_viewer
      integer(kind=kint )  ::  edgepetot_viewer
      integer(kind=kint )  ::  nodpetot_viewer
!
      integer(kind=kint ), allocatable  ::  ie_sf_viewer(:,:)
      integer(kind=kint ), allocatable  ::  ie_edge_viewer(:,:)
      integer(kind=kint ), allocatable  ::  surftyp_viewer(:  )
      integer(kind=kint ), allocatable  ::  edgetyp_viewer(:  )
      integer(kind=kint ), allocatable  ::  iedge_sf_viewer(:,:)
!
      real   (kind=kreal), dimension(:,:), allocatable  ::  xx_view
!
!
      integer(kind=kint ), parameter :: ngrp_domain = ione
      type(viewer_group_data), save :: domain_surf_grp
!
      type(viewer_surface_groups), save :: domain_grps
!domain_grps%edge_grp
!
      type(viewer_node_groups), save :: view_nod_grps
      type(viewer_surface_groups), save :: view_ele_grps
      type(viewer_surface_groups), save :: view_sf_grps
!
!
      character (len = kchara) :: surface_file_head = 'in_surface'
      character (len = kchara) :: surface_file_name = 'in_surface.ksm'
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_num_mesh_sf
!
      allocate( inod_sf_stack(0:num_pe_sf)  )
      allocate( isurf_sf_stack(0:num_pe_sf) )
      allocate( iedge_sf_stack(0:num_pe_sf) )
      inod_sf_stack  = 0
      isurf_sf_stack = 0
      iedge_sf_stack = 0
!
      end subroutine allocate_num_mesh_sf
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_position_viewer
!
      allocate( xx_view(nodpetot_viewer,3) )
      xx_view = 0.0d0
!
      end subroutine allocate_nod_position_viewer
!
!------------------------------------------------------------------
!
      subroutine allocate_surf_type_viewer
!
      allocate( surftyp_viewer(surfpetot_viewer)       )
      surftyp_viewer = 0
!
      end subroutine allocate_surf_type_viewer
!
!------------------------------------------------------------------
!
      subroutine allocate_surf_connect_viewer(nnod_4_surf)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
!
      allocate( ie_sf_viewer(surfpetot_viewer,nnod_4_surf) )
      ie_sf_viewer = 0
!
      end subroutine allocate_surf_connect_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_nod_position_viewer
!
      deallocate( xx_view )
!
      end subroutine deallocate_nod_position_viewer
!
!------------------------------------------------------------------
!
      subroutine deallocate_surf_type_viewer
!
      deallocate( surftyp_viewer )
!
      end subroutine deallocate_surf_type_viewer
!
!------------------------------------------------------------------
!
      subroutine deallocate_surf_connect_viewer
!
      deallocate( ie_sf_viewer )
!
      end subroutine deallocate_surf_connect_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_edge_data_4_sf(nnod_4_edge)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_4_edge
!
!
      allocate (ie_edge_viewer(edgepetot_viewer,nnod_4_edge))
      allocate (iedge_sf_viewer(surfpetot_viewer,nedge_4_surf) )
      ie_edge_viewer = 0
      iedge_sf_viewer = 0
!
      end subroutine allocate_edge_data_4_sf
!
!------------------------------------------------------------------
!
      subroutine allocate_edge_type_viewer
!
      use m_geometry_constants
!
      allocate (edgetyp_viewer(edgepetot_viewer) )
      edgetyp_viewer = 0
!
      end subroutine allocate_edge_type_viewer
!
!------------------------------------------------------------------
!
      subroutine allocate_domain_stack_4_surf
!
!
      domain_grps%num_grp = 1
!
      allocate(domain_grps%grp_name(domain_grps%num_grp))
!
      call alloc_merged_group_stack                                     &
     &   (num_pe_sf, ngrp_domain, domain_surf_grp)
      call alloc_merged_group_stack                                     &
     &   (num_pe_sf, ngrp_domain, domain_grps%edge_grp)
      call alloc_merged_group_stack                                     &
     &   (num_pe_sf, ngrp_domain, domain_grps%node_grp)
!
      domain_grps%grp_name = 'subdomains'
!
      end subroutine allocate_domain_stack_4_surf
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine check_edge_connent_viewer(nnod_4_edge)
!
      integer(kind = kint), intent(in) :: nnod_4_edge
      integer(kind = kint) :: iedge
!
!
      write(50,*) 'edgepetot_viewer', edgepetot_viewer
      write(50,*) 'iedge_sf_stack', iedge_sf_stack
      write(50,*) 'ie_edge_viewer'
      do iedge = 1, edgepetot_viewer
        write(50,*) iedge, ie_edge_viewer(iedge,1:nnod_4_edge)
      end do
!
      end subroutine check_edge_connent_viewer
!
!------------------------------------------------------------------
!
      end module m_surface_mesh_4_merge
