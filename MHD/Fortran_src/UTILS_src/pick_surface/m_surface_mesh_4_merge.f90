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
!      subroutine allocate_nod_grp_stack_4_surf
!      subroutine allocate_ele_grp_stack_4_surf
!      subroutine allocate_surf_grp_stack_4_surf
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
!      subroutine deallocate_sf_gp_nod_item_sf
!      subroutine deallocate_sf_grp_edge_item_sf
!
!      subroutine check_edge_connent_viewer(nnod_4_edge)
!
      module m_surface_mesh_4_merge
!
      use m_precision
!
      implicit none
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
      integer(kind=kint ) :: nsurf_domain_sf
      integer(kind=kint ), allocatable :: isurf_stack_domain_sf(:)
      integer(kind=kint ), allocatable :: isurf_domain_sf(:)
      integer(kind=kint ) :: nedge_domain_sf
      integer(kind=kint ), allocatable :: edge_stack_domain_sf(:)
      integer(kind=kint ), allocatable :: edge_item_domain_sf(:)
      integer(kind=kint ) :: nnod_domain_sf
      integer(kind=kint ), allocatable :: nod_stack_domain_sf(:)
      integer(kind=kint ), allocatable :: nod_item_domain_sf(:)
!
!
      character(len=kchara), allocatable :: nod_gp_name_sf(:)
      character(len=kchara), allocatable :: ele_gp_name_sf(:)
      character(len=kchara), allocatable :: surf_gp_name_sf(:)
      integer(kind=kint ) :: ngrp_nod_sf, nnod_nod_sf
      integer(kind=kint ), allocatable :: nod_stack_sf(:)
      integer(kind=kint ), allocatable :: nod_item_sf(:)
      integer(kind=kint ) :: ngrp_ele_sf
      integer(kind=kint ) :: nele_ele_sf, nedge_ele_sf, nnod_ele_sf
      integer(kind=kint ), allocatable :: ele_stack_sf(:)
      integer(kind=kint ), allocatable :: ele_edge_stack_sf(:)
      integer(kind=kint ), allocatable :: ele_nod_stack_sf(:)
      integer(kind=kint ), allocatable :: ele_item_sf(:)
      integer(kind=kint ), allocatable :: ele_edge_item_sf(:)
      integer(kind=kint ), allocatable :: ele_nod_item_sf(:)
      integer(kind=kint ) :: ngrp_surf_sf
      integer(kind=kint ) :: nsurf_surf_sf, nedge_surf_sf, nnod_surf_sf
      integer(kind=kint ), allocatable :: surf_stack_sf(:)
      integer(kind=kint ), allocatable :: surf_edge_stack_sf(:)
      integer(kind=kint ), allocatable :: surf_nod_stack_sf(:)
      integer(kind=kint ), allocatable :: surf_item_sf(:)
      integer(kind=kint ), allocatable :: surf_edge_item_sf(:)
      integer(kind=kint ), allocatable :: surf_nod_item_sf(:)
!
!
      integer (kind = kint), parameter :: surface_id = 15
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
      allocate( nod_stack_domain_sf(0:num_pe_sf)  )
      allocate( isurf_stack_domain_sf(0:num_pe_sf)  )
      allocate( edge_stack_domain_sf(0:num_pe_sf)  )
!
      nod_stack_domain_sf = 0
      isurf_stack_domain_sf = 0
      edge_stack_domain_sf = 0
!
      end subroutine allocate_domain_stack_4_surf
!
!------------------------------------------------------------------
!
      subroutine allocate_domain_surf_item_sf
!
      allocate( isurf_domain_sf(nsurf_domain_sf) )
      isurf_domain_sf = 0
!
      end subroutine allocate_domain_surf_item_sf
!
!------------------------------------------------------------------
!
      subroutine allocate_domain_nod_item_sf
!
      allocate( nod_item_domain_sf(nnod_domain_sf) )
      nod_item_domain_sf = 0
!
      end subroutine allocate_domain_nod_item_sf
!
!------------------------------------------------------------------
!
      subroutine allocate_domain_edge_item_sf
!
      allocate( edge_item_domain_sf(nedge_domain_sf) )
      edge_item_domain_sf = 0
!
      end subroutine allocate_domain_edge_item_sf
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_grp_stack_4_surf
!
!
      allocate( nod_gp_name_sf(ngrp_nod_sf)  )
      allocate( nod_stack_sf(0:num_pe_sf*ngrp_nod_sf)  )
!
      nod_stack_sf = 0
!
      end subroutine allocate_nod_grp_stack_4_surf
!
!------------------------------------------------------------------
!
      subroutine allocate_ele_grp_stack_4_surf
!
!
      allocate( ele_gp_name_sf(ngrp_ele_sf)  )
      allocate( ele_stack_sf(0:num_pe_sf*ngrp_ele_sf)  )
      allocate( ele_edge_stack_sf(0:num_pe_sf*ngrp_ele_sf)  )
      allocate( ele_nod_stack_sf(0:num_pe_sf*ngrp_ele_sf)  )
!
      ele_stack_sf = 0
      ele_edge_stack_sf = 0
      ele_nod_stack_sf = 0
!
      end subroutine allocate_ele_grp_stack_4_surf
!
!------------------------------------------------------------------
!
      subroutine allocate_surf_grp_stack_4_surf
!
!
      allocate( surf_gp_name_sf(ngrp_surf_sf)  )
      allocate( surf_stack_sf(0:num_pe_sf*ngrp_surf_sf)  )
      allocate( surf_edge_stack_sf(0:num_pe_sf*ngrp_surf_sf)  )
      allocate( surf_nod_stack_sf(0:num_pe_sf*ngrp_surf_sf)  )
!
      surf_stack_sf = 0
      surf_edge_stack_sf = 0
      surf_nod_stack_sf = 0
!
      end subroutine allocate_surf_grp_stack_4_surf
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_grp_item_4_surf
!
      allocate( nod_item_sf(nnod_nod_sf)  )
      nod_item_sf = 0
!
      end subroutine allocate_nod_grp_item_4_surf
!
!------------------------------------------------------------------
!
      subroutine allocate_ele_grp_item_4_surf
!
      allocate( ele_item_sf(nele_ele_sf)  )
      ele_item_sf = 0
!
      end subroutine allocate_ele_grp_item_4_surf
!
!------------------------------------------------------------------
!
      subroutine allocate_ele_gp_nod_item_sf
!
      allocate( ele_nod_item_sf(nnod_ele_sf)  )
      ele_nod_item_sf = 0
!
      end subroutine allocate_ele_gp_nod_item_sf
!
!------------------------------------------------------------------
!
      subroutine allocate_ele_grp_edge_item_sf
!
      allocate( ele_edge_item_sf(nedge_ele_sf)  )
      ele_edge_item_sf = 0
!
      end subroutine allocate_ele_grp_edge_item_sf
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_ele_gp_nod_item_sf
!
      deallocate( ele_nod_item_sf )
!
      end subroutine deallocate_ele_gp_nod_item_sf
!
!------------------------------------------------------------------
!
      subroutine deallocate_ele_grp_edge_item_sf
!
      deallocate( ele_edge_item_sf )
!
      end subroutine deallocate_ele_grp_edge_item_sf
!
!------------------------------------------------------------------
!
      subroutine allocate_surf_grp_item_4_surf
!
      allocate( surf_item_sf(nsurf_surf_sf)  )
      surf_item_sf = 0
!
      end subroutine allocate_surf_grp_item_4_surf
!
!------------------------------------------------------------------
!
      subroutine allocate_sf_gp_nod_item_sf
!
      allocate( surf_nod_item_sf(nnod_surf_sf)  )
      surf_nod_item_sf = 0
!
      end subroutine allocate_sf_gp_nod_item_sf
!
!------------------------------------------------------------------
!
      subroutine allocate_sf_grp_edge_item_sf
!
      allocate( surf_edge_item_sf(nedge_surf_sf)  )
      surf_edge_item_sf = 0
!
      end subroutine allocate_sf_grp_edge_item_sf
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_sf_gp_nod_item_sf
!
      deallocate( surf_nod_item_sf )
!
      end subroutine deallocate_sf_gp_nod_item_sf
!
!------------------------------------------------------------------
!
      subroutine deallocate_sf_grp_edge_item_sf
!
      deallocate( surf_edge_item_sf )
!
      end subroutine deallocate_sf_grp_edge_item_sf
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
