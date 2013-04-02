!
!      module const_grp_edge_4_viewer
!
      module const_grp_edge_4_viewer
!
!     Written by H. Matsui on Jan., 2007
!
      use m_precision
!
      use m_geometry_parameter
      use m_surface_mesh_4_merge
      use m_pickup_table_4_viewer
!
      implicit    none
!
!      subroutine construct_edge_4_ele_grp
!      subroutine construct_edge_4_surf_grp
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine construct_edge_4_ele_grp
!
      use set_edge_hash
      use mark_edge_hash
      use set_edge_data
!
      integer(kind = kint) :: igrp, ngrp, ist, nedge_grp
!
!
      call allocate_ele_grp_edge_item_sf
!
      do igrp = 1, ngrp_ele_sf
!
        ngrp = ele_stack_sf(igrp*num_pe_sf)                             &
     &        - ele_stack_sf( (igrp-1)*num_pe_sf )
        ist = ele_stack_sf((igrp-1)*num_pe_sf) + 1
!
!   set hash data for edge elements using sum of local node ID
!
        call cleear_edge_hash
!
!        write(*,*) 'count_part_edge_hash_4_sf', igrp
        call count_part_edge_hash_4_sf(nodpetot_viewer,                 &
     &      surfpetot_viewer, ngrp, nnod_4_surf, nnod_4_edge,           &
     &      ie_sf_viewer, ele_item_sf(ist) )
!
!        write(*,*) 'set_part_edge_hash_4_sf', igrp
        call set_part_edge_hash_4_sf(surfpetot_viewer, ngrp,            &
     &    nnod_4_surf, ie_sf_viewer, ele_item_sf(ist) )
!
!
!        write(*,*) 'mark_all_edges', igrp
        call mark_all_edges(surfpetot_viewer, nnod_4_surf,              &
     &      ie_sf_viewer )
!
!
        ist = ele_edge_stack_sf( (igrp-1)*num_pe_sf )
!
        call allocate_ele_edge_item_tmp
        ele_edge_item_tmp(1:nedge_ele_sf)                               &
     &          = ele_edge_item_sf(1:nedge_ele_sf)
        call deallocate_ele_grp_edge_item_sf
!
        call count_num_edges(nedge_grp)
        ele_edge_stack_sf(igrp*num_pe_sf)                               &
     &        = ele_edge_stack_sf((igrp-1)*num_pe_sf) + nedge_grp
        nedge_ele_sf = ele_edge_stack_sf(igrp*num_pe_sf)
!
        call allocate_ele_grp_edge_item_sf
        ele_edge_item_sf(1:ist) = ele_edge_item_tmp(1:ist)
        call deallocate_ele_edge_item_tmp
!
!        write(*,*) 'set_part_edges', igrp
        call set_part_edges(surfpetot_viewer, edgepetot_viewer,         &
     &      nedge_grp, iedge_sf_viewer, ele_edge_item_sf(ist+1) )
!
      end do
!
!      write(50,*) 'ele_edge_item_sf', ele_edge_item_sf
!
      end subroutine construct_edge_4_ele_grp
!
!------------------------------------------------------------------
!
      subroutine construct_edge_4_surf_grp
!
      use set_edge_hash
      use mark_edge_hash
      use set_edge_data
!
      integer(kind = kint) :: igrp, ngrp, ist, nedge_grp
!
!
      call allocate_sf_grp_edge_item_sf
!
      do igrp = 1, ngrp_surf_sf
!
        ngrp = surf_stack_sf( igrp*num_pe_sf )                          &
     &        - surf_stack_sf( (igrp-1)*num_pe_sf )
        ist = surf_stack_sf( (igrp-1)*num_pe_sf ) + 1
!
!   set hash data for edge elements using sum of local node ID
!
        call cleear_edge_hash
!
!        write(*,*) 'count_part_edge_hash_4_sf', igrp
        call count_part_edge_hash_4_sf(nodpetot_viewer,                 &
     &      surfpetot_viewer, ngrp, nnod_4_surf, nnod_4_edge,           &
     &      ie_sf_viewer, surf_item_sf(ist) )
!
!        write(*,*) 'set_part_edge_hash_4_sf', igrp
        call set_part_edge_hash_4_sf(surfpetot_viewer, ngrp,            &
     &    nnod_4_surf, ie_sf_viewer, surf_item_sf(ist) )
!
!
!        write(*,*) 'mark_all_edges', igrp
        call mark_all_edges(surfpetot_viewer, nnod_4_surf,              &
     &      ie_sf_viewer )
!
!
        ist = surf_edge_stack_sf( (igrp-1)*num_pe_sf )
!
        call allocate_sf_edge_item_tmp
        surf_edge_item_tmp(1:nedge_surf_sf)                             &
     &          = surf_edge_item_sf(1:nedge_surf_sf)
        call deallocate_sf_grp_edge_item_sf
!
        call count_num_edges(nedge_grp)
        surf_edge_stack_sf(igrp*num_pe_sf)                              &
     &        = surf_edge_stack_sf((igrp-1)*num_pe_sf) + nedge_grp
        nedge_surf_sf = surf_edge_stack_sf(igrp*num_pe_sf)
!
        call allocate_sf_grp_edge_item_sf
        surf_edge_item_sf(1:ist) = surf_edge_item_tmp(1:ist)
        call deallocate_sf_edge_item_tmp
!
!        write(*,*) 'set_part_edges', igrp
        call set_part_edges(surfpetot_viewer, edgepetot_viewer,         &
     &      nedge_grp, iedge_sf_viewer, surf_edge_item_sf(ist+1) )
!
      end do
!
      end subroutine construct_edge_4_surf_grp
!
!------------------------------------------------------------------
!
      end module const_grp_edge_4_viewer
