!
!      module const_grp_edge_4_viewer
!
!     Written by H. Matsui on Jan., 2007
!
!!      subroutine construct_edge_4_ele_grp                             &
!!     &         (nnod_4_surf, nnod_4_edge, edge_sf_tbl)
!!      subroutine construct_edge_4_surf_grp                            &
!!     &         (nnod_4_surf, nnod_4_edge, edge_sf_tbl)
!        type(sum_hash_tbl), intent(inout) :: edge_sf_tbl
!
      module const_grp_edge_4_viewer
!
      use m_precision
!
      use m_surface_mesh_4_merge
      use m_pickup_table_4_viewer
!
      use t_sum_hash
!
      implicit    none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine construct_edge_4_ele_grp                               &
     &         (nnod_4_surf, nnod_4_edge, edge_sf_tbl)
!
      use set_edge_hash_by_sf
      use set_edge_data_by_sf
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      type(sum_hash_tbl), intent(inout) :: edge_sf_tbl
!
      integer(kind = kint) :: igrp, ngrp, ist, nedge_grp
!
!
      call alloc_merged_group_item(ele_edge_grp)
!
      do igrp = 1, ngrp_ele_sf
        ngrp = ele_surf_grp%istack_sf(igrp*num_pe_sf)                   &
     &        - ele_surf_grp%istack_sf( (igrp-1)*num_pe_sf )
        ist = ele_surf_grp%istack_sf((igrp-1)*num_pe_sf) + 1
!
!   set hash data for edge elements using sum of local node ID
!
        call clear_sum_hash(edge_sf_tbl)
!
!        write(*,*) 'const_part_edge_hash_4_sf', igrp
        call const_part_edge_hash_4_sf                                  &
     &     (nodpetot_viewer, surfpetot_viewer, ngrp,                    &
     &      nnod_4_surf, nnod_4_edge,                                   &
     &      ie_sf_viewer, ele_surf_grp%item_sf(ist),                    &
     &      edge_sf_tbl%num_hash, edge_sf_tbl%istack_hash,              &
     &      edge_sf_tbl%iend_hash, edge_sf_tbl%id_hash,                 &
     &      edge_sf_tbl%iflag_hash)
!
!
        ist = ele_edge_grp%istack_sf( (igrp-1)*num_pe_sf )
!
        call allocate_ele_edge_item_tmp
        ele_edge_item_tmp(1:ele_edge_grp%num_item)                      &
     &          = ele_edge_grp%item_sf(1:ele_edge_grp%num_item)
        call dealloc_merged_group_item(ele_edge_grp)
!
        call count_num_edges_by_sf(nodpetot_viewer, surfpetot_viewer,   &
     &      nnod_4_edge, edge_sf_tbl%istack_hash,                       &
     &      edge_sf_tbl%iend_hash, edge_sf_tbl%iflag_hash, nedge_grp)
        ele_edge_grp%istack_sf(igrp*num_pe_sf)                          &
     &        = ele_edge_grp%istack_sf((igrp-1)*num_pe_sf) + nedge_grp
        ele_edge_grp%num_item = ele_edge_grp%istack_sf(igrp*num_pe_sf)
!
        call alloc_merged_group_item(ele_edge_grp)
        ele_edge_grp%item_sf(1:ist) = ele_edge_item_tmp(1:ist)
        call deallocate_ele_edge_item_tmp
!
!        write(*,*) 'set_part_edges_4_sf', igrp
        call set_part_edges_4_sf(nodpetot_viewer, surfpetot_viewer,     &
     &      nnod_4_edge, nedge_grp, iedge_sf_viewer,                    &
     &      edge_sf_tbl%istack_hash, edge_sf_tbl%iend_hash,             &
     &      edge_sf_tbl%id_hash, edge_sf_tbl%iflag_hash,                &
     &      ele_edge_grp%item_sf(ist+1) )
!
      end do
!
!      write(50,*) 'ele_edge_item_sf', ele_edge_grp%item_sf
!
      end subroutine construct_edge_4_ele_grp
!
!------------------------------------------------------------------
!
      subroutine construct_edge_4_surf_grp                              &
     &         (nnod_4_surf, nnod_4_edge, edge_sf_tbl)
!
      use set_edge_hash_by_sf
      use set_edge_data_by_sf
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      type(sum_hash_tbl), intent(inout) :: edge_sf_tbl
!
      integer(kind = kint) :: igrp, ngrp, ist, nedge_grp
!
!
      call alloc_merged_group_item(sf_edge_grp)
!
      do igrp = 1, view_sf_grps%num_grp
        ngrp = sf_surf_grp%istack_sf( igrp*num_pe_sf )                  &
     &        - sf_surf_grp%istack_sf( (igrp-1)*num_pe_sf )
        ist = sf_surf_grp%istack_sf( (igrp-1)*num_pe_sf ) + 1
!
!   set hash data for edge elements using sum of local node ID
!
        call clear_sum_hash(edge_sf_tbl)
!
!        write(*,*) 'const_part_edge_hash_4_sf', igrp
        call const_part_edge_hash_4_sf                                  &
     &     (nodpetot_viewer, surfpetot_viewer, ngrp, nnod_4_surf,       &
     &      nnod_4_edge, ie_sf_viewer, sf_surf_grp%item_sf(ist),        &
     &      edge_sf_tbl%num_hash, edge_sf_tbl%istack_hash,              &
     &      edge_sf_tbl%iend_hash, edge_sf_tbl%id_hash,                 &
     &      edge_sf_tbl%iflag_hash)
!
!
        call allocate_sf_edge_item_tmp
        surf_edge_item_tmp(1:sf_edge_grp%num_item)                      &
     &          = sf_edge_grp%item_sf(1:sf_edge_grp%num_item)
        call dealloc_merged_group_item(sf_edge_grp)
!
        call count_num_edges_by_sf(nodpetot_viewer, surfpetot_viewer,   &
     &      nnod_4_edge, edge_sf_tbl%istack_hash,                       &
     &      edge_sf_tbl%iend_hash, edge_sf_tbl%iflag_hash, nedge_grp)
        sf_edge_grp%istack_sf(igrp*num_pe_sf)                           &
     &        = sf_edge_grp%istack_sf((igrp-1)*num_pe_sf) + nedge_grp
        sf_edge_grp%num_item = sf_edge_grp%istack_sf(igrp*num_pe_sf)
!
        call alloc_merged_group_item(sf_edge_grp)
        ist = sf_edge_grp%istack_sf( (igrp-1)*num_pe_sf )
        sf_edge_grp%item_sf(1:ist) = surf_edge_item_tmp(1:ist)
        call deallocate_sf_edge_item_tmp
!
!        write(*,*) 'set_part_edges_4_sf', igrp
        call set_part_edges_4_sf(nodpetot_viewer, surfpetot_viewer,     &
     &      nnod_4_edge, nedge_grp, iedge_sf_viewer,                    &
     &      edge_sf_tbl%istack_hash, edge_sf_tbl%iend_hash,             &
     &      edge_sf_tbl%id_hash, edge_sf_tbl%iflag_hash,                &
     &      sf_edge_grp%item_sf(ist+1) )
      end do
!
      end subroutine construct_edge_4_surf_grp
!
!------------------------------------------------------------------
!
      end module const_grp_edge_4_viewer
