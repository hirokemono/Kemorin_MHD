!
!      module viewer_group_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!      subroutine write_domain_group_viewer
!      subroutine read_domain_group_viewer
!
!!      subroutine write_nod_group_viewer
!!      subroutine read_nod_group_viewer
!!      subroutine write_ele_group_viewer
!!      subroutine read_ele_group_viewer
!!      subroutine write_surf_group_viewer
!!      subroutine read_surf_group_viewer
!
      module viewer_group_data_IO
!
      use m_precision
!
      use t_surface_mesh_4_merge
!
      implicit none
!
      character (len = 255) :: tmp_character
      private :: tmp_character
      private :: write_viewer_group_data, read_viewer_group_item
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_domain_group_viewer
!
      use m_surface_mesh_4_merge
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 3. node ID for domain boundary'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(i16)') domain_nod_grp%num_item
      write(surface_id,'(8i16)') domain_nod_grp%istack_sf(1:num_pe_sf)
      write(surface_id,'(8i16)')                                        &
     &       domain_nod_grp%item_sf(1:domain_nod_grp%num_item)
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 3.1 surface ID for domain boundary'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(i16)') domain_surf_grp%num_item
      write(surface_id,'(8i16)') domain_surf_grp%istack_sf(1:num_pe_sf)
!
      write(surface_id,'(8i16)')                                        &
     &       domain_surf_grp%item_sf(1:domain_surf_grp%num_item)
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 3.2 edge ID for domain boundary'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(i16)') domain_edge_grp%num_item
      write(surface_id,'(8i16)') domain_edge_grp%istack_sf(1:num_pe_sf)
!
      write(surface_id,'(8i16)')                                        &
     &       domain_edge_grp%item_sf(1:domain_edge_grp%num_item)
!
      end subroutine write_domain_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_domain_group_viewer
!
      use m_surface_mesh_4_merge
!
      use skip_comment_f
!
!      write(surface_id,'(a)') '! 3. node ID for domain boundary'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) domain_nod_grp%num_item
      read(surface_id,*) domain_nod_grp%istack_sf(1:num_pe_sf)
!
      call alloc_merged_group_item(domain_nod_grp)
      read(surface_id,*)                                                &
     &      domain_nod_grp%item_sf(1:domain_nod_grp%num_item)
!
!      write(surface_id,'(a)') '! 3.1 surface ID for domain boundary'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) domain_surf_grp%num_item
      read(surface_id,*) domain_surf_grp%istack_sf(1:num_pe_sf)
!
      call alloc_merged_group_item(domain_surf_grp)
      read(surface_id,*)                                                &
     &      domain_surf_grp%item_sf(1:domain_surf_grp%num_item)
!
!      write(surface_id,'(a)') '! 3.2 edge ID for domain boundary'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) domain_edge_grp%num_item
      read(surface_id,*) domain_edge_grp%istack_sf(1:num_pe_sf)
!
      call alloc_merged_group_item(domain_edge_grp)
      read(surface_id,*)                                                &
     &       domain_edge_grp%item_sf(1:domain_edge_grp%num_item)
!
      end subroutine read_domain_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_nod_group_viewer
!
      use m_surface_mesh_4_merge
!
      use m_fem_mesh_labels
!
      write(surface_id,'(a)', advance='NO') hd_fem_nodgrp()
      write(surface_id,'(i16)') ngrp_nod_sf
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_nod_sf,  &
     &    nod_gp_name_sf, view_nod_grps%node_grp)
!
      end subroutine write_nod_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_nod_group_viewer
!
      use m_surface_mesh_4_merge
!
      use skip_comment_f
!
      integer(kind = kint) :: num
!
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) ngrp_nod_sf
!
      num = num_pe_sf*ngrp_nod_sf
      call allocate_nod_grp_stack_4_surf
      read(surface_id,*) view_nod_grps%node_grp%istack_sf(1:num)
!
      view_nod_grps%node_grp%num_item                                   &
     &    = view_nod_grps%node_grp%istack_sf(ngrp_nod_sf*num_pe_sf)
      call alloc_merged_group_item(view_nod_grps%node_grp)
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_nod_sf,   &
     &    nod_gp_name_sf, view_nod_grps%node_grp)
!
      end subroutine read_nod_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_ele_group_viewer
!
      use m_surface_mesh_4_merge
!
      use m_fem_mesh_labels
!
      write(surface_id,'(a)', advance='NO') hd_fem_elegrp()
      write(surface_id,'(a)') '! 4.2.1 element data'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(2i16)')                                        &
     &    view_ele_grps%num_grp, view_ele_grps%surf_grp%num_item
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe_sf, view_ele_grps%num_grp,                 &
     &    view_ele_grps%grp_name, view_ele_grps%surf_grp)
!
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.2.2 node data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i16)') view_ele_grps%node_grp%num_item
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe_sf, view_ele_grps%num_grp,                 &
     &    view_ele_grps%grp_name, view_ele_grps%node_grp)
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.2.3 edge data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i16)') view_ele_grps%edge_grp%num_item
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe_sf, view_ele_grps%num_grp,                 &
     &    view_ele_grps%grp_name, view_ele_grps%edge_grp)
!
      end subroutine write_ele_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_ele_group_viewer
!
      use m_surface_mesh_4_merge
!
      use skip_comment_f
!
      integer(kind = kint) :: num
!
!      write(surface_id,'(a)') '! 4.2 element group'
!      write(surface_id,'(a)') '! 4.2.1 element data'
!
      num = num_pe_sf * view_ele_grps%num_grp
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*)                                             &
     &   view_ele_grps%num_grp, view_ele_grps%surf_grp%num_item
!
      call alloc_viewer_surf_grps_stack(num_pe_sf, view_ele_grps)
      read(surface_id,*) view_ele_grps%surf_grp%istack_sf(1:num)
!
      call alloc_merged_group_item(view_ele_grps%surf_grp)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe_sf, view_ele_grps%num_grp,                 &
     &    view_ele_grps%grp_name,  view_ele_grps%surf_grp)
!
!      write(surface_id,'(a)') '! 4.2.2 node data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) view_ele_grps%node_grp%num_item
!
      read(surface_id,*) view_ele_grps%node_grp%istack_sf(1:num)
      call alloc_merged_group_item(view_ele_grps%node_grp)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe_sf, view_ele_grps%num_grp,                 &
     &    view_ele_grps%grp_name, view_ele_grps%node_grp)
!
!      write(surface_id,'(a)') '! 4.2.3 edge data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) view_ele_grps%edge_grp%num_item
!
      read(surface_id,*)  view_ele_grps%edge_grp%istack_sf(1:num)
      call alloc_merged_group_item(view_ele_grps%edge_grp)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe_sf, view_ele_grps%num_grp,                 &
     &    view_ele_grps%grp_name, view_ele_grps%edge_grp)
!
      end subroutine read_ele_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_group_viewer
!
      use m_surface_mesh_4_merge
      use m_fem_mesh_labels
!
!
      write(surface_id,'(a)', advance='NO') hd_fem_sfgrp()
      write(surface_id,'(a)') '! 4.3.1 surface data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(2i16)')                                        &
     &       view_sf_grps%num_grp, view_sf_grps%surf_grp%num_item
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe_sf, view_sf_grps%num_grp,                  &
     &    view_sf_grps%grp_name, view_sf_grps%surf_grp)
!
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.3.2 node data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i16)') view_sf_grps%node_grp%num_item
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe_sf, view_sf_grps%num_grp,                  &
     &    view_sf_grps%grp_name, view_sf_grps%node_grp)
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.3.3 edge data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i16)') view_sf_grps%edge_grp%num_item
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe_sf, view_sf_grps%num_grp,                  &
     &    view_sf_grps%grp_name, view_sf_grps%edge_grp)
!
      end subroutine write_surf_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_surf_group_viewer
!
      use skip_comment_f
      use m_surface_mesh_4_merge
!
      integer(kind = kint) :: num
!
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*)                                             &
     &       view_sf_grps%num_grp, view_sf_grps%surf_grp%num_item
      num = num_pe_sf * view_sf_grps%num_grp
!
      call alloc_viewer_surf_grps_stack(num_pe_sf, view_sf_grps)
      read(surface_id,*) view_sf_grps%surf_grp%istack_sf(1:num)
!
      call alloc_merged_group_item(view_sf_grps%surf_grp)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe_sf, view_sf_grps%num_grp,                  &
     &    view_sf_grps%grp_name, view_sf_grps%surf_grp)
!
!      write(surface_id,'(a)') '! 4.3.2 node data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) view_sf_grps%node_grp%num_item
!
      call alloc_merged_group_item(view_sf_grps%node_grp)
      read(surface_id,*) view_sf_grps%node_grp%istack_sf(1:num)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe_sf, view_sf_grps%num_grp,                  &
     &    view_sf_grps%grp_name, view_sf_grps%node_grp)
!
!      write(surface_id,'(a)') '! 4.3.3 edge data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) view_sf_grps%edge_grp%num_item
!
      call alloc_merged_group_item(view_sf_grps%edge_grp)
      read(surface_id,*) view_sf_grps%edge_grp%istack_sf(1:num)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe_sf, view_sf_grps%num_grp,                  &
     &    view_sf_grps%grp_name, view_sf_grps%edge_grp)
!
      end subroutine read_surf_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_viewer_group_data(id_file, nprocs, ngrp,         &
     &          name, group)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nprocs, ngrp
      character(len = kchara), intent(in) :: name(ngrp)
      type(viewer_group_data), intent(in) :: group
!
      integer(kind = kint) :: i, ist, ied
!
!
      write(id_file,'(8i16)') group%istack_sf(1:nprocs*ngrp)
!
      if (ngrp .gt. 0) then
        do i = 1, ngrp
          ist = group%istack_sf(nprocs*(i-1)) + 1
          ied = group%istack_sf(nprocs*i    )
          write(id_file,'(a)') trim(name(i))
          write(id_file,'(8i16)') group%item_sf(ist:ied)
        end do
      else
        write(id_file,*) ''
      end if
!
      end subroutine write_viewer_group_data
!
! -----------------------------------------------------------------------
!
      subroutine read_viewer_group_item(id_file, nprocs, ngrp,          &
     &          name, group)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nprocs, ngrp
!
      character(len = kchara), intent(inout) :: name(ngrp)
      type(viewer_group_data), intent(inout) :: group
!
      integer(kind = kint) :: i, ist, ied
!
!
      do i = 1, ngrp
        ist = group%istack_sf(nprocs*(i-1)) + 1
        ied = group%istack_sf(nprocs*i    )
        read(surface_id,*) name(i)
        read(surface_id,*) group%item_sf(ist:ied)
      end do
!
      end subroutine read_viewer_group_item
!
! -----------------------------------------------------------------------
!
      end module viewer_group_data_IO
