!
!      module viewer_group_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine write_domain_group_viewer(num_pe, domain_grps)
!!        type(viewer_surface_groups), intent(in) :: domain_grps
!!      subroutine read_domain_group_viewer(num_pe, domain_grps)
!!        type(viewer_surface_groups), intent(inout) :: domain_grps
!!
!!      subroutine write_nod_group_viewer(num_pe, view_nod_grps)
!!        type(viewer_node_groups), intent(in) :: view_nod_grps
!!      subroutine read_nod_group_viewer(num_pe, view_nod_grps)
!!        type(viewer_node_groups), intent(inout) :: view_nod_grps
!!      subroutine write_ele_group_viewer(num_pe, view_ele_grps)
!!        type(viewer_surface_groups), intent(in) :: view_ele_grps
!!      subroutine read_ele_group_viewer(num_pe, view_ele_grps)
!!        type(viewer_surface_groups), intent(inout) :: view_ele_grps
!!      subroutine write_surf_group_viewer(num_pe, view_sf_grps)
!!        type(viewer_surface_groups), intent(in) :: view_sf_grps
!!      subroutine read_surf_group_viewer(num_pe, view_sf_grps)
!!        type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
      module viewer_group_data_IO
!
      use m_precision
      use m_constants
!
      use t_viewer_mesh
      use t_viewer_group
      use m_viewer_mesh_labels
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
      subroutine write_domain_group_viewer(num_pe, domain_grps)
!
      integer(kind = kint), intent(in)  :: num_pe
      type(viewer_surface_groups), intent(in) :: domain_grps
!
!
      write(surface_id,'(a)', advance='NO') hd_domain_nod_grp()
!
      write(surface_id,'(i16)') domain_grps%node_grp%num_item
      write(surface_id,'(8i16)')                                        &
     &   domain_grps%node_grp%istack_sf(1:num_pe)
      write(surface_id,'(8i16)')                                        &
     &   domain_grps%node_grp%item_sf(1:domain_grps%node_grp%num_item)
!
      write(surface_id,'(a)', advance='NO') hd_domain_surf_grp()
!
      write(surface_id,'(i16)') domain_grps%surf_grp%num_item
      write(surface_id,'(8i16)')                                        &
     &   domain_grps%surf_grp%istack_sf(1:num_pe)
!
      write(surface_id,'(8i16)')                                        &
     &   domain_grps%surf_grp%item_sf(1:domain_grps%surf_grp%num_item)
!
      write(surface_id,'(a)', advance='NO') hd_domain_edge_grp()
!
!
      write(surface_id,'(i16)') domain_grps%edge_grp%num_item
      write(surface_id,'(8i16)')                                        &
     &     domain_grps%edge_grp%istack_sf(1:num_pe)
!
      write(surface_id,'(8i16)')                                        &
     &   domain_grps%edge_grp%item_sf(1:domain_grps%edge_grp%num_item)
!
      end subroutine write_domain_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_domain_group_viewer(num_pe, domain_grps)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in)  :: num_pe
      type(viewer_surface_groups), intent(inout) :: domain_grps
!
!
!      write(surface_id,'(a)') '! 3. node ID for domain boundary'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) domain_grps%node_grp%num_item
      read(surface_id,*) domain_grps%node_grp%istack_sf(1:num_pe)
!
      call alloc_merged_group_item(domain_grps%node_grp)
      read(surface_id,*)                                                &
     &   domain_grps%node_grp%item_sf(1:domain_grps%node_grp%num_item)
!
!      write(surface_id,'(a)') '! 3.1 surface ID for domain boundary'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) domain_grps%surf_grp%num_item
      read(surface_id,*) domain_grps%surf_grp%istack_sf(1:num_pe)
!
      call alloc_merged_group_item(domain_grps%surf_grp)
      read(surface_id,*)                                                &
     &   domain_grps%surf_grp%item_sf(1:domain_grps%surf_grp%num_item)
!
!      write(surface_id,'(a)') '! 3.2 edge ID for domain boundary'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) domain_grps%edge_grp%num_item
      read(surface_id,*) domain_grps%edge_grp%istack_sf(1:num_pe)
!
      call alloc_merged_group_item(domain_grps%edge_grp)
      read(surface_id,*)                                                &
     &   domain_grps%edge_grp%item_sf(1:domain_grps%edge_grp%num_item)
!
      end subroutine read_domain_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_nod_group_viewer(num_pe, view_nod_grps)
!
      use m_fem_mesh_labels
!
      integer(kind = kint), intent(in)  :: num_pe
      type(viewer_node_groups), intent(in) :: view_nod_grps
!
!
      write(surface_id,'(a)', advance='NO') hd_fem_nodgrp()
      write(surface_id,'(i16)') view_nod_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_nod_grps%num_grp,                    &
     &    view_nod_grps%grp_name, view_nod_grps%node_grp)
!
      end subroutine write_nod_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_nod_group_viewer(num_pe, view_nod_grps)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in)  :: num_pe
      type(viewer_node_groups), intent(inout) :: view_nod_grps
!
      integer(kind = kint) :: num
!
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) view_nod_grps%num_grp
!
      num = num_pe * view_nod_grps%num_grp
      call alloc_merged_node_grps_stack(num_pe, view_nod_grps)
      read(surface_id,*) view_nod_grps%node_grp%istack_sf(1:num)
!
      view_nod_grps%node_grp%num_item                                   &
     &    = view_nod_grps%node_grp%istack_sf(num)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe, view_nod_grps%num_grp,                    &
     &    view_nod_grps%grp_name, view_nod_grps%node_grp)
!
      end subroutine read_nod_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_ele_group_viewer(num_pe, view_ele_grps)
!
      use m_fem_mesh_labels
!
      integer(kind = kint), intent(in)  :: num_pe
      type(viewer_surface_groups), intent(in) :: view_ele_grps
!
!
      write(surface_id,'(a)', advance='NO') hd_fem_elegrp()
      write(surface_id,'(a)', advance='NO') hd_ele_surf_grp()
!
      write(surface_id,'(i16)') view_ele_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_ele_grps%num_grp,                    &
     &    view_ele_grps%grp_name, view_ele_grps%surf_grp)
!
!
      write(surface_id,'(a)', advance='NO') hd_ele_nod_grp()
      write(surface_id,'(i16)') view_ele_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_ele_grps%num_grp,                    &
     &    view_ele_grps%grp_name, view_ele_grps%node_grp)
!
      write(surface_id,'(a)', advance='NO') hd_surf_surf_grp()
      write(surface_id,'(i16)') view_ele_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_ele_grps%num_grp,                    &
     &    view_ele_grps%grp_name, view_ele_grps%edge_grp)
!
      end subroutine write_ele_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_ele_group_viewer(num_pe, view_ele_grps)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in)  :: num_pe
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
!
      integer(kind = kint) :: num, itmp
!
!      write(surface_id,'(a)') '! 4.2 element group'
!      write(surface_id,'(a)') '! 4.2.1 element data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) view_ele_grps%num_grp
!
      num = num_pe * view_ele_grps%num_grp
!
      call alloc_merged_surf_grps_stack(num_pe, view_ele_grps)
      read(surface_id,*) view_ele_grps%surf_grp%istack_sf(1:num)
!
      view_ele_grps%surf_grp%num_item                                   &
     &    = view_ele_grps%surf_grp%istack_sf(num)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe, view_ele_grps%num_grp,                    &
     &    view_ele_grps%grp_name,  view_ele_grps%surf_grp)
!
!      write(surface_id,'(a)') '! 4.2.2 node data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) itmp
      read(surface_id,*) view_ele_grps%node_grp%istack_sf(1:num)
!
      view_ele_grps%node_grp%num_item                                   &
     &    = view_ele_grps%node_grp%istack_sf(num)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe, view_ele_grps%num_grp,                    &
     &    view_ele_grps%grp_name, view_ele_grps%node_grp)
!
!      write(surface_id,'(a)') '! 4.2.3 edge data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) itmp
      read(surface_id,*)  view_ele_grps%edge_grp%istack_sf(1:num)
!
      view_ele_grps%edge_grp%num_item                                   &
     &    = view_ele_grps%edge_grp%istack_sf(num)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe, view_ele_grps%num_grp,                    &
     &    view_ele_grps%grp_name, view_ele_grps%edge_grp)
!
      end subroutine read_ele_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_group_viewer(num_pe, view_sf_grps)
!
      use m_fem_mesh_labels
!
      integer(kind = kint), intent(in)  :: num_pe
      type(viewer_surface_groups), intent(in) :: view_sf_grps
!
!
      write(surface_id,'(a)', advance='NO') hd_fem_sfgrp()
      write(surface_id,'(a)', advance='NO') hd_surf_surf_grp()
      write(surface_id,'(i16)') view_sf_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_sf_grps%num_grp,                     &
     &    view_sf_grps%grp_name, view_sf_grps%surf_grp)
!
!
      write(surface_id,'(a)', advance='NO') hd_surf_nod_grp()
      write(surface_id,'(i16)') view_sf_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_sf_grps%num_grp,                     &
     &    view_sf_grps%grp_name, view_sf_grps%node_grp)
!
      write(surface_id,'(a)', advance='NO') hd_surf_edge_grp()
      write(surface_id,'(i16)') view_sf_grps%num_grp
!
      call write_viewer_group_data                                      &
     &   (surface_id, num_pe, view_sf_grps%num_grp,                     &
     &    view_sf_grps%grp_name, view_sf_grps%edge_grp)
!
      end subroutine write_surf_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_surf_group_viewer(num_pe, view_sf_grps)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in)  :: num_pe
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
      integer(kind = kint) :: num, itmp
!
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) view_sf_grps%num_grp
      num = num_pe * view_sf_grps%num_grp
!
      call alloc_merged_surf_grps_stack(num_pe, view_sf_grps)
      read(surface_id,*) view_sf_grps%surf_grp%istack_sf(1:num)
!
      view_sf_grps%surf_grp%num_item                                    &
     &    = view_sf_grps%surf_grp%istack_sf(num)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe, view_sf_grps%num_grp,                     &
     &    view_sf_grps%grp_name, view_sf_grps%surf_grp)
!
!      write(surface_id,'(a)') '! 4.3.2 node data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) itmp
      read(surface_id,*) view_sf_grps%node_grp%istack_sf(1:num)
!
      view_sf_grps%node_grp%num_item                                    &
     &    = view_sf_grps%node_grp%istack_sf(num)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe, view_sf_grps%num_grp,                     &
     &    view_sf_grps%grp_name, view_sf_grps%node_grp)
!
!      write(surface_id,'(a)') '! 4.3.3 edge data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) itmp
      read(surface_id,*) view_sf_grps%edge_grp%istack_sf(1:num)
!
      view_sf_grps%edge_grp%num_item                                    &
     &    = view_sf_grps%edge_grp%istack_sf(num)
!
      call read_viewer_group_item                                       &
     &   (surface_id, num_pe, view_sf_grps%num_grp,                     &
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
      call alloc_merged_group_item(group)
!
      do i = 1, ngrp
        ist = group%istack_sf(nprocs*(i-1)) + 1
        ied = group%istack_sf(nprocs*i    )
        read(id_file,*) name(i)
        read(id_file,*) group%item_sf(ist:ied)
      end do
!
      end subroutine read_viewer_group_item
!
! -----------------------------------------------------------------------
!
      end module viewer_group_data_IO
