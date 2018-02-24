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
!!     &         (num_pe_sf, ngrp_surf_sf, surf_gp_name_sf,             &
!!     &          sf_surf_grp, sf_edge_grp, sf_nod_grp)
!!        type(viewer_group_data), intent(in) :: sf_surf_grp
!!        type(viewer_group_data), intent(in) :: sf_edge_grp
!!        type(viewer_group_data), intent(in) :: sf_nod_grp
!!      subroutine read_surf_group_viewer
!!     &         (num_pe_sf, ngrp_surf_sf, surf_gp_name_sf,             &
!!     &          sf_surf_grp, sf_edge_grp, sf_nod_grp)
!!        type(viewer_group_data), intent(inout) :: sf_surf_grp
!!        type(viewer_group_data), intent(inout) :: sf_edge_grp
!!        type(viewer_group_data), intent(inout) :: sf_nod_grp
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
      write(surface_id,'(i16)') nsurf_domain_sf
      write(surface_id,'(8i16)') isurf_stack_domain_sf(1:num_pe_sf)
!
      write(surface_id,'(8i16)') isurf_domain_sf(1:nsurf_domain_sf)
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 3.2 edge ID for domain boundary'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(i16)') nedge_domain_sf
      write(surface_id,'(8i16)') edge_stack_domain_sf(1:num_pe_sf)
!
      write(surface_id,'(8i16)') edge_item_domain_sf(1:nedge_domain_sf)
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
      call allocate_domain_nod_item_sf
      read(surface_id,*)                                                &
     &      domain_nod_grp%item_sf(1:domain_nod_grp%num_item)
!
!      write(surface_id,'(a)') '! 3.1 surface ID for domain boundary'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) nsurf_domain_sf
      read(surface_id,*) isurf_stack_domain_sf(1:num_pe_sf)
!
      call allocate_domain_surf_item_sf
      read(surface_id,*) isurf_domain_sf(1:nsurf_domain_sf)
!
!      write(surface_id,'(a)') '! 3.2 edge ID for domain boundary'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) nedge_domain_sf
      read(surface_id,*) edge_stack_domain_sf(1:num_pe_sf)
!
      call allocate_domain_edge_item_sf
      read(surface_id,*) edge_item_domain_sf(1:nedge_domain_sf)
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
     &    nod_nod_grp%num_item, nod_nod_grp%istack_sf,                  &
     &    nod_gp_name_sf, nod_nod_grp%item_sf)
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
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) ngrp_nod_sf
!
      call allocate_nod_grp_stack_4_surf
      read(surface_id,*)                                                &
     &     nod_nod_grp%istack_sf( 1:(num_pe_sf*ngrp_nod_sf) )
!
      nod_nod_grp%num_item                                              &
     &    = nod_nod_grp%istack_sf(ngrp_nod_sf*num_pe_sf)
      call allocate_nod_grp_item_4_surf
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_nod_sf,   &
     &    nod_nod_grp%num_item, nod_nod_grp%istack_sf,                  &
     &    nod_gp_name_sf, nod_nod_grp%item_sf)
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
      write(surface_id,'(2i16)') ngrp_ele_sf, ele_surf_grp%num_item
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_ele_sf,  &
     &    ele_surf_grp%num_item, ele_surf_grp%istack_sf,                &
     &    ele_gp_name_sf, ele_surf_grp%item_sf)
!
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.2.2 node data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i16)') ele_nod_grp%num_item
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_ele_sf,  &
     &    ele_nod_grp%num_item, ele_nod_grp%istack_sf, ele_gp_name_sf,  &
     &    ele_nod_grp%item_sf)
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.2.3 edge data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i16)') ele_edge_grp%num_item
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_ele_sf,  &
     &    ele_edge_grp%num_item, ele_edge_grp%istack_sf,                &
     &    ele_gp_name_sf, ele_edge_grp%item_sf)
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
!
!      write(surface_id,'(a)') '! 4.2 element group'
!      write(surface_id,'(a)') '! 4.2.1 element data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) ngrp_ele_sf, ele_surf_grp%num_item
!
      call allocate_ele_grp_stack_4_surf
      read(surface_id,*)                                                &
     &   ele_surf_grp%istack_sf(1:(num_pe_sf*ngrp_ele_sf))
!
      call allocate_ele_grp_item_4_surf
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_ele_sf,   &
     &    ele_surf_grp%num_item, ele_surf_grp%istack_sf,                &
     &    ele_gp_name_sf,  ele_surf_grp%item_sf)
!
!      write(surface_id,'(a)') '! 4.2.2 node data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) ele_nod_grp%num_item
!
      read(surface_id,*)                                                &
     &    ele_nod_grp%istack_sf(1:(num_pe_sf*ngrp_ele_sf))
      call allocate_ele_gp_nod_item_sf
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_ele_sf,   &
     &    ele_nod_grp%num_item, ele_nod_grp%istack_sf, ele_gp_name_sf,  &
     &    ele_nod_grp%item_sf)
!
!      write(surface_id,'(a)') '! 4.2.3 edge data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) ele_edge_grp%num_item
!
      read(surface_id,*)                                                &
     &    ele_edge_grp%istack_sf(1:num_pe_sf*ngrp_ele_sf)
      call allocate_ele_grp_edge_item_sf
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_ele_sf,   &
     &    ele_edge_grp%num_item, ele_edge_grp%istack_sf,                &
     &    ele_gp_name_sf, ele_edge_grp%item_sf)
!
      end subroutine read_ele_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_group_viewer
!     &         (num_pe_sf, ngrp_surf_sf, surf_gp_name_sf,               &
!     &          sf_surf_grp, sf_edge_grp, sf_nod_grp)
!
      use m_surface_mesh_4_merge
      use m_fem_mesh_labels
!
!      integer(kind = kint), intent(in) :: num_pe_sf, ngrp_surf_sf
!      character(len=kchara), intent(in)                                 &
!     &                      :: surf_gp_name_sf(ngrp_surf_sf)
!      type(viewer_group_data), intent(in) :: sf_surf_grp
!      type(viewer_group_data), intent(in) :: sf_edge_grp
!      type(viewer_group_data), intent(in) :: sf_nod_grp
!
!
      write(surface_id,'(a)', advance='NO') hd_fem_sfgrp()
      write(surface_id,'(a)') '! 4.3.1 surface data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(2i16)') ngrp_surf_sf, sf_surf_grp%num_item
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_surf_sf, &
     &    sf_surf_grp%num_item, sf_surf_grp%istack_sf, surf_gp_name_sf, &
     &    sf_surf_grp%item_sf)
!
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.3.2 node data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i16)') sf_nod_grp%num_item
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_surf_sf, &
     &    sf_nod_grp%num_item, sf_nod_grp%istack_sf, surf_gp_name_sf,   &
     &    sf_nod_grp%item_sf)
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.3.3 edge data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i16)') sf_edge_grp%num_item
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_surf_sf, &
     &    sf_edge_grp%num_item, sf_edge_grp%istack_sf, surf_gp_name_sf, &
     &    sf_edge_grp%item_sf)
!
      end subroutine write_surf_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_surf_group_viewer
!     &         (num_pe_sf, ngrp_surf_sf, surf_gp_name_sf,               &
!     &          sf_surf_grp, sf_edge_grp, sf_nod_grp)
!
      use skip_comment_f
      use m_surface_mesh_4_merge
!
!      integer(kind = kint), intent(in) :: num_pe_sf
!      integer(kind = kint), intent(inout) :: ngrp_surf_sf
!      character(len=kchara), intent(inout)                              &
!     &                      :: surf_gp_name_sf(ngrp_surf_sf)
!      type(viewer_group_data), intent(inout) :: sf_surf_grp
!      type(viewer_group_data), intent(inout) :: sf_edge_grp
!      type(viewer_group_data), intent(inout) :: sf_nod_grp
!
!
!      write(surface_id,'(a)') '! 4.3 surface group'
!      write(surface_id,'(a)') '! 4.3.1 surface data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) ngrp_surf_sf, sf_surf_grp%num_item
!
      call allocate_surf_grp_stack_4_surf
      read(surface_id,*)                                                &
     &       sf_surf_grp%istack_sf(1:(num_pe_sf*ngrp_surf_sf))
!
      call alloc_merged_group_item(sf_surf_grp)
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_surf_sf,  &
     &    sf_surf_grp%num_item, sf_surf_grp%istack_sf,                  &
     &    surf_gp_name_sf, sf_surf_grp%item_sf)
!
!      write(surface_id,'(a)') '! 4.3.2 node data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) sf_nod_grp%num_item
!
      read(surface_id,*) sf_nod_grp%istack_sf(1:num_pe_sf*ngrp_surf_sf)
      call alloc_merged_group_item(sf_nod_grp)
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_surf_sf,  &
     &    sf_nod_grp%num_item, sf_nod_grp%istack_sf, surf_gp_name_sf,   &
     &    sf_nod_grp%item_sf)
!
!      write(surface_id,'(a)') '! 4.3.3 edge data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) sf_edge_grp%num_item
!
      read(surface_id,*)                                                &
     &       sf_edge_grp%istack_sf(1:num_pe_sf*ngrp_surf_sf)
      call alloc_merged_group_item(sf_edge_grp)
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_surf_sf,  &
     &    sf_edge_grp%num_item, sf_edge_grp%istack_sf, surf_gp_name_sf, &
     &    sf_edge_grp%item_sf)
!
      end subroutine read_surf_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_viewer_group_data(id_file, nprocs, ngrp,         &
     &          ntot, istack, name, item)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nprocs, ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
      integer(kind = kint), intent(in) :: item(ntot)
      character(len = kchara), intent(in) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, ied
!
!
      write(id_file,'(8i16)') istack(1:nprocs*ngrp)
!
      if (ngrp .gt. 0) then
        do i = 1, ngrp
          ist = istack(nprocs*(i-1)) + 1
          ied = istack(nprocs*i    )
          write(id_file,'(a)') trim(name(i))
          write(id_file,'(8i16)') item(ist:ied)
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
     &          ntot, istack, name, item)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nprocs, ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
!
      integer(kind = kint), intent(inout) :: item(ntot)
      character(len = kchara), intent(inout) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, ied
!
!
      do i = 1, ngrp
        ist = istack(nprocs*(i-1)) + 1
        ied = istack(nprocs*i    )
        read(surface_id,*) name(i)
        read(surface_id,*) item(ist:ied)
      end do
!
      end subroutine read_viewer_group_item
!
! -----------------------------------------------------------------------
!
      end module viewer_group_data_IO
