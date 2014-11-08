!
!      module viewer_group_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!      subroutine write_domain_group_viewer
!      subroutine read_domain_group_viewer
!
!      subroutine write_nod_group_viewer
!      subroutine read_nod_group_viewer
!      subroutine write_ele_group_viewer
!      subroutine read_ele_group_viewer
!      subroutine write_surf_group_viewer
!      subroutine read_surf_group_viewer
!
      module viewer_group_data_IO
!
      use m_precision
!
      use m_surface_mesh_4_merge
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
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 3. node ID for domain boundary'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(i15)') nnod_domain_sf
      write(surface_id,'(8i10)') nod_stack_domain_sf(1:num_pe_sf)
      write(surface_id,'(8i10)') nod_item_domain_sf(1:nnod_domain_sf)
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 3.1 surface ID for domain boundary'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(i15)') nsurf_domain_sf
      write(surface_id,'(8i10)') isurf_stack_domain_sf(1:num_pe_sf)
!
      write(surface_id,'(8i10)') isurf_domain_sf(1:nsurf_domain_sf)
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 3.2 edge ID for domain boundary'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(i15)') nedge_domain_sf
      write(surface_id,'(8i10)') edge_stack_domain_sf(1:num_pe_sf)
!
      write(surface_id,'(8i10)') edge_item_domain_sf(1:nedge_domain_sf)
!
      end subroutine write_domain_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_domain_group_viewer
!
      use skip_comment_f
!
!      write(surface_id,'(a)') '! 3. node ID for domain boundary'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) nnod_domain_sf
      read(surface_id,*) nod_stack_domain_sf(1:num_pe_sf)
!
      call allocate_domain_nod_item_sf
      read(surface_id,*) nod_item_domain_sf(1:nnod_domain_sf)
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
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4. group information'
      write(surface_id,'(a)') '! 4.1 node group'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i15)') ngrp_nod_sf
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_nod_sf,  &
     &    nnod_nod_sf, nod_stack_sf, nod_gp_name_sf, nod_item_sf)
!
      end subroutine write_nod_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_nod_group_viewer
!
      use skip_comment_f
!
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) ngrp_nod_sf
!
      call allocate_nod_grp_stack_4_surf
      read(surface_id,*) nod_stack_sf( 1:(num_pe_sf*ngrp_nod_sf) )
!
      nnod_nod_sf = nod_stack_sf(ngrp_nod_sf*num_pe_sf)
      call allocate_nod_grp_item_4_surf
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_nod_sf,   &
     &    nnod_nod_sf, nod_stack_sf, nod_gp_name_sf, nod_item_sf)
!
      end subroutine read_nod_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_ele_group_viewer
!
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.2 element group'
      write(surface_id,'(a)') '! 4.2.1 element data'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(2i15)') ngrp_ele_sf, nele_ele_sf
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_ele_sf,  &
     &    nele_ele_sf, ele_stack_sf, ele_gp_name_sf, ele_item_sf)
!
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.2.2 node data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i15)') nnod_ele_sf
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_ele_sf,  &
     &    nnod_ele_sf, ele_nod_stack_sf, ele_gp_name_sf,                &
     &    ele_nod_item_sf)
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.2.3 edge data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i15)') nedge_ele_sf
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_ele_sf,  &
     &    nedge_ele_sf, ele_edge_stack_sf, ele_gp_name_sf,              &
     &    ele_edge_item_sf)
!
      end subroutine write_ele_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_ele_group_viewer
!
      use skip_comment_f
!
!
!      write(surface_id,'(a)') '! 4.2 element group'
!      write(surface_id,'(a)') '! 4.2.1 element data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) ngrp_ele_sf, nele_ele_sf
!
      call allocate_ele_grp_stack_4_surf
      read(surface_id,*) ele_stack_sf(1:(num_pe_sf*ngrp_ele_sf))
!
      call allocate_ele_grp_item_4_surf
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_ele_sf,   &
     &    nele_ele_sf, ele_stack_sf, ele_gp_name_sf,  ele_item_sf)
!
!      write(surface_id,'(a)') '! 4.2.2 node data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) nnod_ele_sf
!
      read(surface_id,*) ele_nod_stack_sf(1:(num_pe_sf*ngrp_ele_sf))
      call allocate_ele_gp_nod_item_sf
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_ele_sf,   &
     &    nnod_ele_sf, ele_nod_stack_sf, ele_gp_name_sf,                &
     &    ele_nod_item_sf)
!
!      write(surface_id,'(a)') '! 4.2.3 edge data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) nedge_ele_sf
!
      read(surface_id,*) ele_edge_stack_sf(1:num_pe_sf*ngrp_ele_sf)
      call allocate_ele_grp_edge_item_sf
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_ele_sf,   &
     &    nedge_ele_sf, ele_edge_stack_sf, ele_gp_name_sf,              &
     &    ele_edge_item_sf)
!
      end subroutine read_ele_group_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_group_viewer
!
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.3 surface group'
      write(surface_id,'(a)') '! 4.3.1 surface data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(2i15)') ngrp_surf_sf, nsurf_surf_sf
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_surf_sf, &
     &    nsurf_surf_sf, surf_stack_sf, surf_gp_name_sf,                &
     &    surf_item_sf)
!
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.3.2 node data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i15)') nnod_surf_sf
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_surf_sf, &
     &    nnod_surf_sf, surf_nod_stack_sf, surf_gp_name_sf,             &
     &    surf_nod_item_sf)
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 4.3.3 edge data'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i15)') nedge_surf_sf
!
      call write_viewer_group_data(surface_id, num_pe_sf, ngrp_surf_sf, &
     &    nedge_surf_sf, surf_edge_stack_sf, surf_gp_name_sf,           &
     &    surf_edge_item_sf)
!
      end subroutine write_surf_group_viewer
!
!------------------------------------------------------------------
!
      subroutine read_surf_group_viewer
!
      use skip_comment_f
!
!
!      write(surface_id,'(a)') '! 4.3 surface group'
!      write(surface_id,'(a)') '! 4.3.1 surface data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) ngrp_surf_sf, nsurf_surf_sf
!
      call allocate_surf_grp_stack_4_surf
      read(surface_id,*)                                                &
     &       surf_stack_sf( 1:(num_pe_sf*ngrp_surf_sf) )
!
      call allocate_surf_grp_item_4_surf
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_surf_sf,  &
     &    nsurf_surf_sf, surf_stack_sf, surf_gp_name_sf, surf_item_sf)
!
!      write(surface_id,'(a)') '! 4.3.2 node data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) nnod_surf_sf
!
      read(surface_id,*)  surf_nod_stack_sf(1:num_pe_sf*ngrp_surf_sf)
      call allocate_sf_gp_nod_item_sf
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_surf_sf,  &
     &    nnod_surf_sf, surf_nod_stack_sf, surf_gp_name_sf,             &
     &    surf_nod_item_sf)
!
!      write(surface_id,'(a)') '! 4.3.3 edge data'
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) nedge_surf_sf
!
      read(surface_id,*)  surf_edge_stack_sf(1:num_pe_sf*ngrp_surf_sf)
      call allocate_sf_grp_edge_item_sf
!
      call read_viewer_group_item(surface_id, num_pe_sf, ngrp_surf_sf,  &
     &    nedge_surf_sf, surf_edge_stack_sf, surf_gp_name_sf,           &
     &    surf_edge_item_sf)
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
      write(id_file,'(8i10)') istack(1:nprocs*ngrp)
!
      if (ngrp .gt. 0) then
        do i = 1, ngrp
          ist = istack(nprocs*(i-1)) + 1
          ied = istack(nprocs*i    )
          write(id_file,'(a)') trim(name(i))
          write(id_file,'(8i10)') item(ist:ied)
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
