!gz_viewer_group_data_IO
!      module gz_viewer_group_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine write_domain_group_viewer_gz
!!      subroutine read_domain_group_viewer_gz
!!
!!      subroutine write_nod_group_viewer_gz
!!      subroutine read_nod_group_viewer_gz
!!      subroutine write_ele_group_viewer_gz
!!      subroutine read_ele_group_viewer_gz
!!      subroutine write_surf_group_viewer_gz                           &
!!     &         (num_pe_sf, ngrp_surf_sf, surf_gp_name_sf,             &
!!     &          sf_surf_grp, sf_edge_grp, sf_nod_grp)
!!        type(viewer_group_data), intent(in) :: sf_surf_grp
!!        type(viewer_group_data), intent(in) :: sf_edge_grp
!!        type(viewer_group_data), intent(in) :: sf_nod_grp
!!      subroutine read_surf_group_viewer_gz                            &
!!     &         (num_pe_sf, ngrp_surf_sf, surf_gp_name_sf,             &
!!     &          sf_surf_grp, sf_edge_grp, sf_nod_grp)
!!        type(viewer_group_data), intent(inout) :: sf_surf_grp
!!        type(viewer_group_data), intent(inout) :: sf_edge_grp
!!        type(viewer_group_data), intent(inout) :: sf_nod_grp
!
      module gz_viewer_group_data_IO
!
      use m_precision
!
      use t_surface_mesh_4_merge
      use skip_gz_comment
!
      implicit none
!
      private :: write_viewer_group_data_gz, read_viewer_group_item_gz
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_domain_group_viewer_gz
!
      use m_surface_mesh_4_merge
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)')                                           &
     &        '! 3. node ID for domain boundary', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      write(textbuf,'(i16,a1)') nnod_domain_sf, char(0)
      call gz_write_textbuf_w_lf
!
      call write_gz_multi_int_8i10(num_pe_sf,                           &
     &    nod_stack_domain_sf(1) )
      call write_gz_multi_int_8i10(nnod_domain_sf,                      &
     &    nod_item_domain_sf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)')                                           &
     &        '! 3.1 surface ID for domain boundary', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      write(textbuf,'(i16,a1)') nsurf_domain_sf, char(0)
      call gz_write_textbuf_w_lf
!
      call write_gz_multi_int_8i10(num_pe_sf,                           &
     &    isurf_stack_domain_sf(1) )
      call write_gz_multi_int_8i10(nsurf_domain_sf,                     &
     &    isurf_domain_sf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)')                                           &
     &       '! 3.2 edge ID for domain boundary', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      write(textbuf,'(i16,a1)') nedge_domain_sf, char(0)
      call gz_write_textbuf_w_lf
!
      call write_gz_multi_int_8i10(num_pe_sf,                           &
     &    edge_stack_domain_sf(1) )
      call write_gz_multi_int_8i10(nedge_domain_sf,                     &
     &    edge_item_domain_sf)
!
      end subroutine write_domain_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_domain_group_viewer_gz
!
      use m_surface_mesh_4_merge
!
!
!      write(surface_id,'(a)') '! 3. node ID for domain boundary'
!
      call skip_gz_comment_int(nnod_domain_sf)
      call read_gz_multi_int(num_pe_sf, nod_stack_domain_sf(1))
!
      call allocate_domain_nod_item_sf
      read(surface_id,*) nod_item_domain_sf(1:nnod_domain_sf)
!
!      write(surface_id,'(a)') '! 3.1 surface ID for domain boundary'
!
      call skip_gz_comment_int(nsurf_domain_sf)
      call read_gz_multi_int(num_pe_sf, isurf_stack_domain_sf(1))
!
      call allocate_domain_surf_item_sf
      call read_gz_multi_int(nsurf_domain_sf, isurf_domain_sf)
!
!      write(surface_id,'(a)') '! 3.2 edge ID for domain boundary'
!
      call skip_gz_comment_int(nedge_domain_sf)
      call read_gz_multi_int(num_pe_sf, edge_stack_domain_sf(1))
!
      call allocate_domain_edge_item_sf
      call read_gz_multi_int(nedge_domain_sf, edge_item_domain_sf)
!
      end subroutine read_domain_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_nod_group_viewer_gz
!
      use m_surface_mesh_4_merge
!
      use m_fem_mesh_labels
!
!
      textbuf = hd_fem_nodgrp() // char(0)
      call gz_write_textbuf_no_lf
!
      write(textbuf,'(i16,a1)') ngrp_nod_sf, char(0)
      call gz_write_textbuf_w_lf
!
     call write_viewer_group_data_gz(num_pe_sf, ngrp_nod_sf,            &
    &    nod_nod_grp%num_item, nod_nod_grp%istack_sf,                   &
    &    nod_gp_name_sf, nod_nod_grp%item_sf)
!
      end subroutine write_nod_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_nod_group_viewer_gz
!
      use m_surface_mesh_4_merge
!
!
      call skip_gz_comment_int(ngrp_nod_sf)
!
      call allocate_nod_grp_stack_4_surf
!
      call read_gz_multi_int((num_pe_sf*ngrp_nod_sf),                   &
     &    nod_nod_grp%istack_sf(1))
      nod_nod_grp%num_item                                              &
     &    = nod_nod_grp%istack_sf(ngrp_nod_sf*num_pe_sf)
!
      call allocate_nod_grp_item_4_surf
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_nod_sf,            &
     &    nod_nod_grp%num_item, nod_nod_grp%istack_sf,                  &
     &    nod_gp_name_sf, nod_nod_grp%item_sf)
!
      end subroutine read_nod_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_ele_group_viewer_gz
!
      use m_surface_mesh_4_merge
!
      use m_fem_mesh_labels
!
!
      textbuf = hd_fem_elegrp() // char(0)
      call gz_write_textbuf_no_lf
      write(textbuf,'(a,a1)') '! 4.2.1 element data', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      write(textbuf,'(2i16,a1)')                                        &
     &     ngrp_ele_sf, ele_surf_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_ele_sf,           &
     &    ele_surf_grp%num_item, ele_surf_grp%istack_sf,                &
     &    ele_gp_name_sf, ele_surf_grp%item_sf)
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 4.2.2 node data', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(i16,a1)') ele_nod_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_ele_sf,           &
     &    ele_nod_grp%num_item, ele_nod_grp%istack_sf, ele_gp_name_sf,  &
     &    ele_nod_grp%item_sf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 4.2.3 edge data', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(i16,a1)') ele_edge_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_ele_sf,           &
     &    ele_edge_grp%num_item, ele_edge_grp%istack_sf,                &
     &    ele_gp_name_sf, ele_edge_grp%item_sf)
!
      end subroutine write_ele_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_ele_group_viewer_gz
!
      use m_surface_mesh_4_merge
!
!
!      write(surface_id,'(a)') '! 4.2 element group'
!      write(surface_id,'(a)') '! 4.2.1 element data'
!
      call skip_gz_comment_int(ngrp_ele_sf)
      read(textbuf,*) ngrp_ele_sf, ele_surf_grp%num_item
!
      call allocate_ele_grp_stack_4_surf
!
      call read_gz_multi_int((num_pe_sf*ngrp_ele_sf),                   &
     &    ele_surf_grp%istack_sf(1))
      call allocate_ele_grp_item_4_surf
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_ele_sf,            &
     &    ele_surf_grp%num_item, ele_surf_grp%istack_sf,                &
     &    ele_gp_name_sf, ele_surf_grp%item_sf)
!
!      write(surface_id,'(a)') '! 4.2.2 node data'
!
      call skip_gz_comment_int(ele_nod_grp%num_item)
!
      call read_gz_multi_int((num_pe_sf*ngrp_ele_sf),                   &
     &    ele_nod_grp%istack_sf(1))
      call allocate_ele_gp_nod_item_sf
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_ele_sf,            &
     &    ele_nod_grp%num_item, ele_nod_grp%istack_sf, ele_gp_name_sf,  &
     &    ele_nod_grp%item_sf)
!
!      write(surface_id,'(a)') '! 4.2.3 edge data'
!
      call skip_gz_comment_int(ele_edge_grp%num_item)
!
      call read_gz_multi_int((num_pe_sf*ngrp_ele_sf),                   &
     &    ele_edge_grp%istack_sf(1))
      call allocate_ele_grp_edge_item_sf
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_ele_sf,            &
     &    ele_edge_grp%num_item, ele_edge_grp%istack_sf,                &
     &    ele_gp_name_sf, ele_edge_grp%item_sf)
!
      end subroutine read_ele_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_group_viewer_gz
!     &         (num_pe_sf, ngrp_surf_sf, surf_gp_name_sf,               &
!     &          sf_surf_grp, sf_edge_grp, sf_nod_grp)
!
      use m_surface_mesh_4_merge
      use m_fem_mesh_labels
!
!      integer(kind = kint), intent(in) :: num_pe_sf
!      integer(kind = kint), intent(in) :: ngrp_surf_sf
!      character(len=kchara), intent(in)                                 &
!     &                      :: surf_gp_name_sf(ngrp_surf_sf)
!      type(viewer_group_data), intent(in) :: sf_surf_grp
!      type(viewer_group_data), intent(in) :: sf_edge_grp
!      type(viewer_group_data), intent(in) :: sf_nod_grp
!
!
      textbuf = hd_fem_sfgrp() // char(0)
      call gz_write_textbuf_no_lf
      write(textbuf,'(a,a1)') '! 4.3.1 surface data', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(2i16,a1)')                                        &
     &    ngrp_surf_sf, sf_surf_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_surf_sf,          &
     &    sf_surf_grp%num_item, sf_surf_grp%istack_sf, surf_gp_name_sf, &
     &    sf_surf_grp%item_sf)
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 4.3.2 node data', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(i16,a1)') sf_nod_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_surf_sf,          &
     &    sf_nod_grp%num_item, sf_nod_grp%istack_sf, surf_gp_name_sf,   &
     &    sf_nod_grp%item_sf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 4.3.3 edge data', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(i16,a1)') sf_edge_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_surf_sf,          &
     &    sf_edge_grp%num_item, sf_edge_grp%istack_sf, surf_gp_name_sf, &
     &    sf_edge_grp%item_sf)
!
      end subroutine write_surf_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_surf_group_viewer_gz
!     &         (num_pe_sf, ngrp_surf_sf, surf_gp_name_sf,               &
!     &          sf_surf_grp, sf_edge_grp, sf_nod_grp)
!
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
      call skip_gz_comment_int(ngrp_surf_sf)
      read(textbuf,*) ngrp_surf_sf, sf_surf_grp%num_item
!
      call allocate_surf_grp_stack_4_surf
!
      call read_gz_multi_int((num_pe_sf*ngrp_surf_sf),                  &
     &    sf_surf_grp%istack_sf(1))
      call alloc_merged_group_item(sf_surf_grp)
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_surf_sf,           &
     &    sf_surf_grp%num_item, sf_surf_grp%istack_sf,                  &
     &    surf_gp_name_sf, sf_surf_grp%item_sf)
!
!      write(surface_id,'(a)') '! 4.3.2 node data'
!
      call skip_gz_comment_int(sf_nod_grp%num_item)
!
      call read_gz_multi_int((num_pe_sf*ngrp_surf_sf),                  &
     &    sf_nod_grp%istack_sf(1))
      call alloc_merged_group_item(sf_nod_grp)
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_surf_sf,           &
     &    sf_nod_grp%num_item, sf_nod_grp%istack_sf, surf_gp_name_sf,   &
     &    sf_nod_grp%item_sf)
!
!      write(surface_id,'(a)') '! 4.3.3 edge data'
!
      call skip_gz_comment_int(sf_edge_grp%num_item)
!
      call read_gz_multi_int((num_pe_sf*ngrp_surf_sf),                  &
     &    sf_edge_grp%istack_sf(1))
      call alloc_merged_group_item(sf_edge_grp)
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_surf_sf,           &
     &    sf_edge_grp%num_item, sf_edge_grp%istack_sf, surf_gp_name_sf, &
     &    sf_edge_grp%item_sf)
!
      end subroutine read_surf_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_viewer_group_data_gz(nprocs, ngrp,              &
     &          ntot, istack, name, item)
!
      integer(kind = kint), intent(in) :: nprocs, ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
      integer(kind = kint), intent(in) :: item(ntot)
      character(len = kchara), intent(in) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, num
!
!
      call write_gz_multi_int_8i10((nprocs*ngrp), istack(1))
!
      if (ngrp .gt. 0) then
        do i = 1, ngrp
          ist = istack(nprocs*(i-1)) + 1
          num = istack(nprocs*i    ) - istack(nprocs*(i-1))
!
          write(textbuf,'(a,a1)') trim(name(i)), char(0)
          call gz_write_textbuf_w_lf
!
          if(num .le. 0) then
            write(textbuf,'(a1)') char(0)
            call gz_write_textbuf_w_lf
          else
            call write_gz_multi_int_8i10(num, item(ist))
          end if
        end do
      else
        write(textbuf,'(a1)') char(0)
        call gz_write_textbuf_w_lf
      end if
!
      end subroutine write_viewer_group_data_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_viewer_group_item_gz(nprocs, ngrp,                &
     &          ntot, istack, name, item)
!
      integer(kind = kint), intent(in) :: nprocs, ngrp, ntot
      integer(kind = kint), intent(in) :: istack(0:ngrp)
!
      integer(kind = kint), intent(inout) :: item(ntot)
      character(len = kchara), intent(inout) :: name(ngrp)
!
      integer(kind = kint) :: i, ist, num
!
!
      do i = 1, ngrp
        ist = istack(nprocs*(i-1)) + 1
        num = istack(nprocs*i    ) - istack(nprocs*(i-1))
        call skip_gz_comment_chara( name(i) )
        call read_gz_multi_int(num, item(ist))
      end do
!
      end subroutine read_viewer_group_item_gz
!
! -----------------------------------------------------------------------
!
      end module gz_viewer_group_data_IO
