!gz_viewer_group_data_IO
!      module gz_viewer_group_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine write_domain_group_viewer_gz                         &
!!     &          (domain_nod_grp, domain_edge_grp, domain_surf_grp)
!!        type(viewer_group_data), intent(in) :: domain_nod_grp
!!        type(viewer_group_data), intent(in) :: domain_edge_grp
!!        type(viewer_group_data), intent(in) :: domain_surf_grp
!!      subroutine read_domain_group_viewer_gz                          &
!!     &          (domain_nod_grp, domain_edge_grp, domain_surf_grp)
!!        type(viewer_group_data), intent(inout) :: domain_nod_grp
!!        type(viewer_group_data), intent(inout) :: domain_edge_grp
!!        type(viewer_group_data), intent(inout) :: domain_surf_grp
!!
!!      subroutine write_nod_group_viewer_gz
!!      subroutine read_nod_group_viewer_gz
!!      subroutine write_ele_group_viewer_gz
!!      subroutine read_ele_group_viewer_gz
!!      subroutine write_surf_group_viewer_gz
!!      subroutine read_surf_group_viewer_gz
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
      subroutine write_domain_group_viewer_gz                           &
     &          (domain_nod_grp, domain_edge_grp, domain_surf_grp)
!
      use m_surface_mesh_4_merge
!
      type(viewer_group_data), intent(in) :: domain_nod_grp
      type(viewer_group_data), intent(in) :: domain_edge_grp
      type(viewer_group_data), intent(in) :: domain_surf_grp
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)')                                           &
     &        '! 3. node ID for domain boundary', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      write(textbuf,'(i16,a1)') domain_nod_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_gz_multi_int_8i10(num_pe_sf,                           &
     &    domain_nod_grp%istack_sf(1) )
      call write_gz_multi_int_8i10(domain_nod_grp%num_item,             &
     &    domain_nod_grp%item_sf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)')                                           &
     &        '! 3.1 surface ID for domain boundary', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      write(textbuf,'(i16,a1)') domain_surf_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_gz_multi_int_8i10(num_pe_sf,                           &
     &    domain_surf_grp%istack_sf(1) )
      call write_gz_multi_int_8i10(domain_surf_grp%num_item,            &
     &    domain_surf_grp%item_sf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)')                                           &
     &       '! 3.2 edge ID for domain boundary', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      write(textbuf,'(i16,a1)') domain_edge_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_gz_multi_int_8i10(num_pe_sf,                           &
     &    domain_edge_grp%istack_sf(1) )
      call write_gz_multi_int_8i10(domain_edge_grp%num_item,            &
     &    domain_edge_grp%item_sf)
!
      end subroutine write_domain_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_domain_group_viewer_gz                            &
     &         (domain_nod_grp, domain_edge_grp, domain_surf_grp)
!
      use m_surface_mesh_4_merge
!
      type(viewer_group_data), intent(inout) :: domain_nod_grp
      type(viewer_group_data), intent(inout) :: domain_edge_grp
      type(viewer_group_data), intent(inout) :: domain_surf_grp
!
!
!      write(surface_id,'(a)') '! 3. node ID for domain boundary'
!
      call skip_gz_comment_int(domain_nod_grp%num_item)
      call read_gz_multi_int(num_pe_sf, domain_nod_grp%istack_sf(1))
!
      call alloc_merged_group_item(domain_nod_grp)
      read(surface_id,*)                                                &
     &      domain_nod_grp%item_sf(1:domain_nod_grp%num_item)
!
!      write(surface_id,'(a)') '! 3.1 surface ID for domain boundary'
!
      call skip_gz_comment_int(domain_surf_grp%num_item)
      call read_gz_multi_int(num_pe_sf, domain_surf_grp%istack_sf(1))
!
      call alloc_merged_group_item(domain_surf_grp)
      call read_gz_multi_int                                            &
     &   (domain_surf_grp%num_item, domain_surf_grp%item_sf)
!
!      write(surface_id,'(a)') '! 3.2 edge ID for domain boundary'
!
      call skip_gz_comment_int(domain_edge_grp%num_item)
      call read_gz_multi_int(num_pe_sf, domain_edge_grp%istack_sf(1))
!
      call alloc_merged_group_item(domain_edge_grp)
      call read_gz_multi_int                                            &
     &   (domain_edge_grp%num_item, domain_edge_grp%item_sf)
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
      write(textbuf,'(i16,a1)') view_nod_grps%num_grp, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, view_nod_grps%num_grp, &
    &     view_nod_grps%grp_name, view_nod_grps%node_grp)
!
      end subroutine write_nod_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_nod_group_viewer_gz
!
      use m_surface_mesh_4_merge
!
      integer(kind = kint) :: num
!
!
      call skip_gz_comment_int(view_nod_grps%num_grp)
!
      call alloc_viewer_node_grps_stack(num_pe_sf, view_nod_grps)
!
      call read_gz_multi_int((num_pe_sf*view_nod_grps%num_grp),         &
     &    view_nod_grps%node_grp%istack_sf(1))

      num = view_nod_grps%num_grp * num_pe_sf
      view_nod_grps%node_grp%num_item                                   &
     &    = view_nod_grps%node_grp%istack_sf(num)
!
      call alloc_merged_group_item(view_nod_grps%node_grp)
!
      call read_viewer_group_item_gz(num_pe_sf, view_nod_grps%num_grp,  &
     &    view_nod_grps%grp_name, view_nod_grps%node_grp)
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
      write(textbuf,'(2i16,a1)') view_ele_grps%num_grp,                 &
     &     view_ele_grps%surf_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, view_ele_grps%num_grp, &
     &    view_ele_grps%grp_name, view_ele_grps%surf_grp)
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 4.2.2 node data', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(i16,a1)')                                         &
     &    view_ele_grps%node_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, view_ele_grps%num_grp, &
     &    view_ele_grps%grp_name, view_ele_grps%node_grp)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 4.2.3 edge data', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(i16,a1)')                                         &
     &    view_ele_grps%edge_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, view_ele_grps%num_grp, &
     &    view_ele_grps%grp_name, view_ele_grps%edge_grp)
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
      call skip_gz_comment_int(view_ele_grps%num_grp)
      read(textbuf,*) view_ele_grps%num_grp,                            &
     &               view_ele_grps%surf_grp%num_item
!
      call alloc_viewer_surf_grps_stack(num_pe_sf, view_ele_grps)
!
      call read_gz_multi_int((num_pe_sf*view_ele_grps%num_grp),         &
     &    view_ele_grps%surf_grp%istack_sf(1))
      call alloc_merged_group_item(view_ele_grps%surf_grp)
!
      call read_viewer_group_item_gz(num_pe_sf, view_ele_grps%num_grp,  &
     &    view_ele_grps%grp_name, view_ele_grps%surf_grp)
!
!      write(surface_id,'(a)') '! 4.2.2 node data'
!
      call skip_gz_comment_int(view_ele_grps%node_grp%num_item)
!
      call read_gz_multi_int((num_pe_sf*view_ele_grps%num_grp),         &
     &    view_ele_grps%node_grp%istack_sf(1))
      call alloc_merged_group_item(view_ele_grps%node_grp)
!
      call read_viewer_group_item_gz(num_pe_sf, view_ele_grps%num_grp,  &
     &    view_ele_grps%grp_name, view_ele_grps%node_grp)
!
!      write(surface_id,'(a)') '! 4.2.3 edge data'
!
      call skip_gz_comment_int(view_ele_grps%edge_grp%num_item)
!
      call read_gz_multi_int((num_pe_sf*view_ele_grps%num_grp),         &
     &    view_ele_grps%edge_grp%istack_sf(1))
      call alloc_merged_group_item(view_ele_grps%edge_grp)
!
      call read_viewer_group_item_gz(num_pe_sf, view_ele_grps%num_grp,  &
     &    view_ele_grps%grp_name, view_ele_grps%edge_grp)
!
      end subroutine read_ele_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_group_viewer_gz
!
      use m_surface_mesh_4_merge
      use m_fem_mesh_labels
!
!
      textbuf = hd_fem_sfgrp() // char(0)
      call gz_write_textbuf_no_lf
      write(textbuf,'(a,a1)') '! 4.3.1 surface data', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(2i16,a1)')                                        &
     &    view_sf_grps%num_grp, view_sf_grps%surf_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, view_sf_grps%num_grp,  &
     &    view_sf_grps%grp_name, view_sf_grps%surf_grp)
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 4.3.2 node data', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(i16,a1)') view_sf_grps%node_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, view_sf_grps%num_grp,  &
     &    view_sf_grps%grp_name, view_sf_grps%node_grp)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 4.3.3 edge data', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(i16,a1)') view_sf_grps%edge_grp%num_item, char(0)
      call gz_write_textbuf_w_lf
!
      call write_viewer_group_data_gz(num_pe_sf, view_sf_grps%num_grp,  &
     &    view_sf_grps%grp_name, view_sf_grps%edge_grp)
!
      end subroutine write_surf_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_surf_group_viewer_gz
!
      use m_surface_mesh_4_merge
!
!
!      write(surface_id,'(a)') '! 4.3 surface group'
!      write(surface_id,'(a)') '! 4.3.1 surface data'
!
      call skip_gz_comment_int(view_sf_grps%num_grp)
      read(textbuf,*)                                                   &
     &        view_sf_grps%num_grp, view_sf_grps%surf_grp%num_item
!
      call alloc_viewer_surf_grps_stack(num_pe_sf, view_sf_grps)
!
      call read_gz_multi_int((num_pe_sf*view_sf_grps%num_grp),          &
     &    view_sf_grps%surf_grp%istack_sf(1))
      call alloc_merged_group_item(view_sf_grps%surf_grp)
!
      call read_viewer_group_item_gz(num_pe_sf, view_sf_grps%num_grp,   &
     &    view_sf_grps%grp_name, view_sf_grps%surf_grp)
!
!      write(surface_id,'(a)') '! 4.3.2 node data'
!
      call skip_gz_comment_int(view_sf_grps%node_grp%num_item)
!
      call read_gz_multi_int((num_pe_sf*view_sf_grps%num_grp),          &
     &    view_sf_grps%node_grp%istack_sf(1))
      call alloc_merged_group_item(view_sf_grps%node_grp)
!
      call read_viewer_group_item_gz(num_pe_sf, view_sf_grps%num_grp,   &
     &    view_sf_grps%grp_name, view_sf_grps%node_grp)
!
!      write(surface_id,'(a)') '! 4.3.3 edge data'
!
      call skip_gz_comment_int(view_sf_grps%edge_grp%num_item)
!
      call read_gz_multi_int((num_pe_sf*view_sf_grps%num_grp),          &
     &    view_sf_grps%edge_grp%istack_sf(1))
      call alloc_merged_group_item(view_sf_grps%edge_grp)
!
      call read_viewer_group_item_gz(num_pe_sf, view_sf_grps%num_grp,   &
     &    view_sf_grps%grp_name, view_sf_grps%edge_grp)
!
      end subroutine read_surf_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_viewer_group_data_gz(nprocs, ngrp, name, group)
!
      integer(kind = kint), intent(in) :: nprocs, ngrp
      character(len = kchara), intent(in) :: name(ngrp)
      type(viewer_group_data), intent(in) :: group
!
      integer(kind = kint) :: i, ist, num
!
!
      call write_gz_multi_int_8i10((nprocs*ngrp), group%istack_sf(1))
!
      if (ngrp .gt. 0) then
        do i = 1, ngrp
          ist = group%istack_sf(nprocs*(i-1)) + 1
          num = group%istack_sf(nprocs*i)                               &
     &         - group%istack_sf(nprocs*(i-1))
!
          write(textbuf,'(a,a1)') trim(name(i)), char(0)
          call gz_write_textbuf_w_lf
!
          if(num .le. 0) then
            write(textbuf,'(a1)') char(0)
            call gz_write_textbuf_w_lf
          else
            call write_gz_multi_int_8i10(num, group%item_sf(ist))
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
      subroutine read_viewer_group_item_gz(nprocs, ngrp, name, group)
!
      integer(kind = kint), intent(in) :: nprocs, ngrp
!
      character(len = kchara), intent(inout) :: name(ngrp)
      type(viewer_group_data), intent(inout) :: group
!
      integer(kind = kint) :: i, ist, num
!
!
      do i = 1, ngrp
        ist = group%istack_sf(nprocs*(i-1)) + 1
        num = group%istack_sf(nprocs*i) - group%istack_sf(nprocs*(i-1))
        call skip_gz_comment_chara( name(i) )
        call read_gz_multi_int(num, group%item_sf(ist))
      end do
!
      end subroutine read_viewer_group_item_gz
!
! -----------------------------------------------------------------------
!
      end module gz_viewer_group_data_IO
