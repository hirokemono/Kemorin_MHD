!gz_viewer_group_data_IO
!      module gz_viewer_group_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine write_domain_group_viewer_gz(num_pe, domain_grps)
!!        type(viewer_surface_groups), intent(in) :: domain_grps
!!      subroutine read_domain_group_viewer_gz(num_pe,  domain_grps)
!!        type(viewer_surface_groups), intent(inout) :: domain_grps
!!
!!      subroutine write_nod_group_viewer_gz(num_pe, view_nod_grps)
!!        type(viewer_node_groups), intent(in) :: view_nod_grps
!!      subroutine read_nod_group_viewer_gz(num_pe, view_nod_grps)
!!        type(viewer_node_groups), intent(inout) :: view_nod_grps
!!      subroutine write_ele_group_viewer_gz(num_pe, view_ele_grps)
!!        type(viewer_surface_groups), intent(in) :: view_ele_grps
!!      subroutine read_ele_group_viewer_gz(num_pe, view_ele_grps)
!!        type(viewer_surface_groups), intent(inout) :: view_ele_grps
!!      subroutine write_surf_group_viewer_gz(num_pe, view_sf_grps)
!!        type(viewer_surface_groups), intent(in) :: view_sf_grps
!!      subroutine read_surf_group_viewer_gz(num_pe, view_sf_grps)
!!        type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
      module gz_viewer_group_data_IO
!
      use m_precision
!
      use t_viewer_group
      use skip_gz_comment
      use m_viewer_mesh_labels
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
      subroutine write_domain_group_viewer_gz(num_pe, domain_grps)
!
      integer, intent(in) :: num_pe
      type(viewer_surface_groups), intent(in) :: domain_grps
!
      integer :: ip
      integer(kind = kint) :: ist, num
      integer(kind = kint) :: ngrp_pe
!
!
      ngrp_pe = int(num_pe, KIND(ngrp_pe))
      zbuf1%fixbuf(1) = hd_domain_nod_grp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      write(zbuf1%fixbuf(1),'(i16,2a1)') domain_grps%node_grp%num_item, &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_gz_multi_int_8i16                                      &
     &   (ngrp_pe, domain_grps%node_grp%istack_sf(1), zbuf1)
      do ip = 1, num_pe
        ist = domain_grps%node_grp%istack_sf(ip-1)
        num = domain_grps%node_grp%istack_sf(ip) - ist
        if(num .gt. 0) then
          call write_gz_multi_int_8i16                                  &
     &       (num, domain_grps%node_grp%item_sf(ist+1), zbuf1)
        end if
      end do
!
      zbuf1%fixbuf(1) = hd_domain_surf_grp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      write(zbuf1%fixbuf(1),'(i16,2a1)') domain_grps%surf_grp%num_item, &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_gz_multi_int_8i16                                      &
     &   (ngrp_pe, domain_grps%surf_grp%istack_sf(1), zbuf1)
      do ip = 1, num_pe
        ist = domain_grps%surf_grp%istack_sf(ip-1)
        num = domain_grps%surf_grp%istack_sf(ip) - ist
        if(num .gt. 0) then
          call write_gz_multi_int_8i16                                  &
     &       (num, domain_grps%surf_grp%item_sf(ist+1), zbuf1)
        end if
      end do
!
      zbuf1%fixbuf(1) = hd_domain_edge_grp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      write(zbuf1%fixbuf(1),'(i16,2a1)') domain_grps%edge_grp%num_item, &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_gz_multi_int_8i16                                      &
     &   (ngrp_pe, domain_grps%edge_grp%istack_sf(1), zbuf1)
      do ip = 1, num_pe
        ist = domain_grps%edge_grp%istack_sf(ip-1)
        num = domain_grps%edge_grp%istack_sf(ip) - ist
        if(num .gt. 0) then
          call write_gz_multi_int_8i16                                  &
     &       (num, domain_grps%edge_grp%item_sf(ist+1), zbuf1)
      end do
!
      end subroutine write_domain_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_domain_group_viewer_gz(num_pe, domain_grps)
!
      integer, intent(in) :: num_pe
      type(viewer_surface_groups), intent(inout) :: domain_grps
!
      integer(kind = kint) :: ngrp_pe
!
!
!      write(surface_id,'(a)') '! 3. node ID for domain boundary'
!
      ngrp_pe = int(num_pe, KIND(ngrp_pe))
      call skip_gz_comment_int(domain_grps%node_grp%num_item, zbuf1)
      call read_gz_multi_int                                            &
     &   (ngrp_pe, domain_grps%node_grp%istack_sf(1), zbuf1)
!
      call alloc_merged_group_item(domain_grps%node_grp)
      call read_gz_multi_int(domain_grps%node_grp%num_item,             &
     &    domain_grps%node_grp%item_sf, zbuf1)
!
!      write(surface_id,'(a)') '! 3.1 surface ID for domain boundary'
!
      call skip_gz_comment_int(domain_grps%surf_grp%num_item, zbuf1)
      call read_gz_multi_int                                            &
     &   (ngrp_pe, domain_grps%surf_grp%istack_sf(1), zbuf1)
!
      call alloc_merged_group_item(domain_grps%surf_grp)
      call read_gz_multi_int(domain_grps%surf_grp%num_item,             &
     &    domain_grps%surf_grp%item_sf, zbuf1)
!
!      write(surface_id,'(a)') '! 3.2 edge ID for domain boundary'
!
      call skip_gz_comment_int(domain_grps%edge_grp%num_item, zbuf1)
      call read_gz_multi_int                                            &
     &   (ngrp_pe, domain_grps%edge_grp%istack_sf(1), zbuf1)
!
      call alloc_merged_group_item(domain_grps%edge_grp)
      call read_gz_multi_int(domain_grps%edge_grp%num_item,             &
     &    domain_grps%edge_grp%item_sf, zbuf1)
!
      end subroutine read_domain_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_nod_group_viewer_gz(num_pe, view_nod_grps)
!
      use m_fem_mesh_labels
!
      integer, intent(in) :: num_pe
      type(viewer_node_groups), intent(in) :: view_nod_grps
!
!
      zbuf1%fixbuf(1) = hd_fem_nodgrp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      write(zbuf1%fixbuf(1),'(i16,2a1)') view_nod_grps%num_grp,         &
     &     char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_viewer_group_data_gz(num_pe, view_nod_grps%num_grp,    &
    &     view_nod_grps%grp_name, view_nod_grps%node_grp)
!
      end subroutine write_nod_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_nod_group_viewer_gz(num_pe, view_nod_grps)
!
      integer, intent(in) :: num_pe
      type(viewer_node_groups), intent(inout) :: view_nod_grps
!
      integer(kind = kint) :: num
!
!
      call skip_gz_comment_int(view_nod_grps%num_grp, zbuf1)
!
      call alloc_merged_node_grps_stack(num_pe, view_nod_grps)
!
      call read_gz_multi_int((num_pe*view_nod_grps%num_grp),            &
     &    view_nod_grps%node_grp%istack_sf(1), zbuf1)

      num = view_nod_grps%num_grp * num_pe
      view_nod_grps%node_grp%num_item                                   &
     &    = view_nod_grps%node_grp%istack_sf(num)
!
      call read_viewer_group_item_gz(num_pe, view_nod_grps%num_grp,     &
     &    view_nod_grps%grp_name, view_nod_grps%node_grp)
!
      end subroutine read_nod_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_ele_group_viewer_gz(num_pe, view_ele_grps)
!
      use m_fem_mesh_labels
!
      integer, intent(in) :: num_pe
      type(viewer_surface_groups), intent(in) :: view_ele_grps
!
!
      zbuf1%fixbuf(1) = hd_fem_elegrp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      zbuf1%fixbuf(1) = hd_ele_surf_grp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      write(zbuf1%fixbuf(1),'(i16,2a1)') view_ele_grps%num_grp,         &
     &                          char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_viewer_group_data_gz(num_pe, view_ele_grps%num_grp,    &
     &    view_ele_grps%grp_name, view_ele_grps%surf_grp)
!
!
      zbuf1%fixbuf(1) = hd_ele_nod_grp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(zbuf1%fixbuf(1),'(i16,2a1)') view_ele_grps%num_grp,         &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_viewer_group_data_gz(num_pe, view_ele_grps%num_grp,    &
     &    view_ele_grps%grp_name, view_ele_grps%node_grp)
!
      zbuf1%fixbuf(1) = hd_ele_edge_grp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(zbuf1%fixbuf(1),'(i16,2a1)') view_ele_grps%num_grp,         &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_viewer_group_data_gz(num_pe, view_ele_grps%num_grp,    &
     &    view_ele_grps%grp_name, view_ele_grps%edge_grp)
!
      end subroutine write_ele_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_ele_group_viewer_gz(num_pe, view_ele_grps)
!
      integer, intent(in) :: num_pe
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
!
      integer(kind = kint) :: num, itmp
!
!      write(surface_id,'(a)') '! 4.2 element group'
!      write(surface_id,'(a)') '! 4.2.1 element data'
!
      call skip_gz_comment_int(view_ele_grps%num_grp, zbuf1)
      read(zbuf1%fixbuf(1),*) view_ele_grps%num_grp
      num = num_pe * view_ele_grps%num_grp
!
      view_ele_grps%surf_grp%num_item                                   &
     &    = view_ele_grps%surf_grp%istack_sf(num)
!
      call alloc_merged_surf_grps_stack(num_pe, view_ele_grps)
      call read_gz_multi_int((num_pe*view_ele_grps%num_grp),            &
     &    view_ele_grps%surf_grp%istack_sf(1), zbuf1)
!
      call read_viewer_group_item_gz(num_pe, view_ele_grps%num_grp,     &
     &    view_ele_grps%grp_name, view_ele_grps%surf_grp)
!
!      write(surface_id,'(a)') '! 4.2.2 node data'
!
      call skip_gz_comment_int(itmp, zbuf1)
      call read_gz_multi_int((num_pe*view_ele_grps%num_grp),            &
     &    view_ele_grps%node_grp%istack_sf(1), zbuf1)
!
      view_ele_grps%node_grp%num_item                                   &
     &    = view_ele_grps%node_grp%istack_sf(num)
!
      call read_viewer_group_item_gz(num_pe, view_ele_grps%num_grp,     &
     &    view_ele_grps%grp_name, view_ele_grps%node_grp)
!
!      write(surface_id,'(a)') '! 4.2.3 edge data'
!
      call skip_gz_comment_int(itmp, zbuf1)
      call read_gz_multi_int((num_pe*view_ele_grps%num_grp),            &
     &    view_ele_grps%edge_grp%istack_sf(1), zbuf1)
!
      view_ele_grps%edge_grp%num_item                                   &
     &    = view_ele_grps%edge_grp%istack_sf(num)
!
      call read_viewer_group_item_gz(num_pe, view_ele_grps%num_grp,     &
     &    view_ele_grps%grp_name, view_ele_grps%edge_grp)
!
      end subroutine read_ele_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_group_viewer_gz(num_pe, view_sf_grps)
!
      use m_fem_mesh_labels
!
      integer, intent(in) :: num_pe
      type(viewer_surface_groups), intent(in) :: view_sf_grps
!
      zbuf1%fixbuf(1) = hd_fem_sfgrp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      zbuf1%fixbuf(1) = hd_surf_surf_grp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(zbuf1%fixbuf(1),'(i16,2a1)') view_sf_grps%num_grp,          &
     &                                  char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_viewer_group_data_gz(num_pe, view_sf_grps%num_grp,     &
     &    view_sf_grps%grp_name, view_sf_grps%surf_grp)
!
!
      zbuf1%fixbuf(1) = hd_surf_nod_grp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(zbuf1%fixbuf(1),'(i16,2a1)') view_sf_grps%num_grp,          &
     &                          char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_viewer_group_data_gz(num_pe, view_sf_grps%num_grp,     &
     &    view_sf_grps%grp_name, view_sf_grps%node_grp)
!
      zbuf1%fixbuf(1) = hd_surf_edge_grp() // char(0)
      call gz_write_textbuf_no_lf(zbuf1)
      write(zbuf1%fixbuf(1),'(i16,2a1)') view_sf_grps%num_grp,          &
     &                          char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf1)
!
      call write_viewer_group_data_gz(num_pe, view_sf_grps%num_grp,     &
     &    view_sf_grps%grp_name, view_sf_grps%edge_grp)
!
      end subroutine write_surf_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_surf_group_viewer_gz(num_pe, view_sf_grps)
!
      integer, intent(in) :: num_pe
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
      integer(kind = kint) :: num, itmp
!
!      write(surface_id,'(a)') '! 4.3 surface group'
!      write(surface_id,'(a)') '! 4.3.1 surface data'
!
      call skip_gz_comment_int(view_sf_grps%num_grp, zbuf1)
      read(zbuf1%fixbuf(1),*) view_sf_grps%num_grp
      num = num_pe * view_sf_grps%num_grp
!
      call alloc_merged_surf_grps_stack(num_pe, view_sf_grps)
      call read_gz_multi_int((num_pe*view_sf_grps%num_grp),             &
     &    view_sf_grps%surf_grp%istack_sf(1), zbuf1)
!
      view_sf_grps%surf_grp%num_item                                    &
     &    = view_sf_grps%surf_grp%istack_sf(num)
!
      call read_viewer_group_item_gz(num_pe, view_sf_grps%num_grp,      &
     &    view_sf_grps%grp_name, view_sf_grps%surf_grp)
!
!      write(surface_id,'(a)') '! 4.3.2 node data'
!
      call skip_gz_comment_int(itmp, zbuf1)
      call read_gz_multi_int((num_pe*view_sf_grps%num_grp),             &
     &    view_sf_grps%node_grp%istack_sf(1), zbuf1)
!
      view_sf_grps%node_grp%num_item                                    &
     &    = view_sf_grps%node_grp%istack_sf(num)
!
      call read_viewer_group_item_gz(num_pe, view_sf_grps%num_grp,      &
     &    view_sf_grps%grp_name, view_sf_grps%node_grp)
!
!      write(surface_id,'(a)') '! 4.3.3 edge data'
!
      call skip_gz_comment_int(itmp, zbuf1)
      call read_gz_multi_int((num_pe*view_sf_grps%num_grp),             &
     &    view_sf_grps%edge_grp%istack_sf(1), zbuf1)
!
      view_sf_grps%edge_grp%num_item                                    &
     &    = view_sf_grps%edge_grp%istack_sf(num)
!
      call read_viewer_group_item_gz(num_pe, view_sf_grps%num_grp,      &
     &    view_sf_grps%grp_name, view_sf_grps%edge_grp)
!
      end subroutine read_surf_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_viewer_group_data_gz                             &
     &         (num_pe, ngrp, name, group)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: ngrp
      character(len = kchara), intent(in) :: name(ngrp)
      type(viewer_group_data), intent(in) :: group
!
      integer :: ip
      integer(kind = kint) :: i, ist, num
      integer(kind = kint) :: ngrp_pe
!
!
      ngrp_pe = int(num_pe, KIND(ngrp_pe))
      do i = 1, ngrp
        ist = (i-1) * num_pe
        call write_gz_multi_int_8i16                                    &
     &     (ngrp_pe, group%istack_sf(ist+1), zbuf1)
      end do
!
      if (ngrp .gt. 0) then
        do i = 1, ngrp
          num = group%istack_sf(num_pe*i)                               &
     &         - group%istack_sf(num_pe*(i-1))
!
          write(zbuf1%fixbuf(1),'(a,2a1)')                              &
     &                         trim(name(i)), char(10), char(0)
          call gz_write_textbuf_no_lf(zbuf1)
!
          if(num .le. 0) then
            write(zbuf1%fixbuf(1),'(2a1)') char(10), char(0)
            call gz_write_textbuf_no_lf(zbuf1)
          else
            do ip = 1, num_pe
              ist = group%istack_sf(num_pe*(i-1)+ip-1)
              num = group%istack_sf(num_pe*(i-1)+ip) - ist
              if(num .gt. 0) then
                call write_gz_multi_int_8i16                            &
     &             (num, group%item_sf(ist+1), zbuf1)
              end if
            end do
          end if
        end do
      else
        write(zbuf1%fixbuf(1),'(2a1)') char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf1)
      end if
!
      end subroutine write_viewer_group_data_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_viewer_group_item_gz(num_pe, ngrp, name, group)
!
      integer, intent(in) :: num_pe
      integer(kind = kint), intent(in) :: ngrp
!
      character(len = kchara), intent(inout) :: name(ngrp)
      type(viewer_group_data), intent(inout) :: group
!
      integer(kind = kint) :: i, ist, num
!
!
      call alloc_merged_group_item(group)
!
      do i = 1, ngrp
        ist = group%istack_sf(num_pe*(i-1)) + 1
        num = group%istack_sf(num_pe*i)                                 &
     &       - group%istack_sf(num_pe*(i-1))
        call skip_gz_comment_chara(name(i), zbuf1)
        call read_gz_multi_int(num, group%item_sf(ist), zbuf1)
      end do
!
      end subroutine read_viewer_group_item_gz
!
! -----------------------------------------------------------------------
!
      end module gz_viewer_group_data_IO
