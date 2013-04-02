!gz_viewer_group_data_IO
!      module gz_viewer_group_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!      subroutine write_domain_group_viewer_gz
!      subroutine read_domain_group_viewer_gz
!
!      subroutine write_nod_group_viewer_gz
!      subroutine read_nod_group_viewer_gz
!      subroutine write_ele_group_viewer_gz
!      subroutine read_ele_group_viewer_gz
!      subroutine write_surf_group_viewer_gz
!      subroutine read_surf_group_viewer_gz
!
      module gz_viewer_group_data_IO
!
      use m_precision
!
      use m_surface_mesh_4_merge
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
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &        '! 3. node ID for domain boundary', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(i10,a1)') nnod_domain_sf, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_gz_multi_int_8i10(num_pe_sf,                           &
     &    nod_stack_domain_sf(1) )
      call write_gz_multi_int_8i10(nnod_domain_sf,                      &
     &    nod_item_domain_sf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &        '! 3.1 surface ID for domain boundary', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(i10,a1)') nsurf_domain_sf, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_gz_multi_int_8i10(num_pe_sf,                           &
     &    isurf_stack_domain_sf(1) )
      call write_gz_multi_int_8i10(nsurf_domain_sf,                     &
     &    isurf_domain_sf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)')                                           &
     &       '! 3.2 edge ID for domain boundary', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(i10,a1)') nedge_domain_sf, char(0)
      call write_compress_txt(nbuf, textbuf)
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
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4. group information', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.1 node group', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(i10,a1)') ngrp_nod_sf, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_nod_sf,           &
     &    nnod_nod_sf, nod_stack_sf, nod_gp_name_sf, nod_item_sf)
!
      end subroutine write_nod_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_nod_group_viewer_gz
!
!
      call skip_gz_comment_int(ngrp_nod_sf)
!
      call allocate_nod_grp_stack_4_surf
!
      call read_gz_multi_int((num_pe_sf*ngrp_nod_sf),                   &
     &    nod_stack_sf(1))
      nnod_nod_sf = nod_stack_sf(ngrp_nod_sf*num_pe_sf)
!
      call allocate_nod_grp_item_4_surf
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_nod_sf,            &
     &    nnod_nod_sf, nod_stack_sf, nod_gp_name_sf, nod_item_sf)
!
      end subroutine read_nod_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_ele_group_viewer_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.2 element group', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.2.1 element data', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(2i10,a1)') ngrp_ele_sf, nele_ele_sf, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_ele_sf,           &
     &    nele_ele_sf, ele_stack_sf, ele_gp_name_sf, ele_item_sf)
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.2.2 node data', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(i10,a1)') nnod_ele_sf, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_ele_sf,           &
     &    nnod_ele_sf, ele_nod_stack_sf, ele_gp_name_sf,                &
     &    ele_nod_item_sf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.2.3 edge data', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(i10,a1)') nedge_ele_sf, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_ele_sf,           &
     &    nedge_ele_sf, ele_edge_stack_sf, ele_gp_name_sf,              &
     &    ele_edge_item_sf)
!
      end subroutine write_ele_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_ele_group_viewer_gz
!
!
!      write(surface_id,'(a)') '! 4.2 element group'
!      write(surface_id,'(a)') '! 4.2.1 element data'
!
      call skip_gz_comment_int(ngrp_ele_sf)
      read(textbuf,*) ngrp_ele_sf, nele_ele_sf
!
      call allocate_ele_grp_stack_4_surf
!
      call read_gz_multi_int((num_pe_sf*ngrp_ele_sf),                   &
     &    ele_stack_sf(1))
      call allocate_ele_grp_item_4_surf
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_ele_sf,            &
     &    nele_ele_sf, ele_stack_sf, ele_gp_name_sf,  ele_item_sf)
!
!      write(surface_id,'(a)') '! 4.2.2 node data'
!
      call skip_gz_comment_int(nnod_ele_sf)
!
      call read_gz_multi_int((num_pe_sf*ngrp_ele_sf),                   &
     &    ele_nod_stack_sf(1))
      call allocate_ele_gp_nod_item_sf
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_ele_sf,            &
     &    nnod_ele_sf, ele_nod_stack_sf, ele_gp_name_sf,                &
     &    ele_nod_item_sf)
!
!      write(surface_id,'(a)') '! 4.2.3 edge data'
!
      call skip_gz_comment_int(nedge_ele_sf)
!
      call read_gz_multi_int((num_pe_sf*ngrp_ele_sf),                   &
     &    ele_edge_stack_sf(1))
      call allocate_ele_grp_edge_item_sf
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_ele_sf,            &
     &    nedge_ele_sf, ele_edge_stack_sf, ele_gp_name_sf,              &
     &    ele_edge_item_sf)
!
      end subroutine read_ele_group_viewer_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_group_viewer_gz
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.3 surface group', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.3.1 surface data', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(2i10,a1)') ngrp_surf_sf, nsurf_surf_sf, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_surf_sf,          &
     &    nsurf_surf_sf, surf_stack_sf, surf_gp_name_sf,                &
     &    surf_item_sf)
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.3.2 node data', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(i10,a1)') nnod_surf_sf, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_surf_sf,          &
     &    nnod_surf_sf, surf_nod_stack_sf, surf_gp_name_sf,             &
     &    surf_nod_item_sf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '! 4.3.3 edge data', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
      write(textbuf,'(i10,a1)') nedge_surf_sf, char(0)
      call write_compress_txt(nbuf, textbuf)
!
      call write_viewer_group_data_gz(num_pe_sf, ngrp_surf_sf,          &
     &    nedge_surf_sf, surf_edge_stack_sf, surf_gp_name_sf,           &
     &    surf_edge_item_sf)
!
      end subroutine write_surf_group_viewer_gz
!
!------------------------------------------------------------------
!
      subroutine read_surf_group_viewer_gz
!
!
!      write(surface_id,'(a)') '! 4.3 surface group'
!      write(surface_id,'(a)') '! 4.3.1 surface data'
!
      call skip_gz_comment_int(ngrp_surf_sf)
      read(textbuf,*) ngrp_surf_sf, nsurf_surf_sf
!
      call allocate_surf_grp_stack_4_surf
!
      call read_gz_multi_int((num_pe_sf*ngrp_surf_sf),                  &
     &    surf_stack_sf(1))
      call allocate_surf_grp_item_4_surf
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_surf_sf,           &
     &    nsurf_surf_sf, surf_stack_sf, surf_gp_name_sf, surf_item_sf)
!
!      write(surface_id,'(a)') '! 4.3.2 node data'
!
      call skip_gz_comment_int(nnod_surf_sf)
!
      call read_gz_multi_int((num_pe_sf*ngrp_surf_sf),                  &
     &    surf_nod_stack_sf(1))
      call allocate_sf_gp_nod_item_sf
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_surf_sf,           &
     &    nnod_surf_sf, surf_nod_stack_sf, surf_gp_name_sf,             &
     &    surf_nod_item_sf)
!
!      write(surface_id,'(a)') '! 4.3.3 edge data'
!
      call skip_gz_comment_int(nedge_surf_sf)
!
      call read_gz_multi_int((num_pe_sf*ngrp_surf_sf),                  &
     &    surf_edge_stack_sf(1))
      call allocate_sf_grp_edge_item_sf
!
      call read_viewer_group_item_gz(num_pe_sf, ngrp_surf_sf,           &
     &    nedge_surf_sf, surf_edge_stack_sf, surf_gp_name_sf,           &
     &    surf_edge_item_sf)
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
          call write_compress_txt(nbuf, textbuf)
!
          if(num .le. 0) then
            write(textbuf,'(a1)') char(0)
            call write_compress_txt(nbuf, textbuf)
          else
            call write_gz_multi_int_8i10(num, item(ist))
          end if
        end do
      else
        write(textbuf,'(a1)') char(0)
        call write_compress_txt(nbuf, textbuf)
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
