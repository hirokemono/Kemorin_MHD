!
!      module gz_viewer_file_IO
!
!      Written by Kemorin on Jan., 2007
!
!!     subroutine output_surface_grid_gz(file_name, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!!      subroutine read_surface_grid_gz                                 &
!!     &          (file_name, nnod_4_ele, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      module gz_viewer_file_IO
!
      use m_precision
!
      use t_buffer_4_gzip
      use t_merged_viewer_mesh
      use set_parallel_file_name
      use gz_viewer_mesh_data_IO
      use gz_viewer_group_data_IO
!
      implicit none
!
      character(len=kchara), private :: gzip_name
      type(buffer_4_gzip), private :: zbuf_v
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine output_surface_grid_gz(file_name, mgd_view_mesh)
!
      use skip_gz_comment
!
      character(len = kchara), intent(in) :: file_name
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
!
      gzip_name = add_gzip_extension(file_name)
      write(*,*) 'write gzipped viewer mesh file: ', trim(gzip_name)
      call open_wt_gzfile_a(gzip_name, zbuf_v)
!
!      write(*,*) 'write_domain_data_viewer_gz'
      call write_domain_data_viewer_gz(mgd_view_mesh, zbuf_v)
!
!      write(*,*) 'write_node_data_viewer_gz'
      call write_node_data_viewer_gz(mgd_view_mesh%view_mesh, zbuf_v)
!
!      write(*,*) 'write_surf_connect_viewer_gz'
      call write_surf_connect_viewer_gz                                 &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%isurf_sf_stack,        &
     &    mgd_view_mesh%view_mesh, zbuf_v)
!
!      write(*,*) 'write_edge_connect_viewer_gz'
      call write_edge_connect_viewer_gz                                 &
     &   (mgd_view_mesh%view_mesh, zbuf_v)
!
!      write(*,*) 'write_domain_group_viewer_gz'
      call write_domain_group_viewer_gz                                 &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps, zbuf_v)
!
!      write(*,*) 'write_nod_group_viewer_gz'
      call write_nod_group_viewer_gz                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_nod_grps, zbuf_v)
!
!      write(*,*) 'write_ele_group_viewer_gz'
      call write_ele_group_viewer_gz                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_ele_grps, zbuf_v)
!
!      write(*,*) 'write_surf_group_viewer_gz'
      call write_surf_group_viewer_gz                                   &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_sf_grps, zbuf_v)
!
      call close_gzfile_a(zbuf_v)
!
      end subroutine output_surface_grid_gz
!
!------------------------------------------------------------------
!
      subroutine read_surface_grid_gz                                   &
     &          (file_name, nnod_4_ele, mgd_view_mesh)
!
      use skip_gz_comment
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nnod_4_ele
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      integer(kind = kint) :: nnod_4_edge
!
!
      gzip_name = add_gzip_extension(file_name)
      write(*,*) 'read gzipped viewer mesh file: ', trim(gzip_name)
      call open_rd_gzfile_a(gzip_name, zbuf_v)
!
!      write(*,*) 'read_domain_data_viewer_gz'
      call read_domain_data_viewer_gz(mgd_view_mesh, zbuf_v)
!
!      write(*,*) 'read_node_data_viewer_gz'
      call read_node_data_viewer_gz(mgd_view_mesh%view_mesh, zbuf_v)
!
!      write(*,*) 'read_surf_connect_viewer_gz'
      call read_surf_connect_viewer_gz                                  &
     &   (nnod_4_ele, nnod_4_edge, mgd_view_mesh%view_mesh, zbuf_v)
!
!      write(*,*) 'read_edge_connect_viewer_gz'
      call read_edge_connect_viewer_gz                                  &
     &   (nnod_4_edge, mgd_view_mesh%view_mesh, zbuf_v)
!
      call alloc_domain_stack_4_surf                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps)
!
!      write(*,*) 'read_domain_group_viewer_gz'
      call read_domain_group_viewer_gz                                  &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps, zbuf_v)
!
!      write(*,*) 'read_nod_group_viewer_gz'
      call read_nod_group_viewer_gz                                     &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_nod_grps, zbuf_v)
!
!      write(*,*) 'read_ele_group_viewer_gz'
      call read_ele_group_viewer_gz                                     &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_ele_grps, zbuf_v)
!
!      write(*,*) 'read_surf_group_viewer_gz'
      call read_surf_group_viewer_gz                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_sf_grps, zbuf_v)
!
      call close_gzfile_a(zbuf_v)
!
      end subroutine read_surface_grid_gz
!
!------------------------------------------------------------------
!
      end module gz_viewer_file_IO
