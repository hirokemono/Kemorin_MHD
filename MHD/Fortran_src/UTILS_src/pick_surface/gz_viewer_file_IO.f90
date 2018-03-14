!
!      module gz_viewer_file_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine output_surface_grid_gz                               &
!!     &         (file_name, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!!      subroutine read_surface_grid_gz(file_name,                      &
!!     &          nnod_4_ele, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      module gz_viewer_file_IO
!
      use m_precision
!
      use t_merged_viewer_mesh
      use set_parallel_file_name
      use gz_viewer_mesh_data_IO
      use gz_viewer_group_data_IO
      use skip_gz_comment
!
      implicit none
!
      character(len=kchara), private :: gzip_name
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine output_surface_grid_gz                                 &
     &         (file_name, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
!
      call add_gzip_extension(file_name, gzip_name)
      write(*,*) 'write gzipped viewer mesh file: ', trim(gzip_name)
      call open_wt_gzfile_f(gzip_name)
!
!      write(*,*) 'write_domain_data_viewer_gz'
      call write_domain_data_viewer_gz(mgd_view_mesh)
!
!      write(*,*) 'write_node_data_viewer_gz'
      call write_node_data_viewer_gz(mgd_view_mesh)
!
!      write(*,*) 'write_surf_connect_viewer_gz'
      call write_surf_connect_viewer_gz(nnod_4_surf, mgd_view_mesh)
!
!      write(*,*) 'write_edge_connect_viewer_gz'
      call write_edge_connect_viewer_gz                                 &
     &   (nnod_4_edge, mgd_view_mesh)
!
!      write(*,*) 'write_domain_group_viewer_gz'
      call write_domain_group_viewer_gz                                 &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps)
!
!      write(*,*) 'write_nod_group_viewer_gz'
      call write_nod_group_viewer_gz                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_nod_grps)
!
!      write(*,*) 'write_ele_group_viewer_gz'
      call write_ele_group_viewer_gz                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_ele_grps)
!
!      write(*,*) 'write_surf_group_viewer_gz'
      call write_surf_group_viewer_gz                                   &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_sf_grps)
!
      call close_gzfile_f
!
      end subroutine output_surface_grid_gz
!
!------------------------------------------------------------------
!
      subroutine read_surface_grid_gz(file_name,                        &
     &          nnod_4_ele, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(inout) :: nnod_4_surf
      integer(kind = kint), intent(inout) :: nnod_4_edge
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
!
      call add_gzip_extension(file_name, gzip_name)
      write(*,*) 'read gzipped viewer mesh file: ', trim(gzip_name)
      call open_rd_gzfile_f(gzip_name)
!
!      write(*,*) 'read_domain_data_viewer_gz'
      call read_domain_data_viewer_gz(mgd_view_mesh)
!
!      write(*,*) 'read_node_data_viewer_gz'
      call read_node_data_viewer_gz(mgd_view_mesh)
!
!      write(*,*) 'read_surf_connect_viewer_gz'
      call read_surf_connect_viewer_gz                                  &
     &   (nnod_4_ele, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!
!      write(*,*) 'read_edge_connect_viewer_gz'
      call read_edge_connect_viewer_gz                                  &
     &   (nnod_4_edge, mgd_view_mesh)
!
      call alloc_domain_stack_4_surf                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps)
!
!      write(*,*) 'read_domain_group_viewer_gz'
      call read_domain_group_viewer_gz                                  &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps)
!
!      write(*,*) 'read_nod_group_viewer_gz'
      call read_nod_group_viewer_gz                                     &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_nod_grps)
!
!      write(*,*) 'read_ele_group_viewer_gz'
      call read_ele_group_viewer_gz                                     &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_ele_grps)
!
!      write(*,*) 'read_surf_group_viewer_gz'
      call read_surf_group_viewer_gz                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_sf_grps)
!
      call close_gzfile_f
!
      end subroutine read_surface_grid_gz
!
!------------------------------------------------------------------
!
      end module gz_viewer_file_IO
