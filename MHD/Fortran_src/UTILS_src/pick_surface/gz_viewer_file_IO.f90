!
!      module gz_viewer_file_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine output_surface_grid_gz(nnod_4_surf, nnod_4_edge)
!!      subroutine read_surface_grid_gz                                 &
!!     &         (nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
      module gz_viewer_file_IO
!
      use m_precision
!
      use m_surface_mesh_4_merge
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
      subroutine output_surface_grid_gz(nnod_4_surf, nnod_4_edge)
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
!
!
      call add_ksm_extension(surface_file_head, surface_file_name)
      call add_gzip_extension(surface_file_name, gzip_name)
      write(*,*) 'write gzipped viewer mesh file: ', trim(gzip_name)
      call open_wt_gzfile_f(gzip_name)
!
!      write(*,*) 'write_domain_data_viewer_gz'
      call write_domain_data_viewer_gz
!
!      write(*,*) 'write_node_data_viewer_gz'
      call write_node_data_viewer_gz
!
!      write(*,*) 'write_surf_connect_viewer_gz'
      call write_surf_connect_viewer_gz(nnod_4_surf)
!
!      write(*,*) 'write_edge_connect_viewer_gz'
      call write_edge_connect_viewer_gz(nnod_4_edge)
!
!      write(*,*) 'write_domain_group_viewer_gz'
      call write_domain_group_viewer_gz(num_pe_sf, domain_grps)
!
!      write(*,*) 'write_nod_group_viewer_gz'
      call write_nod_group_viewer_gz(num_pe_sf, view_nod_grps)
!
!      write(*,*) 'write_ele_group_viewer_gz'
      call write_ele_group_viewer_gz(num_pe_sf, view_ele_grps)
!
!      write(*,*) 'write_surf_group_viewer_gz'
      call write_surf_group_viewer_gz(num_pe_sf, view_sf_grps)
!
      call close_gzfile_f
!
      end subroutine output_surface_grid_gz
!
!------------------------------------------------------------------
!
      subroutine read_surface_grid_gz                                   &
     &         (nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
!      use const_edges_normals_viewer
!      use cal_range_of_domain
!      use cal_normal_of_node_viewer
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(inout) :: nnod_4_surf
      integer(kind = kint), intent(inout) :: nnod_4_edge
!
!
      call add_ksm_extension(surface_file_head, surface_file_name)
      call add_gzip_extension(surface_file_name, gzip_name)
      write(*,*) 'read gzipped viewer mesh file: ', trim(gzip_name)
      call open_rd_gzfile_f(gzip_name)
!
!      write(*,*) 'read_domain_data_viewer_gz'
      call read_domain_data_viewer_gz
!
!      write(*,*) 'read_node_data_viewer_gz'
      call read_node_data_viewer_gz
!
!      write(*,*) 'read_surf_connect_viewer_gz'
      call read_surf_connect_viewer_gz                                  &
     &   (nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
!      write(*,*) 'read_edge_connect_viewer_gz'
      call read_edge_connect_viewer_gz(nnod_4_edge)
!
      call allocate_domain_stack_4_surf
!
!      write(*,*) 'read_domain_group_viewer_gz'
      call read_domain_group_viewer_gz(num_pe_sf, domain_grps)
!
!      write(*,*) 'read_nod_group_viewer_gz'
      call read_nod_group_viewer_gz(num_pe_sf, view_nod_grps)
!
!      write(*,*) 'read_ele_group_viewer_gz'
      call read_ele_group_viewer_gz(num_pe_sf, view_ele_grps)
!
!      write(*,*) 'read_surf_group_viewer_gz'
      call read_surf_group_viewer_gz(num_pe_sf, view_sf_grps)
!
      call close_gzfile_f
!
      end subroutine read_surface_grid_gz
!
!------------------------------------------------------------------
!
      end module gz_viewer_file_IO
