!
!      module viewer_file_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine output_surface_grid                                  &
!!     &         (file_name, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!!      subroutine read_surface_grid(file_name,                         &
!!     &          nnod_4_ele, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      module viewer_file_IO
!
      use m_precision
!
      use t_merged_viewer_mesh
      use set_parallel_file_name
      use viewer_mesh_data_IO
      use viewer_group_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine output_surface_grid                                    &
     &         (file_name, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: nnod_4_edge
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
!
      write(*,*) 'surface mesh file name: ', trim(file_name)
      open (surface_id, file = file_name)
!
!      write(*,*) 'write_domain_data_viewer'
      call write_domain_data_viewer(mgd_view_mesh)
!
!      write(*,*) 'write_node_data_viewer'
      call write_node_data_viewer(mgd_view_mesh%view_mesh)
!
!      write(*,*) 'write_surf_connect_viewer'
      call write_surf_connect_viewer                                    &
     &   (nnod_4_surf, mgd_view_mesh%view_mesh)
!
!      write(*,*) 'write_edge_connect_viewer'
      call write_edge_connect_viewer                                    &
     &   (nnod_4_edge, mgd_view_mesh%view_mesh)
!
!      write(*,*) 'write_domain_group_viewer'
      call write_domain_group_viewer                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps)
!
!      write(*,*) 'write_nod_group_viewer'
      call write_nod_group_viewer                                       &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_nod_grps)
!
!      write(*,*) 'write_ele_group_viewer'
      call write_ele_group_viewer                                       &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_ele_grps)
!
!      write(*,*) 'write_surf_group_viewer'
      call write_surf_group_viewer                                      &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_sf_grps)
!
      close(surface_id)
!
      end subroutine output_surface_grid
!
!------------------------------------------------------------------
!
      subroutine read_surface_grid(file_name,                           &
     &          nnod_4_ele, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(inout) :: nnod_4_surf
      integer(kind = kint), intent(inout) :: nnod_4_edge
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
!
      write(*,*) 'surface mesh file name: ', trim(file_name)
      open (surface_id, file = file_name)
!
!      write(*,*) 'read_domain_data_viewer'
      call read_domain_data_viewer(mgd_view_mesh)
!
!      write(*,*) 'read_node_data_viewer'
      call read_node_data_viewer(mgd_view_mesh%view_mesh)
!
!      write(*,*) 'read_surf_connect_viewer'
      call read_surf_connect_viewer                                     &
     &   (nnod_4_ele, nnod_4_surf, nnod_4_edge,                         &
     &    mgd_view_mesh%view_mesh)
!
!      write(*,*) 'read_edge_connect_viewer'
      call read_edge_connect_viewer(nnod_4_edge,                        &
     &    mgd_view_mesh%view_mesh)
!
      call alloc_domain_stack_4_surf                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps)
!
!      write(*,*) 'read_domain_group_viewer'
      call read_domain_group_viewer                                     &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps)
!
!      write(*,*) 'read_nod_group_viewer'
      call read_nod_group_viewer                                        &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_nod_grps)
!
!      write(*,*) 'read_ele_group_viewer'
      call read_ele_group_viewer                                        &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_ele_grps)
!
!      write(*,*) 'read_surf_group_viewer'
      call read_surf_group_viewer                                       &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_sf_grps)
!
      close(surface_id)
!
!
!      write(*,*) 's_cal_range_of_domain'
!      call s_const_edges_normals_viewer
!      call s_cal_range_of_domain
!      call s_cal_normal_of_node_viewer
!
      end subroutine read_surface_grid
!
!------------------------------------------------------------------
!
      end module viewer_file_IO
