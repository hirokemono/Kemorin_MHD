!
!      module viewer_file_IO
!
!      Written by Kemorin on Jan., 2007
!
!      subroutine output_surface_grid
!      subroutine read_surface_grid
!
      module viewer_file_IO
!
      use m_precision
!
      use m_surface_mesh_4_merge
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
      subroutine output_surface_grid
!
!
      call add_ksm_extension(surface_file_head, surface_file_name)
      write(*,*) 'surface mesh file name: ', trim(surface_file_name)
      open (surface_id, file = surface_file_name)
!
!      write(*,*) 'write_domain_data_viewer'
      call write_domain_data_viewer
!
!      write(*,*) 'write_node_data_viewer'
      call write_node_data_viewer
!
!      write(*,*) 'write_surf_connect_viewer'
      call write_surf_connect_viewer
!
!      write(*,*) 'write_edge_connect_viewer'
      call write_edge_connect_viewer
!
!      write(*,*) 'write_domain_group_viewer'
      call write_domain_group_viewer
!
!      write(*,*) 'write_nod_group_viewer'
      call write_nod_group_viewer
!
!      write(*,*) 'write_ele_group_viewer'
      call write_ele_group_viewer
!
!      write(*,*) 'write_surf_group_viewer'
      call write_surf_group_viewer
!
      close(surface_id)
!
      end subroutine output_surface_grid
!
!------------------------------------------------------------------
!
      subroutine read_surface_grid
!
!      use const_edges_normals_viewer
!      use cal_range_of_domain
!      use cal_normal_of_node_viewer
!
      call add_ksm_extension(surface_file_head, surface_file_name)
      write(*,*) 'surface mesh file name: ', trim(surface_file_name)
      open (surface_id, file = surface_file_name)
!
!      write(*,*) 'read_domain_data_viewer'
      call read_domain_data_viewer
!
!      write(*,*) 'read_node_data_viewer'
      call read_node_data_viewer
!
!      write(*,*) 'read_surf_connect_viewer'
      call read_surf_connect_viewer
!
!      write(*,*) 'read_edge_connect_viewer'
      call read_edge_connect_viewer
!
      call allocate_domain_stack_4_surf
!
!      write(*,*) 'read_domain_group_viewer'
      call read_domain_group_viewer
!
!      write(*,*) 'read_nod_group_viewer'
      call read_nod_group_viewer
!
!      write(*,*) 'read_ele_group_viewer'
      call read_ele_group_viewer
!
!      write(*,*) 'read_surf_group_viewer'
      call read_surf_group_viewer
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
