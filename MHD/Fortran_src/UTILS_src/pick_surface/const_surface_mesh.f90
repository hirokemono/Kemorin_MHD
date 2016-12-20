!
!      module const_surface_mesh
!
!      Written by Kemorin
!      Modified by Kemorin on Dec., 2006
!
!!      subroutine choose_surface_mesh(mesh_file, ele, surf, edge)
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(element_data), intent(inout) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data), intent(inout) :: edge
!
      module const_surface_mesh
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_file_format_switch
      use m_geometry_data_4_merge
      use m_surface_mesh_4_merge
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_file_IO_parameter
!
      implicit none
!
      private :: const_surf_mesh_4_viewer, set_source_mesh_parameter
      private :: find_mesh_format_4_viewer
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine choose_surface_mesh(mesh_file, ele, surf, edge)
!
      type(field_IO_params), intent(inout) :: mesh_file
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!
      surface_file_head = mesh_file%file_prefix
      call find_mesh_format_4_viewer(mesh_file)
      call count_subdomains_4_viewer(mesh_file)
      call const_surf_mesh_4_viewer(mesh_file, ele, surf, edge)
!
      end subroutine choose_surface_mesh
!
!------------------------------------------------------------------
!
      subroutine find_mesh_format_4_viewer(mesh_file)
!
      use t_file_IO_parameter
      use m_file_format_switch
      use mesh_IO_select
!
      type(field_IO_params), intent(inout) ::  mesh_file
!
!  Detect file format
      mesh_file%iflag_format = id_gzip_txt_file_fmt
      if(check_exist_mesh(mesh_file, izero) .gt. 0) return
!
      mesh_file%iflag_format = id_ascii_file_fmt
      if(check_exist_mesh(mesh_file, izero) .gt. 0) return
!
      mesh_file%iflag_format = id_binary_file_fmt
      if(check_exist_mesh(mesh_file, izero) .gt. 0) return
!
      mesh_file%iflag_format = id_gzip_bin_file_fmt
      if(check_exist_mesh(mesh_file, izero) .gt. 0) return
!
      mesh_file%iflag_format = id_gzip_txt_file_fmt
      if(check_exist_mesh(mesh_file, izero) .gt. 0) return
!
      stop 'I cannot find mesh file!!'
!
      end subroutine find_mesh_format_4_viewer
!
!------------------------------------------------------------------
!
      subroutine count_subdomains_4_viewer(mesh_file)
!
      use mesh_IO_select
!
      type(field_IO_params), intent(in) ::  mesh_file
!
      num_pe = 0
      do
        if(check_exist_mesh(mesh_file, num_pe) .eq. 0) exit
      end do
!
      write(*,*) 'Number of subdomains: ', num_pe
!
      end subroutine count_subdomains_4_viewer
!
!------------------------------------------------------------------
!
      subroutine const_surf_mesh_4_viewer(mesh_file, ele, surf, edge)
!
      use set_merged_geometry
      use const_merged_surf_data
      use const_merged_surf_4_group
      use set_surf_connect_4_viewer
      use set_nodes_4_viewer
      use const_edge_4_viewer
      use set_nodes_4_groups_viewer
      use viewer_IO_select_4_zlib
!
      type(field_IO_params), intent(in) :: mesh_file
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!
!  set mesh_information
!
       write(*,*) 'set_overlapped_mesh_and_group'
       call set_overlapped_mesh_and_group(mesh_file, ele%nnod_4_ele)
!
       call dealloc_subdomain_groups
!
!   output grid data
!
       write(*,*) 'set_source_mesh_parameter'
       call set_source_mesh_parameter(ele, surf, edge)
!
!  choose surface
!
       write(*,*) 's_const_merged_surf_data'
       call s_const_merged_surf_data
!
!       write(*,*) 'const_merged_surface_4_ele_grp'
       call const_merged_surface_4_ele_grp
!       write(*,*) 'const_merged_surface_4_sf_grp'
       call const_merged_surface_4_sf_grp
!
!  pickup surface and nodes
!
!       write(*,*) 's_set_surf_connect_4_viewer'
       call s_set_surf_connect_4_viewer(surf%nnod_4_surf)
!       write(*,*) 's_set_nodes_4_viewer'
       call s_set_nodes_4_viewer(surf%nnod_4_surf)
!
       write(*,*) 'set_surf_domain_id_viewer'
       call set_surf_domain_id_viewer
!
!
       call deallocate_subdomain_grp_stack
       call deallocate_array_4_merge
!
       write(*,*)  'construct_edge_4_viewer'
       call construct_edge_4_viewer(surf, edge)
       write(*,*)  's_set_nodes_4_groups_viewer'
       call s_set_nodes_4_groups_viewer                                 &
     &    (surf%nnod_4_surf, edge%nnod_4_edge)
!
      call sel_output_surface_grid(mesh_file%iflag_format,              &
     &    surf%nnod_4_surf, edge%nnod_4_edge)
!
      end subroutine const_surf_mesh_4_viewer
!
!------------------------------------------------------------------
!
      subroutine set_source_mesh_parameter(ele, surf, edge)
!
      use m_geometry_constants
      use m_surf_geometry_4_merge
      use m_node_quad_2_linear_sf
!
      use m_geometry_data_4_merge
      use set_local_id_table_4_1ele
      use set_nnod_4_ele_by_type
!
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!  set array for number of surface
!
      num_pe_sf = num_pe
!
!       write(*,*) 'allocate_num_mesh_sf'
      call allocate_num_mesh_sf
!
!   set number of node in surface
!
      call set_3D_nnod_4_sfed_by_ele                                    &
     &   (ele%nnod_4_ele, surf%nnod_4_surf, edge%nnod_4_edge)
      call allocate_quad4_2_linear(ele%nnod_4_ele)
!
      call allocate_inod_in_surf(surf)
      call set_inod_in_surf                                             &
     &   (surf%nnod_4_surf, surf%node_on_sf, surf%node_on_sf_n)
!
      call allocate_inod_in_edge(edge)
      call copy_inod_in_edge(edge%nnod_4_edge,                          &
     &    edge%node_on_edge, edge%node_on_edge_sf)
!
      merged_surf%nnod_4_surf = surf%nnod_4_surf
      call allocate_inod_in_surf(merged_surf)
      call set_inod_in_surf(merged_surf%nnod_4_surf,                    &
     &    merged_surf%node_on_sf, merged_surf%node_on_sf_n)
!
      end subroutine set_source_mesh_parameter
!
!------------------------------------------------------------------
!
      subroutine set_surf_domain_id_viewer
!
      use m_surf_geometry_4_merge
!
!
      call allocate_surf_type_viewer
!
      if ( merged_surf%nnod_4_surf .eq. 4) then
        surftyp_viewer(1:surfpetot_viewer) = 221
      else if ( merged_surf%nnod_4_surf .eq. 8) then
        surftyp_viewer(1:surfpetot_viewer) = 222
      else if ( merged_surf%nnod_4_surf .eq. 9) then
        surftyp_viewer(1:surfpetot_viewer) = 223
      end if
!
      end subroutine set_surf_domain_id_viewer
!
!------------------------------------------------------------------
!
      end module const_surface_mesh
