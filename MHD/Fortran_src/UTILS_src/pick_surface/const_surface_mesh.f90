!
!      module const_surface_mesh
!
!      Written by Kemorin
!      Modified by Kemorin on Dec., 2006
!
!      subroutine choose_surface_mesh
!
      module const_surface_mesh
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_file_format_switch
      use m_read_mesh_data
      use m_geometry_data_4_merge
      use m_surface_mesh_4_merge
!
      implicit none
!
      private :: set_source_mesh_parameter
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine choose_surface_mesh(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
!
      mesh_file_head =    file_head
      surface_file_head = file_head
      call find_mesh_format_4_viewer
      call const_surf_mesh_4_viewer
!
      end subroutine choose_surface_mesh
!
!------------------------------------------------------------------
!
      subroutine find_mesh_format_4_viewer
!
      use m_file_format_switch
      use set_parallel_file_name
      use mesh_file_IO
!
      character(len=kchara) :: fname, fname_tmp
      integer(kind = kint) :: iflag
!
!
!  Detect file format
      iflag = 0
      call add_int_suffix(izero, mesh_file_head, fname)
      call add_gfm_extension(fname, fname_tmp)
      call add_gzip_extension(fname_tmp, fname)
      open(15,file = fname, status='old', action='read', err=98)
      close(15)
      iflag = 1
      iflag_mesh_file_fmt = id_gzip_txt_file_fmt
   98 continue
!
      if(iflag .eq. 0) then
        call add_int_suffix(izero, mesh_file_head, fname_tmp)
        call add_gzip_extension(fname_tmp, fname)
        open(15,file = fname, status='old', action='read', err=97)
        close(15)
        iflag = 1
        iflag_mesh_file_fmt = id_gzip_txt_file_fmt
      end if
   97 continue
!
      if(iflag .eq. 0) then
        call add_int_suffix(izero, mesh_file_head, fname_tmp)
        call add_gfm_extension(fname_tmp, fname)
        open(15,file = fname, status='old', action='read', err=96)
        close(15)
        iflag = 1
        iflag_mesh_file_fmt = id_ascii_file_fmt
      end if
   96 continue
!
      if(iflag .eq. 0) then
        call add_int_suffix(izero, mesh_file_head, fname)
        open(15,file = fname, status='old', action='read', err=95)
        close(15)
        iflag = 1
        iflag_mesh_file_fmt = id_ascii_file_fmt
      end if
   95 continue
!
      if(iflag .eq. 0) stop 'I cannot find mesh file!!'
!
!   count number of subdomains
!
      num_pe = 0
      do
        call set_mesh_fname(num_pe)
        if(iflag_mesh_file_fmt .eq. id_gzip_txt_file_fmt) then
          call add_gzip_extension(mesh_file_name, fname)
        else
          fname = mesh_file_name
        end if
!
        write(*,*) 'mesh file name: ', trim(fname)
        open(15,file = fname, status='old', action='read', err=99)
        close(15)
        num_pe = num_pe + 1
      end do
  99  continue
!
      write(*,*) 'Number of subdomains: ', num_pe
!
      end subroutine find_mesh_format_4_viewer
!
!------------------------------------------------------------------
!
      subroutine const_surf_mesh_4_viewer
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
!  set mesh_information
!
       write(*,*) 'set_overlapped_mesh_and_group'
       call set_overlapped_mesh_and_group
!
       call dealloc_subdomain_groups
!
!   output grid data
!
       write(*,*) 'set_source_mesh_parameter'
       call set_source_mesh_parameter
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
       call s_set_surf_connect_4_viewer
!       write(*,*) 's_set_nodes_4_viewer'
       call s_set_nodes_4_viewer
!
       write(*,*) 'set_surf_domain_id_viewer'
       call set_surf_domain_id_viewer
!
!
       call deallocate_subdomain_grp_stack
       call deallocate_array_4_merge
!
       write(*,*)  'construct_edge_4_viewer'
       call construct_edge_4_viewer
       write(*,*)  's_set_nodes_4_groups_viewer'
       call s_set_nodes_4_groups_viewer
!
       call sel_output_surface_grid(iflag_mesh_file_fmt)
!
      end subroutine const_surf_mesh_4_viewer
!
!------------------------------------------------------------------
!
      subroutine set_source_mesh_parameter
!
      use m_geometry_constants
      use m_geometry_data
      use m_surf_geometry_4_merge
      use m_node_quad_2_linear_sf
!
      use m_geometry_data_4_merge
      use set_local_id_table_4_1ele
      use set_nnod_4_ele_by_type
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
     &   (ele1%nnod_4_ele, surf1%nnod_4_surf, edge1%nnod_4_edge)
      call allocate_quad4_2_linear(ele1%nnod_4_ele)
!
      call allocate_inod_in_surf(surf1)
      call set_inod_in_surf                                             &
     &   (surf1%nnod_4_surf, surf1%node_on_sf, surf1%node_on_sf_n)
!
      call allocate_inod_in_edge
      call copy_inod_in_edge(edge1%nnod_4_edge,                         &
     &    node_on_edge, node_on_edge_sf)
!
      merged_surf%nnod_4_surf = surf1%nnod_4_surf
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
