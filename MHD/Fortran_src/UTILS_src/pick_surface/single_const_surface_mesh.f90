!>@file   single_const_surface_mesh.f90
!!@brief  module single_const_surface_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine choose_surface_mesh_sgl(mesh_file)
!!        type(field_IO_params), intent(inout) :: mesh_file
!!@endverbatim
!
      module single_const_surface_mesh
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_file_format_switch
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_file_IO_parameter
      use t_mesh_data_4_merge
      use t_viewer_mesh
      use t_merged_viewer_mesh
!
      implicit none
!
      integer(kind = kint), parameter, private :: iflag_add_comm_tbl = 1
!
      private :: find_mesh_format_4_viewer
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine choose_surface_mesh_sgl(mesh_file)
!
      use find_mesh_file_format
      use viewer_IO_select_4_zlib
!
      use load_mesh_data
      use const_mesh_information
      use const_mesh_list_4_viewer
      use extend_group_table
      use copy_mesh_structures
      use set_parallel_file_name
      use viewer_mesh_data_IO
      use viewer_group_data_IO
      use merge_viewer_mesh
!
!
      type(field_IO_params), intent(inout) :: mesh_file
!
      type(mesh_geometry), save :: mesh_p
      type(mesh_groups), save :: group_p
      type(element_geometry), save :: ele_mesh_p
!
      type(group_data), save :: new_nod_grp
!
      type(merged_viewer_mesh), save :: mgd_view_mesh_p
      type(viewer_mesh_data), allocatable :: view_mesh_p(:)
      type(viewer_surface_groups), allocatable :: domain_grps_p(:)
      type(viewer_node_groups), allocatable :: view_nod_grps_p(:)
      type(viewer_surface_groups), allocatable :: view_ele_grps_p(:)
      type(viewer_surface_groups), allocatable :: view_sf_grps_p(:)
!
      type(index_list_4_pick_surface), save :: idx_lst_s
!
      type(merged_mesh) :: mgd_mesh1
!
      integer(kind = kint) :: ip, inum
      integer(kind = kint) :: ierr
      character(len = kchara) :: fname_tmp, file_name
!
!
      mgd_view_mesh_p%surface_file_head = mesh_file%file_prefix
      write(*,*) 'find_mesh_format_4_viewer'
      call find_mesh_format_4_viewer(mesh_file)
      write(*,*) 'count_subdomains_4_viewer'
      call count_subdomains_4_viewer(mesh_file, mgd_mesh1%num_pe)
!
!  set mesh_information
!
       call alloc_num_mesh_sf(ione, mgd_view_mesh_p)
       allocate(view_mesh_p(mgd_mesh1%num_pe))
       allocate(domain_grps_p(mgd_mesh1%num_pe))
       allocate(view_nod_grps_p(mgd_mesh1%num_pe))
       allocate(view_ele_grps_p(mgd_mesh1%num_pe))
       allocate(view_sf_grps_p(mgd_mesh1%num_pe))
!
      do ip = 1, mgd_mesh1%num_pe
        call input_mesh(mesh_file, (ip-1), mesh_p, group_p,             &
     &      ele_mesh_p%surf%nnod_4_surf,                                &
     &      ele_mesh_p%edge%nnod_4_edge, ierr)
!
        if(iflag_add_comm_tbl .gt. 0) then
          call add_comm_table_in_node_group(mgd_mesh1%num_pe,           &
     &        mesh_p%nod_comm, group_p%nod_grp, new_nod_grp)
          call deallocate_grp_type(group_p%nod_grp)
          call copy_group_data(new_nod_grp, group_p%nod_grp)
          call deallocate_grp_type(new_nod_grp)
        end if
!
        call const_mesh_infos((ip-1), mesh_p, group_p, ele_mesh_p)
!
        write(*,*) 'const_viewer_mesh', ip
        call const_viewer_mesh                                          &
     &     (mesh_p, ele_mesh_p, group_p, view_mesh_p(ip),               &
     &      domain_grps_p(ip), view_nod_grps_p(ip),                     &
     &      view_ele_grps_p(ip), view_sf_grps_p(ip))
!
        call dealloc_mesh_infomations(mesh_p, group_p, ele_mesh_p)
      end do
!
      do ip = 1, mgd_mesh1%num_pe
        mgd_view_mesh_p%inod_sf_stack(1) =  view_mesh_p(ip)%nnod_viewer
        mgd_view_mesh_p%isurf_sf_stack(1) = view_mesh_p(ip)%nsurf_viewer
        mgd_view_mesh_p%iedge_sf_stack(1) = view_mesh_p(ip)%nedge_viewer
!
        call add_int_suffix                                             &
     &     ((ip-1), mesh_file%file_prefix, fname_tmp)
        call add_ksm_extension(fname_tmp, file_name)
        write(*,*) 'surface mesh file name: ', trim(file_name)
        open (surface_id, file = file_name)
!
        call write_domain_data_viewer(mgd_view_mesh_p)
        call write_node_data_viewer(view_mesh_p(ip))
        call write_surf_connect_viewer                                  &
       &   (ele_mesh_p%surf%nnod_4_surf, view_mesh_p(ip))
        call write_edge_connect_viewer                                  &
       &   (ele_mesh_p%edge%nnod_4_edge, view_mesh_p(ip))
!
        call write_domain_group_viewer(ione, domain_grps_p(ip))
!
        call write_nod_group_viewer(ione, view_nod_grps_p(ip))
        call write_ele_group_viewer(ione, view_ele_grps_p(ip))
        call write_surf_group_viewer(ione, view_sf_grps_p(ip))
        close(surface_id)
      end do
      call dealloc_num_mesh_sf(mgd_view_mesh_p)
!
      call s_merge_viewer_mesh(mgd_mesh1%num_pe,                        &
     &    ele_mesh_p%surf%nnod_4_surf, ele_mesh_p%edge%nnod_4_edge,     &
     &    view_mesh_p, domain_grps_p,                                   &
     &    view_nod_grps_p, view_ele_grps_p, view_sf_grps_p,             &
     &    mgd_view_mesh_p)
!
      do ip = 1, mgd_mesh1%num_pe
        call dealloc_viewer_mesh(view_mesh_p(ip), domain_grps_p(ip),    &
     &    view_nod_grps_p(ip), view_ele_grps_p(ip), view_sf_grps_p(ip))
      end do
      deallocate(view_mesh_p, domain_grps_p)
      deallocate(view_nod_grps_p, view_ele_grps_p, view_sf_grps_p)
!
      call sel_output_surface_grid(mesh_file%iflag_format,              &
     &    ele_mesh_p%surf%nnod_4_surf, ele_mesh_p%edge%nnod_4_edge,     &
     &    mgd_view_mesh_p)
!
      call dealloc_viewer_mesh(mgd_view_mesh_p%view_mesh,               &
     &    mgd_view_mesh_p%domain_grps, mgd_view_mesh_p%view_nod_grps,   &
     &    mgd_view_mesh_p%view_ele_grps, mgd_view_mesh_p%view_sf_grps)
      call dealloc_num_mesh_sf(mgd_view_mesh_p)
!
      end subroutine choose_surface_mesh_sgl
!
!------------------------------------------------------------------
!
      subroutine find_mesh_format_4_viewer(mesh_file)
!
      use t_file_IO_parameter
      use m_file_format_switch
      use mesh_file_name_by_param
!
      type(field_IO_params), intent(inout) ::  mesh_file
!
!  Detect file format
      mesh_file%iflag_format = id_gzip_txt_file_fmt
      if(check_exist_mesh(mesh_file, izero) .eq. 0) return
!
      mesh_file%iflag_format = id_ascii_file_fmt
      if(check_exist_mesh(mesh_file, izero) .eq. 0) return
!
      mesh_file%iflag_format = id_binary_file_fmt
      if(check_exist_mesh(mesh_file, izero) .eq. 0) return
!
      mesh_file%iflag_format = id_gzip_bin_file_fmt
      if(check_exist_mesh(mesh_file, izero) .eq. 0) return
!
      mesh_file%iflag_format = id_gzip_txt_file_fmt
      if(check_exist_mesh(mesh_file, izero) .eq. 0) return
!
      stop 'I cannot find mesh file!!'
!
      end subroutine find_mesh_format_4_viewer
!
!------------------------------------------------------------------
!
!      subroutine set_source_mesh_parameter                              &
!     &         (ele, surf, edge, merged_surf)
!
!      call set_3D_nnod_4_sfed_by_ele                                    &
!     &   (ele%nnod_4_ele, surf%nnod_4_surf, edge%nnod_4_edge)
!      call allocate_quad4_2_linear(ele%nnod_4_ele)
!
!      call allocate_inod_in_surf(surf)
!      call set_inod_in_surf                                             &
!     &   (surf%nnod_4_surf, surf%node_on_sf, surf%node_on_sf_n)
!
!      call alloc_inod_in_edge(edge)
!      call copy_inod_in_edge(edge%nnod_4_edge,                          &
!     &    edge%node_on_edge, edge%node_on_edge_sf)
!
!      merged_surf%nnod_4_surf = surf%nnod_4_surf
!      call allocate_inod_in_surf(merged_surf)
!      call set_inod_in_surf(merged_surf%nnod_4_surf,                    &
!     &    merged_surf%node_on_sf, merged_surf%node_on_sf_n)
!
!      end subroutine set_source_mesh_parameter
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_viewer_mesh                                      &
     &         (mesh, ele_mesh, group, view_mesh, domain_grps,          &
     &          view_nod_grps, view_ele_grps, view_sf_grps)
!
      use t_const_mesh_data_4_viewer
      use const_mesh_list_4_viewer
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_groups), intent(in) :: group
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(viewer_surface_groups), intent(inout) :: domain_grps
      type(viewer_node_groups), intent(inout) :: view_nod_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
      type(index_list_4_pick_surface) :: idx_lst_s
!
!
      call alloc_index_list_pick_surf                                   &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge, idx_lst_s)
!
      call const_index_list_4_viewer                                    &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge,                      &
     &    group%nod_grp, group%ele_grp, group%surf_grp,                 &
     &    idx_lst_s, view_mesh)
!
      call const_mesh_data_4_viewer                                     &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge,                      &
     &    idx_lst_s, view_mesh)
!
!
      call const_domain_groups_4_viewer                                 &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge,                      &
     &    idx_lst_s, domain_grps)
!
      call const_node_groups_4_viewer(mesh%node, group%nod_grp,         &
     &    idx_lst_s%inod_ksm, idx_lst_s%iflag_node, view_nod_grps)
      call const_element_groups_4_viewer                                &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge,                      &
     &    group%ele_grp, idx_lst_s, view_ele_grps)
      call const_surface_groups_4_viewer                                &
     &   (mesh%node, ele_mesh%surf, ele_mesh%edge,                      &
     &    group%surf_grp, idx_lst_s, view_sf_grps)
!
      call dealloc_index_list_pick_surf(idx_lst_s)
!
      end subroutine const_viewer_mesh
!
!------------------------------------------------------------------
!
      subroutine dealloc_viewer_mesh(view_mesh, domain_grps,            &
     &          view_nod_grps, view_ele_grps, view_sf_grps)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(viewer_surface_groups), intent(inout) :: domain_grps
      type(viewer_node_groups), intent(inout) :: view_nod_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
!
      call dealloc_merged_group_item(view_nod_grps%node_grp)
      call dealloc_viewer_node_grps_stack(view_nod_grps)
!
      call dealloc_merged_group_item(view_ele_grps%node_grp)
      call dealloc_merged_group_item(view_ele_grps%edge_grp)
      call dealloc_merged_group_item(view_ele_grps%surf_grp)
      call dealloc_viewer_surf_grps_stack(view_ele_grps)
!
      call dealloc_merged_group_item(view_sf_grps%node_grp)
      call dealloc_merged_group_item(view_sf_grps%edge_grp)
      call dealloc_merged_group_item(view_sf_grps%surf_grp)
      call dealloc_viewer_surf_grps_stack(view_sf_grps)
!
      call dealloc_merged_group_item(domain_grps%node_grp)
      call dealloc_merged_group_item(domain_grps%edge_grp)
      call dealloc_merged_group_item(domain_grps%surf_grp)
      call dealloc_viewer_surf_grps_stack(domain_grps)
!
      call dealloc_surf_type_viewer(view_mesh)
      call dealloc_edge_data_4_sf(view_mesh)
      call dealloc_surf_connect_viewer(view_mesh)
      call dealloc_nod_position_viewer(view_mesh)
!
      end subroutine dealloc_viewer_mesh
!
!------------------------------------------------------------------
!
      end module single_const_surface_mesh
