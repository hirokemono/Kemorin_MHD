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
      use t_grp_data_merged_surfaces
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
      use pickup_node_4_viewer
      use pickup_surface_4_viewer
      use set_edge_data_by_sf
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
      integer(kind = kint), allocatable :: inod_ksm(:)
      integer(kind = kint), allocatable :: isurf_ksm(:)
      integer(kind = kint), allocatable :: iedge_ksm(:)
!
!
      type(element_data) :: ele_v
      type(surface_data) :: surf_v
      type(edge_data) :: edge_v
!
      type(merged_mesh) :: mgd_mesh1
      type(group_data_merged_surf) :: mgd_sf_grp1
      type(merged_viewer_mesh) :: mgd_view_mesh1
!
      integer(kind = kint) :: ip, inum
      integer(kind = kint) :: ierr
      character(len = kchara) :: fname_tmp, file_name
!
!
      mgd_view_mesh1%surface_file_head = mesh_file%file_prefix
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
        allocate(inod_ksm(mesh_p%node%numnod))
        allocate(isurf_ksm(ele_mesh_p%surf%numsurf))
        allocate(iedge_ksm(ele_mesh_p%edge%numedge))
        inod_ksm = 0
        isurf_ksm = 0
        iedge_ksm = 0
!
        write(*,*) 's_const_mesh_list_4_viewer', ip
        call s_const_mesh_list_4_viewer                                 &
     &     (mesh_p%node, ele_mesh_p%surf, ele_mesh_p%edge,              &
     &      group_p%nod_grp, group_p%ele_grp, group_p%surf_grp,         &
     &      inod_ksm, isurf_ksm, iedge_ksm,                             &
     &      view_mesh_p(ip)%nnod_viewer, view_mesh_p(ip)%nsurf_viewer,  &
     &      view_mesh_p(ip)%nedge_viewer)
!
        call alloc_nod_position_viewer(view_mesh_p(ip))
        call set_node_position_4_viewer                                 &
     &     (mesh_p%node, inod_ksm, view_mesh_p(ip))
!
        call alloc_surf_connect_viewer                                  &
     &     (ele_mesh_p%surf%nnod_4_surf, view_mesh_p(ip))
        call set_surf_connect_viewer(mesh_p%node, ele_mesh_p%surf,      &
     &      inod_ksm, isurf_ksm, view_mesh_p(ip))
        call set_surf_domain_id_viewer                                  &
     &     (ele_mesh_p%surf, view_mesh_p(ip))
!
        call alloc_edge_data_4_sf                                       &
     &     (ele_mesh_p%edge%nnod_4_edge, view_mesh_p(ip))
        call set_edges_connect_by_sf                                    &
     &     (mesh_p%node, ele_mesh_p%surf, ele_mesh_p%edge,              &
     &      inod_ksm, isurf_ksm, iedge_ksm, view_mesh_p(ip))
!
!
        call const_group_lists_4_viewer                                 &
     &     (mesh_p%node, ele_mesh_p%surf, ele_mesh_p%edge, group_p,     &
     &      inod_ksm, isurf_ksm, iedge_ksm, domain_grps_p(ip),          &
     &      view_nod_grps_p(ip), view_ele_grps_p(ip),                   &
     &      view_sf_grps_p(ip))
!
        deallocate(inod_ksm,  isurf_ksm, iedge_ksm)
        call dealloc_mesh_infomations(mesh_p, group_p, ele_mesh_p)
      end do
!
      do ip = 1, mgd_mesh1%num_pe
        write(*,*) 'number of mesh', ip-1, view_mesh_p(ip)%nnod_viewer, &
     &      view_mesh_p(ip)%nsurf_viewer, view_mesh_p(ip)%nedge_viewer
!
        write(*,*) 'domain group', ip-1,                                &
     &    domain_grps_p(ip)%node_grp%num_item,                          &
     &    domain_grps_p(ip)%surf_grp%num_item,                          &
     &    domain_grps_p(ip)%edge_grp%num_item
        write(*,*) 'node group', ip-1,                                  &
     &    view_nod_grps_p(ip)%node_grp%num_item
        write(*,*) 'element group', ip-1,                               &
     &    view_ele_grps_p(ip)%node_grp%num_item,                        &
     &    view_ele_grps_p(ip)%surf_grp%num_item,                        &
     &    view_ele_grps_p(ip)%edge_grp%num_item
        write(*,*) 'surface group', ip-1,                               &
     &    view_sf_grps_p(ip)%node_grp%num_item,                         &
     &    view_sf_grps_p(ip)%surf_grp%num_item,                         &
     &    view_sf_grps_p(ip)%edge_grp%num_item
!
        write(*,*) 'Check surface surface group',   &
     &         view_sf_grps_p(ip)%node_grp%istack_sf
        do inum = 1,  view_sf_grps_p(ip)%node_grp%num_item
          if(view_sf_grps_p(ip)%node_grp%item_sf(inum) .eq. 0)  &
     &        write(*,*) 'Wrong group item', ip-1, inum
        end do
        write(*,*) 'Check surface node group',   &
     &         view_sf_grps_p(ip)%surf_grp%istack_sf
        do inum = 1,  view_sf_grps_p(ip)%surf_grp%num_item
          if(view_sf_grps_p(ip)%surf_grp%item_sf(inum) .eq. 0)  &
     &        write(*,*) 'Wrong group item', ip-1, inum
        end do
        write(*,*) 'Check surface edge group', &
     &            view_sf_grps_p(ip)%edge_grp%istack_sf
        do inum = 1,  view_sf_grps_p(ip)%edge_grp%num_item
          if(view_sf_grps_p(ip)%edge_grp%item_sf(inum) .eq. 0)  &
     &        write(*,*) 'Wrong group item', ip-1, inum
        end do
!
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
        call dealloc_merged_group_item(view_nod_grps_p(ip)%node_grp)
        call dealloc_viewer_node_grps_stack(view_nod_grps_p(ip))
!
        call dealloc_merged_group_item(view_ele_grps_p(ip)%node_grp)
        call dealloc_merged_group_item(view_ele_grps_p(ip)%edge_grp)
        call dealloc_merged_group_item(view_ele_grps_p(ip)%surf_grp)
        call dealloc_viewer_surf_grps_stack(view_ele_grps_p(ip))
!
        call dealloc_merged_group_item(view_sf_grps_p(ip)%node_grp)
        call dealloc_merged_group_item(view_sf_grps_p(ip)%edge_grp)
        call dealloc_merged_group_item(view_sf_grps_p(ip)%surf_grp)
        call dealloc_viewer_surf_grps_stack(view_sf_grps_p(ip))
!
        call dealloc_merged_group_item(domain_grps_p(ip)%node_grp)
        call dealloc_merged_group_item(domain_grps_p(ip)%edge_grp)
        call dealloc_merged_group_item(domain_grps_p(ip)%surf_grp)
        call dealloc_viewer_surf_grps_stack(domain_grps_p(ip))
!
        call dealloc_surf_type_viewer(view_mesh_p(ip))
        call dealloc_edge_data_4_sf(view_mesh_p(ip))
        call dealloc_surf_connect_viewer(view_mesh_p(ip))
        call dealloc_nod_position_viewer(view_mesh_p(ip))
      end do
      deallocate(view_mesh_p, domain_grps_p)
      deallocate(view_nod_grps_p, view_ele_grps_p, view_sf_grps_p)
!
      call sel_output_surface_grid(mesh_file%iflag_format,              &
     &    ele_mesh_p%surf%nnod_4_surf, ele_mesh_p%edge%nnod_4_edge,     &
     &    mgd_view_mesh_p)
!
      call dealloc_merged_group_item(mgd_view_mesh_p%view_nod_grps%node_grp)
      call dealloc_viewer_node_grps_stack(mgd_view_mesh_p%view_nod_grps)
!
      call dealloc_merged_group_item(mgd_view_mesh_p%view_ele_grps%node_grp)
      call dealloc_merged_group_item(mgd_view_mesh_p%view_ele_grps%edge_grp)
      call dealloc_merged_group_item(mgd_view_mesh_p%view_ele_grps%surf_grp)
      call dealloc_viewer_surf_grps_stack(mgd_view_mesh_p%view_ele_grps)
!
      call dealloc_merged_group_item(mgd_view_mesh_p%view_sf_grps%node_grp)
      call dealloc_merged_group_item(mgd_view_mesh_p%view_sf_grps%edge_grp)
      call dealloc_merged_group_item(mgd_view_mesh_p%view_sf_grps%surf_grp)
      call dealloc_viewer_surf_grps_stack(mgd_view_mesh_p%view_sf_grps)
!
      call dealloc_merged_group_item(mgd_view_mesh_p%domain_grps%node_grp)
      call dealloc_merged_group_item(mgd_view_mesh_p%domain_grps%edge_grp)
      call dealloc_merged_group_item(mgd_view_mesh_p%domain_grps%surf_grp)
      call dealloc_viewer_surf_grps_stack(mgd_view_mesh_p%domain_grps)
!
      call dealloc_surf_type_viewer(mgd_view_mesh_p%view_mesh)
      call dealloc_edge_data_4_sf(mgd_view_mesh_p%view_mesh)
      call dealloc_surf_connect_viewer(mgd_view_mesh_p%view_mesh)
      call dealloc_nod_position_viewer(mgd_view_mesh_p%view_mesh)
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
      subroutine const_surf_mesh_4_viewer                               &
     &         (surf, edge, mgd_mesh, mgd_sf_grp, mgd_view_mesh)
!
      use set_merged_geometry
      use const_merged_surf_data
      use const_merged_surf_4_group
      use set_surf_connect_4_viewer
      use set_nodes_4_viewer
      use const_edge_4_viewer
      use set_nodes_4_groups_viewer
!
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_mesh), intent(inout) :: mgd_mesh
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
!
!  pickup surface and nodes
!
!
      call alloc_num_mesh_sf(mgd_mesh%num_pe, mgd_view_mesh)
!
!       write(*,*) 's_set_surf_connect_4_viewer'
       call s_set_surf_connect_4_viewer(surf%nnod_4_surf,               &
     &    mgd_mesh, mgd_sf_grp,  mgd_view_mesh%num_pe_sf,               &
     &     mgd_view_mesh%nsurf_sf, mgd_view_mesh%isurf_sf_stack,        &
     &     mgd_view_mesh%view_mesh, mgd_view_mesh%domain_grps,          &
     &     mgd_view_mesh%view_ele_grps, mgd_view_mesh%view_sf_grps)
!       write(*,*) 's_set_nodes_4_viewer'
       call s_set_nodes_4_viewer                                        &
     &    (surf%nnod_4_surf, mgd_mesh, mgd_view_mesh%num_pe_sf,         &
     &     mgd_view_mesh%nnod_sf, mgd_view_mesh%inod_sf_stack,          &
     &     mgd_view_mesh%view_mesh, mgd_view_mesh%view_nod_grps)
!
!       write(*,*) 'set_surf_domain_id_viewer'
!       call set_surf_domain_id_viewer                                   &
!     &    (mgd_mesh%merged_surf, mgd_view_mesh%view_mesh)
!
!
       call dealloc_array_4_merge(mgd_mesh)
!
       write(*,*)  'construct_edge_4_viewer'
       call construct_edge_4_viewer(surf, edge,                         &
     &     mgd_view_mesh%num_pe_sf, mgd_view_mesh%inod_sf_stack,        &
     &     mgd_view_mesh%nedge_sf, mgd_view_mesh%iedge_sf_stack,        &
     &     mgd_view_mesh%view_mesh, mgd_view_mesh%domain_grps,          &
     &     mgd_view_mesh%view_ele_grps, mgd_view_mesh%view_sf_grps)
       write(*,*)  's_set_nodes_4_groups_viewer'
       call s_set_nodes_4_groups_viewer                                 &
     &    (surf%nnod_4_surf, edge%nnod_4_edge,                          &
     &     mgd_view_mesh%num_pe_sf, mgd_view_mesh%inod_sf_stack,        &
     &     mgd_view_mesh%view_mesh, mgd_view_mesh%domain_grps,          &
     &     mgd_view_mesh%view_ele_grps, mgd_view_mesh%view_sf_grps)
!
      end subroutine const_surf_mesh_4_viewer
!
!------------------------------------------------------------------
!
      subroutine const_merged_mesh_data                                 &
     &         (mesh_file, ele, surf, edge, mgd_mesh, mgd_sf_grp)
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
      type(merged_mesh), intent(inout) :: mgd_mesh
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
!  set mesh_information
!
       write(*,*) 'set_overlapped_mesh_and_group'
       call set_overlapped_mesh_and_group                               &
     &    (mesh_file, ele%nnod_4_ele, mgd_mesh)
!
       write(*,*) 'set_source_mesh_parameter'
       call set_source_mesh_parameter                                   &
     &    (ele, surf, edge, mgd_mesh%merged_surf)
!
!  choose surface
!
       write(*,*) 's_const_merged_surf_data'
       call s_const_merged_surf_data(mgd_mesh)
!
       write(*,*) 'const_merged_surface_4_ele_grp'
       call const_merged_surface_4_ele_grp                              &
     &    (mgd_mesh%merged, mgd_mesh%merged_grp, mgd_mesh%merged_surf,  &
     &     mgd_sf_grp)
       write(*,*) 'const_merged_surface_4_sf_grp'
       call const_merged_surface_4_sf_grp                               &
     &    (mgd_mesh%merged_grp, mgd_mesh%merged_surf, mgd_sf_grp)
!
      end subroutine const_merged_mesh_data
!
!------------------------------------------------------------------
!
      subroutine set_source_mesh_parameter                              &
     &         (ele, surf, edge, merged_surf)
!
      use m_geometry_constants
      use m_node_quad_2_linear_sf
!
      use t_merged_viewer_mesh
!
      use set_local_id_table_4_1ele
      use set_nnod_4_ele_by_type
!
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(surface_data), intent(inout) :: merged_surf
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
      call alloc_inod_in_edge(edge)
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
      subroutine set_surf_domain_id_viewer(merged_surf, view_mesh)
!
      type(surface_data), intent(in) :: merged_surf
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
!
      call alloc_surf_type_viewer(view_mesh)
!
      if ( merged_surf%nnod_4_surf .eq. 4) then
        view_mesh%surftyp_viewer(1:view_mesh%nsurf_viewer) = 221
      else if ( merged_surf%nnod_4_surf .eq. 8) then
        view_mesh%surftyp_viewer(1:view_mesh%nsurf_viewer) = 222
      else if ( merged_surf%nnod_4_surf .eq. 9) then
        view_mesh%surftyp_viewer(1:view_mesh%nsurf_viewer) = 223
      end if
!
      end subroutine set_surf_domain_id_viewer
!
!------------------------------------------------------------------
!
      end module single_const_surface_mesh
