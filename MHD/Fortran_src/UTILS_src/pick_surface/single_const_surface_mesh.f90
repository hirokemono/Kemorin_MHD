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
      use m_node_quad_2_linear_sf
      use find_mesh_file_format
      use viewer_mesh_IO_select
!
      type(field_IO_params), intent(inout) :: mesh_file
!
      type(element_data) :: ele_v
      type(surface_data) :: surf_v
      type(edge_data) :: edge_v
!
      integer(kind = kint) :: nprocs_sf
!
      type(group_data_merged_surf) :: mgd_sf_grp1
      type(merged_viewer_mesh) :: mgd_view_mesh1
      type(element_data), save :: ele_p
      type(surface_data), save :: surf_p
      type(edge_data), save :: edge_p
      type(merged_mesh), allocatable :: mgd_mesh_p(:)
      type(group_data_merged_surf), allocatable :: mgd_sf_grp_p(:)
      type(merged_viewer_mesh), allocatable :: mgd_view_mesh_p(:)
!
      integer(kind = kint) :: ip, id_rank
!
!
      write(*,*) 'find_mesh_format_4_viewer'
      call find_mesh_format_4_viewer(mesh_file)
      write(*,*) 'count_subdomains_4_viewer'
      call count_subdomains_4_viewer(mesh_file, nprocs_sf)
!
!  set mesh_information
!
      allocate(mgd_mesh_p(nprocs_sf))
      allocate(mgd_sf_grp_p(nprocs_sf))
      allocate(mgd_view_mesh_p(nprocs_sf))
!
      do ip = 1, nprocs_sf
        id_rank = ip - 1
        write(*,*) 'const_merged_mesh_sgl', ip
        call const_merged_mesh_sgl                                      &
     &    (id_rank, mesh_file, ele_p, surf_p, edge_p, mgd_mesh_p(ip), mgd_sf_grp_p(ip))
!
        call alloc_num_mesh_sf(mgd_mesh_p(ip)%num_pe,  mgd_view_mesh_p(ip))
        call const_surf_mesh_4_viewer                                   &
     &     (surf_p, edge_p, mgd_mesh_p(ip), mgd_sf_grp_p(ip),   &
     &   mgd_view_mesh_p(ip)%num_pe_sf, mgd_view_mesh_p(ip)%nnod_sf, &
     &    mgd_view_mesh_p(ip)%nsurf_sf, mgd_view_mesh_p(ip)%nedge_sf,  &
     &   mgd_view_mesh_p(ip)%inod_sf_stack, mgd_view_mesh_p(ip)%isurf_sf_stack, &
     &    mgd_view_mesh_p(ip)%iedge_sf_stack, mgd_view_mesh_p(ip)%view_mesh,   &
     &    mgd_view_mesh_p(ip)%domain_grps, mgd_view_mesh_p(ip)%view_nod_grps,   &
     &    mgd_view_mesh_p(ip)%view_ele_grps, mgd_view_mesh_p(ip)%view_sf_grps)
!
       call dealloc_n_iso_surf_4_ele_grp(mgd_sf_grp_p(ip))
       call dealloc_iso_surf_4_egrp_m(mgd_sf_grp_p(ip))
       call dealloc_iso_surf_4_sgrp_m(mgd_sf_grp_p(ip))
!
       call deallocate_inod_in_surf_type(surf_p)
       call dealloc_inod_in_edge(edge_p)
!
       call deallocate_ext_surface_type(mgd_mesh_p(ip)%merged_surf)
       call deallocate_inod_in_surf_type(mgd_mesh_p(ip)%merged_surf)
       call deallocate_node_geometry_type(mgd_mesh_p(ip)%merged%node)
       call deallocate_ele_connect_type(mgd_mesh_p(ip)%merged%ele)
!
        call sel_output_single_surface_grid(id_rank, mesh_file,         &
     &    surf_p%nnod_4_surf, edge_p%nnod_4_edge,                       &
     &    mgd_view_mesh_p(ip)%view_mesh, mgd_view_mesh_p(ip)%domain_grps,       &
     &    mgd_view_mesh_p(ip)%view_nod_grps,    &
     &    mgd_view_mesh_p(ip)%view_ele_grps, &
     &    mgd_view_mesh_p(ip)%view_sf_grps)
!
        call dealloc_number_of_mesh(mgd_mesh_p(ip))
        call deallocate_quad4_2_linear
      end do
!
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
     &         (surf, edge, mgd_mesh, mgd_sf_grp,                       &
     &   num_pe_sf, nnod_sf, &
     &    nsurf_sf, nedge_sf,  &
     &   inod_sf_stack, isurf_sf_stack, &
     &    iedge_sf_stack, view_mesh,   &
     &    domain_grps, view_nod_grps,   &
     &    view_ele_grps, view_sf_grps)
!
      use set_merged_geometry
      use const_merged_surf_data
      use const_merged_surf_4_group
      use set_surf_connect_4_viewer
      use set_nodes_4_viewer
      use const_edge_4_viewer
      use set_nodes_4_groups_viewer
!
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(merged_mesh), intent(inout) :: mgd_mesh
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
        integer(kind = kint), intent(in) :: num_pe_sf
        integer(kind = kint), intent(inout) :: nnod_sf(:)
        integer(kind = kint), intent(inout) :: nsurf_sf(:)
        integer(kind = kint), intent(inout) :: nedge_sf(:)
        integer(kind = kint), intent(inout) :: inod_sf_stack(:)
        integer(kind = kint), intent(inout) :: isurf_sf_stack(:)
        integer(kind = kint), intent(inout) :: iedge_sf_stack(:)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(viewer_surface_groups), intent(inout) :: domain_grps
!
      type(viewer_node_groups), intent(inout) :: view_nod_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
!  pickup surface and nodes
!
!       write(*,*) 's_set_surf_connect_4_viewer'
       call s_set_surf_connect_4_viewer(surf%nnod_4_surf,               &
     &    mgd_mesh, mgd_sf_grp,  num_pe_sf,               &
     &     nsurf_sf, isurf_sf_stack,        &
     &     view_mesh, domain_grps, view_ele_grps, view_sf_grps)
!       write(*,*) 's_set_nodes_4_viewer'
       call s_set_nodes_4_viewer                                        &
     &    (surf%nnod_4_surf, mgd_mesh, num_pe_sf,         &
     &     nnod_sf, inod_sf_stack,          &
     &     view_mesh, view_nod_grps)
!
       write(*,*) 'set_surf_domain_id_viewer'
       call set_surf_domain_id_viewer                                   &
     &    (mgd_mesh%merged_surf, view_mesh)
!
!
       call dealloc_array_4_merge(mgd_mesh)
!
       write(*,*)  'construct_edge_4_viewer'
       call construct_edge_4_viewer(surf, edge,                         &
     &     num_pe_sf, inod_sf_stack,        &
     &     nedge_sf, iedge_sf_stack,        &
     &     view_mesh, domain_grps, view_ele_grps, view_sf_grps)
       write(*,*)  's_set_nodes_4_groups_viewer'
       call s_set_nodes_4_groups_viewer                                 &
     &    (surf%nnod_4_surf, edge%nnod_4_edge,                          &
     &     num_pe_sf, inod_sf_stack,        &
     &     view_mesh, domain_grps, view_ele_grps, view_sf_grps)
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
      use viewer_mesh_IO_select
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
!       write(*,*) 'const_merged_surface_4_ele_grp'
       call const_merged_surface_4_ele_grp                              &
     &    (mgd_mesh%merged, mgd_mesh%merged_grp, mgd_mesh%merged_surf,  &
     &     mgd_sf_grp)
!       write(*,*) 'const_merged_surface_4_sf_grp'
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
!------------------------------------------------------------------
!
      subroutine const_merged_mesh_sgl                                  &
     &         (id_rank, mesh_file, ele, surf, edge,                    &
     &          mgd_mesh, mgd_sf_grp)
!
      use t_file_IO_parameter
      use load_mesh_data
      use set_group_types_4_IO
      use count_number_with_overlap
      use set_merged_geometry
      use mesh_IO_select
      use const_merged_surf_data
      use const_merged_surf_4_group
!
      integer(kind = kint) :: id_rank
      type(field_IO_params), intent(in) :: mesh_file
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_mesh), intent(inout) :: mgd_mesh
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
      type(mesh_data) :: fem_IO_p
      integer(kind = kint) :: ierr
!
!
      mgd_mesh%num_pe = ione
      call alloc_number_of_mesh(mgd_mesh)
      call alloc_subdomain_groups(mgd_mesh)
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call sel_read_mesh(mesh_file, id_rank, fem_IO_p, ierr)
!
      call set_mesh_geometry_data(fem_IO_p%mesh,                        &
     &    mgd_mesh%subdomain(1)%nod_comm, mgd_mesh%subdomain(1)%node,   &
     &    mgd_mesh%subdomain(1)%ele)
      call set_grp_data_from_IO(fem_IO_p%group,                         &
     &    mgd_mesh%sub_nod_grp(1), mgd_mesh%sub_ele_grp(1),             &
     &    mgd_mesh%sub_surf_grp(1))
      call dealloc_groups_data(fem_IO_p%group)
      ele%nnod_4_ele = fem_IO_p%mesh%ele%nnod_4_ele
!
      call count_num_overlap_geom_type                                  &
     &   (mgd_mesh%num_pe, mgd_mesh%subdomain, mgd_mesh%merge_tbl)
      call count_num_geometry_w_overlap                                 &
     &   (mgd_mesh%num_pe, mgd_mesh%subdomain, mgd_mesh%merge_tbl,      &
     &    mgd_mesh%merged)
!
      call count_overlapped_mesh_groups(mgd_mesh)
!
!
       write(*,*) 'set_source_mesh_parameter'
       call set_source_mesh_parameter                                   &
     &    (ele, surf, edge, mgd_mesh%merged_surf)
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
      end subroutine const_merged_mesh_sgl
!
!------------------------------------------------------------------
!
      subroutine collect_single_viewer_mesh                             &
     &         (nprocs_sf, mesh_file,  surf, edge, mgd_v_mesh_p)
!
      use renumber_para_viewer_mesh
      use viewer_mesh_IO_select
      use const_global_element_ids
!
      integer(kind = kint) :: nprocs_sf
      type(field_IO_params), intent(in) :: mesh_file
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_viewer_mesh), intent(inout) :: mgd_v_mesh_p(nprocs_sf)
!
      type(merged_viewer_mesh) :: mgd_vmesh
!
      integer(kind = kint) :: ip
!
      call alloc_num_mesh_sf(nprocs_sf, mgd_vmesh)
!
      do ip = 1, nprocs_sf
        mgd_vmesh%inod_sf_stack(ip)                                     &
     &           = mgd_vmesh%inod_sf_stack(ip-1)                        &
     &            + mgd_v_mesh_p(ip)%view_mesh%nnod_viewer
        mgd_vmesh%isurf_sf_stack(ip)                                    &
     &           = mgd_vmesh%isurf_sf_stack(ip-1)                       &
     &            + mgd_v_mesh_p(ip)%view_mesh%nsurf_viewer
        mgd_vmesh%iedge_sf_stack(ip)                                    &
     &           = mgd_vmesh%iedge_sf_stack(ip-1)                       &
     &            + mgd_v_mesh_p(ip)%view_mesh%nedge_viewer
!
        call s_renumber_para_viewer_mesh                                &
     &     (mgd_vmesh%inod_sf_stack(ip-1),  &
     &      mgd_vmesh%isurf_sf_stack(ip-1),  &
     &      mgd_vmesh%iedge_sf_stack(ip-1),  &
     &      surf, edge, mgd_vmesh)
      end do
!
!      call s_merge_viewer_mesh(nprocs,                          &
!     &          nnod_4_surf, nnod_4_edge, vmesh, domain_grps_p,       &
!     &          view_nod_grps_p, view_ele_grps_p, view_sf_grps_p,     &
!     &          mgd_vmesh)
!
      call sel_output_surface_grid                                      &
     &   (mesh_file, surf%nnod_4_surf, edge%nnod_4_edge, mgd_vmesh)
!
      call dealloc_num_mesh_sf(mgd_vmesh)
!
      end subroutine collect_single_viewer_mesh
!
! -----------------------------------------------------------------------
!
      end module single_const_surface_mesh
