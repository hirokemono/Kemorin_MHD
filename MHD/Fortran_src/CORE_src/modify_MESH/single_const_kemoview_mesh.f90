!>@file   single_const_kemoview_mesh.f90
!!@brief  module single_const_kemoview_mesh
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
      module single_const_kemoview_mesh
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
      use t_viewer_ele_grp_surface
      use t_merged_viewer_mesh
!
      implicit none
!
      private :: find_mesh_format_4_viewer
!
      integer(kind = kint), parameter :: iflag_output_SURF = 0
      integer(kind = kint), parameter :: iflag_add_comm_tbl = 1
      integer(kind = kint), parameter :: iflag_write_subdomain = 0
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine choose_surface_mesh_sgl(nprocs_sf, mesh_file)
!
      use m_node_quad_2_linear_sf
      use find_mesh_file_format
      use viewer_mesh_IO_select
!
      integer(kind = kint), intent(in) :: nprocs_sf
      type(field_IO_params), intent(inout) :: mesh_file
!
      type(element_data) :: ele_v
      type(surface_data) :: surf_v
      type(edge_data) :: edge_v
!
!
      type(viewer_ele_grp_surface) :: mgd_sf_grp1
      type(merged_viewer_mesh) :: mgd_view_mesh1
!
      type(mesh_geometry), allocatable :: mesh0(:)
      type(mesh_groups), allocatable :: group0(:)
      type(surface_data), allocatable :: surf0(:)
!
      type(element_data), save :: ele_p
      type(surface_data), save :: surf_p
      type(edge_data), save :: edge_p
!
      type(viewer_mesh_data), allocatable :: view_mesh(:)
      type(viewer_surface_groups), allocatable :: domain_grps(:)
!
      type(viewer_node_groups), allocatable :: view_nod_grps(:)
      type(viewer_surface_groups), allocatable :: view_ele_grps(:)
      type(viewer_surface_groups), allocatable :: view_sf_grps(:)
!
      integer(kind = kint) :: ip, id_rank
!
!  set mesh_information
!
      allocate(mesh0(nprocs_sf))
      allocate(group0(nprocs_sf))
      allocate(surf0(nprocs_sf))
!
      allocate(view_mesh(nprocs_sf))
      allocate(domain_grps(nprocs_sf))
      allocate(view_nod_grps(nprocs_sf))
      allocate(view_ele_grps(nprocs_sf))
      allocate(view_sf_grps(nprocs_sf))
!
      do ip = 1, nprocs_sf
        id_rank = ip - 1
        write(*,*) 'const_merged_mesh_sgl', ip
        call const_merged_mesh_sgl                                      &
     &    (id_rank, nprocs_sf, mesh_file, mesh0(ip), group0(ip), surf0(ip),         &
     &     ele_p, surf_p, edge_p)
!
        call allocate_quad4_2_linear(ele_p%nnod_4_ele)
        call const_surf_mesh_4_viewer(mesh0(ip), group0(ip),            &
     &      edge_p, surf0(ip),                         &
     &     view_mesh(ip), domain_grps(ip), view_nod_grps(ip),           &
     &     view_ele_grps(ip), view_sf_grps(ip))
!
       call deallocate_inod_in_surf_type(surf_p)
       call dealloc_inod_in_edge(edge_p)
!
        if(iflag_write_subdomain .gt. 0) then
          call sel_output_single_surface_grid(id_rank, mesh_file,       &
     &        view_mesh(ip), domain_grps(ip), view_nod_grps(ip),        &
     &        view_ele_grps(ip), view_sf_grps(ip))
        end if
!
        call deallocate_quad4_2_linear
      end do
!
      call collect_single_viewer_mesh                                   &
     &   (nprocs_sf, mesh_file, view_mesh, domain_grps,                 &
     &    view_nod_grps, view_ele_grps, view_sf_grps)
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
     &       (mesh, group, edge, surf, view_mesh, domain_grps,          &
     &        view_nod_grps, view_ele_grps, view_sf_grps)
!
      use set_merged_geometry
      use const_merged_surf_4_group
      use set_surf_connect_4_viewer
      use set_nodes_4_viewer
      use const_edge_4_viewer
      use set_nodes_4_groups_viewer
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(inout) :: group
!
      type(edge_data), intent(in) :: edge
!
      type(surface_data), intent(inout) :: surf
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(viewer_surface_groups), intent(inout) :: domain_grps
!
      type(viewer_node_groups), intent(inout) :: view_nod_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
      type(viewer_ele_grp_surface) :: mgd_sf_grp
!
!
       write(*,*) 'const_merged_surface_4_ele_grp'
       call const_merged_surface_4_ele_grp(mesh%node, mesh%ele,         &
     &     group, surf, mgd_sf_grp)
!
       write(*,*) 'const_merged_surface_4_sf_grp'
       call const_merged_surface_4_sf_grp(group, surf, mgd_sf_grp)
!
!  pickup surface and nodes
!
       call s_set_surf_connect_4_viewer(group, surf, mgd_sf_grp,        &
     &     view_mesh, domain_grps, view_ele_grps, view_sf_grps)
!       write(*,*) 's_set_nodes_4_viewer'
!
      call deallocate_sf_grp_type(group%surf_grp)
      call deallocate_grp_type(group%ele_grp)
!
      call deallocate_surface_connect_type(surf)
      call deallocate_iso_surface_type(surf)
!
       call s_set_nodes_4_viewer                                        &
     &    (surf%nnod_4_surf, mesh, group, view_mesh, view_nod_grps)
       call deallocate_grp_type(group%nod_grp)
!
!       write(*,*) 'set_surf_domain_id_viewer'
       call set_surf_domain_id_viewer(surf, view_mesh)
!
!       write(*,*)  'construct_edge_4_viewer'
       call construct_edge_4_viewer(surf%nnod_4_surf, edge,             &
     &     view_mesh, domain_grps, view_ele_grps, view_sf_grps)
!       write(*,*)  's_set_nodes_4_groups_viewer'
       call s_set_nodes_4_groups_viewer                                 &
     &    (surf%nnod_4_surf, edge%nnod_4_edge,                          &
     &     view_mesh, domain_grps, view_ele_grps, view_sf_grps)
!
       call dealloc_n_iso_surf_4_ele_grp(mgd_sf_grp)
       call dealloc_iso_surf_4_egrp_m(mgd_sf_grp)
       call dealloc_iso_surf_4_sgrp_m(mgd_sf_grp)
!
      end subroutine const_surf_mesh_4_viewer
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
     &         (id_rank, nprocs_sf, mesh_file, mesh0, group0, surf0, &
     &          ele, surf, edge)
!
      use t_file_IO_parameter
      use load_mesh_data
      use set_group_types_4_IO
      use count_number_with_overlap
      use set_merged_geometry
      use mesh_IO_select
      use const_surface_data
      use copy_mesh_structures
      use add_comm_table_in_node_grp
!
      use load_mesh_data
      use const_mesh_information
      use set_nnod_4_ele_by_type
      use copy_mesh_structures
      use set_element_data_4_IO
!
      integer(kind = kint), intent(in) :: id_rank, nprocs_sf
      type(field_IO_params), intent(in) :: mesh_file
!
      type(mesh_geometry), intent(inout) :: mesh0
      type(mesh_groups), intent(inout) ::   group0
      type(surface_data), intent(inout) :: surf0
!
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
      type(mesh_data) :: fem_IO_p
      type(group_data) :: new_nod_grp
      type(edge_data) :: edge0
      integer(kind = kint) :: ierr
!
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call sel_read_mesh(mesh_file, id_rank, fem_IO_p, ierr)
!
      if(iflag_add_comm_tbl .gt. 0) then
        call add_comm_table_in_node_group(nprocs_sf,                    &
     &     fem_IO_p%mesh%nod_comm, fem_IO_p%group%nod_grp, new_nod_grp)
        call deallocate_grp_type(fem_IO_p%group%nod_grp)
        call copy_group_data(new_nod_grp, fem_IO_p%group%nod_grp)
        call deallocate_grp_type(new_nod_grp)
      end if
!
!      call set_mesh                                                     &
!     &  (fem_IO_p, mesh0, group0, surf0%nnod_4_surf, edge0%0nnod_4_edge)
      call copy_comm_tbl_type(fem_IO_p%mesh%nod_comm, mesh0%nod_comm)
      call copy_node_geometry_types(fem_IO_p%mesh%node, mesh0%node)
      call copy_ele_connect_from_IO(fem_IO_p%mesh%ele, mesh0%ele)
      call set_grp_data_from_IO(fem_IO_p%group,                         &
     &    group0%nod_grp, group0%ele_grp, group0%surf_grp)
      call set_3D_nnod_4_sfed_by_ele                                    &
     &   (mesh0%ele%nnod_4_ele, surf0%nnod_4_surf, edge0%nnod_4_edge)
!
      call alloc_sph_node_geometry(mesh0%node)
!
      call set_local_element_info(surf0, edge0)
      call construct_surface_data                                       &
     &   (mesh0%node, mesh0%ele, surf0)
!
!
      call dealloc_groups_data(fem_IO_p%group)
!
      ele%nnod_4_ele = fem_IO_p%mesh%ele%nnod_4_ele
      call set_3D_nnod_4_sfed_by_ele                                    &
     &   (ele%nnod_4_ele, surf%nnod_4_surf, edge%nnod_4_edge)
      call set_local_element_info(surf, edge)
!
      end subroutine const_merged_mesh_sgl
!
!------------------------------------------------------------------
!
      subroutine collect_single_viewer_mesh                             &
     &         (nprocs_sf, mesh_file, view_mesh, domain_grps,           &
     &          view_nod_grps, view_ele_grps, view_sf_grps)
!
      use viewer_mesh_IO_select
      use merge_viewer_mesh
!
      integer(kind = kint) :: nprocs_sf
      type(field_IO_params), intent(in) :: mesh_file
!
      type(viewer_mesh_data), intent(in) :: view_mesh(nprocs_sf)
!
      type(viewer_surface_groups), intent(inout)                        &
     &                  :: domain_grps(nprocs_sf)
!
      type(viewer_node_groups), intent(inout)                           &
     &                  :: view_nod_grps(nprocs_sf)
      type(viewer_surface_groups), intent(inout)                        &
     &                  :: view_ele_grps(nprocs_sf)
      type(viewer_surface_groups), intent(inout)                        &
     &                  :: view_sf_grps(nprocs_sf)
!
      type(merged_viewer_mesh)  :: mgd_vmesh
!
!
      call alloc_num_mesh_sf(nprocs_sf, mgd_vmesh)
      call s_merge_viewer_mesh(nprocs_sf, view_mesh, domain_grps,       &
     &    view_nod_grps, view_ele_grps, view_sf_grps, mgd_vmesh)
!
      call sel_output_surface_grid(mesh_file, mgd_vmesh)
!
      call dealloc_num_mesh_sf(mgd_vmesh)
!
      end subroutine collect_single_viewer_mesh
!
! -----------------------------------------------------------------------
!
      end module single_const_kemoview_mesh
