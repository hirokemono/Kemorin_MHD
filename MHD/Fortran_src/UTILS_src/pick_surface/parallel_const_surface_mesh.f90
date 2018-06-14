!>@file   parallel_const_surface_mesh.f90
!!@brief  module parallel_const_surface_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine choose_surface_mesh_para(mesh_file)
!!        type(field_IO_params), intent(inout) :: mesh_file
!!@endverbatim
!
      module parallel_const_surface_mesh
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_file_format_switch
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_file_IO_parameter
      use t_mesh_data_4_merge
      use t_grp_data_merged_surfaces
      use t_merged_viewer_mesh
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine choose_surface_mesh_para(mesh_file)
!
      use m_node_quad_2_linear_sf
      use find_mesh_file_format
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use single_const_surface_mesh
      use const_surface_data
      use set_parallel_file_name
!
      type(field_IO_params), intent(inout) :: mesh_file
!
      type(element_data), save :: ele_p
      type(surface_data), save :: surf_p
      type(edge_data), save :: edge_p
      type(merged_mesh), save :: mgd_mesh_p
      type(group_data_merged_surf), save :: mgd_sf_grp_p
      type(merged_viewer_mesh), save :: mgd_view_mesh_p
!
!
      if(my_rank .eq. 0) then
        if(iflag_debug .eq. 0) write(*,*) 'find_merged_mesh_format'
        call find_merged_mesh_format(mesh_file)
      end if
      call calypso_mpi_barrier
      call MPI_BCAST(mesh_file%iflag_format, ione,                      &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call const_merged_mesh_para                                       &
     &   (mesh_file, ele_p, surf_p, edge_p, mgd_mesh_p, mgd_sf_grp_p)
!
      call const_surf_mesh_4_viewer                                     &
     &   (surf_p, edge_p, mgd_mesh_p, mgd_sf_grp_p, mgd_view_mesh_p)
!
!
      call collect_surf_mesh_4_viewer                                   &
     &   (mesh_file, surf_p, edge_p, mgd_view_mesh_p)
!
      call deallocate_quad4_2_linear
!
      end subroutine choose_surface_mesh_para
!
!------------------------------------------------------------------
!
      subroutine const_merged_mesh_para                                 &
     &         (mesh_file, ele, surf, edge, mgd_mesh, mgd_sf_grp)
!
      use t_file_IO_parameter
      use load_mesh_data
      use set_group_types_4_IO
      use count_number_with_overlap
      use set_merged_geometry
      use mesh_MPI_IO_select
      use single_const_surface_mesh
      use const_merged_surf_data
      use const_merged_surf_4_group
!
      type(field_IO_params), intent(in) :: mesh_file
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_mesh), intent(inout) :: mgd_mesh
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
      type(mesh_data) :: fem_IO_p
!
!
      mgd_mesh%num_pe = ione
      call alloc_number_of_mesh(mgd_mesh)
      call alloc_subdomain_groups(mgd_mesh)
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call sel_mpi_read_mesh(mesh_file, fem_IO_p)
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
!
      end subroutine const_merged_mesh_para
!
!------------------------------------------------------------------
!
      subroutine collect_surf_mesh_4_viewer                             &
     &         (mesh_file,  surf, edge, mgd_v_mesh_p)
!
      use renumber_para_viewer_mesh
      use viewer_mesh_IO_select
      use const_global_element_ids
!
      type(field_IO_params), intent(in) :: mesh_file
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_viewer_mesh), intent(inout) :: mgd_v_mesh_p
!
      type(mpi_viewer_mesh_param) :: mgd_view_prm
!
      call alloc_mpi_viewer_mesh_param(nprocs, mgd_view_prm)
!
      call count_number_of_node_stack4(mgd_v_mesh_p%inod_sf_stack(1),   &
     &     mgd_view_prm%istack_v_node)
      call count_number_of_node_stack4(mgd_v_mesh_p%isurf_sf_stack(1),  &
     &     mgd_view_prm%istack_v_surf)
      call count_number_of_node_stack4(mgd_v_mesh_p%iedge_sf_stack(1),  &
     &     mgd_view_prm%istack_v_edge)
!
      call s_renumber_para_viewer_mesh                                  &
     &   (mgd_view_prm%istack_v_node(my_rank),  &
     &    mgd_view_prm%istack_v_surf(my_rank),  &
     &    mgd_view_prm%istack_v_edge(my_rank),  &
     &    surf, edge, mgd_v_mesh_p)
!
      call sel_mpi_output_surface_grid                                  &
     &   (mesh_file, surf%nnod_4_surf, edge%nnod_4_edge,                &
     &    mgd_v_mesh_p, mgd_view_prm)
!
      call dealloc_mpi_viewer_mesh_param(mgd_view_prm)
!
      end subroutine collect_surf_mesh_4_viewer
!
! -----------------------------------------------------------------------
!
      end module parallel_const_surface_mesh
