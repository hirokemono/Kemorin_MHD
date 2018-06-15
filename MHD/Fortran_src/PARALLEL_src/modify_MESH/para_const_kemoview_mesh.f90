!>@file   para_const_kemoview_mesh.f90
!!@brief  module para_const_kemoview_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine pickup_surface_mesh_para(mesh_file)
!!        type(field_IO_params), intent(inout) :: mesh_file
!!@endverbatim
!
      module para_const_kemoview_mesh
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
      use t_merged_viewer_mesh
!
      implicit none
!
      integer(kind = kint), parameter :: iflag_output_SURF = 0
      integer(kind = kint), parameter :: iflag_add_comm_tbl = 1
      integer(kind = kint), parameter :: iflag_write_subdomain = 0
!
      private :: iflag_output_SURF, iflag_add_comm_tbl
      private :: iflag_write_subdomain
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine pickup_surface_mesh_para(mesh_file)
!
      use m_node_quad_2_linear_sf
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use single_const_kemoview_mesh
      use set_parallel_file_name
      use viewer_mesh_IO_select
      use add_comm_table_in_node_grp
!
      type(field_IO_params), intent(inout) :: mesh_file
!
      type(mesh_geometry), save :: mesh1
      type(mesh_groups), save :: group1
!
      type(surface_data), save :: surf_p
      type(edge_data), save :: edge_p
      type(merged_viewer_mesh), save :: mgd_view_mesh_p
!
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(mesh_file, nprocs, mesh1, group1,             &
     &    surf_p%nnod_4_surf, edge_p%nnod_4_edge)
      call allocate_quad4_2_linear(mesh1%ele%nnod_4_ele)
!
      if(iflag_add_comm_tbl .gt. 0) then
        call add_comm_tbl_in_node_grp_mesh(nprocs, mesh1, group1)
      end if
!
      if(my_rank .eq. 0) write(*,*) 'Construct kemoviewer data'
      call const_surf_mesh_4_viewer                                     &
     &   (mesh1, group1, surf_p, edge_p, mgd_view_mesh_p%view_mesh,     &
     &    mgd_view_mesh_p%domain_grps, mgd_view_mesh_p%view_nod_grps,   &
     &    mgd_view_mesh_p%view_ele_grps, mgd_view_mesh_p%view_sf_grps)
!
      call deallocate_iso_surface_type(surf_p)
      call deallocate_ext_surface_type(surf_p)
      call deallocate_surface_connect_type(surf_p)
      call deallocate_inod_in_surf_type(surf_p)
!
      call dealloc_mesh_infos(mesh1, group1)
      call dealloc_inod_in_edge(edge_p)
!
      if(iflag_write_subdomain .gt. 0) then
        call sel_output_single_surface_grid(my_rank, mesh_file,         &
     &      mgd_view_mesh_p%view_mesh, mgd_view_mesh_p%domain_grps,     &
     &      mgd_view_mesh_p%view_nod_grps,                              &
     &      mgd_view_mesh_p%view_ele_grps,                              &
     &      mgd_view_mesh_p%view_sf_grps)
      end if
!
      call collect_surf_mesh_4_viewer(mesh_file, mgd_view_mesh_p)
!
      call deallocate_quad4_2_linear
!
      end subroutine pickup_surface_mesh_para
!
!------------------------------------------------------------------
!
      subroutine collect_surf_mesh_4_viewer(mesh_file, mgd_v_mesh)
!
      use renumber_para_viewer_mesh
      use viewer_mesh_MPI_IO_select
      use const_global_element_ids
!
      type(field_IO_params), intent(in) :: mesh_file
      type(merged_viewer_mesh), intent(inout) :: mgd_v_mesh
!
      type(mpi_viewer_mesh_param) :: mgd_view_prm
!
!
      call alloc_mpi_viewer_mesh_param(nprocs, mgd_view_prm)
!
      call count_number_of_node_stack4                                  &
     &  (mgd_v_mesh%view_mesh%nnod_viewer, mgd_view_prm%istack_v_node)
      call count_number_of_node_stack4                                  &
     &  (mgd_v_mesh%view_mesh%nsurf_viewer, mgd_view_prm%istack_v_surf)
      call count_number_of_node_stack4                                  &
     &  (mgd_v_mesh%view_mesh%nedge_viewer, mgd_view_prm%istack_v_edge)
!
      call s_renumber_para_viewer_mesh                                  &
     &   (mgd_view_prm%istack_v_node(my_rank),                          &
     &    mgd_view_prm%istack_v_surf(my_rank),                          &
     &    mgd_view_prm%istack_v_edge(my_rank), mgd_v_mesh)
!
      call sel_mpi_output_surface_grid                                  &
     &   (mesh_file, mgd_v_mesh, mgd_view_prm)
!
      call dealloc_mpi_viewer_mesh_param(mgd_view_prm)
!
      end subroutine collect_surf_mesh_4_viewer
!
! -----------------------------------------------------------------------
!
      end module para_const_kemoview_mesh
