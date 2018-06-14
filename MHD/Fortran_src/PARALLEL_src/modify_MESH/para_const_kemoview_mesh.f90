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
      use t_mesh_data
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
      integer(kind = kint), parameter :: iflag_write_subdomain = 1
!
      private :: iflag_output_SURF, iflag_add_comm_tbl
      private :: iflag_write_subdomain
      private :: collect_surf_mesh_4_viewer
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
!
      use const_viewer_mesh
      use add_comm_table_in_node_grp
      use copy_mesh_structures
      use viewer_mesh_IO_select
!
      type(field_IO_params), intent(inout) :: mesh_file
!
      type(mesh_geometry), save :: mesh_p
      type(mesh_groups), save :: group_p
      type(element_geometry), save :: ele_mesh_p
!
      type(merged_viewer_mesh), save :: mgd_v_mesh_s
      type(group_data), save :: new_nod_grp
!
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(mesh_file, nprocs, mesh_p, group_p,           &
     &    ele_mesh_p%surf%nnod_4_surf, ele_mesh_p%edge%nnod_4_edge)
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_init_with_IO'
!
      if(iflag_add_comm_tbl .gt. 0) then
        call add_comm_table_in_node_group                               &
     &     (nprocs, mesh_p%nod_comm, group_p%nod_grp, new_nod_grp)
        call deallocate_grp_type(group_p%nod_grp)
        call copy_group_data(new_nod_grp, group_p%nod_grp)
        call deallocate_grp_type(new_nod_grp)
      end if
!
      call FEM_mesh_init_with_IO(iflag_output_SURF,                     &
     &    mesh_file, mesh_p, group_p, ele_mesh_p)
!
      call alloc_num_mesh_sf(ione, mgd_v_mesh_s)
!
      call s_const_viewer_mesh                                          &
     &   (mesh_p, ele_mesh_p, group_p, mgd_v_mesh_s%view_mesh,          &
     &    mgd_v_mesh_s%domain_grps, mgd_v_mesh_s%view_nod_grps,         &
     &    mgd_v_mesh_s%view_ele_grps, mgd_v_mesh_s%view_sf_grps)
!
      if(iflag_write_subdomain .gt. 0) then
        call sel_output_single_surface_grid(my_rank, mesh_file,         &
     &      ele_mesh_p%surf%nnod_4_surf, ele_mesh_p%edge%nnod_4_edge,   &
     &      mgd_v_mesh_s%view_mesh, mgd_v_mesh_s%domain_grps,           &
     &      mgd_v_mesh_s%view_nod_grps, mgd_v_mesh_s%view_ele_grps,     &
     &      mgd_v_mesh_s%view_sf_grps)
      end if
!
      call collect_surf_mesh_4_viewer(mesh_file, ele_mesh_p%surf,       &
     &    ele_mesh_p%edge, mgd_v_mesh_s)
!
      call dealloc_viewer_mesh(mgd_v_mesh_s%view_mesh,                  &
     &    mgd_v_mesh_s%domain_grps, mgd_v_mesh_s%view_nod_grps,         &
     &    mgd_v_mesh_s%view_ele_grps, mgd_v_mesh_s%view_sf_grps)
      call dealloc_num_mesh_sf(mgd_v_mesh_s)
      call dealloc_mesh_infomations(mesh_p, group_p, ele_mesh_p)
!
!      call deallocate_quad4_2_linear
!
      end subroutine pickup_surface_mesh_para
!
!------------------------------------------------------------------
!
      subroutine collect_surf_mesh_4_viewer                             &
     &         (mesh_file, surf, edge, mgd_v_mesh)
!
      use renumber_para_viewer_mesh
      use viewer_mesh_MPI_IO_select
      use const_global_element_ids
!
      type(field_IO_params), intent(in) :: mesh_file
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_viewer_mesh), intent(inout) :: mgd_v_mesh
!
      type(mpi_viewer_mesh_param) :: mgd_view_prm
!
!
      call alloc_mpi_viewer_mesh_param(nprocs, mgd_view_prm)
!
      call count_number_of_node_stack4                                  &
     &   (mgd_v_mesh%view_mesh%nnod_viewer,                             &
     &    mgd_view_prm%istack_v_node)
      call count_number_of_node_stack4                                  &
     &   (mgd_v_mesh%view_mesh%nsurf_viewer,                            &
     &    mgd_view_prm%istack_v_surf)
      call count_number_of_node_stack4                                  &
     &   (mgd_v_mesh%view_mesh%nedge_viewer,                            &
     &    mgd_view_prm%istack_v_edge)
!
      call s_renumber_para_viewer_mesh                                  &
     &   (mgd_view_prm%istack_v_node(my_rank),                          &
     &    mgd_view_prm%istack_v_surf(my_rank),                          &
     &    mgd_view_prm%istack_v_edge(my_rank),                          &
     &    surf, edge, mgd_v_mesh)
!
      call sel_mpi_output_surface_grid                                  &
     &   (mesh_file, surf%nnod_4_surf, edge%nnod_4_edge,                &
     &    mgd_v_mesh, mgd_view_prm)
!
      call dealloc_mpi_viewer_mesh_param(mgd_view_prm)
!
      end subroutine collect_surf_mesh_4_viewer
!
! -----------------------------------------------------------------------
!
      end module para_const_kemoview_mesh
