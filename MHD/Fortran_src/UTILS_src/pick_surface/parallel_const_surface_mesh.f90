!>@file   parallel_const_surface_mesh.f90
!!@brief  module parallel_const_surface_mesh
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
      module parallel_const_surface_mesh
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
      integer(kind = kint), parameter, private :: iflag_output_SURF = 0
      integer(kind = kint), parameter, private :: iflag_add_comm_tbl = 1
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
      use find_mesh_file_format
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use single_const_surface_mesh
      use const_surface_data
      use set_parallel_file_name
!
      use const_mesh_list_4_viewer
      use pickup_surface_4_viewer
      use extend_group_table
      use copy_mesh_structures
      use set_parallel_file_name
      use viewer_mesh_data_IO
      use viewer_group_data_IO
!
      type(field_IO_params), intent(inout) :: mesh_file
!
      type(mesh_geometry), save :: mesh_p
      type(mesh_groups), save :: group_p
      type(element_geometry), save :: ele_mesh_p
!
      type(group_data), save :: new_nod_grp
!
      type(merged_viewer_mesh), save :: mgd_v_mesh_s
!
      type(index_list_4_pick_surface), save :: idx_lst_s
      character(len = kchara) :: fname_tmp, file_name
!
!
      mgd_v_mesh_s%surface_file_head = mesh_file%file_prefix
!
      if(my_rank .eq. 0) then
        if(iflag_debug .eq. 0) write(*,*) 'find_merged_mesh_format'
        call find_merged_mesh_format(mesh_file)
      end if
      call calypso_mpi_barrier
      call MPI_BCAST(mesh_file%iflag_format, ione,                      &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
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
!      write(50+my_rank,*) 'iflag_surf_z', ele_mesh_p%surf%numsurf_iso
!      write(50+my_rank,*) 'iflag_surf_z', ele_mesh_p%surf%isf_isolate
!
      call alloc_num_mesh_sf(ione, mgd_v_mesh_s)
!
      call const_viewer_mesh                                            &
     &   (mesh_p, ele_mesh_p, group_p, mgd_v_mesh_s%view_mesh,          &
     &    mgd_v_mesh_s%domain_grps, mgd_v_mesh_s%view_nod_grps,         &
     &    mgd_v_mesh_s%view_ele_grps, mgd_v_mesh_s%view_sf_grps)
!
      mgd_v_mesh_s%inod_sf_stack(1)                                     &
     &      =  mgd_v_mesh_s%view_mesh%nnod_viewer
      mgd_v_mesh_s%isurf_sf_stack(1)                                    &
     &      = mgd_v_mesh_s%view_mesh%nsurf_viewer
      mgd_v_mesh_s%iedge_sf_stack(1)                                    &
     &      = mgd_v_mesh_s%view_mesh%nedge_viewer
!
      call add_int_suffix                                               &
     &     (my_rank, mesh_file%file_prefix, fname_tmp)
      call add_ksm_extension(fname_tmp, file_name)
      write(*,*) 'surface mesh file name: ', trim(file_name)
      open (surface_id, file = file_name)
!
      call write_domain_data_viewer(mgd_v_mesh_s)
      call write_node_data_viewer(mgd_v_mesh_s%view_mesh)
      call write_surf_connect_viewer(ione, mgd_v_mesh_s%isurf_sf_stack, &
       &    ele_mesh_p%surf%nnod_4_surf, mgd_v_mesh_s%view_mesh)
      call write_edge_connect_viewer                                    &
       &   (ele_mesh_p%edge%nnod_4_edge, mgd_v_mesh_s%view_mesh)
!
      call write_domain_group_viewer(ione, mgd_v_mesh_s%domain_grps)
!
      call write_nod_group_viewer(ione, mgd_v_mesh_s%view_nod_grps)
      call write_ele_group_viewer(ione, mgd_v_mesh_s%view_ele_grps)
      call write_surf_group_viewer(ione, mgd_v_mesh_s%view_sf_grps)
      close(surface_id)
!
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
      use viewer_IO_select_4_zlib
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
      write(*,*) 's_renumber_para_viewer_mesh'
      call s_renumber_para_viewer_mesh                                  &
     &   (mgd_view_prm%istack_v_node(my_rank),                          &
     &    mgd_view_prm%istack_v_surf(my_rank),                          &
     &    mgd_view_prm%istack_v_edge(my_rank),                          &
     &    surf, edge, mgd_v_mesh)
!
      call sel_mpi_output_surface_grid                                  &
     &   (mesh_file%iflag_format, surf%nnod_4_surf, edge%nnod_4_edge,   &
     &    mgd_v_mesh, mgd_view_prm)
!
      call dealloc_mpi_viewer_mesh_param(mgd_view_prm)
!
      end subroutine collect_surf_mesh_4_viewer
!
! -----------------------------------------------------------------------
!
      end module parallel_const_surface_mesh
