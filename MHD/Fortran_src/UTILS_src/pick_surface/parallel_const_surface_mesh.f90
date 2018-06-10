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
      type(merged_viewer_mesh), save :: mgd_view_mesh1
!
      type(mesh_geometry), save :: mesh_p
      type(mesh_groups), save :: group_p
      type(element_geometry), save :: ele_mesh_p
!
      type(group_data), save :: new_nod_grp
!
      type(merged_viewer_mesh), save :: mgd_view_mesh_p
!
      integer(kind = kint), allocatable :: inod_ksm(:)
      integer(kind = kint), allocatable :: isurf_ksm(:)
      integer(kind = kint), allocatable :: iedge_ksm(:)
      character(len = kchara) :: fname_tmp, file_name
!
!
      mgd_view_mesh1%surface_file_head = mesh_file%file_prefix
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
      call alloc_num_mesh_sf(ione, mgd_view_mesh_p)
      allocate(inod_ksm(mesh_p%node%numnod))
      allocate(isurf_ksm(ele_mesh_p%surf%numsurf))
      allocate(iedge_ksm(ele_mesh_p%edge%numedge))
      inod_ksm = 0
      isurf_ksm = 0
      iedge_ksm = 0
!
      call s_const_mesh_list_4_viewer                                   &
     &   (mesh_p%node, ele_mesh_p%surf, ele_mesh_p%edge,                &
     &    group_p%nod_grp, group_p%ele_grp, group_p%surf_grp,           &
     &    inod_ksm, isurf_ksm, iedge_ksm,                               &
     &    mgd_view_mesh_p%view_mesh%nnod_viewer,                        &
     &    mgd_view_mesh_p%view_mesh%nsurf_viewer,                       &
     &    mgd_view_mesh_p%view_mesh%nedge_viewer)
!
      call alloc_nod_position_viewer(mgd_view_mesh_p%view_mesh)
      call set_node_position_4_viewer                                   &
     &   (mesh_p%node, inod_ksm, mgd_view_mesh_p%view_mesh)
!
      call alloc_surf_connect_viewer                                    &
     &   (ele_mesh_p%surf%nnod_4_surf, mgd_view_mesh_p%view_mesh)
      call set_surf_connect_viewer(mesh_p%node, ele_mesh_p%surf,        &
     &    inod_ksm, isurf_ksm, mgd_view_mesh_p%view_mesh)
       call set_surf_domain_id_viewer                                   &
     &    (ele_mesh_p%surf, mgd_view_mesh_p%view_mesh)
!
      call alloc_edge_data_4_sf                                         &
     &   (ele_mesh_p%edge%nnod_4_edge, mgd_view_mesh_p%view_mesh)
      call set_edge_connect_viewer                                      &
     &   (mesh_p%node, ele_mesh_p%surf, ele_mesh_p%edge,                &
     &    inod_ksm, isurf_ksm, iedge_ksm, mgd_view_mesh_p%view_mesh)
!
!
      call const_group_lists_4_viewer                                   &
     &   (mesh_p%node, ele_mesh_p%surf, ele_mesh_p%edge, group_p,       &
     &    inod_ksm, isurf_ksm, iedge_ksm, mgd_view_mesh_p%domain_grps,  &
     &    mgd_view_mesh_p%view_nod_grps, mgd_view_mesh_p%view_ele_grps, &
     &    mgd_view_mesh_p%view_sf_grps)
!
      write(*,*) 'number of mesh', my_rank,                             &
     &    mgd_view_mesh_p%view_mesh%nnod_viewer,                        &
     &    mgd_view_mesh_p%view_mesh%nsurf_viewer,                       &
     &    mgd_view_mesh_p%view_mesh%nedge_viewer
      call calypso_mpi_barrier
      write(*,*) 'domain group', my_rank,                               &
     &  mgd_view_mesh_p%domain_grps%node_grp%num_item,                  &
     &  mgd_view_mesh_p%domain_grps%surf_grp%num_item,                  &
     &  mgd_view_mesh_p%domain_grps%edge_grp%num_item
      write(*,*) 'node group', my_rank,                                 &
     &  mgd_view_mesh_p%view_nod_grps%node_grp%num_item
      call calypso_mpi_barrier
      write(*,*) 'element group', my_rank,                              &
     &  mgd_view_mesh_p%view_ele_grps%node_grp%num_item,                              &
     &  mgd_view_mesh_p%view_ele_grps%surf_grp%num_item,                              &
     &  mgd_view_mesh_p%view_ele_grps%edge_grp%num_item
      call calypso_mpi_barrier
      write(*,*) 'surface group', my_rank,                              &
     &  mgd_view_mesh_p%view_sf_grps%node_grp%num_item,                 &
     &  mgd_view_mesh_p%view_sf_grps%surf_grp%num_item,                 &
     &  mgd_view_mesh_p%view_sf_grps%edge_grp%num_item
      call calypso_mpi_barrier
!
      mgd_view_mesh_p%inod_sf_stack(1)                                  &
     &      =  mgd_view_mesh_p%view_mesh%nnod_viewer
      mgd_view_mesh_p%isurf_sf_stack(1)                                 &
     &      = mgd_view_mesh_p%view_mesh%nsurf_viewer
      mgd_view_mesh_p%iedge_sf_stack(1)                                 &
     &      = mgd_view_mesh_p%view_mesh%nedge_viewer
!
      call add_int_suffix                                               &
     &     (my_rank, mesh_file%file_prefix, fname_tmp)
      call add_ksm_extension(fname_tmp, file_name)
      write(*,*) 'surface mesh file name: ', trim(file_name)
      open (surface_id, file = file_name)
!
      call write_domain_data_viewer(mgd_view_mesh_p)
      call write_node_data_viewer(mgd_view_mesh_p%view_mesh)
      call write_surf_connect_viewer                                    &
       &   (ele_mesh_p%surf%nnod_4_surf, mgd_view_mesh_p%view_mesh)
      call write_edge_connect_viewer                                    &
       &   (ele_mesh_p%edge%nnod_4_edge, mgd_view_mesh_p%view_mesh)
!
      call write_domain_group_viewer(ione, mgd_view_mesh_p%domain_grps)
!
      call write_nod_group_viewer(ione, mgd_view_mesh_p%view_nod_grps)
      call write_ele_group_viewer(ione, mgd_view_mesh_p%view_ele_grps)
      call write_surf_group_viewer(ione, mgd_view_mesh_p%view_sf_grps)
      close(surface_id)
!
!
      call collect_surf_mesh_4_viewer(mesh_file, ele_mesh_p%surf,       &
     &    ele_mesh_p%edge, mgd_view_mesh_p, mgd_view_mesh1)
!
!
!
      deallocate(inod_ksm,  isurf_ksm, iedge_ksm)
      call dealloc_mesh_infomations(mesh_p, group_p, ele_mesh_p)
!
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
!      call deallocate_quad4_2_linear
!
      end subroutine pickup_surface_mesh_para
!
!------------------------------------------------------------------
!
      subroutine collect_surf_mesh_4_viewer                             &
     &         (mesh_file,  surf, edge, mgd_v_mesh_p, mgd_view_mesh)
!
      use renumber_para_viewer_mesh
      use viewer_IO_select_4_zlib
      use const_global_element_ids
!
      type(field_IO_params), intent(in) :: mesh_file
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_viewer_mesh), intent(inout) :: mgd_v_mesh_p
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
!
      call alloc_num_mesh_sf(nprocs, mgd_view_mesh)
!
      call count_number_of_node_stack4                                  &
     &   (mgd_v_mesh_p%view_mesh%nnod_viewer,                           &
     &    mgd_view_mesh%inod_sf_stack)
      call count_number_of_node_stack4                                  &
     &   (mgd_v_mesh_p%view_mesh%nsurf_viewer,                          &
     &    mgd_view_mesh%isurf_sf_stack)
      call count_number_of_node_stack4                                  &
     &   (mgd_v_mesh_p%view_mesh%nedge_viewer,                          &
     &    mgd_view_mesh%iedge_sf_stack)
      call num_merged_viewer_nod_surf_edge(mgd_view_mesh)
!
      write(*,*) 's_renumber_para_viewer_mesh'
      call s_renumber_para_viewer_mesh                                  &
     &   (mgd_view_mesh%inod_sf_stack(my_rank),                         &
     &    mgd_view_mesh%isurf_sf_stack(my_rank),                        &
     &    mgd_view_mesh%iedge_sf_stack(my_rank),                        &
     &    surf, edge, mgd_v_mesh_p)
!
      call sel_mpi_output_surface_grid                                  &
     &   (mesh_file%iflag_format, surf%nnod_4_surf, edge%nnod_4_edge,   &
     &    mgd_v_mesh_p, mgd_view_mesh)
!
      call dealloc_num_mesh_sf(mgd_view_mesh)
!
      end subroutine collect_surf_mesh_4_viewer
!
! -----------------------------------------------------------------------
!
      end module parallel_const_surface_mesh
