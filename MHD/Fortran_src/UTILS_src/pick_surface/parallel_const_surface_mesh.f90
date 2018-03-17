!>@file   parallel_const_surface_mesh.f90
!!@brief  module parallel_const_surface_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine choose_surface_mesh_para                             &
!!     &         (mesh_file, ele, surf, edge)
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(element_data), intent(inout) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data), intent(inout) :: edge
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
      subroutine choose_surface_mesh_para                               &
     &         (mesh_file, ele, surf, edge)
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
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
      type(merged_mesh), save :: mgd_mesh1
      type(group_data_merged_surf), save :: mgd_sf_grp1
      type(merged_viewer_mesh), save :: mgd_view_mesh1
!
      type(element_data), save :: ele_p
      type(surface_data), save :: surf_p
      type(edge_data), save :: edge_p
      type(merged_mesh), save :: mgd_mesh_p
      type(group_data_merged_surf), save :: mgd_sf_grp_p
      type(merged_viewer_mesh), save :: mgd_view_mesh_p
!
      character(len=kchara) :: fhead_tmp
      integer(kind = kint) :: i
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
      fhead_tmp = mesh_file%file_prefix
      call const_merged_mesh_para                                       &
     &   (mesh_file, ele_p, surf_p, edge_p, mgd_mesh_p, mgd_sf_grp_p)
!
      call add_int_suffix(my_rank, fhead_tmp,                           &
     &    mgd_view_mesh_p%surface_file_head)
      call const_surf_mesh_4_viewer                                     &
     &   (mesh_file, surf_p, edge_p, mgd_mesh_p, mgd_sf_grp_p,          &
     &    mgd_view_mesh_p)
!
!
      call collect_surf_mesh_4_viewer                                   &
     &   (mesh_file, surf_p, edge_p, mgd_view_mesh_p, mgd_view_mesh1)
!
      call gz_collect_surf_mesh_4_viewer                                &
     &   (mesh_file, surf_p, edge_p, mgd_view_mesh_p, mgd_view_mesh1)
!
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
     &         (mesh_file,  surf, edge, mgd_v_mesh_p, mgd_view_mesh)
!
      use set_merged_geometry
      use const_merged_surf_data
      use const_merged_surf_4_group
      use set_surf_connect_4_viewer
      use set_nodes_4_viewer
      use const_edge_4_viewer
      use set_nodes_4_groups_viewer
      use viewer_IO_select_4_zlib
      use const_global_element_ids
!
      use m_viewer_mesh_labels
      use m_fem_mesh_labels
      use t_calypso_mpi_IO_param
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use MPI_node_geometry_IO
      use MPI_element_connect_IO
!
      type(field_IO_params), intent(in) :: mesh_file
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_viewer_mesh), intent(inout) :: mgd_v_mesh_p
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      character(len=kchara) :: file_name = 'aho.ksm'
      type(calypso_MPI_IO_params) :: IO_param
!
      integer(kind = kint) :: i, k, total_count
!
!  pickup surface and nodes
!
!
      call alloc_num_mesh_sf(nprocs, mgd_view_mesh)
!
      call count_number_of_node_stack4(mgd_v_mesh_p%inod_sf_stack(1),   &
     &     mgd_view_mesh%inod_sf_stack)
      call count_number_of_node_stack4(mgd_v_mesh_p%isurf_sf_stack(1),  &
     &     mgd_view_mesh%isurf_sf_stack)
      call count_number_of_node_stack4(mgd_v_mesh_p%iedge_sf_stack(1),  &
     &     mgd_view_mesh%iedge_sf_stack)
!
      call num_merged_viewer_nod_surf_edge(mgd_view_mesh)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write ascii mesh file: ', trim(file_name)
!
!
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs, my_rank, IO_param)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_ndomain_viewer()), hd_ndomain_viewer())
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(mgd_view_mesh%num_pe_sf))
      call mpi_write_stack_over_domain                                  &
     &   (IO_param, mgd_v_mesh_p%view_mesh%nodpetot_viewer)
      call mpi_write_stack_over_domain                                  &
     &   (IO_param, mgd_v_mesh_p%view_mesh%surfpetot_viewer)
      call mpi_write_stack_over_domain                                  &
     &   (IO_param, mgd_v_mesh_p%view_mesh%edgepetot_viewer)
!
      do i = 1, mgd_v_mesh_p%view_mesh%nodpetot_viewer
        mgd_v_mesh_p%view_mesh%inod_gl_view(i)                          &
     &          = i + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
!
      do i = 1, mgd_v_mesh_p%view_mesh%surfpetot_viewer
        mgd_v_mesh_p%view_mesh%isurf_gl_view(i)                         &
     &          = i + mgd_view_mesh%isurf_sf_stack(my_rank)
      end do
      do k = 1, surf%nnod_4_surf
        do i = 1, mgd_v_mesh_p%view_mesh%surfpetot_viewer
          mgd_v_mesh_p%view_mesh%ie_sf_viewer(i,k)                      &
     &          = mgd_v_mesh_p%view_mesh%ie_sf_viewer(i,k)              &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
        end do
      end do
!
      do i = 1, mgd_v_mesh_p%view_mesh%edgepetot_viewer
        mgd_v_mesh_p%view_mesh%iedge_gl_view(i)                         &
     &          = i + mgd_view_mesh%iedge_sf_stack(my_rank)
      end do
      do k = 1, edge%nnod_4_edge
        do i = 1, mgd_v_mesh_p%view_mesh%edgepetot_viewer
          mgd_v_mesh_p%view_mesh%ie_edge_viewer(i,k)                    &
     &          = mgd_v_mesh_p%view_mesh%ie_edge_viewer(i,k)            &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
        end do
      end do
      do k = 1, nedge_4_surf
        do i = 1, mgd_v_mesh_p%view_mesh%surfpetot_viewer
          if(mgd_v_mesh_p%view_mesh%iedge_sf_viewer(i,k) .gt. 0) then
            mgd_v_mesh_p%view_mesh%iedge_sf_viewer(i,k)                 &
     &          = mgd_v_mesh_p%view_mesh%iedge_sf_viewer(i,k)           &
     &           + mgd_view_mesh%iedge_sf_stack(my_rank)
          else
            mgd_v_mesh_p%view_mesh%iedge_sf_viewer(i,k)                 &
     &          = mgd_v_mesh_p%view_mesh%iedge_sf_viewer(i,k)           &
     &           - mgd_view_mesh%iedge_sf_stack(my_rank)
          end if
        end do
      end do
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_node_viewer()), hd_node_viewer())
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(mgd_view_mesh%view_mesh%nodpetot_viewer))
!
!
      call mpi_write_viewer_position(IO_param,                          &
     &    mgd_v_mesh_p%view_mesh%nodpetot_viewer, ithree,               &
     &    mgd_v_mesh_p%view_mesh%inod_gl_view,                          &
     &    mgd_v_mesh_p%view_mesh%xx_view)
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_surf_viewer()), hd_surf_viewer())
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(mgd_view_mesh%view_mesh%surfpetot_viewer))
!
      call mpi_write_viewer_element_type                                &
     &   (IO_param, iten, mgd_v_mesh_p%view_mesh%surfpetot_viewer,      &
     &    mgd_v_mesh_p%view_mesh%surftyp_viewer)
!
      call mpi_write_viewer_connect                                     &
     &   (IO_param, mgd_v_mesh_p%view_mesh%surfpetot_viewer,            &
     &    surf%nnod_4_surf, mgd_v_mesh_p%view_mesh%isurf_gl_view,       &
     &    mgd_v_mesh_p%view_mesh%ie_sf_viewer)
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_edge_viewer()), hd_edge_viewer())
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(mgd_view_mesh%view_mesh%edgepetot_viewer))
!
      call mpi_write_viewer_connect                                     &
     &   (IO_param, mgd_v_mesh_p%view_mesh%edgepetot_viewer,            &
     &    edge%nnod_4_edge, mgd_v_mesh_p%view_mesh%iedge_gl_view,       &
     &    mgd_v_mesh_p%view_mesh%ie_edge_viewer)
!
!
      call mpi_write_charahead(IO_param,                                &
     &    len(hd_edge_on_sf_viewer()), hd_edge_on_sf_viewer())
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(mgd_view_mesh%view_mesh%surfpetot_viewer))
!
!
      call mpi_write_viewer_connect(IO_param,                           &
     &    mgd_v_mesh_p%view_mesh%surfpetot_viewer, nedge_4_surf,        &
     &    mgd_v_mesh_p%view_mesh%isurf_gl_view,                         &
     &    mgd_v_mesh_p%view_mesh%iedge_sf_viewer)
!
!
!
      do i = 1, mgd_v_mesh_p%domain_grps%node_grp%num_item
        mgd_v_mesh_p%domain_grps%node_grp%item_sf(i)                    &
     &          = mgd_v_mesh_p%domain_grps%node_grp%item_sf(i)          &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%domain_grps%surf_grp%num_item
        mgd_v_mesh_p%domain_grps%surf_grp%item_sf(i)                    &
     &          = mgd_v_mesh_p%domain_grps%surf_grp%item_sf(i)          &
     &           + mgd_view_mesh%isurf_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%domain_grps%edge_grp%num_item
        mgd_v_mesh_p%domain_grps%edge_grp%item_sf(i)                    &
     &          = mgd_v_mesh_p%domain_grps%edge_grp%item_sf(i)          &
     &           + mgd_view_mesh%iedge_sf_stack(my_rank)
      end do
!
      do i = 1, mgd_v_mesh_p%view_nod_grps%node_grp%num_item
        mgd_v_mesh_p%view_nod_grps%node_grp%item_sf(i)                  &
     &          = mgd_v_mesh_p%view_nod_grps%node_grp%item_sf(i)        &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
!
      do i = 1, mgd_v_mesh_p%view_ele_grps%node_grp%num_item
        mgd_v_mesh_p%view_ele_grps%node_grp%item_sf(i)                  &
     &          = mgd_v_mesh_p%view_ele_grps%node_grp%item_sf(i)        &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%view_ele_grps%surf_grp%num_item
        mgd_v_mesh_p%view_ele_grps%surf_grp%item_sf(i)                  &
     &          = mgd_v_mesh_p%view_ele_grps%surf_grp%item_sf(i)        &
     &           + mgd_view_mesh%isurf_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%view_ele_grps%edge_grp%num_item
        if(mgd_v_mesh_p%view_ele_grps%edge_grp%item_sf(i) .gt. 0) then
          mgd_v_mesh_p%view_ele_grps%edge_grp%item_sf(i)                &
     &          = mgd_v_mesh_p%view_ele_grps%edge_grp%item_sf(i)        &
     &           + mgd_view_mesh%iedge_sf_stack(my_rank)
        else
          mgd_v_mesh_p%view_ele_grps%edge_grp%item_sf(i)                &
     &          = mgd_v_mesh_p%view_ele_grps%edge_grp%item_sf(i)        &
     &           - mgd_view_mesh%iedge_sf_stack(my_rank)
        end if
      end do
!
      do i = 1, mgd_v_mesh_p%view_sf_grps%node_grp%num_item
        mgd_v_mesh_p%view_sf_grps%node_grp%item_sf(i)                   &
     &          = mgd_v_mesh_p%view_sf_grps%node_grp%item_sf(i)         &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%view_sf_grps%surf_grp%num_item
        mgd_v_mesh_p%view_sf_grps%surf_grp%item_sf(i)                   &
     &          = mgd_v_mesh_p%view_sf_grps%surf_grp%item_sf(i)         &
     &           + mgd_view_mesh%isurf_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%view_sf_grps%edge_grp%num_item
        if(mgd_v_mesh_p%view_sf_grps%edge_grp%item_sf(i) .gt. 0) then
          mgd_v_mesh_p%view_sf_grps%edge_grp%item_sf(i)                 &
     &          = mgd_v_mesh_p%view_sf_grps%edge_grp%item_sf(i)         &
     &           + mgd_view_mesh%iedge_sf_stack(my_rank)
        else
          mgd_v_mesh_p%view_sf_grps%edge_grp%item_sf(i)                 &
     &          = mgd_v_mesh_p%view_sf_grps%edge_grp%item_sf(i)         &
     &           - mgd_view_mesh%iedge_sf_stack(my_rank)
        end if
      end do
!
      call MPI_allREDUCE(mgd_v_mesh_p%domain_grps%node_grp%num_item,    &
     &    total_count, ione, CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM,    &
     &    ierr_MPI)
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_domain_nod_grp()), hd_domain_nod_grp())
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(total_count))
      call mpi_write_stack_over_domain                                  &
     &   (IO_param, mgd_v_mesh_p%domain_grps%node_grp%num_item)
      call mpi_write_viewer_grp_item(IO_param, ieight,                  &
     &    mgd_v_mesh_p%domain_grps%node_grp%num_item,                   &
     &    mgd_v_mesh_p%domain_grps%node_grp%item_sf)
!
      call MPI_allREDUCE(mgd_v_mesh_p%domain_grps%surf_grp%num_item,    &
     &    total_count, ione, CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM,    &
     &    ierr_MPI)
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_domain_surf_grp()), hd_domain_surf_grp())
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(total_count))
      call mpi_write_stack_over_domain                                  &
     &   (IO_param, mgd_v_mesh_p%domain_grps%surf_grp%num_item)
      call mpi_write_viewer_grp_item(IO_param, ieight,                  &
     &    mgd_v_mesh_p%domain_grps%surf_grp%num_item,                   &
     &    mgd_v_mesh_p%domain_grps%surf_grp%item_sf)
!
      call MPI_allREDUCE(mgd_v_mesh_p%domain_grps%edge_grp%num_item,    &
     &    total_count, ione, CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM,    &
     &    ierr_MPI)
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_domain_edge_grp()), hd_domain_edge_grp())
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(total_count))
      call mpi_write_stack_over_domain                                  &
     &   (IO_param, mgd_v_mesh_p%domain_grps%edge_grp%num_item)
      call mpi_write_viewer_grp_item(IO_param, ieight,                  &
     &    mgd_v_mesh_p%domain_grps%edge_grp%num_item,                   &
     &    mgd_v_mesh_p%domain_grps%edge_grp%item_sf)
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_nodgrp()), hd_fem_nodgrp())
      call mpi_write_viewer_grp_data                                    &
     &   (IO_param, mgd_v_mesh_p%view_nod_grps%num_grp,                 &
     &    mgd_v_mesh_p%view_nod_grps%grp_name,                          &
     &    mgd_v_mesh_p%view_nod_grps%node_grp)
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_elegrp()), hd_fem_elegrp())
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_ele_surf_grp()), hd_ele_surf_grp())
      call mpi_write_viewer_grp_data                                    &
     &   (IO_param, mgd_v_mesh_p%view_ele_grps%num_grp,                 &
     &    mgd_v_mesh_p%view_ele_grps%grp_name,                          &
     &    mgd_v_mesh_p%view_ele_grps%surf_grp)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_ele_nod_grp()), hd_ele_nod_grp())
      call mpi_write_viewer_grp_data                                    &
     &   (IO_param, mgd_v_mesh_p%view_ele_grps%num_grp,                 &
     &    mgd_v_mesh_p%view_ele_grps%grp_name,                          &
     &    mgd_v_mesh_p%view_ele_grps%node_grp)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_ele_edge_grp()), hd_ele_edge_grp())
      call mpi_write_viewer_grp_data                                    &
     &   (IO_param, mgd_v_mesh_p%view_ele_grps%num_grp,                 &
     &    mgd_v_mesh_p%view_ele_grps%grp_name,                          &
     &    mgd_v_mesh_p%view_ele_grps%edge_grp)
!
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_sfgrp()), hd_fem_sfgrp())
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_surf_surf_grp()), hd_surf_surf_grp())
      call mpi_write_viewer_grp_data                                    &
     &   (IO_param, mgd_v_mesh_p%view_sf_grps%num_grp,                  &
     &    mgd_v_mesh_p%view_sf_grps%grp_name,                           &
     &    mgd_v_mesh_p%view_sf_grps%surf_grp)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_surf_nod_grp()), hd_surf_nod_grp())
      call mpi_write_viewer_grp_data                                    &
     &   (IO_param, mgd_v_mesh_p%view_sf_grps%num_grp,                  &
     &    mgd_v_mesh_p%view_sf_grps%grp_name,                           &
     &    mgd_v_mesh_p%view_sf_grps%node_grp)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_surf_edge_grp()), hd_surf_edge_grp())
      call mpi_write_viewer_grp_data                                    &
     &   (IO_param, mgd_v_mesh_p%view_sf_grps%num_grp,                  &
     &    mgd_v_mesh_p%view_sf_grps%grp_name,                           &
     &    mgd_v_mesh_p%view_sf_grps%edge_grp)
!
!
      call close_mpi_file(IO_param)
      call dealloc_num_mesh_sf(mgd_view_mesh)
!
      end subroutine collect_surf_mesh_4_viewer
!
!------------------------------------------------------------------
!
      subroutine mpi_write_viewer_grp_data                              &
     &         (IO_param, num_grp, grp_name, view_grp)
!
      use t_calypso_mpi_IO_param
      use data_IO_to_textline
      use MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num_grp
      character(len=kchara), intent(in) :: grp_name(num_grp)
      type(viewer_group_data), intent(inout) :: view_grp
!
      integer(kind = kint) :: i, ist, ied, num, ntot, ip
      integer(kind = kint) :: num_global(nprocs)
!
!
      call mpi_write_charahead(IO_param, len_int_txt,                   &
     &    integer_textline(num_grp))
!
      ntot = 0
      do i = 1, num_grp
        num = view_grp%istack_sf(i) - view_grp%istack_sf(i-1)
        call MPI_Allgather(num, ione, CALYPSO_INTEGER, num_global,      &
     &      ione, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
        do ip = 2, nprocs
          num_global(ip) = num_global(ip-1) + num_global(ip)
        end do
        num = ntot + num_global(my_rank+1)
        ntot = ntot + num_global(nprocs)
        call mpi_write_num_of_data(IO_param, num)
      end do
!
      do i = 1, num_grp
        call mpi_write_charahead(IO_param,                              &
     &      len_one_word_textline(grp_name(i)),                         &
     &      one_word_textline(grp_name(i)))
!
        ist = view_grp%istack_sf(i-1) + 1
        num = view_grp%istack_sf(i) - view_grp%istack_sf(i-1)
        call mpi_write_viewer_grp_item                                  &
     &     (IO_param, ieight, num, view_grp%item_sf(ist))
      end do
!
      end subroutine mpi_write_viewer_grp_data
!
!------------------------------------------------------------------
!
      subroutine mpi_write_viewer_position                              &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      use t_calypso_mpi_IO_param
      use data_IO_to_textline
      use MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: xx(nnod, numdir)
!
      integer(kind = kint) :: i, led, ilength
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength = len_int8_and_vector_textline(numdir)
      led = nnod * len_int8_and_vector_textline(numdir)
      call set_istack_4_parallell_data(led, IO_param)
!      call mpi_write_stack_over_domain(IO_param, led)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nnod .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else
        do i = 1, nnod
          xx_tmp(1:numdir) = xx(i,1:numdir)
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        int8_and_vector_textline(id_global(i), numdir, xx_tmp))
        end do
      end if
      call calypso_mpi_barrier
!
      end subroutine mpi_write_viewer_position
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_viewer_element_type                          &
     &         (IO_param, ncolumn, num, int_dat)
!
      use t_calypso_mpi_IO_param
      use data_IO_to_textline
      use MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, loop, led
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(num .le. 0) then
        led = ione
      else if(num .gt. 0) then
        nrest = mod((num-1),ncolumn) + 1
        loop = (num-1)/ncolumn
        led = len_multi_6digit_line(ncolumn) * loop                     &
     &       + len_multi_6digit_line(nrest)
      end if
!
!      call mpi_write_stack_over_domain(IO_param, led)
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(num .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else if(num .gt. 0) then
        do i = 0, (num-1)/ncolumn - 1
          call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,  &
     &        len_multi_6digit_line(ncolumn),                           &
     &        mul_6digit_int_line(ncolumn, int_dat(ncolumn*i+1)))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      len_multi_6digit_line(nrest),                               &
     &      mul_6digit_int_line(nrest, int_dat(num-nrest+1)))
      end if
!
      end subroutine mpi_write_viewer_element_type
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_viewer_connect                               &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      use t_calypso_mpi_IO_param
      use data_IO_to_textline
      use MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(in) :: id_global(nele)
      integer(kind=kint), intent(in) :: ie(nele,nnod_4_ele)
!
      integer(kind = kint) :: i, led, ilength
      integer(kind = kint) :: ie_tmp(nnod_4_ele)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      ilength = len_int8_and_mul_int_textline(nnod_4_ele)
!
      if(nele .le. 0) then
        led = ione
      else
        led = ilength * nele
      end if
!
!      call mpi_write_stack_over_domain(IO_param, led)
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(nele .le. 0) then
        call calypso_mpi_seek_write_chara                               &
     &     (IO_param%id_file, ioffset, ione, char(10))
      else
        do i = 1, nele
          ie_tmp(1:nnod_4_ele) = ie(i,1:nnod_4_ele)
          call calypso_mpi_seek_write_chara                             &
     &       (IO_param%id_file, ioffset, ilength,                       &
     &        int8_and_mul_int_textline(id_global(i),                   &
     &                                  nnod_4_ele, ie_tmp))
        end do
      end if
!
      end subroutine mpi_write_viewer_connect
!
! -----------------------------------------------------------------------
!
      subroutine mpi_write_viewer_grp_item                              &
     &         (IO_param, ncolumn, num, int_dat)
!
      use t_calypso_mpi_IO_param
      use data_IO_to_textline
      use MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest, loop, led
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(num .le. 0) then
!        led = ione
         led = izero
      else if(num .gt. 0) then
        nrest = mod((num-1),ncolumn) + 1
        loop = (num-1)/ncolumn
        led = len_multi_int_textline(ncolumn) * loop                    &
     &       + len_multi_int_textline(nrest)
      end if
!
!      call mpi_write_stack_over_domain(IO_param, led)
      call set_istack_4_parallell_data(led, IO_param)
!
      ioffset = IO_param%ioff_gl                                        &
     &         + IO_param%istack_merged(IO_param%id_rank)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &         + IO_param%istack_merged(IO_param%nprocs_in)
!
      if(IO_param%id_rank .ge. IO_param%nprocs_in) return
      if(num .gt. 0) then
        do i = 0, (num-1)/ncolumn - 1
          call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,  &
     &        len_multi_int_textline(ncolumn),                          &
     &        multi_int_textline(ncolumn, int_dat(ncolumn*i+1)))
        end do
        nrest = mod((num-1),ncolumn) + 1
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &      len_multi_int_textline(nrest),                              &
     &      multi_int_textline(nrest, int_dat(num-nrest+1)))
!      else
!        call calypso_mpi_seek_write_chara                              &
!     &     (IO_param%id_file, ioffset, ione, char(10))
      end if
!
      end subroutine mpi_write_viewer_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_collect_surf_mesh_4_viewer                          &
     &         (mesh_file,  surf, edge, mgd_v_mesh_p, mgd_view_mesh)
!
      use set_merged_geometry
      use const_merged_surf_data
      use const_merged_surf_4_group
      use set_surf_connect_4_viewer
      use set_nodes_4_viewer
      use const_edge_4_viewer
      use set_nodes_4_groups_viewer
      use viewer_IO_select_4_zlib
      use const_global_element_ids
!
      use m_viewer_mesh_labels
      use m_fem_mesh_labels
      use t_calypso_mpi_IO_param
      use MPI_domain_data_IO
      use MPI_ascii_data_IO
      use gz_MPI_ascii_data_IO
      use MPI_node_geometry_IO
      use MPI_element_connect_IO
!
      type(field_IO_params), intent(in) :: mesh_file
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_viewer_mesh), intent(inout) :: mgd_v_mesh_p
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      character(len=kchara) :: file_name = 'baka.ksm.gz'
      type(calypso_MPI_IO_params) :: IO_param
!
      integer(kind = kint) :: i, k, total_count
!
!  pickup surface and nodes
!
!
      call alloc_num_mesh_sf(nprocs, mgd_view_mesh)
!
      call count_number_of_node_stack4(mgd_v_mesh_p%inod_sf_stack(1),   &
     &     mgd_view_mesh%inod_sf_stack)
      call count_number_of_node_stack4(mgd_v_mesh_p%isurf_sf_stack(1),  &
     &     mgd_view_mesh%isurf_sf_stack)
      call count_number_of_node_stack4(mgd_v_mesh_p%iedge_sf_stack(1),  &
     &     mgd_view_mesh%iedge_sf_stack)
!
      call num_merged_viewer_nod_surf_edge(mgd_view_mesh)
!
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped ascii mesh file: ', trim(file_name)
!
      call open_write_mpi_file                                          &
     &   (file_name, nprocs, my_rank, IO_param)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ndomain_viewer()), hd_ndomain_viewer())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(mgd_view_mesh%num_pe_sf))
      call gz_mpi_write_stack_over_domain                               &
     &   (IO_param, mgd_v_mesh_p%view_mesh%nodpetot_viewer)
      call gz_mpi_write_stack_over_domain                               &
     &   (IO_param, mgd_v_mesh_p%view_mesh%surfpetot_viewer)
      call gz_mpi_write_stack_over_domain                               &
     &   (IO_param, mgd_v_mesh_p%view_mesh%edgepetot_viewer)
!
      go to 10

      do i = 1, mgd_v_mesh_p%view_mesh%nodpetot_viewer
        mgd_v_mesh_p%view_mesh%inod_gl_view(i)                          &
     &          = i + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
!
      do i = 1, mgd_v_mesh_p%view_mesh%surfpetot_viewer
        mgd_v_mesh_p%view_mesh%isurf_gl_view(i)                         &
     &          = i + mgd_view_mesh%isurf_sf_stack(my_rank)
      end do
      do k = 1, surf%nnod_4_surf
        do i = 1, mgd_v_mesh_p%view_mesh%surfpetot_viewer
          mgd_v_mesh_p%view_mesh%ie_sf_viewer(i,k)                      &
     &          = mgd_v_mesh_p%view_mesh%ie_sf_viewer(i,k)              &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
        end do
      end do
!
      do i = 1, mgd_v_mesh_p%view_mesh%edgepetot_viewer
        mgd_v_mesh_p%view_mesh%iedge_gl_view(i)                         &
     &          = i + mgd_view_mesh%iedge_sf_stack(my_rank)
      end do
      do k = 1, edge%nnod_4_edge
        do i = 1, mgd_v_mesh_p%view_mesh%edgepetot_viewer
          mgd_v_mesh_p%view_mesh%ie_edge_viewer(i,k)                    &
     &          = mgd_v_mesh_p%view_mesh%ie_edge_viewer(i,k)            &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
        end do
      end do
      do k = 1, nedge_4_surf
        do i = 1, mgd_v_mesh_p%view_mesh%surfpetot_viewer
          if(mgd_v_mesh_p%view_mesh%iedge_sf_viewer(i,k) .gt. 0) then
            mgd_v_mesh_p%view_mesh%iedge_sf_viewer(i,k)                 &
     &          = mgd_v_mesh_p%view_mesh%iedge_sf_viewer(i,k)           &
     &           + mgd_view_mesh%iedge_sf_stack(my_rank)
          else
            mgd_v_mesh_p%view_mesh%iedge_sf_viewer(i,k)                 &
     &          = mgd_v_mesh_p%view_mesh%iedge_sf_viewer(i,k)           &
     &           - mgd_view_mesh%iedge_sf_stack(my_rank)
          end if
        end do
      end do
!
      do i = 1, mgd_v_mesh_p%domain_grps%node_grp%num_item
        mgd_v_mesh_p%domain_grps%node_grp%item_sf(i)                    &
     &          = mgd_v_mesh_p%domain_grps%node_grp%item_sf(i)          &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%domain_grps%surf_grp%num_item
        mgd_v_mesh_p%domain_grps%surf_grp%item_sf(i)                    &
     &          = mgd_v_mesh_p%domain_grps%surf_grp%item_sf(i)          &
     &           + mgd_view_mesh%isurf_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%domain_grps%edge_grp%num_item
        mgd_v_mesh_p%domain_grps%edge_grp%item_sf(i)                    &
     &          = mgd_v_mesh_p%domain_grps%edge_grp%item_sf(i)          &
     &           + mgd_view_mesh%iedge_sf_stack(my_rank)
      end do
!
      do i = 1, mgd_v_mesh_p%view_nod_grps%node_grp%num_item
        mgd_v_mesh_p%view_nod_grps%node_grp%item_sf(i)                  &
     &          = mgd_v_mesh_p%view_nod_grps%node_grp%item_sf(i)        &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
!
      do i = 1, mgd_v_mesh_p%view_ele_grps%node_grp%num_item
        mgd_v_mesh_p%view_ele_grps%node_grp%item_sf(i)                  &
     &          = mgd_v_mesh_p%view_ele_grps%node_grp%item_sf(i)        &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%view_ele_grps%surf_grp%num_item
        mgd_v_mesh_p%view_ele_grps%surf_grp%item_sf(i)                  &
     &          = mgd_v_mesh_p%view_ele_grps%surf_grp%item_sf(i)        &
     &           + mgd_view_mesh%isurf_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%view_ele_grps%edge_grp%num_item
        if(mgd_v_mesh_p%view_ele_grps%edge_grp%item_sf(i) .gt. 0) then
          mgd_v_mesh_p%view_ele_grps%edge_grp%item_sf(i)                &
     &          = mgd_v_mesh_p%view_ele_grps%edge_grp%item_sf(i)        &
     &           + mgd_view_mesh%iedge_sf_stack(my_rank)
        else
          mgd_v_mesh_p%view_ele_grps%edge_grp%item_sf(i)                &
     &          = mgd_v_mesh_p%view_ele_grps%edge_grp%item_sf(i)        &
     &           - mgd_view_mesh%iedge_sf_stack(my_rank)
        end if
      end do
!
      do i = 1, mgd_v_mesh_p%view_sf_grps%node_grp%num_item
        mgd_v_mesh_p%view_sf_grps%node_grp%item_sf(i)                   &
     &          = mgd_v_mesh_p%view_sf_grps%node_grp%item_sf(i)         &
     &           + mgd_view_mesh%inod_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%view_sf_grps%surf_grp%num_item
        mgd_v_mesh_p%view_sf_grps%surf_grp%item_sf(i)                   &
     &          = mgd_v_mesh_p%view_sf_grps%surf_grp%item_sf(i)         &
     &           + mgd_view_mesh%isurf_sf_stack(my_rank)
      end do
      do i = 1, mgd_v_mesh_p%view_sf_grps%edge_grp%num_item
        if(mgd_v_mesh_p%view_sf_grps%edge_grp%item_sf(i) .gt. 0) then
          mgd_v_mesh_p%view_sf_grps%edge_grp%item_sf(i)                 &
     &          = mgd_v_mesh_p%view_sf_grps%edge_grp%item_sf(i)         &
     &           + mgd_view_mesh%iedge_sf_stack(my_rank)
        else
          mgd_v_mesh_p%view_sf_grps%edge_grp%item_sf(i)                 &
     &          = mgd_v_mesh_p%view_sf_grps%edge_grp%item_sf(i)         &
     &           - mgd_view_mesh%iedge_sf_stack(my_rank)
        end if
      end do
      
 10   continue
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_node_viewer()), hd_node_viewer())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(mgd_view_mesh%view_mesh%nodpetot_viewer))
!
!
      call gz_mpi_write_viewer_position(IO_param,                       &
     &    mgd_v_mesh_p%view_mesh%nodpetot_viewer, ithree,               &
     &    mgd_v_mesh_p%view_mesh%inod_gl_view,                          &
     &    mgd_v_mesh_p%view_mesh%xx_view)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_surf_viewer()), hd_surf_viewer())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(mgd_view_mesh%view_mesh%surfpetot_viewer))
!
      call gz_mpi_write_viewer_element_type                             &
     &   (IO_param, iten, mgd_v_mesh_p%view_mesh%surfpetot_viewer,      &
     &    mgd_v_mesh_p%view_mesh%surftyp_viewer)
!
      call gz_mpi_write_viewer_connect                                  &
     &   (IO_param, mgd_v_mesh_p%view_mesh%surfpetot_viewer,            &
     &    surf%nnod_4_surf, mgd_v_mesh_p%view_mesh%isurf_gl_view,       &
     &    mgd_v_mesh_p%view_mesh%ie_sf_viewer)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_edge_viewer()), hd_edge_viewer())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(mgd_view_mesh%view_mesh%edgepetot_viewer))
!
      call gz_mpi_write_viewer_connect                                  &
     &   (IO_param, mgd_v_mesh_p%view_mesh%edgepetot_viewer,            &
     &    edge%nnod_4_edge, mgd_v_mesh_p%view_mesh%iedge_gl_view,       &
     &    mgd_v_mesh_p%view_mesh%ie_edge_viewer)
!
!
      call gz_mpi_write_charahead(IO_param,                             &
     &    len(hd_edge_on_sf_viewer()), hd_edge_on_sf_viewer())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(mgd_view_mesh%view_mesh%surfpetot_viewer))
!
!
      call gz_mpi_write_viewer_connect(IO_param,                        &
     &    mgd_v_mesh_p%view_mesh%surfpetot_viewer, nedge_4_surf,        &
     &    mgd_v_mesh_p%view_mesh%isurf_gl_view,                         &
     &    mgd_v_mesh_p%view_mesh%iedge_sf_viewer)
!
!
!
      call MPI_allREDUCE(mgd_v_mesh_p%domain_grps%node_grp%num_item,    &
     &    total_count, ione, CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM,    &
     &    ierr_MPI)
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_domain_nod_grp()), hd_domain_nod_grp())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(total_count))
      call gz_mpi_write_stack_over_domain                               &
     &   (IO_param, mgd_v_mesh_p%domain_grps%node_grp%num_item)
      call gz_mpi_write_viewer_grp_item(IO_param, ieight,               &
     &    mgd_v_mesh_p%domain_grps%node_grp%num_item,                   &
     &    mgd_v_mesh_p%domain_grps%node_grp%item_sf)
!
      call MPI_allREDUCE(mgd_v_mesh_p%domain_grps%surf_grp%num_item,    &
     &    total_count, ione, CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM,    &
     &    ierr_MPI)
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_domain_surf_grp()), hd_domain_surf_grp())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(total_count))
      call gz_mpi_write_stack_over_domain                               &
     &   (IO_param, mgd_v_mesh_p%domain_grps%surf_grp%num_item)
      call gz_mpi_write_viewer_grp_item(IO_param, ieight,               &
     &    mgd_v_mesh_p%domain_grps%surf_grp%num_item,                   &
     &    mgd_v_mesh_p%domain_grps%surf_grp%item_sf)
!
      call MPI_allREDUCE(mgd_v_mesh_p%domain_grps%edge_grp%num_item,    &
     &    total_count, ione, CALYPSO_INTEGER, MPI_SUM, CALYPSO_COMM,    &
     &    ierr_MPI)
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_domain_edge_grp()), hd_domain_edge_grp())
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(total_count))
      call gz_mpi_write_stack_over_domain                               &
     &   (IO_param, mgd_v_mesh_p%domain_grps%edge_grp%num_item)
      call gz_mpi_write_viewer_grp_item(IO_param, ieight,               &
     &    mgd_v_mesh_p%domain_grps%edge_grp%num_item,                   &
     &    mgd_v_mesh_p%domain_grps%edge_grp%item_sf)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_nodgrp()), hd_fem_nodgrp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh_p%view_nod_grps%num_grp,                 &
     &    mgd_v_mesh_p%view_nod_grps%grp_name,                          &
     &    mgd_v_mesh_p%view_nod_grps%node_grp)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_elegrp()), hd_fem_elegrp())
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ele_surf_grp()), hd_ele_surf_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh_p%view_ele_grps%num_grp,                 &
     &    mgd_v_mesh_p%view_ele_grps%grp_name,                          &
     &    mgd_v_mesh_p%view_ele_grps%surf_grp)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ele_nod_grp()), hd_ele_nod_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh_p%view_ele_grps%num_grp,                 &
     &    mgd_v_mesh_p%view_ele_grps%grp_name,                          &
     &    mgd_v_mesh_p%view_ele_grps%node_grp)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_ele_edge_grp()), hd_ele_edge_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh_p%view_ele_grps%num_grp,                 &
     &    mgd_v_mesh_p%view_ele_grps%grp_name,                          &
     &    mgd_v_mesh_p%view_ele_grps%edge_grp)
!
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_fem_sfgrp()), hd_fem_sfgrp())
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_surf_surf_grp()), hd_surf_surf_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh_p%view_sf_grps%num_grp,                  &
     &    mgd_v_mesh_p%view_sf_grps%grp_name,                           &
     &    mgd_v_mesh_p%view_sf_grps%surf_grp)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_surf_nod_grp()), hd_surf_nod_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh_p%view_sf_grps%num_grp,                  &
     &    mgd_v_mesh_p%view_sf_grps%grp_name,                           &
     &    mgd_v_mesh_p%view_sf_grps%node_grp)
!
      call gz_mpi_write_charahead                                       &
     &   (IO_param, len(hd_surf_edge_grp()), hd_surf_edge_grp())
      call gz_mpi_write_viewer_grp_data                                 &
     &   (IO_param, mgd_v_mesh_p%view_sf_grps%num_grp,                  &
     &    mgd_v_mesh_p%view_sf_grps%grp_name,                           &
     &    mgd_v_mesh_p%view_sf_grps%edge_grp)
!
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_collect_surf_mesh_4_viewer
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_viewer_grp_data                           &
     &         (IO_param, num_grp, grp_name, view_grp)
!
      use t_calypso_mpi_IO_param
      use data_IO_to_textline
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint), intent(in) :: num_grp
      character(len=kchara), intent(in) :: grp_name(num_grp)
      type(viewer_group_data), intent(inout) :: view_grp
!
      integer(kind = kint) :: i, ist, ied, num, ntot, ip
      integer(kind = kint) :: num_global(nprocs)
!
!
      call gz_mpi_write_charahead(IO_param, len_int_txt,                &
     &    integer_textline(num_grp))
!
      ntot = 0
      do i = 1, num_grp
        num = view_grp%istack_sf(i) - view_grp%istack_sf(i-1)
        call MPI_Allgather(num, ione, CALYPSO_INTEGER, num_global,      &
     &      ione, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
        do ip = 2, nprocs
          num_global(ip) = num_global(ip-1) + num_global(ip)
        end do
        num = ntot + num_global(my_rank+1)
        ntot = ntot + num_global(nprocs)
        call gz_mpi_write_num_of_data(IO_param, num)
      end do
!
      do i = 1, num_grp
        call gz_mpi_write_charahead(IO_param,                           &
     &      len_one_word_textline(grp_name(i)),                         &
     &      one_word_textline(grp_name(i)))
!
        ist = view_grp%istack_sf(i-1) + 1
        num = view_grp%istack_sf(i) - view_grp%istack_sf(i-1)
        call gz_mpi_write_viewer_grp_item                               &
     &     (IO_param, ieight, num, view_grp%item_sf(ist))
      end do
!
      end subroutine gz_mpi_write_viewer_grp_data
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_viewer_grp_item                           &
     &         (IO_param, ncolumn, num, int_dat)
!
      use t_calypso_mpi_IO_param
      use data_IO_to_textline
      use gz_MPI_ascii_data_IO
      use MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_gz = int(real(num*len_int_txt) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(num .le. 0) then
!        call gzip_defleat_once(ione, char(10),                          &
!     &      ilen_gz, ilen_gzipped, gzip_buf(1))
         ilen_gzipped = 0
      else if(num .le. ncolumn) then
        call gzip_defleat_once(len_multi_int_textline(num),             &
     &      multi_int_textline(num, int_dat(1)),                        &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(num .gt. 0) then
        call gzip_defleat_begin(len_multi_int_textline(ncolumn),        &
     &      multi_int_textline(ncolumn, int_dat(1)),                    &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 1, (num-1)/ncolumn - 1
          call gzip_defleat_cont(len_multi_int_textline(ncolumn),       &
     &        multi_int_textline(ncolumn, int_dat(ncolumn*i+1)),        &
     &        ilen_gz, ilen_gzipped)
        end do
        nrest = mod((num-1),ncolumn) + 1
        call gzip_defleat_last(len_multi_int_textline(nrest),           &
     &      multi_int_textline(nrest, int_dat(num-nrest+1)),            &
     &      ilen_gz, ilen_gzipped)
      end if
!
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
!      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_viewer_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_viewer_position                           &
     &         (IO_param, nnod, numdir, id_global, xx)
!
      use t_calypso_mpi_IO_param
      use data_IO_to_textline
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nnod, numdir
      integer(kind=kint_gl), intent(in) :: id_global(nnod)
      real(kind=kreal), intent(in) :: xx(nnod, numdir)
!
      integer(kind = kint) :: i
      real(kind = kreal) :: xx_tmp(numdir)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
!      call gz_mpi_write_num_of_data(IO_param, nnod)
!
      ilen_line = len_int8_and_vector_textline(numdir)
      ilen_gz = int(real(nnod*ilen_line *1.01)) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nnod .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      int8_and_vector_textline                                    &
     &         (id_global(1), numdir, xx(1,1)),                         &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nnod .gt. 0) then
        xx_tmp(1:numdir) = xx(1,1:numdir)
        call gzip_defleat_begin(ilen_line,                              &
     &     int8_and_vector_textline(id_global(1), numdir, xx_tmp),      &
     &     ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, nnod - 1
          xx_tmp(1:numdir) = xx(i,1:numdir)
          call gzip_defleat_cont(ilen_line,                             &
     &     int8_and_vector_textline(id_global(i), numdir, xx_tmp),      &
     &        ilen_gz, ilen_gzipped)
        end do
        xx_tmp(1:numdir) = xx(nnod,1:numdir)
        call gzip_defleat_last(ilen_line,                               &
     &     int8_and_vector_textline                                     &
     &          (id_global(nnod), numdir, xx_tmp),                      &
     &      ilen_gz, ilen_gzipped)
      end if
!
!      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_viewer_position
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_viewer_element_type                       &
     &         (IO_param, ncolumn, num, int_dat)
!
      use t_calypso_mpi_IO_param
      use data_IO_to_textline
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: num, ncolumn
      integer(kind=kint), intent(in) :: int_dat(num)
!
      integer(kind = kint) :: i, nrest
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_gz = int(real(num*len_6digit_txt) *1.01) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(num .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(num .le. ncolumn) then
        call gzip_defleat_once(len_multi_6digit_line(num),              &
     &      mul_6digit_int_line(num, int_dat(1)),                       &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(num .gt. 0) then
        call gzip_defleat_begin(len_multi_6digit_line(ncolumn),         &
     &      mul_6digit_int_line(ncolumn, int_dat(1)),                   &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 1, (num-1)/ncolumn - 1
          call gzip_defleat_cont(len_multi_6digit_line(ncolumn),        &
     &        mul_6digit_int_line(ncolumn, int_dat(ncolumn*i+1)),       &
     &        ilen_gz, ilen_gzipped)
        end do
        nrest = mod((num-1),ncolumn) + 1
        call gzip_defleat_last(len_multi_6digit_line(nrest),            &
     &      mul_6digit_int_line(nrest, int_dat(num-nrest+1)),           &
     &      ilen_gz, ilen_gzipped)
      end if
!
!      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_viewer_element_type
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_viewer_connect                            &
     &         (IO_param, nele, nnod_4_ele, id_global, ie)
!
      use t_calypso_mpi_IO_param
      use data_IO_to_textline
      use gz_MPI_ascii_data_IO
      use gz_MPI_domain_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind=kint), intent(in) :: nele, nnod_4_ele
      integer(kind=kint_gl), intent(in) :: id_global(nele)
      integer(kind=kint), intent(in) :: ie(nele,nnod_4_ele)
!
      integer(kind = kint) :: i
      integer(kind = kint) :: ie_tmp(nnod_4_ele)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilen_line, ilen_gz, ilen_gzipped
!
      character(len=1), allocatable :: gzip_buf(:)
!
!
      ilen_line = len_int8_and_mul_int_textline(nnod_4_ele)
      ilen_gz = int(real(nele*ilen_line *1.01)) + 24
      allocate(gzip_buf(ilen_gz))
!
      if(nele .le. 0) then
        call gzip_defleat_once(ione, char(10),                          &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nele .eq. 1) then
        call gzip_defleat_once(ilen_line,                               &
     &      int8_and_mul_int_textline                                   &
     &         (id_global(1), nnod_4_ele, ie(1,1)),                     &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
      else if(nele .gt. 0) then
        ie_tmp(1:nnod_4_ele) = ie(1,1:nnod_4_ele)
        call gzip_defleat_begin(ilen_line,                              &
     &     int8_and_mul_int_textline(id_global(1), nnod_4_ele, ie_tmp), &
     &     ilen_gz, ilen_gzipped, gzip_buf(1))
        do i = 2, nele - 1
          ie_tmp(1:nnod_4_ele) = ie(i,1:nnod_4_ele)
          call gzip_defleat_cont(ilen_line,                             &
     &     int8_and_mul_int_textline(id_global(i), nnod_4_ele, ie_tmp), &
     &        ilen_gz, ilen_gzipped)
        end do
        ie_tmp(1:nnod_4_ele) = ie(nele,1:nnod_4_ele)
        call gzip_defleat_last(ilen_line,                               &
     &     int8_and_mul_int_textline                                    &
     &          (id_global(nele), nnod_4_ele, ie_tmp),                  &
     &      ilen_gz, ilen_gzipped)
      end if
!
!      call gz_mpi_write_stack_over_domain(IO_param, ilen_gzipped)
      call set_istack_4_parallell_data(ilen_gzipped, IO_param)
!
      if(ilen_gzipped .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &           + IO_param%istack_merged(IO_param%id_rank)
        call calypso_mpi_seek_write_chara(IO_param%id_file, ioffset,    &
     &     ilen_gzipped, gzip_buf(1))
      end if
!
      deallocate(gzip_buf)
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &                  + IO_param%istack_merged(IO_param%nprocs_in)
!
      end subroutine gz_mpi_write_viewer_connect
!
! -----------------------------------------------------------------------
!
      end module parallel_const_surface_mesh
