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
!!     &         (mesh_file, ele, surf, edge, mgd_mesh)
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(element_data), intent(inout) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data), intent(inout) :: edge
!!        type(merged_mesh), intent(inout) :: mgd_mesh
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
      use m_surface_mesh_4_merge
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_file_IO_parameter
      use t_mesh_data_4_merge
!
      implicit none
!
      private :: const_surf_mesh_4_viewer
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine choose_surface_mesh_para                               &
     &         (mesh_file, ele, surf, edge, mgd_mesh)
!
      use find_mesh_file_format
!
      type(field_IO_params), intent(inout) :: mesh_file
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_mesh), intent(inout) :: mgd_mesh
!
!
      surface_file_head = mesh_file%file_prefix
      mgd_mesh%num_pe = nprocs
!
      if(my_rank .eq. 0) then
        if(iflag_debug .eq. 0) write(*,*) 'find_merged_mesh_format'
        call find_merged_mesh_format(mesh_file)
      end if
      call calypso_mpi_barrier
      call MPI_BCAST(mesh_file%iflag_format, ione,                      &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
!
      if(my_rank .eq. 0) then
        write(*,*) 'const_surf_mesh_4_viewer'
        call const_surf_mesh_4_viewer                                   &
     &     (mesh_file, ele, surf, edge, mgd_mesh)
      end if
      call calypso_mpi_barrier
!
      end subroutine choose_surface_mesh_para
!
!------------------------------------------------------------------
!
      subroutine const_surf_mesh_4_viewer                               &
     &         (mesh_file, ele, surf, edge, mgd_mesh)
!
      use set_merged_geometry
      use const_merged_surf_data
      use const_merged_surf_4_group
      use set_surf_connect_4_viewer
      use set_nodes_4_viewer
      use const_edge_4_viewer
      use set_nodes_4_groups_viewer
      use viewer_IO_select_4_zlib
      use single_const_surface_mesh
!
      type(field_IO_params), intent(in) :: mesh_file
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_mesh), intent(inout) :: mgd_mesh
!
!
!  set mesh_information
!
       write(*,*) 'set_overlapped_mesh_and_group'
       call set_overlapped_mesh_and_group                               &
     &    (mesh_file, ele%nnod_4_ele, mgd_mesh)
!
!   output grid data
!
       write(*,*) 'set_source_mesh_parameter'
       call set_source_mesh_parameter                                   &
     &    (mgd_mesh%num_pe, ele, surf, edge, mgd_mesh%merged_surf)
!
!  choose surface
!
       write(*,*) 's_const_merged_surf_data'
       call s_const_merged_surf_data(mgd_mesh)
!
!       write(*,*) 'const_merged_surface_4_ele_grp'
       call const_merged_surface_4_ele_grp                              &
     &    (mgd_mesh%merged, mgd_mesh%merged_grp, mgd_mesh%merged_surf)
!       write(*,*) 'const_merged_surface_4_sf_grp'
       call const_merged_surface_4_sf_grp                               &
     &    (mgd_mesh%merged_grp, mgd_mesh%merged_surf)
!
!  pickup surface and nodes
!
!       write(*,*) 's_set_surf_connect_4_viewer'
       call s_set_surf_connect_4_viewer(surf%nnod_4_surf, mgd_mesh)
!       write(*,*) 's_set_nodes_4_viewer'
       call s_set_nodes_4_viewer(surf%nnod_4_surf, mgd_mesh)
!
       write(*,*) 'set_surf_domain_id_viewer'
       call set_surf_domain_id_viewer(mgd_mesh%merged_surf)
!
!
       call dealloc_array_4_merge(mgd_mesh)
!
       write(*,*)  'construct_edge_4_viewer'
       call construct_edge_4_viewer(surf, edge)
       write(*,*)  's_set_nodes_4_groups_viewer'
       call s_set_nodes_4_groups_viewer                                 &
     &    (surf%nnod_4_surf, edge%nnod_4_edge)
!
      call sel_output_surface_grid(mesh_file%iflag_format,              &
     &    surf%nnod_4_surf, edge%nnod_4_edge)
!
      end subroutine const_surf_mesh_4_viewer
!
!------------------------------------------------------------------
!
      end module parallel_const_surface_mesh
