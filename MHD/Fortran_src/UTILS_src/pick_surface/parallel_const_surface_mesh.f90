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
      use t_grp_data_merged_surfaces
!
      implicit none
!
      type(mesh_geometry), save, private :: mesh_p
      type(mesh_groups), save, private ::   group_p
      type(element_geometry), save, private :: ele_mesh_p
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
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use single_const_surface_mesh
      use const_surface_data
!
      type(field_IO_params), intent(inout) :: mesh_file
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(merged_mesh), intent(inout) :: mgd_mesh
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
      if (iflag_debug.gt.0) write(*,*) 'FEM_mesh_init_with_IO'
      call FEM_mesh_init_with_IO(izero, mesh_file,                      &
     &    mesh_p, group_p, ele_mesh_p)
!
      mgd_mesh%num_pe = nprocs
      call const_surf_mesh_4_viewer_para
!
      if(my_rank .eq. 0) then
        write(*,*) 'const_surf_mesh_4_viewer'
        call const_surf_mesh_4_viewer                                   &
     &     (mesh_file, ele, surf, edge, mgd_mesh, mgd_view_mesh1)
      end if
      call calypso_mpi_barrier
!
      end subroutine choose_surface_mesh_para
!
!------------------------------------------------------------------
!
      subroutine const_surf_mesh_4_viewer_para
!
!
      write(*,*) 'numsurf_iso', ele_mesh_p%surf%numsurf_iso
!
      end subroutine const_surf_mesh_4_viewer_para
!
!------------------------------------------------------------------
!
      end module parallel_const_surface_mesh
