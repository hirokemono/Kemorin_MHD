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
      use t_grp_data_merged_surfaces
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
!
      type(field_IO_params), intent(inout) :: mesh_file
!
      type(merged_viewer_mesh), save :: mgd_view_mesh1
!
      type(mesh_geometry), save :: mesh_p
      type(mesh_groups), save :: group_p
      type(element_geometry), save :: ele_mesh_p
!
      type(element_data), save :: ele_p
      type(surface_data), save :: surf_p
      type(edge_data), save :: edge_p
      type(merged_mesh), save :: mgd_mesh_p
      type(group_data_merged_surf), save :: mgd_sf_grp_p
      type(merged_viewer_mesh), save :: mgd_view_mesh_p
!
      integer(kind = kint), allocatable :: inod_ksm(:)
      integer(kind = kint), allocatable :: isurf_ksm(:)
      integer(kind = kint), allocatable :: iedge_ksm(:)
      integer(kind = kint) :: numnod_ksm
      integer(kind = kint) :: numsurf_ksm
      integer(kind = kint) :: numedge_ksm
      type(viewer_mesh_data), save :: view_mesh_p
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
      call FEM_mesh_init_with_IO(iflag_output_SURF,                     &
     &    mesh_file, mesh_p, group_p, ele_mesh_p)
!
!      write(50+my_rank,*) 'iflag_surf_z', ele_mesh_p%surf%numsurf_iso
!      write(50+my_rank,*) 'iflag_surf_z', ele_mesh_p%surf%isf_isolate
!
      allocate(inod_ksm(mesh_p%node%numnod))
      allocate(isurf_ksm(ele_mesh_p%surf%numsurf))
      allocate(iedge_ksm(ele_mesh_p%edge%numedge))
      inod_ksm = 0
      isurf_ksm = 0
      iedge_ksm = 0
!
      call s_const_mesh_list_4_viewer(iflag_add_comm_tbl,               &
     &  mesh_p%node, mesh_p%nod_comm, ele_mesh_p%surf, ele_mesh_p%edge, &
     &  group_p%nod_grp, group_p%ele_grp, group_p%surf_grp,             &
     &  inod_ksm, isurf_ksm, iedge_ksm,                                 &
     &  view_mesh_p%nnod_viewer, view_mesh_p%nsurf_viewer,              &
     &  view_mesh_p%edgepetot_viewer)
!
      write(*,*) my_rank, view_mesh_p%nnod_viewer, view_mesh_p%nsurf_viewer,          &
     &  view_mesh_p%edgepetot_viewer
      
!
!      call const_surf_mesh_4_viewer                                     &
!     &   (surf_p, edge_p, mgd_mesh_p, mgd_sf_grp_p, mgd_view_mesh_p)
!
!
!      call collect_surf_mesh_4_viewer                                   &
!     &   (mesh_file, surf_p, edge_p, mgd_view_mesh_p, mgd_view_mesh1)
!
!      call deallocate_quad4_2_linear
!
      end subroutine pickup_surface_mesh_para
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
      use renumber_para_viewer_mesh
      use viewer_IO_select_4_zlib
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
      call s_renumber_para_viewer_mesh                                  &
     &   (surf, edge, mgd_v_mesh_p, mgd_view_mesh)
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
! -----------------------------------------------------------------------
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
      type(merged_viewer_mesh), save :: mgd_view_mesh1
!
      type(element_data), save :: ele_p
      type(surface_data), save :: surf_p
      type(edge_data), save :: edge_p
      type(merged_mesh), save :: mgd_mesh_p
      type(group_data_merged_surf), save :: mgd_sf_grp_p
      type(merged_viewer_mesh), save :: mgd_view_mesh_p
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
      call const_merged_mesh_para                                       &
     &   (mesh_file, ele_p, surf_p, edge_p, mgd_mesh_p, mgd_sf_grp_p)
!
      call const_surf_mesh_4_viewer                                     &
     &   (surf_p, edge_p, mgd_mesh_p, mgd_sf_grp_p, mgd_view_mesh_p)
!
!
      call collect_surf_mesh_4_viewer                                   &
     &   (mesh_file, surf_p, edge_p, mgd_view_mesh_p, mgd_view_mesh1)
!
      call deallocate_quad4_2_linear
!
      end subroutine choose_surface_mesh_para
!
!------------------------------------------------------------------
!
      end module parallel_const_surface_mesh
