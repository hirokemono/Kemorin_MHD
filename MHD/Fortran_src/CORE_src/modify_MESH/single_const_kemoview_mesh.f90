!>@file   single_const_kemoview_mesh.f90
!!@brief  module single_const_kemoview_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine choose_surface_mesh_sgl(np, mesh_file, sgl_viewer)
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(single_make_vierwer_mesh), intent(inout) :: sgl_viewer
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
      use t_mesh_data
      use t_file_IO_parameter
      use t_viewer_mesh
      use t_viewer_ele_grp_surface
      use t_merged_viewer_mesh
!
      implicit none
!
      integer(kind = kint) :: iflag_add_comm_tbl = 1
      integer(kind = kint), parameter :: iflag_write_subdomain = 0
!
      type single_make_vierwer_mesh
        type(mesh_data) :: fem
        type(element_geometry) :: ele_mesh
!
        integer :: nprocs_sf
        type(viewer_mesh_data), allocatable :: view_mesh(:)
        type(viewer_surface_groups), allocatable :: domain_grps(:)
!
        type(viewer_node_groups), allocatable :: view_nod_grps(:)
        type(viewer_surface_groups), allocatable :: view_ele_grps(:)
        type(viewer_surface_groups), allocatable :: view_sf_grps(:)
      end type single_make_vierwer_mesh
!
private :: collect_single_viewer_mesh
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine choose_surface_mesh_sgl(np, mesh_file, sgl_viewer)
!
      use m_node_quad_2_linear_sf
      use find_mesh_file_format
      use viewer_mesh_IO_select
      use load_mesh_data
      use add_comm_table_in_node_grp
      use const_kemoview_mesh
!
      integer(kind = kint), intent(in) :: np
      type(field_IO_params), intent(inout) :: mesh_file
      type(single_make_vierwer_mesh), intent(inout) :: sgl_viewer
!
!
      integer(kind = kint) :: ierr
      integer :: ip, id_rank
!
!  set mesh_information
!
      sgl_viewer%nprocs_sf = int(np)
      if(sgl_viewer%nprocs_sf .eq. 1) iflag_add_comm_tbl = 0
!
      allocate(sgl_viewer%view_mesh(sgl_viewer%nprocs_sf))
      allocate(sgl_viewer%domain_grps(sgl_viewer%nprocs_sf))
      allocate(sgl_viewer%view_nod_grps(sgl_viewer%nprocs_sf))
      allocate(sgl_viewer%view_ele_grps(sgl_viewer%nprocs_sf))
      allocate(sgl_viewer%view_sf_grps(sgl_viewer%nprocs_sf))
!
      do ip = 1, sgl_viewer%nprocs_sf
        id_rank = ip - 1
        if (iflag_debug.gt.0) write(*,*) 'input_mesh'
        call input_mesh(mesh_file, id_rank,                             &
     &     sgl_viewer%fem, sgl_viewer%ele_mesh, ierr)
        call allocate_quad4_2_linear                                    &
     &     (sgl_viewer%fem%mesh%ele%nnod_4_ele)
!
!
        if(iflag_add_comm_tbl .gt. 0) then
          call add_comm_tbl_in_node_grp_mesh(sgl_viewer%nprocs_sf,      &
     &        sgl_viewer%fem%mesh, sgl_viewer%fem%group)
        end if
!
        write(*,*) 'Construct kemoviewer data for rank ', id_rank
        call const_surf_mesh_4_viewer                                   &
     &     (sgl_viewer%fem%mesh, sgl_viewer%fem%group,                  &
     &      sgl_viewer%ele_mesh%surf, sgl_viewer%ele_mesh%edge,         &
     &      sgl_viewer%view_mesh(ip), sgl_viewer%domain_grps(ip),       &
     &      sgl_viewer%view_nod_grps(ip), sgl_viewer%view_ele_grps(ip), &
     &      sgl_viewer%view_sf_grps(ip))
!
        call deallocate_iso_surface_type(sgl_viewer%ele_mesh%surf)
        call deallocate_ext_surface_type(sgl_viewer%ele_mesh%surf)
        call deallocate_surface_connect_type(sgl_viewer%ele_mesh%surf)
        call deallocate_inod_in_surf_type(sgl_viewer%ele_mesh%surf)
!
        call dealloc_mesh_infos                                         &
     &     (sgl_viewer%fem%mesh, sgl_viewer%fem%group)
        call dealloc_inod_in_edge(sgl_viewer%ele_mesh%edge)
!
        if(iflag_write_subdomain .gt. 0) then
          call sel_output_single_surface_grid(id_rank, mesh_file,       &
     &        sgl_viewer%view_mesh(ip), sgl_viewer%domain_grps(ip),     &
     &        sgl_viewer%view_nod_grps(ip),                             &
     &        sgl_viewer%view_ele_grps(ip),                             &
     &        sgl_viewer%view_sf_grps(ip))
        end if
!
        call deallocate_quad4_2_linear
      end do
!
      call collect_single_viewer_mesh                                   &
     &   (sgl_viewer%nprocs_sf, mesh_file, sgl_viewer%view_mesh,        &
     &    sgl_viewer%domain_grps, sgl_viewer%view_nod_grps,             &
     &    sgl_viewer%view_ele_grps, sgl_viewer%view_sf_grps)
!
      end subroutine choose_surface_mesh_sgl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine collect_single_viewer_mesh                             &
     &         (nprocs_sf, mesh_file, view_mesh, domain_grps,           &
     &          view_nod_grps, view_ele_grps, view_sf_grps)
!
      use viewer_mesh_IO_select
      use merge_viewer_mesh
!
      integer, intent(in) :: nprocs_sf
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
