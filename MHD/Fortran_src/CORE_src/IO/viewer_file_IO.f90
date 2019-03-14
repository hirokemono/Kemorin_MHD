!>@file   viewer_file_IO.f90
!!@brief  module viewer_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief ASCII viewer mesh file IO
!!
!!@verbatim
!!      subroutine output_surface_grid(file_name, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!!      subroutine output_single_surface_grid                           &
!!     &         (file_name, isurf_sf_stack, view_mesh, domain_grps,    &
!!     &          view_nod_grps, view_ele_grps, view_sf_grps)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(viewer_surface_groups), intent(in) :: domain_grps
!!        type(viewer_node_groups), intent(in) :: view_nod_grps
!!        type(viewer_surface_groups), intent(in) :: view_ele_grps
!!        type(viewer_surface_groups), intent(in) :: view_sf_grps
!!
!!      subroutine read_surface_grid(file_name,                         &
!!     &          nnod_4_ele, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!!@endverbatim
!
      module viewer_file_IO
!
      use m_precision
!
      use t_merged_viewer_mesh
      use set_parallel_file_name
      use viewer_mesh_data_IO
      use viewer_group_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine output_surface_grid(file_name, mgd_view_mesh)
!
      character(len = kchara), intent(in) :: file_name
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
!
      write(*,*) 'surface mesh file name: ', trim(file_name)
      open (surface_id, file = file_name)
!
!      write(*,*) 'write_domain_data_viewer'
      call write_domain_data_viewer(mgd_view_mesh)
!
!      write(*,*) 'write_node_data_viewer'
      call write_node_data_viewer(mgd_view_mesh%view_mesh)
!
!      write(*,*) 'write_surf_connect_viewer'
      call write_surf_connect_viewer                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%isurf_sf_stack,        &
     &    mgd_view_mesh%view_mesh)
!
!      write(*,*) 'write_edge_connect_viewer'
      call write_edge_connect_viewer(mgd_view_mesh%view_mesh)
!
!      write(*,*) 'write_domain_group_viewer'
      call write_domain_group_viewer                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps)
!
!      write(*,*) 'write_nod_group_viewer'
      call write_nod_group_viewer                                       &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_nod_grps)
!
!      write(*,*) 'write_ele_group_viewer'
      call write_ele_group_viewer                                       &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_ele_grps)
!
!      write(*,*) 'write_surf_group_viewer'
      call write_surf_group_viewer                                      &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_sf_grps)
!
      close(surface_id)
!
      end subroutine output_surface_grid
!
!------------------------------------------------------------------
!
      subroutine output_single_surface_grid                             &
     &         (file_name, isurf_sf_stack, view_mesh, domain_grps,      &
     &          view_nod_grps, view_ele_grps, view_sf_grps)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: isurf_sf_stack(0:1)
!
      type(viewer_mesh_data), intent(in) :: view_mesh
      type(viewer_surface_groups), intent(in) :: domain_grps
      type(viewer_node_groups), intent(in) :: view_nod_grps
      type(viewer_surface_groups), intent(in) :: view_ele_grps
      type(viewer_surface_groups), intent(in) :: view_sf_grps
!
!
      write(*,*) 'subdomain surface mesh file name: ', trim(file_name)
      open (surface_id, file = file_name)
!
      call write_sgl_domain_data_viewer(view_mesh)
!
      call write_node_data_viewer(view_mesh)
      call write_surf_connect_viewer                                    &
     &   (1, isurf_sf_stack, view_mesh)
      call write_edge_connect_viewer(view_mesh)
!
      call write_domain_group_viewer(1, domain_grps)
!
      call write_nod_group_viewer(1, view_nod_grps)
      call write_ele_group_viewer(1, view_ele_grps)
      call write_surf_group_viewer(1, view_sf_grps)
!
      close(surface_id)
!
      end subroutine output_single_surface_grid
!
!------------------------------------------------------------------
!
      subroutine read_surface_grid(file_name,                           &
     &          nnod_4_ele, mgd_view_mesh)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nnod_4_ele
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      integer(kind = kint) :: nnod_4_edge
!
      write(*,*) 'surface mesh file name: ', trim(file_name)
      open (surface_id, file = file_name)
!
!      write(*,*) 'read_domain_data_viewer'
      call read_domain_data_viewer(mgd_view_mesh)
!
!      write(*,*) 'read_node_data_viewer'
      call read_node_data_viewer(mgd_view_mesh%view_mesh)
!
!      write(*,*) 'read_surf_connect_viewer'
      call read_surf_connect_viewer                                     &
     &   (nnod_4_ele, nnod_4_edge, mgd_view_mesh%view_mesh)
!
!      write(*,*) 'read_edge_connect_viewer'
      call read_edge_connect_viewer(nnod_4_edge,                        &
     &    mgd_view_mesh%view_mesh)
!
      call alloc_domain_stack_4_surf                                    &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps)
!
!      write(*,*) 'read_domain_group_viewer'
      call read_domain_group_viewer                                     &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%domain_grps)
!
!      write(*,*) 'read_nod_group_viewer'
      call read_nod_group_viewer                                        &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_nod_grps)
!
!      write(*,*) 'read_ele_group_viewer'
      call read_ele_group_viewer                                        &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_ele_grps)
!
!      write(*,*) 'read_surf_group_viewer'
      call read_surf_group_viewer                                       &
     &   (mgd_view_mesh%num_pe_sf, mgd_view_mesh%view_sf_grps)
!
      close(surface_id)
!
!
!      write(*,*) 's_cal_range_of_domain'
!      call s_const_edges_normals_viewer
!      call s_cal_range_of_domain
!      call s_cal_normal_of_node_viewer
!
      end subroutine read_surface_grid
!
!------------------------------------------------------------------
!
      end module viewer_file_IO
