!>@file   single_const_kemoview_mesh.f90
!!@brief  module single_const_kemoview_mesh
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!!      Modified in June, 2018
!
!>@brief Surface mesh data generator for kemoviewer
!!
!!@verbatim
!!      subroutine choose_surface_mesh_sgl(num_pe_s, mesh_file)
!!        type(field_IO_params), intent(inout) :: mesh_file
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
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_file_IO_parameter
      use t_viewer_mesh
      use t_merged_viewer_mesh
!
      implicit none
!
      integer(kind = kint), parameter, private :: iflag_add_comm_tbl = 1
      integer(kind = kint), parameter :: iflag_write_subdomain = 1
!
      private :: iflag_write_subdomain
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine choose_surface_mesh_sgl(num_pe_s, mesh_file)
!
      use load_mesh_data
      use const_mesh_information
      use const_mesh_list_4_viewer
      use add_comm_table_in_node_grp
      use copy_mesh_structures
      use const_viewer_mesh
      use viewer_mesh_IO_select
      use merge_viewer_mesh
!
!
      integer(kind = kint), intent(in) :: num_pe_s
      type(field_IO_params), intent(inout) :: mesh_file
!
      type(mesh_geometry), save :: mesh_p
      type(mesh_groups), save :: group_p
      type(element_geometry), save :: ele_mesh_p
!
      type(group_data), save :: new_nod_grp
!
      type(merged_viewer_mesh), save :: mgd_view_mesh_p
      type(viewer_mesh_data), allocatable :: view_mesh_p(:)
      type(viewer_surface_groups), allocatable :: domain_grps_p(:)
      type(viewer_node_groups), allocatable :: view_nod_grps_p(:)
      type(viewer_surface_groups), allocatable :: view_ele_grps_p(:)
      type(viewer_surface_groups), allocatable :: view_sf_grps_p(:)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: ierr
!
!
      allocate(view_mesh_p(num_pe_s))
      allocate(domain_grps_p(num_pe_s))
      allocate(view_nod_grps_p(num_pe_s))
      allocate(view_ele_grps_p(num_pe_s))
      allocate(view_sf_grps_p(num_pe_s))
!
write(*,*) 'num_pe_s', num_pe_s
      do ip = 1, num_pe_s
write(*,*) 'choose_surface_mesh_sgl, ip', ip
        call input_mesh(mesh_file, (ip-1), mesh_p, group_p,             &
     &      ele_mesh_p%surf%nnod_4_surf,                                &
     &      ele_mesh_p%edge%nnod_4_edge, ierr)
write(*,*) mesh_p%node%numnod, mesh_p%node%internal_node, mesh_p%ele%numele
write(*,*) ele_mesh_p%surf%nnod_4_surf, ele_mesh_p%edge%nnod_4_edge
!
        if(iflag_add_comm_tbl .gt. 0) then
          call add_comm_table_in_node_group(num_pe_s,                   &
     &        mesh_p%nod_comm, group_p%nod_grp, new_nod_grp)
          call deallocate_grp_type(group_p%nod_grp)
          call copy_group_data(new_nod_grp, group_p%nod_grp)
          call deallocate_grp_type(new_nod_grp)
        end if
!
        call const_mesh_infos((ip-1), mesh_p, group_p, ele_mesh_p)
!
        call s_const_viewer_mesh                                        &
     &     (mesh_p, ele_mesh_p, group_p, view_mesh_p(ip),               &
     &      domain_grps_p(ip), view_nod_grps_p(ip),                     &
     &      view_ele_grps_p(ip), view_sf_grps_p(ip))
!
        call dealloc_mesh_infomations(mesh_p, group_p, ele_mesh_p)
!
        if(iflag_write_subdomain .gt. 0) then
          call sel_output_single_surface_grid((ip-1), mesh_file,        &
     &        ele_mesh_p%surf%nnod_4_surf, ele_mesh_p%edge%nnod_4_edge, &
     &        view_mesh_p(ip), domain_grps_p(ip), view_nod_grps_p(ip),  &
     &        view_ele_grps_p(ip), view_sf_grps_p(ip))
        else
          write(*,*) 'Viewer mesh data fpr ', (ip-1), ' is made'
        end if
      end do
!
      call alloc_num_mesh_sf(num_pe_s, mgd_view_mesh_p)
write(*,*) 's_merge_viewer_mesh',num_pe_s,ele_mesh_p%surf%nnod_4_surf, ele_mesh_p%edge%nnod_4_edge
write(*,*) 'each mesh node:', view_mesh_p(:)%nnod_viewer
write(*,*) 'each mesh surface:', view_mesh_p(:)%nsurf_viewer
      call s_merge_viewer_mesh(num_pe_s,                                &
     &    ele_mesh_p%surf%nnod_4_surf, ele_mesh_p%edge%nnod_4_edge,     &
     &    view_mesh_p, domain_grps_p,                                   &
     &    view_nod_grps_p, view_ele_grps_p, view_sf_grps_p,             &
     &    mgd_view_mesh_p)
!
      do ip = 1, num_pe_s
        call dealloc_viewer_mesh(view_mesh_p(ip), domain_grps_p(ip),    &
     &    view_nod_grps_p(ip), view_ele_grps_p(ip), view_sf_grps_p(ip))
      end do
      deallocate(view_mesh_p, domain_grps_p)
      deallocate(view_nod_grps_p, view_ele_grps_p, view_sf_grps_p)
!
      call sel_output_surface_grid(mesh_file,                           &
     &    ele_mesh_p%surf%nnod_4_surf, ele_mesh_p%edge%nnod_4_edge,     &
     &    mgd_view_mesh_p)
!
      call dealloc_viewer_mesh(mgd_view_mesh_p%view_mesh,               &
     &    mgd_view_mesh_p%domain_grps, mgd_view_mesh_p%view_nod_grps,   &
     &    mgd_view_mesh_p%view_ele_grps, mgd_view_mesh_p%view_sf_grps)
      call dealloc_num_mesh_sf(mgd_view_mesh_p)
!
      end subroutine choose_surface_mesh_sgl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
!      subroutine set_source_mesh_parameter                              &
!     &         (ele, surf, edge, merged_surf)
!
!      call set_3D_nnod_4_sfed_by_ele                                    &
!     &   (ele%nnod_4_ele, surf%nnod_4_surf, edge%nnod_4_edge)
!      call allocate_quad4_2_linear(ele%nnod_4_ele)
!
!      call allocate_inod_in_surf(surf)
!      call set_inod_in_surf                                             &
!     &   (surf%nnod_4_surf, surf%node_on_sf, surf%node_on_sf_n)
!
!      call alloc_inod_in_edge(edge)
!      call copy_inod_in_edge(edge%nnod_4_edge,                          &
!     &    edge%node_on_edge, edge%node_on_edge_sf)
!
!      merged_surf%nnod_4_surf = surf%nnod_4_surf
!      call allocate_inod_in_surf(merged_surf)
!      call set_inod_in_surf(merged_surf%nnod_4_surf,                    &
!     &    merged_surf%node_on_sf, merged_surf%node_on_sf_n)
!
!      end subroutine set_source_mesh_parameter
!
!------------------------------------------------------------------
!
      end module single_const_kemoview_mesh
