!cvt_quad_2_linear_mesh.f90
!      module cvt_quad_2_linear_mesh
!
!      Written by H. Matsui on Apr., 2006
!
!!      subroutine generate_linear_nod_by_quad                          &
!!     &         (node_q, ele_q, surf_q, node_l)
!!      subroutine connect_quad_mesh_2_linear                           &
!!     &         (node_q, ele_q, surf_q, node_l, ele_l)
!!      subroutine connect_lag_mesh_2_linear(ele_q, node_l, ele_l)
!!      subroutine gen_linear_group_info                                &
!!     &         (ele_grp, sf_grp, ele_grp_l, surf_grp_l)
!!
!!      subroutine set_internal_list_lin_20(node_q, ele_q, surf_q,      &
!!     &          node_l, ele_l, surf_l, edge_l)
!!      subroutine set_internal_list_lin_27                             &
!!     &         (node_q,  node_l, ele_l, surf_l, edge_l)
!!
!!      subroutine init_linear_nod_phys(node_l, nod_fld_q, nod_fld_l)
!!      subroutine copy_nod_phys_2_linear                               &
!!     &         (node_q, nod_fld_q, node_l, nod_fld_l)
!!      subroutine generate_phys_on_surf(node_q, ele_q, surf_q,         &
!!     &          node_l, nod_fld_l)
!
      module cvt_quad_2_linear_mesh
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: ie_4_333(:,:)
      private :: ie_4_333
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine generate_linear_nod_by_quad                            &
     &         (node_q, ele_q, surf_q, node_l)
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
!
      use set_geometry_4_quad27
      use cal_mesh_position
!
      type(node_data), intent(in) ::    node_q
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
!
      type(node_data), intent(inout) ::    node_l
!
!
      node_l%numnod = node_q%numnod + surf_q%numsurf + ele_q%numele
!
      call allocate_node_geometry_type(node_l)
!
      call set_position_on_surf                                         &
     &   (node_q%numnod, surf_q%numsurf, ele_q%numele,                  &
     &    node_q%xx, ele_q%x_ele, surf_q%x_surf,                        &
     &    node_l%numnod, node_l%xx)
!
      call set_spherical_position(node_l)
!
      end subroutine generate_linear_nod_by_quad
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine connect_quad_mesh_2_linear                             &
     &         (node_q, ele_q, surf_q, node_l, ele_l)
!
      use m_geometry_constants
      use t_mesh_data
      use t_geometry_data
!
      use m_27quad_2_8x8linear
!
      type(node_data), intent(in) ::    node_q
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
      type(node_data), intent(inout) ::    node_l
      type(element_data), intent(inout) :: ele_l
!
!
      ele_l%numele = 8 * ele_q%numele
      ele_l%nnod_4_ele = num_t_linear
!
      call allocate_ele_connect_type(ele_l)
      call alloc_ele_geometry(ele_l)
!
      allocate(ie_4_333(ele_q%numele,27) )
!
      call gen_connect_quad27_from_quad20                               &
     &   (node_q%numnod, ele_q%numele, surf_q%numsurf, ele_q%ie,        &
     &    surf_q%isf_4_ele, ie_4_333)
!
      call set_27quad_2_8x8linear(ele_q%numele, ie_4_333,               &
     &    node_l%numnod, ele_l%ie)
!
      deallocate(ie_4_333)
!
      end subroutine connect_quad_mesh_2_linear
!
!  ---------------------------------------------------------------------
!
      subroutine connect_lag_mesh_2_linear(ele_q, node_l, ele_l)
!
      use m_geometry_constants
      use t_mesh_data
      use t_geometry_data
!
      use m_27quad_2_8x8linear
!
      type(element_data), intent(in) :: ele_q
      type(node_data), intent(inout) ::    node_l
      type(element_data), intent(inout) :: ele_l
!
!
      ele_l%numele = 8 * ele_q%numele
      ele_l%nnod_4_ele = num_t_linear
!
      call allocate_ele_connect_type(ele_l)
      call alloc_ele_geometry(ele_l)
!
      call set_27quad_2_8x8linear(ele_q%numele, ele_q%ie,               &
     &    node_l%numnod, ele_l%ie)
!
      end subroutine connect_lag_mesh_2_linear
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gen_linear_group_info                                  &
     &         (ele_grp, sf_grp, ele_grp_l, surf_grp_l)
!
      use t_mesh_data
      use t_group_data
      use convert_group_2_linear
!
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
!
      type(group_data), intent(inout) :: ele_grp_l
      type(surface_group_data), intent(inout) :: surf_grp_l
!
      ele_grp_l%num_grp = ele_grp%num_grp
      ele_grp_l%num_item = 8 * ele_grp%num_item
      call allocate_grp_type_num(ele_grp_l)
      call allocate_grp_type_item(ele_grp_l)
!
      call convert_ele_group_2_linear                                   &
     &   (ele_grp%num_grp, ele_grp%num_item, ele_grp%grp_name,          &
     &    ele_grp%istack_grp, ele_grp%item_grp,                         &
     &    ele_grp_l%num_item, ele_grp_l%grp_name,                       &
     &    ele_grp_l%istack_grp, ele_grp_l%item_grp)
!
      surf_grp_l%num_grp = sf_grp%num_grp
      surf_grp_l%num_item = 4 * sf_grp%num_item
      call allocate_sf_grp_type_num(surf_grp_l)
      call allocate_sf_grp_type_item(surf_grp_l)
!
      call convert_surf_group_2_linear(sf_grp%num_grp, sf_grp%num_item, &
     &    sf_grp%grp_name, sf_grp%istack_grp, sf_grp%item_sf_grp,       &
     &    surf_grp_l%num_item, surf_grp_l%grp_name,                     &
     &    surf_grp_l%istack_grp, surf_grp_l%item_sf_grp)
!
      end subroutine gen_linear_group_info
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_internal_list_lin_20(node_q, ele_q, surf_q,        &
     &          node_l, ele_l, surf_l, edge_l)
!
      use m_machine_parameter
      use t_geometry_data
      use t_mesh_data
      use t_surface_data
      use t_edge_data
!
      use set_internal_list_4_linear
!
      type(node_data), intent(in) ::    node_q
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
!
      type(node_data), intent(inout) ::    node_l
      type(element_data), intent(inout) :: ele_l
      type(surface_data), intent(inout) :: surf_l
      type(edge_data),    intent(inout) :: edge_l
!
!
      call set_internal_list_4_linear_20                                &
     &   (node_q%numnod, node_q%internal_node, ele_q%numele,            &
     &    surf_q%numsurf, ele_q%interior_ele, surf_q%interior_surf,     &
     &    node_l%numnod, ele_l%numele, surf_l%numsurf,                  &
     &    edge_l%numedge, ele_l%ie, surf_l%ie_surf,                     &
     &    edge_l%ie_edge, ele_l%interior_ele,                           &
     &    surf_l%interior_surf, edge_l%interior_edge)
!
      end subroutine set_internal_list_lin_20
!
!  ---------------------------------------------------------------------
!
      subroutine set_internal_list_lin_27                               &
     &         (node_q, node_l, ele_l, surf_l, edge_l)
!
      use m_machine_parameter
      use t_geometry_data
      use t_mesh_data
      use t_surface_data
      use t_edge_data
!
      use set_internal_list_4_linear
!
      type(node_data), intent(in) ::    node_q
!
      type(node_data), intent(inout) ::    node_l
      type(element_data), intent(inout) :: ele_l
      type(surface_data), intent(inout) :: surf_l
      type(edge_data),    intent(inout) :: edge_l
!
!
      call set_internal_list_4_linear_27(node_q%internal_node,          &
     &    node_l%numnod, ele_l%numele, surf_l%numsurf,                  &
     &    edge_l%numedge, ele_l%ie, surf_l%ie_surf,                     &
     &    edge_l%ie_edge, ele_l%interior_ele,                           &
     &    surf_l%interior_surf, edge_l%interior_edge)
!
      end subroutine set_internal_list_lin_27
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_linear_nod_phys(node_l, nod_fld_q, nod_fld_l)
!
      use t_mesh_data
      use t_phys_data
!
      type(node_data), intent(in) ::    node_l
      type(phys_data), intent(in) :: nod_fld_q
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call copy_field_name_type(nod_fld_q, nod_fld_l)
      call alloc_phys_data_type(node_l%numnod, nod_fld_l)
!
      end subroutine init_linear_nod_phys
!
!  ---------------------------------------------------------------------
!
      subroutine copy_nod_phys_2_linear                                 &
     &         (node_q, nod_fld_q, node_l, nod_fld_l)
!
      use t_geometry_data
      use t_mesh_data
      use t_phys_data
!
      use set_data_4_quad27
!
      type(node_data), intent(in) ::    node_q
      type(phys_data), intent(in) :: nod_fld_q
!
      type(node_data), intent(in) ::    node_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call copy_original_data(node_q%numnod, nod_fld_q%ntot_phys,       &
     &    nod_fld_q%d_fld, node_l%numnod, nod_fld_l%d_fld)
!
      end subroutine copy_nod_phys_2_linear
!
!  ---------------------------------------------------------------------
!
      subroutine generate_phys_on_surf(node_q, ele_q, surf_q,           &
     &          node_l, nod_fld_l)
!
      use t_geometry_data
      use t_mesh_data
      use t_phys_data
!
      use set_data_4_quad27
!
      type(node_data), intent(in) ::    node_q
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
!
      type(node_data), intent(in) ::    node_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_fields_on_surf                                           &
     &   (node_q%numnod, surf_q%numsurf, ele_q%numele,                  &
     &    ele_q%ie, surf_q%ie_surf, nod_fld_l%ntot_phys,                &
     &    node_l%numnod, nod_fld_l%d_fld)
!
      end subroutine generate_phys_on_surf
!
!  ---------------------------------------------------------------------
!
      end module cvt_quad_2_linear_mesh
