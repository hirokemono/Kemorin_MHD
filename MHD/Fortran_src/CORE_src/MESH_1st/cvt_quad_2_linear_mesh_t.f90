!cvt_quad_2_linear_mesh_t.f90
!      module cvt_quad_2_linear_mesh_t
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine generate_linear_nod_t_by_1st(mesh_l)
!      subroutine connect_quad_mesh_2_linear_t(mesh_l)
!      subroutine connect_lag_mesh_2_linear_t(mesh_l)
!      subroutine gen_linear_group_type(group_l)
!
!      subroutine set_internal_list_4_ltype_20(mesh_l, surf_l, edge_l)
!      subroutine set_internal_list_4_ltype_27(mesh_l, surf_l, edge_l)
!
!      subroutine init_linear_phys_type_by_1st(mesh_l, nod_fld_l)
!      subroutine copy_nod_phys_2_linear_t(mesh_l, nod_fld_l)
!      subroutine generate_phys_on_surf_t(mesh_l, nod_fld_l)
!
      module cvt_quad_2_linear_mesh_t
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
      subroutine generate_linear_nod_t_by_1st(mesh_l)
!
      use t_mesh_data
      use m_geometry_parameter
      use m_geometry_data
      use m_surface_geometry_data
!
      use set_geometry_4_quad27
      use coordinate_converter
!
      type(mesh_geometry), intent(inout) :: mesh_l
!
!
      mesh_l%node%numnod = numnod + numsurf + numele
!
      call allocate_node_geometry_type(mesh_l%node)
!
      call set_position_on_surf(numnod, numsurf, numele,                &
     &    xx, x_ele, x_surf, mesh_l%node%numnod,  mesh_l%node%xx)
!
      call position_2_sph(mesh_l%node%numnod, mesh_l%node%xx,           &
     &    mesh_l%node%rr, mesh_l%node%theta, mesh_l%node%phi,           &
     &    mesh_l%node%a_r, mesh_l%node%ss, mesh_l%node%a_s)
!
      end subroutine generate_linear_nod_t_by_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine connect_quad_mesh_2_linear_t(mesh_l)
!
      use t_mesh_data
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
!
      use m_27quad_2_8x8linear
!
      type(mesh_geometry), intent(inout) :: mesh_l
!
!
      mesh_l%ele%numele = 8*numele
      mesh_l%ele%nnod_4_ele = num_t_linear
!
      call allocate_ele_connect_type(mesh_l%ele)
      call allocate_ele_geometry_type(mesh_l%ele)
!
      allocate(ie_4_333(numele,27) )
!
      call gen_connect_quad27_from_quad20(numnod, numele, numsurf, ie,  &
     &    isf_4_ele, ie_4_333)
!
      call set_27quad_2_8x8linear(numele, ie_4_333,                     &
     &    mesh_l%node%numnod, mesh_l%ele%ie)
!
      deallocate(ie_4_333)
!
      end subroutine connect_quad_mesh_2_linear_t
!
!  ---------------------------------------------------------------------
!
      subroutine connect_lag_mesh_2_linear_t(mesh_l)
!
      use t_mesh_data
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
!
      use m_27quad_2_8x8linear
!
      type(mesh_geometry), intent(inout) :: mesh_l
!
!
      mesh_l%ele%numele = 8 * numele
      mesh_l%ele%nnod_4_ele = num_t_linear
!
      call allocate_ele_connect_type(mesh_l%ele)
      call allocate_ele_geometry_type(mesh_l%ele)
!
      call set_27quad_2_8x8linear(numele, ie,                           &
     &    mesh_l%node%numnod, mesh_l%ele%ie)
!
      end subroutine connect_lag_mesh_2_linear_t
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gen_linear_group_type(group_l)
!
      use t_mesh_data
      use t_group_data
      use m_element_group
      use m_surface_group
      use link_group_type_2_1st_mesh
      use convert_group_2_linear
!
      type(mesh_groups), intent(inout) :: group_l
!
!
      call link_node_group_to_type(group_l%nod_grp)
!
      group_l%ele_grp%num_grp = num_mat
      group_l%ele_grp%num_item = 8*num_mat_bc
      call allocate_grp_type_num(group_l%ele_grp)
      call allocate_grp_type_item(group_l%ele_grp)
!
      call convert_ele_group_2_linear(num_mat, num_mat_bc,              &
     &    mat_name, mat_istack, mat_item, group_l%ele_grp%num_item,     &
     &    group_l%ele_grp%grp_name, group_l%ele_grp%istack_grp,         &
     &    group_l%ele_grp%item_grp)
!
      group_l%surf_grp%num_grp = sf_grp1%num_grp
      group_l%surf_grp%num_item = 4*num_surf_bc
      call allocate_sf_grp_type_num(group_l%surf_grp)
      call allocate_sf_grp_type_item(group_l%surf_grp)
!
      call convert_surf_group_2_linear(sf_grp1%num_grp, num_surf_bc,    &
     &    surf_name, surf_istack, surf_item, group_l%surf_grp%num_item, &
     &    group_l%surf_grp%grp_name, group_l%surf_grp%istack_grp,       &
     &    group_l%surf_grp%item_sf_grp)
!
      end subroutine gen_linear_group_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_internal_list_4_ltype_20(mesh_l, surf_l, edge_l)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use t_mesh_data
      use t_surface_data
      use t_edge_data
!
      use set_internal_list_4_linear
!
      type(mesh_geometry), intent(inout) :: mesh_l
      type(surface_data), intent(inout) :: surf_l
      type(edge_data),    intent(inout) :: edge_l
!
!
      call set_internal_list_4_linear_20(numnod, internal_node,         &
     &    numele, numsurf, interior_ele, interior_surf,                 &
     &    mesh_l%node%numnod, mesh_l%ele%numele, surf_l%numsurf,        &
     &    edge_l%numedge, mesh_l%ele%ie, surf_l%ie_surf,                &
     &    edge_l%ie_edge, mesh_l%ele%interior_ele,                      &
     &    surf_l%interior_surf, edge_l%interior_edge)
!
      end subroutine set_internal_list_4_ltype_20
!
!  ---------------------------------------------------------------------
!
      subroutine set_internal_list_4_ltype_27(mesh_l, surf_l, edge_l)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use t_mesh_data
      use t_surface_data
      use t_edge_data
!
      use set_internal_list_4_linear
!
      type(mesh_geometry), intent(inout) :: mesh_l
      type(surface_data), intent(inout) :: surf_l
      type(edge_data),    intent(inout) :: edge_l
!
!
      call set_internal_list_4_linear_27(internal_node,                 &
     &    mesh_l%node%numnod, mesh_l%ele%numele, surf_l%numsurf,        &
     &    edge_l%numedge, mesh_l%ele%ie, surf_l%ie_surf,                &
     &    edge_l%ie_edge, mesh_l%ele%interior_ele,                      &
     &    surf_l%interior_surf, edge_l%interior_edge)
!
      end subroutine set_internal_list_4_ltype_27
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_linear_phys_type_by_1st(mesh_l, nod_fld_l)
!
      use m_node_phys_data
      use t_mesh_data
      use t_phys_data
      use link_data_type_to_1st_mesh
!
      type(mesh_geometry), intent(in) :: mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call link_nodal_fld_type_names(nod_fld_l)
      call alloc_phys_data_type(mesh_l%node%numnod, nod_fld_l)
!
      end subroutine init_linear_phys_type_by_1st
!
!  ---------------------------------------------------------------------
!
      subroutine copy_nod_phys_2_linear_t(mesh_l, nod_fld_l)
!
      use m_geometry_parameter
      use m_node_phys_data
      use t_mesh_data
      use t_phys_data
!
      use set_data_4_quad27
!
      type(mesh_geometry), intent(in) :: mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call copy_original_data(numnod, num_tot_nod_phys,                 &
     &    d_nod, mesh_l%node%numnod, nod_fld_l%d_fld)
!
      end subroutine copy_nod_phys_2_linear_t
!
!  ---------------------------------------------------------------------
!
      subroutine generate_phys_on_surf_t(mesh_l, nod_fld_l)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use t_mesh_data
      use t_phys_data
!
      use set_data_4_quad27
!
      type(mesh_geometry), intent(in) :: mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_fields_on_surf(numnod, numsurf, numele,                  &
     &    ie, ie_surf, nod_fld_l%ntot_phys, mesh_l%node%numnod,         &
     &    nod_fld_l%d_fld)
!
      end subroutine generate_phys_on_surf_t
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module cvt_quad_2_linear_mesh_t
