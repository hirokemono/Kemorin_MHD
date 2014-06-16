!cvt_quad_t_2_linear_t.f90
!      module cvt_quad_t_2_linear_t
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine generate_linear_nod_by_quad_t(mesh_q, surf_q, mesh_l)
!      subroutine connect_quad_type_2_linear_t(mesh_q, surf_q, mesh_l)
!      subroutine connect_lag_type_2_linear_t(mesh_q, mesh_l)
!      subroutine gen_linear_grp_type_by_t(group_q, group_l)
!
!      subroutine set_internal_list_4_ltype_20t(mesh_q, surf_q,         &
!     &          mesh_l, surf_l, edge_l)
!      subroutine set_internal_list_4_ltype_27t(mesh_q, surf_q,         &
!     &          mesh_l, surf_l, edge_l)
!
!      subroutine init_linear_phys_type(mesh_l, nod_fld_q, nod_fld_l)
!      subroutine copy_nod_phys_t_2_linear_t(mesh_q, nod_fld_q,         &
!     &          mesh_l, nod_fld_l)
!      subroutine generate_phys_t_on_sf_t(mesh_q, surf_q,               &
!     &          mesh_l, nod_fld_l)
!
      module cvt_quad_t_2_linear_t
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
      subroutine generate_linear_nod_by_quad_t(mesh_q, surf_q, mesh_l)
!
      use t_mesh_data
      use t_surface_data
!
      use set_geometry_4_quad27
      use coordinate_converter
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(surface_data), intent(in) :: surf_q
      type(mesh_geometry), intent(inout) :: mesh_l
!
!
      mesh_l%node%numnod = mesh_q%node%numnod                           &
     &                    + surf_q%numsurf + mesh_q%ele%numele
!
      call allocate_node_geometry_type(mesh_l%node)
!
      call set_position_on_surf(mesh_q%node%numnod, surf_q%numsurf,     &
     &    mesh_q%ele%numele, mesh_q%node%xx, mesh_q%ele%x_ele,          &
     &    surf_q%x_surf, mesh_l%node%numnod, mesh_l%node%xx)
!
      call position_2_sph(mesh_l%node%numnod, mesh_l%node%xx,           &
     &    mesh_l%node%rr, mesh_l%node%theta, mesh_l%node%phi,           &
     &    mesh_l%node%a_r, mesh_l%node%ss, mesh_l%node%a_s)
!
      end subroutine generate_linear_nod_by_quad_t
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine connect_quad_type_2_linear_t(mesh_q, surf_q, mesh_l)
!
      use t_mesh_data
      use t_surface_data
      use m_geometry_constants
!
      use m_27quad_2_8x8linear
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(surface_data), intent(in) :: surf_q
      type(mesh_geometry), intent(inout) :: mesh_l
!
!
      mesh_l%ele%numele = 8 * mesh_q%ele%numele
      mesh_l%ele%nnod_4_ele = num_t_linear
!
      call allocate_ele_connect_type(mesh_l%ele)
      call allocate_ele_geometry_type(mesh_l%ele)
!
      allocate(ie_4_333(mesh_q%ele%numele,27) )
!
      call init_27quad_2_8x8linear
      call gen_connect_quad27_from_quad20(mesh_q%node%numnod,           &
     &    mesh_q%ele%numele, surf_q%numsurf, mesh_q%ele%ie,             &
     &    surf_q%isf_4_ele, ie_4_333)
!
      call set_27quad_2_8x8linear(mesh_q%ele%numele, ie_4_333,          &
     &    mesh_l%node%numnod, mesh_l%ele%ie)
!
      deallocate(ie_4_333)
!
      end subroutine connect_quad_type_2_linear_t
!
!  ---------------------------------------------------------------------
!
      subroutine connect_lag_type_2_linear_t(mesh_q, mesh_l)
!
      use t_mesh_data
!
      use m_27quad_2_8x8linear
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(mesh_geometry), intent(inout) :: mesh_l
!
!
      mesh_l%ele%numele = 8 * mesh_q%ele%numele
!
      call allocate_ele_connect_type(mesh_l%ele)
      call allocate_ele_geometry_type(mesh_l%ele)
!
      call init_27quad_2_8x8linear
      call set_27quad_2_8x8linear(mesh_q%ele%numele, mesh_q%ele%ie,     &
     &    mesh_l%node%numnod, mesh_l%ele%ie)
!
      end subroutine connect_lag_type_2_linear_t
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gen_linear_grp_type_by_t(group_q, group_l)
!
      use t_mesh_data
      use t_group_data
      use convert_group_2_linear
!
      type(mesh_groups), intent(in) :: group_q
      type(mesh_groups), intent(inout) :: group_l
!
!
      call link_group_type(group_q%nod_grp, group_l%nod_grp)
!
      group_l%ele_grp%num_grp = group_q%ele_grp%num_grp
      group_l%ele_grp%num_item = 8 * group_q%ele_grp%num_item
      call allocate_grp_type_num(group_l%ele_grp)
      call allocate_grp_type_item(group_l%ele_grp)
!
      call convert_ele_group_2_linear(group_q%ele_grp%num_grp,          &
     &    group_q%ele_grp%num_item, group_q%ele_grp%grp_name,           &
     &    group_q%ele_grp%istack_grp, group_q%ele_grp%item_grp,         &
     &    group_l%ele_grp%num_item, group_l%ele_grp%grp_name,           &
     &    group_l%ele_grp%istack_grp, group_l%ele_grp%item_grp)
!
      group_l%surf_grp%num_grp = group_q%surf_grp%num_grp
      group_l%surf_grp%num_item = 4 * group_q%surf_grp%num_item
      call allocate_sf_grp_type_num(group_l%surf_grp)
      call allocate_sf_grp_type_item(group_l%surf_grp)
!
      call convert_surf_group_2_linear(group_q%surf_grp%num_grp,        &
     &    group_q%surf_grp%num_item, group_q%surf_grp%grp_name,         &
     &    group_q%surf_grp%istack_grp, group_q%surf_grp%item_sf_grp,    &
     &    group_l%surf_grp%num_item, group_l%surf_grp%grp_name,         &
     &    group_l%surf_grp%istack_grp, group_l%surf_grp%item_sf_grp)
!
      end subroutine gen_linear_grp_type_by_t
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_internal_list_4_ltype_20t(mesh_q, surf_q,          &
     &          mesh_l, surf_l, edge_l)
!
      use t_mesh_data
      use t_surface_data
      use t_edge_data
!
      use set_internal_list_4_linear
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(surface_data), intent(in) :: surf_q
!
      type(mesh_geometry), intent(inout) :: mesh_l
      type(surface_data), intent(inout) :: surf_l
      type(edge_data),    intent(inout) :: edge_l
!
!
      call set_internal_list_4_linear_20(mesh_q%node%numnod,            &
     &    mesh_q%node%internal_node, mesh_q%ele%numele, surf_q%numsurf, &
     &    mesh_q%ele%interior_ele, surf_q%interior_surf,                &
     &    mesh_l%node%numnod, mesh_l%ele%numele, surf_l%numsurf,        &
     &    edge_l%numedge, mesh_l%ele%ie, surf_l%ie_surf,                &
     &    edge_l%ie_edge, mesh_l%ele%interior_ele,                      &
     &    surf_l%interior_surf, edge_l%interior_edge)
!
      end subroutine set_internal_list_4_ltype_20t
!
!  ---------------------------------------------------------------------
!
      subroutine set_internal_list_4_ltype_27t(mesh_q, surf_q,          &
     &          mesh_l, surf_l, edge_l)
!
      use t_mesh_data
      use t_surface_data
      use t_edge_data
!
      use set_internal_list_4_linear
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(surface_data), intent(in) :: surf_q
!
      type(mesh_geometry), intent(inout) :: mesh_l
      type(surface_data), intent(inout) :: surf_l
      type(edge_data),    intent(inout) :: edge_l
!
!
      call set_internal_list_4_linear_27(mesh_q%node%internal_node,     &
     &    mesh_l%node%numnod, mesh_l%ele%numele, surf_l%numsurf,        &
     &    edge_l%numedge, mesh_l%ele%ie, surf_l%ie_surf,                &
     &    edge_l%ie_edge, mesh_l%ele%interior_ele,                      &
     &    surf_l%interior_surf, edge_l%interior_edge)
!
      end subroutine set_internal_list_4_ltype_27t
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_linear_phys_type(mesh_l, nod_fld_q, nod_fld_l)
!
      use t_mesh_data
      use t_phys_data
!
      type(mesh_geometry), intent(in) :: mesh_l
      type(phys_data), intent(in) :: nod_fld_q
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call link_field_name_type(nod_fld_q, nod_fld_l)
      call alloc_phys_data_type(mesh_l%node%numnod, nod_fld_l)
!
      end subroutine init_linear_phys_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_nod_phys_t_2_linear_t(mesh_q, nod_fld_q,          &
     &          mesh_l, nod_fld_l)
!
      use t_mesh_data
      use t_phys_data
!
      use set_data_4_quad27
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(phys_data), intent(in) :: nod_fld_q
      type(mesh_geometry), intent(in) :: mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call copy_original_data(mesh_q%node%numnod, nod_fld_q%ntot_phys,  &
     &    nod_fld_q%d_fld, mesh_l%node%numnod, nod_fld_l%d_fld)
!
      end subroutine copy_nod_phys_t_2_linear_t
!
!  ---------------------------------------------------------------------
!
      subroutine generate_phys_t_on_sf_t(mesh_q, surf_q,                &
     &          mesh_l, nod_fld_l)
!
      use t_mesh_data
      use t_surface_data
      use t_phys_data
!
      use set_data_4_quad27
!
      type(mesh_geometry), intent(in) :: mesh_q
      type(surface_data), intent(in) :: surf_q
      type(mesh_geometry), intent(in) :: mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_fields_on_surf(mesh_q%node%numnod, surf_q%numsurf,       &
     &    mesh_q%ele%numele, mesh_q%ele%ie, surf_q%ie_surf,             &
     &    nod_fld_l%ntot_phys, mesh_l%node%numnod, nod_fld_l%d_fld)
!
      end subroutine generate_phys_t_on_sf_t
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module cvt_quad_t_2_linear_t
