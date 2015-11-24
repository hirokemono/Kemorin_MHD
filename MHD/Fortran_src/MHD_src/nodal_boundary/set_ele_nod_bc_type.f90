!
!      module set_ele_nod_bc_type
!
!      Written by H. Matsui nad H. Okuda
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine set_ele_nod_bc_type_vect_layer(mesh, layer, vector_bc)
!        type(mesh_geometry),       intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: layer
!        type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!      subroutine set_ele_nod_bc_type_vect(mesh, vector_bc)
!        type(mesh_geometry),       intent(in) :: mesh
!        type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
!      subroutine set_ele_nodal_bc_type_rotate(mesh, fluid, rotate)
!        type(mesh_geometry),    intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: fluid
!        type(scaler_rotaion_nod_bc_type), intent(inout) :: rotate
!      subroutine set_ele_nod_bc_type_layer(mesh, layer, scalar_bc)
!        type(mesh_geometry),    intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: layer
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!      subroutine set_ele_nodal_bc_type_potential(mesh, scalar_bc)
!        type(mesh_geometry),    intent(in) :: mesh
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!      subroutine set_ele_nodal_bc_type_part_pot(mesh, layer, scalar_bc)
!        type(mesh_geometry),       intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: layer
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      module set_ele_nod_bc_type
!
      use m_precision
!
      use m_machine_parameter
      use t_mesh_data
      use t_nodal_bc_data
      use set_bc_element_type
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nod_bc_type_vect_layer(mesh, layer, vector_bc)
!
      use t_geometry_data_MHD
      use set_ele_nod_bc_vectors
!
      type(mesh_geometry),       intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
!
      call set_ele_nodal_bc_vector(mesh%node, mesh%ele, vector_bc)
      call set_ele_nodal_bc_vector_layer(mesh%node, mesh%ele,           &
     &   layer%iele_start_fld, layer%iele_end_fld,vector_bc)
      call dealloc_vector_ibc_type(vector_bc)
!
      end subroutine set_ele_nod_bc_type_vect_layer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_type_rotate(mesh, fluid, rotate)
!
      use t_geometry_data_MHD
      use set_ele_nod_bc_vectors
!
      type(mesh_geometry),    intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rotate
!
!
      call set_ele_nodal_bc_4_rotate(mesh%node, mesh%ele,               &
     &    fluid%iele_start_fld, fluid%iele_end_fld, rotate)
      call dealloc_rotate_ibc_type(rotate)
!
      end subroutine set_ele_nodal_bc_type_rotate
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nod_bc_type_layer(mesh, layer, scalar_bc)
!
      use t_geometry_data_MHD
      use set_ele_nod_bc_vectors
!
      type(mesh_geometry),    intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      call set_ele_nodal_bc_scalar_layer(mesh%node, mesh%ele,           &
     &    layer%iele_start_fld, layer%iele_end_fld, scalar_bc)
      call dealloc_scalar_ibc_type(scalar_bc)
!
      end subroutine set_ele_nod_bc_type_layer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_type_potential(mesh, scalar_bc)
!
      use m_geometry_constants
      use set_ele_nod_bc_vectors
!
      type(mesh_geometry),    intent(in) :: mesh
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      call set_ele_nodal_bc_potential(mesh%node, mesh%ele, scalar_bc)
      call dealloc_scalar_ibc_type(scalar_bc)
!
      end subroutine set_ele_nodal_bc_type_potential
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_type_part_pot(mesh, layer, scalar_bc)
!
      use m_geometry_constants
      use t_geometry_data_MHD
      use set_ele_nod_bc_vectors
!
      type(mesh_geometry),       intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
!
      call ele_nodal_bc_potential_layer(mesh%node, mesh%ele,            &
     &    layer%iele_start_fld, layer%iele_end_fld, scalar_bc)
      call dealloc_scalar_ibc_type(scalar_bc)
!
      end subroutine set_ele_nodal_bc_type_part_pot
!
!  ---------------------------------------------------------------------
!
      end module set_ele_nod_bc_type
