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
!      subroutine set_ele_nod_bc_type_layer(mesh, layer, scaler_bc)
!        type(mesh_geometry),    intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: layer
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!
!      subroutine set_ele_nodal_bc_type_potential(mesh, scaler_bc)
!        type(mesh_geometry),    intent(in) :: mesh
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!      subroutine set_ele_nodal_bc_type_part_pot(mesh, layer, scaler_bc)
!        type(mesh_geometry),       intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: layer
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
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
      use count_num_nodal_fields
      use set_bc_element
!
      type(mesh_geometry),       intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      integer (kind= kint) :: nd
!
!
      do nd = 1, 3
        call count_bc_element_layer(mesh%node, mesh%ele,                &
     &   layer%iele_start_fld, layer%iele_end_fld,                      &
     &   vector_bc%num_idx_ibc(nd), vector_bc%ibc(1:mesh%node%numnod,nd))
        call count_bc_element_layer(mesh%node, mesh%ele,                &
     &   layer%iele_start_fld, layer%iele_end_fld,                      &
     &   vector_bc%num_idx_ibc2(nd), vector_bc%ibc2(1:mesh%node%numnod,nd))
      end do
!
!
      call cal_max_int_4_vector(vector_bc%nmax_idx_ibc,                 &
     &    vector_bc%num_idx_ibc)
      call cal_max_int_4_vector(vector_bc%nmax_idx_ibc2,                &
     &    vector_bc%num_idx_ibc2)
!
      call alloc_nod_bc_vector_ele_type(np_smp, mesh%ele%nnod_4_ele,    &
     &    vector_bc)
!
      call set_vect_bc_ele_type_layer(mesh%ele%nnod_4_ele,              &
     &    mesh, layer, vector_bc)
!
      call set_ele_4_vector_nodal_bc_type(np_smp, mesh, vector_bc)
!
      call dealloc_vector_ibc_type(vector_bc)
!
      end subroutine set_ele_nod_bc_type_vect_layer
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nod_bc_type_vect(mesh, vector_bc)
!
      use count_num_nodal_fields
      use set_bc_element
!
      type(mesh_geometry),       intent(in) :: mesh
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      integer (kind= kint) :: nd
!
!
      do nd = 1, 3
        call count_bc_element_whole(mesh%node, mesh%ele,                &
     &      vector_bc%num_idx_ibc(nd),       &
     &      vector_bc%ibc(1:mesh%node%numnod,nd))
        call count_bc_element_whole(mesh%node, mesh%ele,                &
     &      vector_bc%num_idx_ibc2(nd),       &
     &      vector_bc%ibc2(1:mesh%node%numnod,nd))
      end do
!
!
      call cal_max_int_4_vector(vector_bc%nmax_idx_ibc,                 &
     &    vector_bc%num_idx_ibc)
      call cal_max_int_4_vector(vector_bc%nmax_idx_ibc2,                &
     &    vector_bc%num_idx_ibc2)
!
      call alloc_nod_bc_vector_ele_type(np_smp, mesh%ele%nnod_4_ele,    &
     &    vector_bc)
!
      call s_set_vect_bc_element_type(mesh%ele%nnod_4_ele,              &
     &    mesh, vector_bc)
!
      call set_ele_4_vector_nodal_bc_type(np_smp, mesh, vector_bc)
!
      call dealloc_vector_ibc_type(vector_bc)
!
      end subroutine set_ele_nod_bc_type_vect
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_type_rotate(mesh, fluid, rotate)
!
      use t_geometry_data_MHD
      use set_bc_element
!
      type(mesh_geometry),    intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rotate
!
!
      call count_bc_element_layer(mesh%node, mesh%ele,                  &
     &    fluid%iele_start_fld, fluid%iele_end_fld,                     &
     &    rotate%num_idx_ibc, rotate%ibc)
      call count_bc_element_layer(mesh%node, mesh%ele,                  &
     &    fluid%iele_start_fld, fluid%iele_end_fld,                     &
     &    rotate%num_idx_ibc2, rotate%ibc2)
!
      call alloc_nod_bc_rotate_ele_type(np_smp, mesh%ele%nnod_4_ele,    &
     &    rotate)
!
      call set_bc_ele_type_rotate(mesh%ele%nnod_4_ele, mesh,            &
     &   fluid, rotate)
!
      call set_ele_4_rotate_nodal_bc_type(np_smp, mesh%ele%nnod_4_ele,  &
     &    rotate)
!
      call dealloc_rotate_ibc_type(rotate)
!
      end subroutine set_ele_nodal_bc_type_rotate
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nod_bc_type_layer(mesh, layer, scaler_bc)
!
      use t_geometry_data_MHD
      use set_bc_element
!
      type(mesh_geometry),    intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!
!
      call count_bc_element_layer(mesh%node, mesh%ele,                  &
     &    layer%iele_start_fld, layer%iele_end_fld,                     &
     &    scaler_bc%num_idx_ibc, scaler_bc%ibc)
      call count_bc_element_layer(mesh%node, mesh%ele,                  &
     &    layer%iele_start_fld, layer%iele_end_fld,                     &
     &    scaler_bc%num_idx_ibc2, scaler_bc%ibc2)
!
      call alloc_nod_bc_scalar_ele_type(np_smp, mesh%ele%nnod_4_ele,    &
     &    scaler_bc)
!
      call set_bc_ele_type_layer(mesh%ele%nnod_4_ele, mesh,             &
     &     layer, scaler_bc)
!
      call set_ele_4_scalar_nodal_bc_type(np_smp, mesh%ele%nnod_4_ele,  &
     &    scaler_bc)
!
      call dealloc_scalar_ibc_type(scaler_bc)
!
      end subroutine set_ele_nod_bc_type_layer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_type_potential(mesh, scaler_bc)
!
      use m_geometry_constants
      use set_bc_element
!
      type(mesh_geometry),    intent(in) :: mesh
      type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!
!
!   conunt node in elements for boundary
      call count_bc_element_whole                                       &
     &   (mesh%node, mesh%ele, scaler_bc%num_idx_ibc, scaler_bc%ibc)
      call count_bc_element_whole                                       &
     &   (mesh%node, mesh%ele, scaler_bc%num_idx_ibc2, scaler_bc%ibc2)
!
      call alloc_nod_bc_scalar_ele_type(np_smp, mesh%ele%nnod_4_ele,    &
     &    scaler_bc)
!
      call s_set_bc_element_type(num_t_linear, mesh, scaler_bc)
!
      call set_ele_4_scalar_nodal_bc_type(np_smp, num_t_linear,         &
     &    scaler_bc)
!
      call dealloc_scalar_ibc_type(scaler_bc)
!
      end subroutine set_ele_nodal_bc_type_potential
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_nodal_bc_type_part_pot(mesh, layer, scaler_bc)
!
      use m_geometry_constants
      use t_geometry_data_MHD
      use set_bc_element
!
      type(mesh_geometry),       intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!
!
      call count_bc_element_layer(mesh%node, mesh%ele,                  &
     &    layer%iele_start_fld, layer%iele_end_fld,                     &
     &    scaler_bc%num_idx_ibc, scaler_bc%ibc)
      call count_bc_element_layer(mesh%node, mesh%ele,                  &
     &    layer%iele_start_fld, layer%iele_end_fld,                     &
     &    scaler_bc%num_idx_ibc2, scaler_bc%ibc2)
!
      call alloc_nod_bc_scalar_ele_type(np_smp, mesh%ele%nnod_4_ele,    &
     &    scaler_bc)
!
      call set_bc_ele_type_layer(num_t_linear, mesh, layer, scaler_bc)
!
      call set_ele_4_scalar_nodal_bc_type(np_smp,  num_t_linear,        &
     &    scaler_bc)
!
      call dealloc_scalar_ibc_type(scaler_bc)
!
      end subroutine set_ele_nodal_bc_type_part_pot
!
!  ---------------------------------------------------------------------
!
      end module set_ele_nod_bc_type
