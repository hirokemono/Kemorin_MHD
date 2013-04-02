!
!     module set_bc_element_type
!
!      Written by H. Matsui and H.Okuda  on July 2001
!      Modified by H. Matsui on June, 2005
!      Modified by H. Matsui on Jan., 2009
!
!      subroutine s_set_bc_element_type(num_t, mesh, scaler_bc)
!        integer (kind= kint), intent(in) :: num_t
!        type(mesh_geometry),  intent(in) :: mesh
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!      subroutine set_bc_ele_type_layer(num_t, mesh, layer, scaler_bc)
!        integer (kind= kint), intent(in)    :: num_t
!        type(mesh_geometry),       intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: layer
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!      subroutine set_bc_ele_type_rotate(num_t, mesh, layer, rotate)
!        integer (kind= kint), intent(in)    :: num_t
!        type(mesh_geometry),       intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: layer
!        type(scaler_rotaion_nod_bc_type), intent(inout) :: rotate
!
!      subroutine s_set_vect_bc_element_type(num_t, mesh, vector_bc)
!        integer (kind= kint), intent(in) :: num_t
!        type(mesh_geometry),  intent(in) :: mesh
!        type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!      subroutine set_vect_bc_ele_type_layer(num_t, mesh, layer,        &
!     &          vector_bc)
!        integer (kind= kint), intent(in)    :: num_t
!        type(mesh_geometry),       intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: layer
!        type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
!      subroutine set_ele_4_vector_nodal_bc_type(np_smp, mesh, vector_bc)
!      integer(kind = kint), intent(in) :: np_smp
!        type(mesh_geometry), intent(in) :: mesh
!        type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!      subroutine set_ele_4_scalar_nodal_bc_type(np_smp, num_t,         &
!     &          mesh, scaler_bc)
!        integer (kind= kint), intent(in) :: np_smp, num_t
!        type(mesh_geometry),    intent(in) :: mesh
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!      subroutine set_ele_4_rotate_nodal_bc_type(np_smp, num_t,         &
!     &          mesh, rotate)
!        integer (kind= kint), intent(in) :: np_smp, num_t
!        type(mesh_geometry),    intent(in) :: mesh
!        type(scaler_rotaion_nod_bc_type), intent(inout) :: rotate
!
      module set_bc_element_type
!
      use m_precision
!
      use t_mesh_data
      use t_nodal_bc_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_bc_element_type(num_t, mesh, scaler_bc)
!
      use set_bc_element
!
      integer (kind= kint), intent(in) :: num_t
      type(mesh_geometry),  intent(in) :: mesh
      type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!
      integer (kind= kint), parameter :: ione = 1
!
!
      if ( scaler_bc%num_idx_ibc.gt. 0 ) then
        call set_bc_ele(mesh%node%numnod,                               &
     &      mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,          &
     &      scaler_bc%num_idx_ibc, ione, mesh%ele%numele,               &
     &      scaler_bc%ibc,  scaler_bc%ele_bc_id,  scaler_bc%nod_bc_id,  &
     &      num_t)
      end if
!
      if ( scaler_bc%num_idx_ibc2.gt. 0 ) then
        call set_bc_ele(mesh%node%numnod,                               &
     &      mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,          &
     &      scaler_bc%num_idx_ibc2, ione, mesh%ele%numele,              &
     &      scaler_bc%ibc2, scaler_bc%ele_bc2_id, scaler_bc%nod_bc2_id, &
     &      num_t)
      end if
!
      end subroutine s_set_bc_element_type
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_ele_type_layer(num_t, mesh, layer, scaler_bc)
!
      use t_geometry_data_MHD
      use set_bc_element
!
      integer (kind= kint), intent(in)    :: num_t
      type(mesh_geometry),       intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!
!
      if ( scaler_bc%num_idx_ibc.gt. 0 ) then
        call set_bc_ele(mesh%node%numnod, mesh%ele%numele,              &
     &      mesh%ele%nnod_4_ele, mesh%ele%ie, scaler_bc%num_idx_ibc,    &
     &      layer%iele_start_fld, layer%iele_end_fld, scaler_bc%ibc,    &
     &      scaler_bc%ele_bc_id,  scaler_bc%nod_bc_id,  num_t)
      end if
!
      if ( scaler_bc%num_idx_ibc2.gt. 0 ) then
        call set_bc_ele(mesh%node%numnod, mesh%ele%numele,              &
     &      mesh%ele%nnod_4_ele, mesh%ele%ie, scaler_bc%num_idx_ibc2,   &
     &      layer%iele_start_fld, layer%iele_end_fld, scaler_bc%ibc2,   &
     &      scaler_bc%ele_bc2_id, scaler_bc%nod_bc2_id, num_t)
      end if
!
      end subroutine set_bc_ele_type_layer
!
!-----------------------------------------------------------------------
!
      subroutine set_bc_ele_type_rotate(num_t, mesh, layer, rotate)
!
      use t_geometry_data_MHD
      use set_bc_element
!
      integer (kind= kint), intent(in)    :: num_t
      type(mesh_geometry),       intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rotate
!
!
      if ( rotate%num_idx_ibc.gt. 0 ) then
        call set_bc_ele(mesh%node%numnod, mesh%ele%numele,              &
     &      mesh%ele%nnod_4_ele, mesh%ele%ie, rotate%num_idx_ibc,       &
     &      layer%iele_start_fld, layer%iele_end_fld, rotate%ibc,       &
     &      rotate%ele_bc_id,  rotate%nod_bc_id,  num_t)
      end if
!
      if ( rotate%num_idx_ibc2.gt. 0 ) then
        call set_bc_ele(mesh%node%numnod, mesh%ele%numele,              &
     &      mesh%ele%nnod_4_ele, mesh%ele%ie, rotate%num_idx_ibc2,      &
     &      layer%iele_start_fld, layer%iele_end_fld, rotate%ibc2,      &
     &      rotate%ele_bc2_id, rotate%nod_bc2_id, num_t)
      end if
!
      end subroutine set_bc_ele_type_rotate
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_set_vect_bc_element_type(num_t, mesh, vector_bc)
!
      use set_bc_element
!
      integer (kind= kint), intent(in) :: num_t
      type(mesh_geometry),  intent(in) :: mesh
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      integer (kind= kint) :: nd
      integer (kind= kint), parameter :: ione = 1
!
!
      do nd = 1, 3
        if ( vector_bc%num_idx_ibc(nd).gt. 0 ) then
          call set_bc_ele(mesh%node%numnod,                             &
     &        mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,        &
     &        vector_bc%nmax_idx_ibc, ione, mesh%ele%numele,            &
     &        vector_bc%ibc(1:mesh%node%numnod,nd),                     &
     &        vector_bc%ele_bc_id(1:vector_bc%nmax_idx_ibc,nd),         &
     &        vector_bc%nod_bc_id(1:vector_bc%nmax_idx_ibc,nd),         &
     &        num_t)
        end if
!
        if ( vector_bc%num_idx_ibc2(nd).gt. 0 ) then
          call set_bc_ele(mesh%node%numnod,                             &
     &        mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,        &
     &        vector_bc%nmax_idx_ibc2, ione, mesh%ele%numele,           &
     &        vector_bc%ibc2(1:mesh%node%numnod,nd),                    &
     &        vector_bc%ele_bc2_id(1:vector_bc%nmax_idx_ibc2,nd),       &
     &        vector_bc%nod_bc2_id(1:vector_bc%nmax_idx_ibc2,nd),       &
     &        num_t)
        end if
      end do
!
      end subroutine s_set_vect_bc_element_type
!
!-----------------------------------------------------------------------
!
      subroutine set_vect_bc_ele_type_layer(num_t, mesh, layer,         &
     &          vector_bc)
!
      use t_geometry_data_MHD
      use set_bc_element
!
      integer (kind= kint), intent(in)    :: num_t
      type(mesh_geometry),       intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      integer (kind= kint) :: nd
!
!
      do nd = 1, 3
        if ( vector_bc%num_idx_ibc(nd) .gt. 0 ) then
          call set_bc_ele(mesh%node%numnod, mesh%ele%numele,            &
     &        mesh%ele%nnod_4_ele, mesh%ele%ie,                         &
     &        vector_bc%nmax_idx_ibc,                                   &
     &        layer%iele_start_fld, layer%iele_end_fld,                 &
     &        vector_bc%ibc(1:mesh%node%numnod,nd),                     &
     &        vector_bc%ele_bc_id(1:vector_bc%nmax_idx_ibc,nd),         &
     &        vector_bc%nod_bc_id(1:vector_bc%nmax_idx_ibc,nd),         &
     &        num_t)
        end if
!
        if ( vector_bc%num_idx_ibc2(nd) .gt. 0 ) then
          call set_bc_ele(mesh%node%numnod, mesh%ele%numele,            &
     &        mesh%ele%nnod_4_ele, mesh%ele%ie,                         &
     &        vector_bc%nmax_idx_ibc2,                                  &
     &        layer%iele_start_fld, layer%iele_end_fld,                 &
     &        vector_bc%ibc2(1:mesh%node%numnod,nd),                    &
     &        vector_bc%ele_bc2_id(1:vector_bc%nmax_idx_ibc2,nd),       &
     &        vector_bc%nod_bc2_id(1:vector_bc%nmax_idx_ibc2,nd),       &
     &        num_t)
        end if
      end do
!
      end subroutine set_vect_bc_ele_type_layer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_ele_4_vector_nodal_bc_type(np_smp, mesh, vector_bc)
!
      use ordering_ele_4_fix_bd
!
      integer(kind = kint), intent(in) :: np_smp
      type(mesh_geometry), intent(in) :: mesh
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      integer(kind = kint) :: nd
!
      do nd = 1, 3
        call reordering_ele_4_fix_bd(np_smp,                            &
     &      vector_bc%nmax_idx_ibc, vector_bc%num_idx_ibc(nd),          &
     &      vector_bc%ele_bc_id(1:vector_bc%nmax_idx_ibc,nd),           &
     &      vector_bc%nod_bc_id(1:vector_bc%nmax_idx_ibc,nd),           &
     &      vector_bc%ibc_end(nd),                                      &
     &      vector_bc%ibc_shape(1:mesh%ele%nnod_4_ele,nd),              &
     &      vector_bc%ibc_stack(0:mesh%ele%nnod_4_ele,nd),              &
     &      vector_bc%ibc_stack_smp(0:mesh%ele%nnod_4_ele*np_smp,nd),   &
     &      mesh%ele%nnod_4_ele )
      end do
!
      end subroutine set_ele_4_vector_nodal_bc_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_4_scalar_nodal_bc_type(np_smp, num_t,          &
     &          mesh, scaler_bc)
!
      use ordering_ele_4_fix_bd
!
      integer (kind= kint), intent(in) :: np_smp, num_t
      type(mesh_geometry),    intent(in) :: mesh
      type(scaler_fixed_nod_bc_type), intent(inout) :: scaler_bc
!
!
      call reordering_ele_4_fix_bd(np_smp, scaler_bc%num_idx_ibc,       &
     &    scaler_bc%num_idx_ibc, scaler_bc%ele_bc_id,                   &
     &    scaler_bc%nod_bc_id, scaler_bc%ibc_end, scaler_bc%ibc_shape,  &
     &    scaler_bc%ibc_stack, scaler_bc%ibc_stack_smp, num_t)
!
      end subroutine set_ele_4_scalar_nodal_bc_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_4_rotate_nodal_bc_type(np_smp, num_t,          &
     &          mesh, rotate)
!
      use ordering_ele_4_fix_bd
!
      integer (kind= kint), intent(in) :: np_smp, num_t
      type(mesh_geometry),    intent(in) :: mesh
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rotate
!
!
      call reordering_ele_4_fix_bd(np_smp, rotate%num_idx_ibc,       &
     &    rotate%num_idx_ibc, rotate%ele_bc_id,                      &
     &    rotate%nod_bc_id, rotate%ibc_end, rotate%ibc_shape,        &
     &    rotate%ibc_stack, rotate%ibc_stack_smp, num_t)
!
      end subroutine set_ele_4_rotate_nodal_bc_type
!
!  ---------------------------------------------------------------------
!
      end module set_bc_element_type
