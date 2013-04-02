!
!     module count_bc_type_ele
!
!      Written by H. Matsui and H.Okuda  on July 2001
!      Modified by H. Matsui on June, 2005
!      Modified by H. Matsui on Nov., 2006
!      Modified by H. Matsui on Jan., 2009
!
!
!      subroutine count_bc_type_ele_vect(mesh, vector_bc)
!        type(mesh_geometry),    intent(in) :: mesh
!        type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!      subroutine count_bc_type_ele_vect_layer(mesh, layer, vector_bc)
!        type(mesh_geometry),    intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: layer
!        type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!      subroutine count_bc_type_ele_scalar(mesh, scalar_bc)
!        type(mesh_geometry),    intent(in) :: mesh
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!      subroutine count_bc_type_ele_layer(mesh, layer, rotate)
!        type(mesh_geometry),       intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: layer
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!      subroutine count_rot_bc_type_ele(mesh, layer, rotate)
!        type(mesh_geometry),       intent(in) :: mesh
!        type(field_geometry_data), intent(in) :: layer
!        type(scaler_rotaion_nod_bc_type), intent(inout) :: rotate
!
      module count_bc_type_ele
!
      use m_precision
!
      use t_mesh_data
      use t_nodal_bc_data
      use count_bc_element
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_type_ele_vect(mesh, vector_bc)
!
      type(mesh_geometry),    intent(in) :: mesh
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      integer (kind= kint), parameter :: ione = 1
      integer (kind= kint) :: nd
!
      do nd = 1, 3
        call count_bc_ele(mesh%node%numnod,                             &
     &      mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,          &
     &      ione, mesh%ele%numele,                                      &
     &      vector_bc%num_idx_ibc(nd),                                  &
     &      vector_bc%ibc(1:mesh%node%numnod,nd) )
      end do
!
      do nd = 1, 3
        call count_bc_ele(mesh%node%numnod,                             &
     &      mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,          &
     &      ione, mesh%ele%numele,                                      &
     &      vector_bc%num_idx_ibc2(nd),                                 &
     &      vector_bc%ibc2(1:mesh%node%numnod,nd) )
      end do
!
      end subroutine count_bc_type_ele_vect
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_type_ele_vect_layer(mesh, layer, vector_bc)
!
      use t_geometry_data_MHD
!
      type(mesh_geometry),    intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(vect_fixed_nod_bc_type), intent(inout) :: vector_bc
!
      integer (kind= kint) :: nd
!
      do nd = 1, 3
        call count_bc_ele(mesh%node%numnod,                             &
     &      mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,          &
     &      layer%iele_start_fld, layer%iele_end_fld,                   &
     &      vector_bc%num_idx_ibc(nd),                                  &
     &      vector_bc%ibc(1:mesh%node%numnod,nd) )
      end do
!
      do nd = 1, 3
        call count_bc_ele(mesh%node%numnod,                             &
     &      mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,          &
     &      layer%iele_start_fld, layer%iele_end_fld,                   &
     &      vector_bc%num_idx_ibc2(nd),                                 &
     &      vector_bc%ibc2(1:mesh%node%numnod,nd) )
      end do
!
      end subroutine count_bc_type_ele_vect_layer
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_bc_type_ele_scalar(mesh, scalar_bc)
!
      type(mesh_geometry),    intent(in) :: mesh
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      integer (kind= kint), parameter :: ione = 1
!
!   conunt node in elements for boundary
      call count_bc_ele(mesh%node%numnod,                               &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,            &
     &    ione, mesh%ele%numele, scalar_bc%num_idx_ibc, scalar_bc%ibc)
!
      call count_bc_ele(mesh%node%numnod,                               &
     &    mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,            &
     &    ione, mesh%ele%numele,                                        &
     &    scalar_bc%num_idx_ibc2, scalar_bc%ibc2)
!
      end subroutine count_bc_type_ele_scalar
!
!-----------------------------------------------------------------------
!
      subroutine count_bc_type_ele_layer(mesh, layer, scalar_bc)
!
      use t_geometry_data_MHD
!
      type(mesh_geometry),       intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      call count_bc_ele(mesh%node%numnod,                               &
     &     mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,           &
     &     layer%iele_start_fld, layer%iele_end_fld,                    &
     &     scalar_bc%num_idx_ibc, scalar_bc%ibc)
!
      call count_bc_ele(mesh%node%numnod,                               &
     &     mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,           &
     &     layer%iele_start_fld, layer%iele_end_fld,                    &
     &     scalar_bc%num_idx_ibc2, scalar_bc%ibc2)
!
      end subroutine count_bc_type_ele_layer
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_rot_bc_type_ele(mesh, layer, rotate)
!
      use t_geometry_data_MHD
!
      type(mesh_geometry),       intent(in) :: mesh
      type(field_geometry_data), intent(in) :: layer
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rotate
!
      call count_bc_ele(mesh%node%numnod,                               &
     &     mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,           &
     &     layer%iele_start_fld, layer%iele_end_fld,                    &
     &     rotate%num_idx_ibc, rotate%ibc)
!
      call count_bc_ele(mesh%node%numnod,                               &
     &     mesh%ele%numele, mesh%ele%nnod_4_ele, mesh%ele%ie,           &
     &     layer%iele_start_fld, layer%iele_end_fld,                    &
     &     rotate%num_idx_ibc2, rotate%ibc2)
!
      end subroutine count_rot_bc_type_ele
!
!-----------------------------------------------------------------------
!
      end module count_bc_type_ele
