!
!     module set_layers_4_MHD
!.......................................................................
!
!     programmed H.Matsui in 2005
!     Modified by H. Matsui on Dec., 2008
!
!!      subroutine set_layers(node, ele, ele_grp, MHD_mesh)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(inout) :: ele_grp
!!        type(mesh_data_MHD), intent(inout) :: MHD_mesh
!       subroutine for start and end element number of each layer
!       and set node lists for each layer
!
!
      module set_layers_4_MHD
!
      use m_precision
!
      use t_geometry_data
      use t_group_data
      use t_geometry_data_MHD
!
      implicit none
!
      private :: set_layer_fluid, set_layers_4_induction
!
! ---------------------------------------------------------------------
!
      contains
!
! ---------------------------------------------------------------------
!
      subroutine set_layers(node, ele, ele_grp, MHD_mesh)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(group_data), intent(inout) :: ele_grp
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
!
!    set node list for fluid
!
      call set_layer_fluid(node, ele, MHD_mesh%fluid)
!
!    set node and element list for conductor
!
      call set_layers_4_induction(node, ele, ele_grp,                   &
     &    MHD_mesh%conduct, MHD_mesh%insulate, MHD_mesh%inner_core)
!
      end subroutine set_layers
!
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
      subroutine set_layer_fluid(node, ele, fluid)
!
      use m_set_layers
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(field_geometry_data), intent(inout) :: fluid
!
!    set node list for fluid
!
      call alloc_mat_node_flag(node%numnod)
!
      call count_node_4_layer(node%numnod, node%internal_node,          &
     &     fluid%numnod_fld, fluid%internal_node_fld,                   &
     &     fluid%iele_start_fld, fluid%iele_end_fld,                    &
     &     ele%numele, ele%nnod_4_ele, ele%ie)
!
      call allocate_field_nod_list(fluid)
!
      call set_node_4_layer                                             &
     &   (node%numnod, fluid%numnod_fld, fluid%inod_fld,                &
     &    fluid%iele_start_fld, fluid%iele_end_fld,                     &
     &    ele%numele, ele%nnod_4_ele, ele%ie)
!
      call dealloc_mat_node_flag
!
      call count_smp_size_4_area(fluid)
!
      end subroutine set_layer_fluid
!
! ---------------------------------------------------------------------
!
      subroutine set_layers_4_induction(node, ele, ele_grp,             &
     &          conduct, insulate, inner_core)
!
      use m_control_parameter
      use m_set_layers
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
!
      type(field_geometry_data), intent(inout) :: conduct, insulate
      type(field_geometry_data), intent(inout) :: inner_core
!
!
!    count number of element for insulated core
!
      call count_ele_4_layer(ele%numele, inner_core%numele_fld,         &
     &    FEM_prm1%inner_core_group%num_group,                          &
     &    FEM_prm1%inner_core_group%group_name,                         &
     &    ele_grp%num_grp, ele_grp%istack_grp, ele_grp%grp_name)
!
!    set node list for conductor
!
      call alloc_mat_node_flag(node%numnod)
!
      call count_node_4_layer(node%numnod, node%internal_node,          &
     &    conduct%numnod_fld, conduct%internal_node_fld,                &
     &    conduct%iele_start_fld, conduct%iele_end_fld,                 &
     &    ele%numele, ele%nnod_4_ele, ele%ie)
!
      call count_node_4_layer(node%numnod, node%internal_node,          &
     &    insulate%numnod_fld, insulate%internal_node_fld,              &
     &    insulate%iele_start_fld, insulate%iele_end_fld,               &
     &    ele%numele, ele%nnod_4_ele, ele%ie)
!
!      call count_node_4_layer(node%numnod, node%internal_node,         &
!     &    inner_core%numnod_fld, inner_core%internal_node_fld,         &
!     &    iele_ic_start, iele_ic_end,                                  &
!     &    ele%numele, ele%nnod_4_ele, ele%ie)
!
!  allocate list vector for fluid and conductive layer
!
      call allocate_field_nod_list(conduct)
      call allocate_field_nod_list(insulate)
      call allocate_field_nod_list(inner_core)
!
!  set node list
!
      call set_node_4_layer                                             &
     &     (node%numnod, conduct%numnod_fld, conduct%inod_fld,          &
     &      conduct%iele_start_fld, conduct%iele_end_fld,               &
     &      ele%numele, ele%nnod_4_ele, ele%ie)
!
      call set_node_4_layer                                             &
     &     (node%numnod, insulate%numnod_fld, insulate%inod_fld,        &
     &      insulate%iele_start_fld, insulate%iele_end_fld,             &
     &      ele%numele, ele%nnod_4_ele, ele%ie)
!
!      call set_node_4_layer                                            &
!     &     (node%numnod, inner_core%numnod_fld, inner_core%inod_fld,   &
!     &      iele_ic_start, iele_ic_end,                                &
!     &      ele%numele, ele%nnod_4_ele, ele%ie)
!
      call dealloc_mat_node_flag
!
      call count_smp_size_4_area(conduct)
      call count_smp_size_4_area(insulate)
!      call count_smp_size_4_area(inner_core)
!
      end subroutine set_layers_4_induction
!
! ---------------------------------------------------------------------
!
      end module set_layers_4_MHD
