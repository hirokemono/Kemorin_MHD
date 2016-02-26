!
!     module set_layers_4_MHD
!.......................................................................
!
!     programmed H.Matsui in 2005
!     Modified by H. Matsui on Dec., 2008
!
!      subroutine set_layers(node, ele, ele_grp)
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
      subroutine set_layers(node, ele, ele_grp)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(inout) :: ele_grp
!
!    set node list for fluid
!
      call set_layer_fluid(node, ele)
!
!    set node and element list for conductor
!
      call set_layers_4_induction(node, ele, ele_grp)
!
      end subroutine set_layers
!
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
      subroutine set_layer_fluid(node, ele)
!
      use m_geometry_data_MHD
      use m_set_layers
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
!    set node list for fluid
!
      call alloc_mat_node_flag(node%numnod)
!
      call count_node_4_layer(node%numnod, node%internal_node,          &
     &     fluid1%numnod_fld, fluid1%internal_node_fld,                 &
     &     fluid1%iele_start_fld, fluid1%iele_end_fld,                  &
     &     ele%numele, ele%nnod_4_ele, ele%ie)
!
      call allocate_field_nod_list(fluid1)
!
      call set_node_4_layer                                             &
     &   (node%numnod, fluid1%numnod_fld, fluid1%inod_fld,              &
     &    fluid1%iele_start_fld, fluid1%iele_end_fld,                   &
     &    ele%numele, ele%nnod_4_ele, ele%ie)
!
      call dealloc_mat_node_flag
!
      call count_smp_size_4_area(fluid1)
!
      end subroutine set_layer_fluid
!
! ---------------------------------------------------------------------
!
      subroutine set_layers_4_induction(node, ele, ele_grp)
!
      use m_control_parameter
      use m_geometry_data_MHD
      use m_set_layers
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
!
!    count number of element for insulated core
!
      call count_ele_4_layer(ele%numele, inner_core1%numele_fld,        &
     &    num_in_core_ele_grp, in_core_ele_grp_name,                    &
     &    ele_grp%num_grp, ele_grp%istack_grp, ele_grp%grp_name)
!
!    set node list for conductor
!
      call alloc_mat_node_flag(node%numnod)
!
      call count_node_4_layer(node%numnod, node%internal_node,          &
     &    conduct1%numnod_fld, conduct1%internal_node_fld,              &
     &    conduct1%iele_start_fld, conduct1%iele_end_fld,               &
     &    ele%numele, ele%nnod_4_ele, ele%ie)
!
      call count_node_4_layer(node%numnod, node%internal_node,          &
     &    insulate1%numnod_fld, insulate1%internal_node_fld,            &
     &    insulate1%iele_start_fld, insulate1%iele_end_fld,             &
     &    ele%numele, ele%nnod_4_ele, ele%ie)
!
!      call count_node_4_layer(node%numnod, node%internal_node,         &
!     &    inner_core1%numnod_fld, inner_core1%internal_node_fld,       &
!     &    iele_ic_start, iele_ic_end,                                  &
!     &    ele%numele, ele%nnod_4_ele, ele%ie)
!
!  allocate list vector for fluid and conductive layer
!
      call allocate_field_nod_list(conduct1)
      call allocate_field_nod_list(insulate1)
      call allocate_field_nod_list(inner_core1)
!
!  set node list
!
      call set_node_4_layer                                             &
     &     (node%numnod, conduct1%numnod_fld, conduct1%inod_fld,        &
     &      conduct1%iele_start_fld, conduct1%iele_end_fld,             &
     &      ele%numele, ele%nnod_4_ele, ele%ie)
!
      call set_node_4_layer                                             &
     &     (node%numnod, insulate1%numnod_fld, insulate1%inod_fld,      &
     &      insulate1%iele_start_fld, insulate1%iele_end_fld,           &
     &      ele%numele, ele%nnod_4_ele, ele%ie)
!
!      call set_node_4_layer                                            &
!     &     (node%numnod, inner_core1%numnod_fld, inner_core1%inod_fld, &
!     &      iele_ic_start, iele_ic_end,                                &
!     &      ele%numele, ele%nnod_4_ele, ele%ie)
!
      call dealloc_mat_node_flag
!
      call count_smp_size_4_area(conduct1)
      call count_smp_size_4_area(insulate1)
!      call count_smp_size_4_area(inner_core1)
!
      end subroutine set_layers_4_induction
!
! ---------------------------------------------------------------------
!
      end module set_layers_4_MHD
