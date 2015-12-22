!
!     module set_layers_4_MHD
!.......................................................................
!
!     programmed H.Matsui in 2005
!     Modified by H. Matsui on Dec., 2008
!
!      subroutine set_layers
!       subroutine for start and end element number of each layer
!       and set node lists for each layer
!
!
      module set_layers_4_MHD
!
      use m_precision
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
      subroutine set_layers
!
      use m_group_data
!
!    set node list for fluid
!
      call set_layer_fluid
!
!    set node and element list for conductor
!
      call set_layers_4_induction(ele_grp1)
!
      end subroutine set_layers
!
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
      subroutine set_layer_fluid
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_set_layers
!
!    set node list for fluid
!
      call alloc_mat_node_flag(node1%numnod)
!
      call count_node_4_layer(node1%numnod, node1%internal_node,        &
     &     fluid1%numnod_fld, fluid1%internal_node_fld,                 &
     &     fluid1%iele_start_fld, fluid1%iele_end_fld,                  &
     &     ele1%numele, ele1%nnod_4_ele, ele1%ie)
!
      call allocate_field_nod_list(fluid1)
!
      call set_node_4_layer                                             &
     &   (node1%numnod, fluid1%numnod_fld, fluid1%inod_fld,             &
     &    fluid1%iele_start_fld, fluid1%iele_end_fld,                   &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie)
!
      call dealloc_mat_node_flag
!
      call count_smp_size_4_area(fluid1)
!
      end subroutine set_layer_fluid
!
! ---------------------------------------------------------------------
!
      subroutine set_layers_4_induction(ele_grp)
!
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_set_layers
      use t_group_data
!
      type(group_data), intent(inout) :: ele_grp
!
!    count number of element for insulated core
!
      call count_ele_4_layer(ele1%numele, inner_core%numele_fld,        &
     &    num_in_core_ele_grp, in_core_ele_grp_name,                    &
     &    ele_grp%num_grp, ele_grp%istack_grp, ele_grp%grp_name)
!
!    set node list for conductor
!
      call alloc_mat_node_flag(node1%numnod)
!
      call count_node_4_layer(node1%numnod, node1%internal_node,        &
     &    conduct1%numnod_fld, conduct1%internal_node_fld,              &
     &    conduct1%iele_start_fld, conduct1%iele_end_fld,               &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie)
!
      call count_node_4_layer(node1%numnod, node1%internal_node,        &
     &    insulate1%numnod_fld, insulate1%internal_node_fld,            &
     &    insulate1%iele_start_fld, insulate1%iele_end_fld,             &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie)
!
!      call count_node_4_layer(node1%numnod, node1%internal_node,       &
!     &    inner_core%numnod_fld, inner_core%internal_node_fld,         &
!     &    iele_ic_start, iele_ic_end,                                  &
!     &    ele1%numele, ele1%nnod_4_ele, ele1%ie)
!
!  allocate list vector for fluid and conductive layer
!
      call allocate_field_nod_list(conduct1)
      call allocate_field_nod_list(insulate1)
      call allocate_field_nod_list(inner_core)
!
!  set node list
!
      call set_node_4_layer                                             &
     &     (node1%numnod, conduct1%numnod_fld, conduct1%inod_fld,       &
     &      conduct1%iele_start_fld, conduct1%iele_end_fld,             &
     &      ele1%numele, ele1%nnod_4_ele, ele1%ie)
!
      call set_node_4_layer                                             &
     &     (node1%numnod, insulate1%numnod_fld, insulate1%inod_fld,     &
     &      insulate1%iele_start_fld, insulate1%iele_end_fld,           &
     &      ele1%numele, ele1%nnod_4_ele, ele1%ie)
!
!      call set_node_4_layer                                            &
!     &     (node1%numnod, inner_core%numnod_fld, inner_core%inod_fld,  &
!     &      iele_ic_start, iele_ic_end,                                &
!     &      ele1%numele, ele1%nnod_4_ele, ele1%ie)
!
      call dealloc_mat_node_flag
!
      call count_smp_size_4_area(conduct1)
      call count_smp_size_4_area(insulate1)
!      call count_smp_size_4_area(inner_core)
!
      end subroutine set_layers_4_induction
!
! ---------------------------------------------------------------------
!
      end module set_layers_4_MHD
