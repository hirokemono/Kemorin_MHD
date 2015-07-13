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
!    set node list for fluid
!
      call set_layer_fluid
!
!    set node and element list for conductor
!
      call set_layers_4_induction
!
      end subroutine set_layers
!
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
      subroutine set_layer_fluid
!
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_set_layers
      use count_smp_size_4_MHD
!
!
!    set node list for fluid
!
      call alloc_mat_node_flag(numnod)
!
      call count_node_4_layer(numnod, internal_node,                    &
     &     numnod_fluid, internal_node_fluid,                           &
     &     iele_fl_start, iele_fl_end, numele, nnod_4_ele, ie)
!
      call allocate_fluid_node_list
!
        call set_node_4_layer(numnod, numnod_fluid, inod_fluid,         &
     &        iele_fl_start, iele_fl_end, numele, nnod_4_ele, ie)
!
      call dealloc_mat_node_flag
!
      call count_smp_size_4_fluid
!
      end subroutine set_layer_fluid
!
! ---------------------------------------------------------------------
!
      subroutine set_layers_4_induction
!
      use m_control_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_element_group
      use m_set_layers
      use count_smp_size_4_MHD
!
!
!    count number of element for insulated core
!
      call count_ele_4_layer(numele, numele_in_core,                    &
     &    num_in_core_ele_grp, in_core_ele_grp_name,                    &
     &    ele_grp1%num_grp, mat_istack, mat_name)
!
!    set node list for conductor
!
      call alloc_mat_node_flag(numnod)
!
      call count_node_4_layer(numnod, internal_node,                    &
     &    numnod_conduct, internal_node_conduct,                        &
     &    iele_cd_start, iele_cd_end, numele, nnod_4_ele, ie)
!
      call count_node_4_layer(numnod, internal_node,                    &
     &    numnod_insulate, internal_node_insulate,                      &
     &    iele_ins_start, iele_ins_end, numele, nnod_4_ele, ie)
!
!      call count_node_4_layer(numnod, internal_node,                   &
!     &    numnod_in_core, internal_node_in_core,                       &
!     &    iele_ic_start, iele_ic_end, numele, nnod_4_ele, ie)
!
!  allocate list vector for fluid and conductive layer
!
      call allocate_conduct_node_list
!
!  set node list
!
        call set_node_4_layer(numnod, numnod_conduct, inod_conduct,     &
     &        iele_cd_start, iele_cd_end, numele, nnod_4_ele, ie)
!
        call set_node_4_layer(numnod, numnod_insulate, inod_insulate,   &
     &        iele_ins_start, iele_ins_end, numele, nnod_4_ele, ie)
!
!        call set_node_4_layer(numnod, numnod_in_core, inod_in_core,    &
!     &        iele_ic_start, iele_ic_end, numele, nnod_4_ele, ie)
!
      call dealloc_mat_node_flag
!
      call count_smp_size_4_conduct
      call count_smp_size_4_insulator
!      call count_smp_size_4_inner_core
!
      end subroutine set_layers_4_induction
!
! ---------------------------------------------------------------------
!
      end module set_layers_4_MHD
