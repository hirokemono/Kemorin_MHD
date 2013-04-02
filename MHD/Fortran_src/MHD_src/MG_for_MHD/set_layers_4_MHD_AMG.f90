!
!     module set_layers_4_MHD_AMG
!.......................................................................
!
!        programmed H.Matsui on Dec., 2008
!
!      subroutine set_layers_type_4_MHD(geom, grps, MHD_mesh)
!      subroutine set_empty_layers_type_4_MHD(geom, grps, MHD_mesh)
!        type(mesh_geometry), intent(in) :: geom
!        type(mesh_groups), intent(in) :: grps
!
!        type(mesh_data_MHD), intent(inout) :: MHD_mesh
!
!       subroutine for start and end element number of each layer
!       and set node lists for each layer
!
!
      module set_layers_4_MHD_AMG
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: set_layers_4_field
      private :: count_size_4_smp_layers_type
      private :: set_empty_layers_4_field, count_empty_smp_layers_type
!
! ---------------------------------------------------------------------
!
      contains
!
! ---------------------------------------------------------------------
!
      subroutine set_layers_type_4_MHD(geom, grps, MHD_mesh)
!
      use m_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use m_set_layers
!
      type(mesh_geometry), intent(in) :: geom
      type(mesh_groups), intent(in) :: grps
!
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
!
!
      call alloc_mat_node_flag(geom%node%numnod)
!
!    count number of element for insulated core
!
      call count_ele_4_layer(geom%ele%numele,                           &
     &    MHD_mesh%inner_core%numele_fld,                               &
     &    num_in_core_ele_grp, in_core_ele_grp_name,                    &
     &    grps%ele_grp%num_grp, grps%ele_grp%istack_grp,                &
     &    grps%ele_grp%grp_name)
!
!    count number of node for each layer
!
      call set_layers_4_field(geom%node, geom%ele, MHD_mesh%fluid)
      call set_layers_4_field(geom%node, geom%ele, MHD_mesh%conduct)
      call set_layers_4_field(geom%node, geom%ele, MHD_mesh%insulate)
!      call set_layers_4_field(geom%node, geom%ele, MHD_mesh%inner_core)
!
      call dealloc_mat_node_flag
!
      call count_size_4_smp_layers_type(MHD_mesh%fluid)
      call count_size_4_smp_layers_type(MHD_mesh%conduct)
      call count_size_4_smp_layers_type(MHD_mesh%insulate)
!      call count_size_4_smp_layers_type(MHD_mesh%inner_core)
!
      end subroutine set_layers_type_4_MHD
!
! ---------------------------------------------------------------------
!
      subroutine set_empty_layers_type_4_MHD(geom, grps, MHD_mesh)
!
      use m_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
!
      type(mesh_geometry), intent(in) :: geom
      type(mesh_groups), intent(in) :: grps
!
      type(mesh_data_MHD), intent(inout) :: MHD_mesh
!
!
!    count number of element for insulated core
!
      MHD_mesh%inner_core%numele_fld = 0
!
!    count number of node for each layer
!
      call set_empty_layers_4_field(MHD_mesh%fluid)
      call set_empty_layers_4_field(MHD_mesh%conduct)
      call set_empty_layers_4_field(MHD_mesh%insulate)
!      call set_empty_layers_4_field(MHD_mesh%inner_core)
!
      call count_empty_smp_layers_type(MHD_mesh%fluid)
      call count_empty_smp_layers_type(MHD_mesh%conduct)
      call count_empty_smp_layers_type(MHD_mesh%insulate)
!      call count_empty_smp_layers_type(MHD_mesh%inner_core)
!
      end subroutine set_empty_layers_type_4_MHD
!
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
      subroutine set_layers_4_field(nod, ele, fld)
!
      use m_control_parameter
      use t_geometry_data
      use t_geometry_data_MHD
      use m_set_layers
!
      type(node_data), intent(in) :: nod
      type(element_data), intent(in) :: ele
!
      type(field_geometry_data), intent(inout) :: fld
!
!
!    count number of node for each field
!
      call count_node_4_layer(nod%numnod, nod%internal_node,            &
     &    fld%numnod_fld, fld%internal_node_fld,                        &
     &    fld%iele_start_fld, fld%iele_end_fld,                         &
     &    ele%numele, ele%nnod_4_ele, ele%ie)
!
!  allocate list vector
!
      call allocate_field_nod_list(fld)
!
!  set node list
!
      call set_node_4_layer(nod%numnod, fld%numnod_fld, fld%inod_fld,   &
     &    fld%iele_start_fld, fld%iele_end_fld,                         &
     &    ele%numele, ele%nnod_4_ele, ele%ie)
!
!
      end subroutine set_layers_4_field
!
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
      subroutine count_size_4_smp_layers_type(fld)
!
      use m_machine_parameter
      use t_geometry_data_MHD
      use cal_minmax_and_stacks
!
      type(field_geometry_data), intent(inout) :: fld
!
!
      call allocate_geometry_field_smp(fld)
!
      call count_number_4_smp( np_smp, ione, fld%numnod_fld,            &
     &    fld%istack_nod_fld_smp, fld%maxnod_fld_smp )
!
      call count_number_4_smp( np_smp, ione, fld%internal_node_fld,     &
     &    fld%istack_inter_fld_smp, fld%max_in_nod_fld_smp )
!
      call count_number_4_smp( np_smp, fld%iele_start_fld,              &
     &    fld%iele_end_fld, fld%istack_ele_fld_smp, fld%maxele_fld_smp)
!
      end subroutine count_size_4_smp_layers_type
!
!-----------------------------------------------------------------------
! ---------------------------------------------------------------------
!
      subroutine set_empty_layers_4_field(fld)
!
      use t_geometry_data_MHD
      use m_set_layers
!
      type(field_geometry_data), intent(inout) :: fld
!
      fld%numnod_fld =        0
      fld%internal_node_fld = 0
!
!  allocate list vector
!
      call allocate_field_nod_list(fld)
!
      end subroutine set_empty_layers_4_field
!
! ---------------------------------------------------------------------
!
      subroutine count_empty_smp_layers_type(fld)
!
      use t_geometry_data_MHD
!
      type(field_geometry_data), intent(inout) :: fld
!
!
      call allocate_geometry_field_smp(fld)
!
      fld%maxnod_fld_smp =     0
      fld%max_in_nod_fld_smp = 0
      fld%maxele_fld_smp =     0
!
      fld%istack_nod_fld_smp = 0
      fld%istack_inter_fld_smp = 0
      fld%istack_ele_fld_smp = 0
!
      end subroutine count_empty_smp_layers_type
!
!-----------------------------------------------------------------------
!
      end module set_layers_4_MHD_AMG
