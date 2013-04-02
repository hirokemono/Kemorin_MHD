!
!      module set_geometry_to_types
!
!     Written by H. Matsui on Jan., 2009
!
!      subroutine set_mesh_data_2_type(femmesh)
!        type(mesh_data), intent(inout) :: femmesh
!
!      subroutine set_nod_comm_tbl_2_type(nod_comm)
!        type(communication_table), intent(inout) :: nod_comm
!      subroutine set_nod_geometry_2_type(nod)
!        type(node_data), intent(inout) :: nod
!      subroutine set_ele_geometry_2_type(ele)
!        type(element_data), intent(inout) :: ele
!
!
      module set_geometry_to_types
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_mesh_data_2_type(femmesh)
!
      use t_mesh_data
!
      type(mesh_data), intent(inout) :: femmesh
!
      call set_nod_comm_tbl_2_type(femmesh%mesh%nod_comm)
      call set_nod_geometry_2_type(femmesh%mesh%node)
      call set_ele_geometry_2_type(femmesh%mesh%ele)
!
      end subroutine set_mesh_data_2_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_nod_comm_tbl_2_type(nod_comm)
!
      use t_comm_table
      use m_nod_comm_table
!
      type(communication_table), intent(inout) :: nod_comm
!
      nod_comm%num_neib =    num_neib
      nod_comm%ntot_import = ntot_import
      nod_comm%ntot_export = ntot_export
!
      nod_comm%id_neib =>        id_neib
      nod_comm%num_import =>     num_import
      nod_comm%istack_import =>  istack_import
      nod_comm%item_import =>    item_import
      nod_comm%num_export =>     num_export
      nod_comm%istack_export =>  istack_export
      nod_comm%item_export =>    item_export
!
      end subroutine set_nod_comm_tbl_2_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_nod_geometry_2_type(nod)
!
      use t_geometry_data
      use m_geometry_parameter
      use m_geometry_data
!
      type(node_data), intent(inout) :: nod
!
!
      nod%numnod =        numnod
      nod%internal_node = internal_node
!
      nod%max_nod_smp =          maxnod_4_smp
      nod%max_internal_nod_smp = max_in_nod_4_smp
!
      nod%istack_nod_smp =>      inod_smp_stack
      nod%istack_internal_smp => inter_smp_stack
!
!
      nod%inod_global => globalnodid
      nod%xx => xx
!
      nod%rr =>    radius
      nod%a_r =>   a_radius
      nod%phi =>   longitude
      nod%theta => colatitude
      nod%ss =>    s_cylinder
      nod%a_s =>   a_s_cylinder
!
      end subroutine set_nod_geometry_2_type
!
!  ---------------------------------------------------------------------
!
      subroutine set_ele_geometry_2_type(ele)
!
      use t_geometry_data
      use m_geometry_parameter
      use m_geometry_data
!
      type(element_data), intent(inout) :: ele
!
!
      ele%numele =     numele
      ele%nnod_4_ele = nnod_4_ele
!
      ele%max_ele_smp =     maxele_4_smp
!
      ele%istack_ele_smp => iele_smp_stack
!
      ele%iele_global => globalelmid
      ele%ie =>          ie
      ele%elmtyp =>      elmtyp
      ele%nodelm =>      nodelm
!
      ele%interior_ele => interior_ele
      ele%e_multi =>      e_multi
!
      ele%x_ele =>     x_ele
      ele%r_ele =>     r_ele
      ele%ar_ele =>    ar_ele
      ele%phi_ele =>   phi_ele
      ele%theta_ele => theta_ele
      ele%s_ele =>     s_ele
      ele%as_ele =>    as_ele
!
      ele%volume_ele => volume_ele
      ele%a_vol_ele =>  a_vol_ele
!
      ele%volume =       volume
      ele%a_vol =        a_vol
!
      end subroutine set_ele_geometry_2_type
!
!  ---------------------------------------------------------------------
!
      end module set_geometry_to_types
