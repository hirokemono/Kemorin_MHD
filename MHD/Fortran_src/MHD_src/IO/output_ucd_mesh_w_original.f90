!output_ucd_mesh_w_original.f90
!      module output_ucd_mesh_w_original
!
!      Programmed by H.Matsui on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine output_grd_file_w_org_connect                         &
!     &          (node, ele, nod_comm, nod_fld, ucd, m_ucd)
!      subroutine link_global_org_mesh_4_ucd(node, ele, ucd)
!        type(node_data), intent(in) :: node
!        type(element_data), intent(in) :: ele
!        type(communication_table), intent(in) :: nod_comm
!        type(phys_data), intent(in) :: nod_fld
!        type(ucd_data), intent(inout) :: ucd
!        type(merged_ucd_data), intent(inout) :: m_ucd
!
      module output_ucd_mesh_w_original
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_ucd_data
!
      implicit none
!
      private :: link_local_org_mesh_4_ucd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine output_grd_file_w_org_connect                          &
     &          (node, ele, nod_comm, nod_fld, ucd, m_ucd)
!
      use t_comm_table
      use t_phys_data
      use m_field_file_format
      use m_t_step_parameter
      use set_ucd_data_to_type
!
      use merged_udt_vtk_file_IO
      use parallel_ucd_IO_select
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(phys_data), intent(in) :: nod_fld
!
      type(ucd_data), intent(inout) :: ucd
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      if(i_step_output_ucd .eq. 0) return
!
      call link_num_field_2_ucd(nod_fld, ucd)
      call link_local_org_mesh_4_ucd(node, ele, ucd)
      call link_field_data_to_ucd(nod_fld, ucd)
!
      if (ucd%ifmt_file/icent .eq. iflag_single/icent) then
        call init_merged_ucd                                            &
     &     (node, ele, nod_comm, ucd, m_ucd)
      end if
!
      call sel_write_parallel_ucd_mesh(ucd, m_ucd)
!
      if(   mod(ucd%ifmt_file,icent)/iten .eq. iflag_udt/iten           &
     & .or. mod(ucd%ifmt_file,icent)/iten .eq. iflag_vtd/iten) then
        call deallocate_ucd_ele(ucd)
      end if
!
      if(mod(ucd%ifmt_file,icent)/iten .eq. iflag_vtd/iten) then
        call deallocate_ucd_node(ucd)
      end if
!
      end subroutine output_grd_file_w_org_connect
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_global_org_mesh_4_ucd(node, ele, ucd)
!
      use m_geometry_data_MHD
      use set_ucd_data_to_type
      use set_and_cal_udt_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_node_data_2_ucd(node, ucd)
      call const_udt_global_connect                                     &
     &   (node%internal_node, ele%numele, ele%nnod_4_ele,               &
     &    iele_global_org, ie_org, ucd)
!
      end subroutine link_global_org_mesh_4_ucd
!
!-----------------------------------------------------------------------
!
      subroutine link_local_org_mesh_4_ucd(node, ele, ucd)
!
      use m_geometry_data_MHD
      use set_and_cal_udt_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call const_udt_local_nodes(node%numnod, node%xx, ucd)
      call const_udt_local_connect                                      &
     &   (node%internal_node, ele%numele, ele%nnod_4_ele,               &
     &    ie_org, ucd)
!
      end subroutine link_local_org_mesh_4_ucd
!
!-----------------------------------------------------------------------
!
      end module output_ucd_mesh_w_original
