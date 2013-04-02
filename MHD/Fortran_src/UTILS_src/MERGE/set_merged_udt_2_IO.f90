!
!      module set_merged_udt_2_IO
!
!      Written by H. Matsui on Apr., 2010
!
!      subroutine link_merged_grd_2_udt_IO
!      subroutine link_merged_data_2_udt_IO
!      subroutine link_merged_phys_2_udt_IO
!
      module set_merged_udt_2_IO
!
      use m_precision
!
      use m_geometry_data_4_merge
      use m_ucd_data
      use set_ucd_data_to_type
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine link_merged_grd_2_udt_IO
!
      use set_and_cal_udt_data
!
!
      nnod_ucd = merge_tbl%nnod_merged
      call link_node_data_type_2_output(merged%node)
!
      call count_udt_elements(merged%node%numnod,                       &
     &    merge_tbl%nele_merged, merged%ele%nnod_4_ele,                 &
     &    merged%ele%ie)
      call allocate_ucd_ele
!
      call set_udt_global_connect(merged%node%numnod,                   &
     &    merge_tbl%nele_merged, merged%ele%nnod_4_ele,                 &
     &    merged%ele%iele_global, merged%ele%ie)
!
      end subroutine link_merged_grd_2_udt_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_merged_data_2_udt_IO
!
!
      nnod_ucd = merge_tbl%nnod_merged
      call link_node_data_type_2_output(merged%node)
!
      d_nod_ucd =>       merged_fld%d_fld
!
      end subroutine link_merged_data_2_udt_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_merged_phys_2_udt_IO
!
!
      nnod_ucd = merge_tbl%nnod_merged
      num_field_ucd =   merged_fld%num_phys
      ntot_comp_ucd =   merged_fld%ntot_phys
!
      istack_comp_ucd => merged_fld%istack_component
      num_comp_ucd =>    merged_fld%num_component
      phys_name_ucd =>   merged_fld%phys_name
      d_nod_ucd =>       merged_fld%d_fld
!
      end subroutine link_merged_phys_2_udt_IO
!
! -----------------------------------------------------------------------
!
      end module set_merged_udt_2_IO
