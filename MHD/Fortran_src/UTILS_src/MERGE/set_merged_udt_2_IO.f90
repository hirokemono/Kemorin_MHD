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
      fem_ucd%nnod = merge_tbl%nnod_merged
      call link_node_data_type_2_output(merged%node)
!
      call count_udt_elements(merged%node%numnod,                       &
     &    merge_tbl%nele_merged, merged%ele%nnod_4_ele,                 &
     &    merged%ele%ie, fem_ucd)
      call allocate_ucd_ele(fem_ucd)
!
      call set_udt_global_connect(merged%node%numnod,                   &
     &    merge_tbl%nele_merged, merged%ele%nnod_4_ele,                 &
     &    merged%ele%iele_global, merged%ele%ie, fem_ucd)
!
      end subroutine link_merged_grd_2_udt_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_merged_data_2_udt_IO
!
!
      fem_ucd%nnod = merge_tbl%nnod_merged
      call link_node_data_type_2_output(merged%node)
!
      fem_ucd%d_ucd =>       merged_fld%d_fld
!
      end subroutine link_merged_data_2_udt_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_merged_phys_2_udt_IO
!
!
      fem_ucd%nnod =        merge_tbl%nnod_merged
      fem_ucd%num_field =   merged_fld%num_phys
      fem_ucd%ntot_comp =   merged_fld%ntot_phys
!
      fem_ucd%num_comp =>    merged_fld%num_component
      fem_ucd%phys_name =>   merged_fld%phys_name
      fem_ucd%d_ucd =>       merged_fld%d_fld
!
      end subroutine link_merged_phys_2_udt_IO
!
! -----------------------------------------------------------------------
!
      end module set_merged_udt_2_IO
