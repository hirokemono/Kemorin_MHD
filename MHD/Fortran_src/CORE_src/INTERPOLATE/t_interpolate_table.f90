!t_interpolate_table.f90
!      module t_interpolate_table
!
!> @brief Structure of interpolation table
!
!      Written by H.Matsui on Dec., 2008
!
!      subroutine unlink_mesh_data_4_itp_type(itp_info)
!      subroutine alloc_zero_itp_tables(itp_info)
!        type(interpolate_table), intent(inout) :: itp_info
!
!
      module t_interpolate_table
!
      use m_precision
!
      use m_constants
      use t_comm_table
      use t_geometry_data
      use t_interpolate_table_orgin
      use t_interpolate_tbl_dest
!
      implicit none
!
!
!> Structure of interpolation table
      type interpolate_table
        type(interpolate_table_org) ::  tbl_org
!< Structure of interpolation table for source grid
        type(interpolate_table_dest) :: tbl_dest
!< Structure of interpolation table for target grid
        type(communication_table) ::    comm_dest
!< Structure of commnucation table for target grid
        type(element_data) ::           ele_org
!< Structure of element data for original grid
      end type interpolate_table
!
!> Structure of interpolation table for both way
      type MG_itp_table
        type(interpolate_table) :: f2c
!< Structure of interpolation table for restoriction
        type(interpolate_table) :: c2f
!< Structure of interpolation table for prolongation
      end type MG_itp_table
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine dealloc_interpolate_tbl_type(itp_info)
!
      type(interpolate_table), intent(inout) :: itp_info
!
!
      call dealloc_type_itp_table_dest(itp_info%tbl_dest)
      call dealloc_type_itp_num_dest(itp_info%tbl_dest)
!
      call dealloc_type_itp_table_org(itp_info%tbl_org)
      call dealloc_type_istack_tbl_wtp_smp(itp_info%tbl_org)
      call dealloc_type_itp_num_org(itp_info%tbl_org)
!
      end subroutine dealloc_interpolate_tbl_type
!
!------------------------------------------------------------------
!
      subroutine alloc_zero_itp_tables(itp_info)
!
      type(interpolate_table), intent(inout) :: itp_info
!
!
      itp_info%tbl_org%num_dest_domain = 0
      itp_info%tbl_org%ntot_table_org =  0
      call alloc_type_itp_num_org(itp_info%tbl_org)
      call alloc_type_istack_tbl_wtp_smp(izero, itp_info%tbl_org)
      call alloc_type_itp_table_org(itp_info%tbl_org)
!
!
      itp_info%tbl_dest%num_org_domain =  0
      itp_info%tbl_dest%ntot_table_dest = 0
      call alloc_type_itp_num_dest(itp_info%tbl_dest)
      call alloc_type_itp_table_dest(itp_info%tbl_dest)
!
      end subroutine alloc_zero_itp_tables
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine unlink_mesh_data_4_itp_type(itp_info)
!
      type(interpolate_table), intent(inout) :: itp_info
!
!
      call unlink_ele_connect_type(itp_info%ele_org)
!
      end subroutine unlink_mesh_data_4_itp_type
!
!------------------------------------------------------------------
!
      end module t_interpolate_table
