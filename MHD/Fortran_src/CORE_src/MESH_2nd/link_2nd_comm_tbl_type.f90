!link_2nd_comm_tbl_type.f90
!     module link_2nd_comm_tbl_type
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine link_2nd_nod_comm_tbl_type(nod_comm)
!        type(communication_table), intent(in) :: nod_comm
!      subroutine link_2nd_ele_comm_tbl_type(ele_comm)
!        type(communication_table), intent(in) :: ele_comm
!      subroutine link_2nd_surf_comm_tbl_type(surf_comm)
!        type(communication_table), intent(in) :: surf_comm
!      subroutine link_2nd_edge_comm_tbl_type(edge_comm)
!        type(communication_table), intent(in) :: edge_comm
!
      module link_2nd_comm_tbl_type
!
      use m_precision
!
      use t_comm_table
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine link_2nd_nod_comm_tbl_type(nod_comm)
!
      use m_2nd_nod_comm_table
!
      type(communication_table), intent(in) :: nod_comm
!
!
      num_neib_2 =    nod_comm%num_neib
      ntot_import_2 = nod_comm%ntot_import
      ntot_export_2 = nod_comm%ntot_export
!
      id_neib_2 =>    nod_comm%id_neib
!
      num_import_2 =>    nod_comm%num_import
      istack_import_2 => nod_comm%istack_import
      item_import_2 =>   nod_comm%item_import
!
      num_export_2 =>    nod_comm%num_export
      istack_export_2 => nod_comm%istack_export
      item_export_2 =>   nod_comm%item_export
!
      end subroutine link_2nd_nod_comm_tbl_type
!
!------------------------------------------------------------------
!
      subroutine link_2nd_ele_comm_tbl_type(ele_comm)
!
      use m_2nd_ele_comm_table
!
      type(communication_table), intent(in) :: ele_comm
!
!
      num_neib_ele_2 =    ele_comm%num_neib
      ntot_import_ele_2 = ele_comm%ntot_import
      ntot_export_ele_2 = ele_comm%ntot_export
!
      id_neib_ele_2 =>    ele_comm%id_neib
!
      num_import_ele_2 =>    ele_comm%num_import
      istack_import_ele_2 => ele_comm%istack_import
      item_import_ele_2 =>   ele_comm%item_import
!
      num_export_ele_2 =>    ele_comm%num_export
      istack_export_ele_2 => ele_comm%istack_export
      item_export_ele_2 =>   ele_comm%item_export
!
      end subroutine link_2nd_ele_comm_tbl_type
!
!------------------------------------------------------------------
!
      subroutine link_2nd_surf_comm_tbl_type(surf_comm)
!
      use m_2nd_surf_comm_table
!
      type(communication_table), intent(in) :: surf_comm
!
!
      num_neib_surf_2 =    surf_comm%num_neib
      ntot_import_surf_2 = surf_comm%ntot_import
      ntot_export_surf_2 = surf_comm%ntot_export
!
      id_neib_surf_2 =>    surf_comm%id_neib
!
      num_import_surf_2 =>    surf_comm%num_import
      istack_import_surf_2 => surf_comm%istack_import
      item_import_surf_2 =>   surf_comm%item_import
!
      num_export_surf_2 =>    surf_comm%num_export
      istack_export_surf_2 => surf_comm%istack_export
      item_export_surf_2 =>   surf_comm%item_export
!
      end subroutine link_2nd_surf_comm_tbl_type
!
!------------------------------------------------------------------
!
      subroutine link_2nd_edge_comm_tbl_type(edge_comm)
!
      use m_2nd_geometry_data
!
      type(communication_table), intent(in) :: edge_comm
!
!
      edge_comm_2nd%num_neib =    edge_comm%num_neib
      edge_comm_2nd%ntot_import = edge_comm%ntot_import
      edge_comm_2nd%ntot_export = edge_comm%ntot_export
!
      edge_comm_2nd%id_neib =>    edge_comm%id_neib
!
      edge_comm_2nd%num_import =>    edge_comm%num_import
      edge_comm_2nd%istack_import => edge_comm%istack_import
      edge_comm_2nd%item_import =>   edge_comm%item_import
!
      edge_comm_2nd%num_export =>    edge_comm%num_export
      edge_comm_2nd%istack_export => edge_comm%istack_export
      edge_comm_2nd%item_export =>   edge_comm%item_export
!
      end subroutine link_2nd_edge_comm_tbl_type
!
!------------------------------------------------------------------
!
      end module link_2nd_comm_tbl_type
