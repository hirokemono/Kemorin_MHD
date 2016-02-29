!
!     module   m_sorted_node_MHD
!.......................................................................
!
!     Written by H. Matsui
!
!!      subroutine set_index_list_4_mat_etr_l
!!     &         (node, ele, rhs_tbl, mat_tbl_q)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(table_mat_const), intent(in) :: mat_tbl_q
!!
!!      subroutine set_index_list_4_mat_fl(node, ele, fluid, rhs_tbl)
!!      subroutine set_index_list_4_mat_fl_l(node, ele, fluid, rhs_tbl)
!!
!!      subroutine set_index_list_4_mat_cd(node, ele, conduct, rhs_tbl)
!!      subroutine set_index_list_4_mat_cd_l(node, ele, conduct, rhs_tbl)
!!      subroutine set_index_list_4_mat_ins                             &
!!     &        (node, ele, insulate, rhs_tbl)
!!       subroutine set_index_list_4_mat_ins_l                          &
!!     &        (node, ele, insulate, rhs_tbl)
!
!      subroutine deallocate_marix_list_l
!
!      subroutine deallocate_marix_list_fl
!      subroutine deallocate_marix_list_cd
!      subroutine deallocate_marix_list_ins
!
!      subroutine deallocate_marix_list_fl_l
!      subroutine deallocate_marix_list_cd_l
!      subroutine deallocate_marix_list_ins_l
!
      module   m_sorted_node_MHD
!
      use m_precision
      use t_sorted_node_MHD
!
      implicit  none
!
!>  Structures for FEM marix table
      type(tables_MHD_mat_const), save :: MHD1_mat_tbls
!
      end module m_sorted_node_MHD
