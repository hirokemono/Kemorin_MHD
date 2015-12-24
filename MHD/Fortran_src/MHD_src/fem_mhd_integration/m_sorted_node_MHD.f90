!
!     module   m_sorted_node_MHD
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine set_index_list_4_mat_etr_l
!
!      subroutine set_index_list_4_mat_fl
!      subroutine set_index_list_4_mat_fl_l
!
!      subroutine set_index_list_4_mat_cd
!      subroutine set_index_list_4_mat_cd_l
!      subroutine set_index_list_4_mat_ins
!      subroutine set_index_list_4_mat_ins_l
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
      use m_geometry_constants
      use m_geometry_data
      use m_element_id_4_node
      use m_geometry_data_MHD
      use m_solver_djds_MHD
!
      implicit  none
!
!>  Structure for linear FEM marix table
      type(table_mat_const) :: mat_tbl_l1
!
!>  Structure for quad FEM marix table for fluid
      type(table_mat_const) :: mat_tbl_fl_q
!mat_tbl_fl_q%idx_4_mat
!>  Structure for quad FEM marix table for conductor
      type(table_mat_const) :: mat_tbl_cd_q
!>  Structure for quad FEM marix table for insulator
      type(table_mat_const) :: mat_tbl_ins_q
!>  Structure for quad FEM marix table for conductor but whole domain
      type(table_mat_const) :: mat_tbl_full_cd_q
!
!>  Structure for linear FEM marix table for fluid
      type(table_mat_const) :: mat_tbl_fl_l
!>  Structure for linear FEM marix table for conductor
      type(table_mat_const) :: mat_tbl_cd_l
!>  Structure for linear FEM marix table for insulator
      type(table_mat_const) :: mat_tbl_ins_l
!>  Structure for linear FEM marix table for conductor but whole domain
      type(table_mat_const) :: mat_tbl_full_cd_l
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_etr_l
!
      use set_index_list_4_djds
!
!
      call alloc_type_marix_list(num_t_linear, rhs_tbl1, mat_tbl_l1)
!
      if (ele1%nnod_4_ele .ne. num_t_linear) then
        call set_index_list_4_DJDS_mat_etr                              &
     &     (node1, ele1, rhs_tbl1, DJDS_linear, mat_tbl_l1)
      else
        mat_tbl_l1%idx_4_mat = mat_tbl_q1%idx_4_mat
      end if
!
      end subroutine set_index_list_4_mat_etr_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_fl
!
      use set_index_list_4_djds
!
!
      call alloc_type_marix_list                                        &
     &   (ele1%nnod_4_ele, rhs_tbl1, mat_tbl_fl_q)
      call set_index_list_4_DJDS_mat                                    &
     &   (fluid1%iele_start_fld, fluid1%iele_end_fld,                   &
     &    node1, ele1, rhs_tbl1, DJDS_fluid, mat_tbl_fl_q)
!
      end subroutine set_index_list_4_mat_fl
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_fl_l
!
      use set_index_list_4_djds
!
!
      call alloc_type_marix_list(num_t_linear, rhs_tbl1, mat_tbl_fl_l)
!
      if (ele1%nnod_4_ele .ne. num_t_linear) then
        call set_index_list_4_DJDS_mat                                  &
     &     (conduct1%iele_start_fld, conduct1%iele_end_fld,             &
     &      node1, ele1, rhs_tbl1, DJDS_fl_l, mat_tbl_fl_l)
      else
        mat_tbl_fl_l%idx_4_mat = mat_tbl_fl_q%idx_4_mat
      end if
!
      end subroutine set_index_list_4_mat_fl_l
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_cd
!
      use set_index_list_4_djds
!
!
!      call alloc_type_marix_list                                       &
!     &   (ele1%nnod_4_ele, rhs_tbl1, mat_tbl_cd_q)
!      call set_index_list_4_DJDS_mat                                   &
!     &   (conduct1%iele_start_fld, conduct1%iele_end_fld,              &
!     &    node1, ele1, rhs_tbl1, DJDS_conduct, mat_tbl_cd_q)
!
      call alloc_type_marix_list                                        &
     &   (ele1%nnod_4_ele, rhs_tbl1, mat_tbl_full_cd_q)
      call set_index_list_4_DJDS_mat                                    &
     &   (conduct1%iele_start_fld, conduct1%iele_end_fld,               &
     &    node1, ele1, rhs_tbl1, DJDS_entire, mat_tbl_full_cd_q)
!
      end subroutine set_index_list_4_mat_cd
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_cd_l
!
      use set_index_list_4_djds
!
!
      call alloc_type_marix_list(num_t_linear, rhs_tbl1, mat_tbl_cd_l)
      call alloc_type_marix_list                                        &
     &   (num_t_linear, rhs_tbl1, mat_tbl_full_cd_l)
!
      if (ele1%nnod_4_ele .ne. num_t_linear) then
        call set_index_list_4_DJDS_mat                                  &
     &     (conduct1%iele_start_fld, conduct1%iele_end_fld,             &
     &      node1, ele1, rhs_tbl1, DJDS_cd_l, mat_tbl_cd_l)
        call set_index_list_4_DJDS_mat                                  &
     &     (conduct1%iele_start_fld, conduct1%iele_end_fld,             &
     &      node1, ele1, rhs_tbl1, DJDS_linear, mat_tbl_full_cd_l)
      else
        mat_tbl_cd_l%idx_4_mat =      mat_tbl_cd_q%idx_4_mat
        mat_tbl_full_cd_l%idx_4_mat = mat_tbl_full_cd_q%idx_4_mat
      end if
!
      end subroutine set_index_list_4_mat_cd_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_ins
!
      use set_index_list_4_djds
!
!
      call alloc_type_marix_list                                        &
     &   (ele1%nnod_4_ele, rhs_tbl1, mat_tbl_ins_q)
      call set_index_list_4_DJDS_mat                                    &
     &   (insulate1%iele_start_fld, insulate1%iele_end_fld,             &
     &    node1, ele1, rhs_tbl1, DJDS_insulator, mat_tbl_ins_q)
!
      end subroutine set_index_list_4_mat_ins
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_ins_l
!
      use set_index_list_4_djds
!
!
      call alloc_type_marix_list                                        &
     &   (num_t_linear, rhs_tbl1, mat_tbl_ins_l)
!
      if (ele1%nnod_4_ele .ne. num_t_linear) then
        call set_index_list_4_DJDS_mat                                  &
     &     (insulate1%iele_start_fld, insulate1%iele_end_fld,           &
     &      node1, ele1, rhs_tbl1, DJDS_ins_l,  mat_tbl_ins_l)
      else
        mat_tbl_ins_l%idx_4_mat = mat_tbl_ins_q%idx_4_mat
      end if

      end subroutine set_index_list_4_mat_ins_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine deallocate_marix_list_l
!
      call dealloc_type_marix_list(mat_tbl_l1)
!
       end subroutine deallocate_marix_list_l
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_marix_list_fl
!
      call dealloc_type_marix_list(mat_tbl_fl_q)
!
      end subroutine deallocate_marix_list_fl
!
! ----------------------------------------------
!
      subroutine deallocate_marix_list_cd
!
      call dealloc_type_marix_list(mat_tbl_cd_q)
      call dealloc_type_marix_list(mat_tbl_full_cd_q)
!
      end subroutine deallocate_marix_list_cd
!
! ----------------------------------------------
!
      subroutine deallocate_marix_list_ins
!
      call dealloc_type_marix_list(mat_tbl_ins_q)
!
      end subroutine deallocate_marix_list_ins
!
! ----------------------------------------------
! ----------------------------------------------
!
      subroutine deallocate_marix_list_fl_l
!
      call dealloc_type_marix_list(mat_tbl_fl_l)
!
      end subroutine deallocate_marix_list_fl_l
!
! ----------------------------------------------
!
      subroutine deallocate_marix_list_cd_l
!
      call dealloc_type_marix_list(mat_tbl_cd_l)
      call dealloc_type_marix_list(mat_tbl_full_cd_l)
!
      end subroutine deallocate_marix_list_cd_l
!
! ----------------------------------------------
!
      subroutine deallocate_marix_list_ins_l
!
      call dealloc_type_marix_list(mat_tbl_ins_l)
!
      end subroutine deallocate_marix_list_ins_l
!
! ----------------------------------------------
!-----------------------------------------------------------------------
!
      end module m_sorted_node_MHD
