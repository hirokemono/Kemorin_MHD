!
!     module   t_sorted_node_MHD
!.......................................................................
!
!     Written by H. Matsui
!
!!      subroutine set_index_list_4_mat_etr_l                           &
!!     &         (node, ele, rhs_tbl, mat_tbl_q, linear)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(table_mat_const), intent(in) :: mat_tbl_q
!!
!!      subroutine set_index_list_4_mat_fl                              &
!!     &         (node, ele, fluid, rhs_tbl, fluid_q)
!!      subroutine set_index_list_4_mat_fl_l                            &
!!     &         (node, ele, fluid, rhs_tbl, fluid_q, fluid_l)
!!
!!      subroutine set_index_list_4_mat_cd(node, ele, conduct, rhs_tbl, &
!!     &         conduct_q, full_conduct_q)
!!      subroutine set_index_list_4_mat_cd_l                            &
!!     &         (node, ele, conduct, rhs_tbl, MHD_mat_tbls)
!!      subroutine set_index_list_4_mat_ins                             &
!!     &         (node, ele, insulate, rhs_tbl, insulate_q)
!!      subroutine set_index_list_4_mat_ins_l                           &
!!     &         (node, ele, insulate, rhs_tbl, insulate_q, insulate_l)
!!
!!      subroutine deallocate_MHD_matrix_lists(MHD_mat_tbls)
!
      module   t_sorted_node_MHD
!
      use m_precision
      use m_geometry_constants
      use m_solver_djds_MHD
      use t_geometry_data_MHD
      use t_geometry_data
      use t_table_FEM_const
!
      implicit  none
!
!>  Structures for FEM marix table
      type tables_MHD_mat_const
!>  Structure for linear FEM marix table
        type(table_mat_const) :: linear
!
!>  Structure for quad FEM marix table for fluid
        type(table_mat_const) :: fluid_q
!>  Structure for quad FEM marix table for conductor
        type(table_mat_const) :: conduct_q
!>  Structure for quad FEM marix table for insulator
        type(table_mat_const) :: insulate_q
!>  Structure for quad FEM marix table for conductor but whole domain
        type(table_mat_const) :: full_conduct_q
!
!>  Structure for linear FEM marix table for fluid
        type(table_mat_const) :: fluid_l
!>  Structure for linear FEM marix table for conductor
        type(table_mat_const) :: conduct_l
!>  Structure for linear FEM marix table for insulator
        type(table_mat_const) :: insulate_l
!>  Structure for linear FEM marix table for conductor but whole domain
        type(table_mat_const) :: full_conduct_l
      end type tables_MHD_mat_const
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_etr_l                             &
     &         (node, ele, rhs_tbl, mat_tbl_q, linear)
!
      use set_index_list_4_djds
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl_q
!
      type(table_mat_const), intent(inout) :: linear
!
!
      call alloc_type_marix_list(num_t_linear, rhs_tbl, linear)
!
      if (ele%nnod_4_ele .ne. num_t_linear) then
        call set_index_list_4_DJDS_mat_etr                              &
     &     (node, ele, rhs_tbl, DJDS_linear, linear)
      else
        linear%idx_4_mat = mat_tbl_q%idx_4_mat
      end if
!
      end subroutine set_index_list_4_mat_etr_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_fl                                &
     &         (node, ele, fluid, rhs_tbl, fluid_q)
!
      use set_index_list_4_djds
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(table_mat_const), intent(inout) :: fluid_q
!
!
      call alloc_type_marix_list(ele%nnod_4_ele, rhs_tbl, fluid_q)
      call set_index_list_4_DJDS_mat                                    &
     &   (fluid%iele_start_fld, fluid%iele_end_fld,                     &
     &    node, ele, rhs_tbl, DJDS_fluid, fluid_q)
!
      end subroutine set_index_list_4_mat_fl
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_fl_l                              &
     &         (node, ele, fluid, rhs_tbl, fluid_q, fluid_l)
!
      use set_index_list_4_djds
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: fluid_q
!
      type(table_mat_const), intent(inout) :: fluid_l
!
!
      call alloc_type_marix_list(num_t_linear, rhs_tbl, fluid_l)
!
      if (ele%nnod_4_ele .ne. num_t_linear) then
        call set_index_list_4_DJDS_mat                                  &
     &     (fluid%iele_start_fld, fluid%iele_end_fld,                   &
     &      node, ele, rhs_tbl, DJDS_fl_l, fluid_l)
      else
        fluid_l%idx_4_mat = fluid_q%idx_4_mat
      end if
!
      end subroutine set_index_list_4_mat_fl_l
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_cd(node, ele, conduct, rhs_tbl,   &
     &         conduct_q, full_conduct_q)
!
      use set_index_list_4_djds
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(table_mat_const), intent(inout) :: conduct_q
      type(table_mat_const), intent(inout) :: full_conduct_q
!
!
      call alloc_type_marix_list(ele%nnod_4_ele, rhs_tbl, conduct_q)
!      call set_index_list_4_DJDS_mat                                   &
!     &   (conduct%iele_start_fld, conduct%iele_end_fld,                &
!     &    node, ele, rhs_tbl, DJDS_conduct, conduct_q)
!
      call alloc_type_marix_list                                        &
     &   (ele%nnod_4_ele, rhs_tbl, full_conduct_q)
      call set_index_list_4_DJDS_mat                                    &
     &   (conduct%iele_start_fld, conduct%iele_end_fld,                 &
     &    node, ele, rhs_tbl, DJDS_entire, full_conduct_q)
!
      end subroutine set_index_list_4_mat_cd
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_cd_l                              &
     &         (node, ele, conduct, rhs_tbl, MHD_mat_tbls)
!
      use set_index_list_4_djds
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: conduct
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(tables_MHD_mat_const), intent(inout) :: MHD_mat_tbls
!
!
      call alloc_type_marix_list                                        &
     &   (num_t_linear, rhs_tbl, MHD_mat_tbls%conduct_l)
      call alloc_type_marix_list                                        &
     &   (num_t_linear, rhs_tbl, MHD_mat_tbls%full_conduct_l)
!
      if (ele%nnod_4_ele .ne. num_t_linear) then
        call set_index_list_4_DJDS_mat                                  &
     &     (conduct%iele_start_fld, conduct%iele_end_fld,               &
     &      node, ele, rhs_tbl, DJDS_cd_l, MHD_mat_tbls%conduct_l)
        call set_index_list_4_DJDS_mat                                  &
     &     (conduct%iele_start_fld, conduct%iele_end_fld,               &
     &      node, ele, rhs_tbl, DJDS_linear,                            &
     &     MHD_mat_tbls%full_conduct_l)
      else
        MHD_mat_tbls%conduct_l%idx_4_mat                                &
     &         = MHD_mat_tbls%conduct_q%idx_4_mat
        MHD_mat_tbls%full_conduct_l%idx_4_mat                           &
     &         = MHD_mat_tbls%full_conduct_q%idx_4_mat
      end if
!
      end subroutine set_index_list_4_mat_cd_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_ins                               &
     &         (node, ele, insulate, rhs_tbl, insulate_q)
!
      use set_index_list_4_djds
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: insulate
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(table_mat_const), intent(inout) :: insulate_q
!
!
      call alloc_type_marix_list(ele%nnod_4_ele, rhs_tbl, insulate_q)
      call set_index_list_4_DJDS_mat                                    &
     &   (insulate%iele_start_fld, insulate%iele_end_fld,               &
     &    node, ele, rhs_tbl, DJDS_insulator, insulate_q)
!
      end subroutine set_index_list_4_mat_ins
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_ins_l                             &
     &         (node, ele, insulate, rhs_tbl, insulate_q, insulate_l)
!
      use set_index_list_4_djds
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: insulate
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: insulate_q
!
      type(table_mat_const), intent(inout) :: insulate_l
!
!
      call alloc_type_marix_list(num_t_linear, rhs_tbl, insulate_l)
!
      if (ele%nnod_4_ele .ne. num_t_linear) then
        call set_index_list_4_DJDS_mat                                  &
     &     (insulate%iele_start_fld, insulate%iele_end_fld,             &
     &      node, ele, rhs_tbl, DJDS_ins_l, insulate_l)
      else
        insulate_l%idx_4_mat = insulate_q%idx_4_mat
      end if

      end subroutine set_index_list_4_mat_ins_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_MHD_matrix_lists(MHD_mat_tbls)
!
      type(tables_MHD_mat_const), intent(inout) :: MHD_mat_tbls
!
!
      call dealloc_type_marix_list(MHD_mat_tbls%linear)
!
      call dealloc_type_marix_list(MHD_mat_tbls%fluid_q)
      call dealloc_type_marix_list(MHD_mat_tbls%conduct_q)
      call dealloc_type_marix_list(MHD_mat_tbls%full_conduct_q)
      call dealloc_type_marix_list(MHD_mat_tbls%insulate_q)
!
      call dealloc_type_marix_list(MHD_mat_tbls%fluid_l)
      call dealloc_type_marix_list(MHD_mat_tbls%conduct_l)
      call dealloc_type_marix_list(MHD_mat_tbls%full_conduct_l)
      call dealloc_type_marix_list(MHD_mat_tbls%insulate_l)
!
      end subroutine deallocate_MHD_matrix_lists
!
!-----------------------------------------------------------------------
!
      end module t_sorted_node_MHD
