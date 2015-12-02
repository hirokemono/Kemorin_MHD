!t_table_FEM_const.f90
!      module t_table_FEM_const
!
!       Written by H. Matsui on Dec., 2008
!
!> @brief Stricture for index table for FEM assemble
!
!      subroutine set_idx_list_whole_crs_mat                            &
!     &         (node, ele, tbl_crs, rhs_tbl, mat_tbl)
!        type(node_data), intent(in) :: node
!        type(element_data), intent(in) :: ele
!        type(CRS_matrix_connect), intent(in) :: tbl_crs
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(table_mat_const), intent(inout) :: mat_tbl
!!
!      subroutine alloc_type_sorted_node(np_smp, numnod, rhs_tbl)
!        integer(kind = kint), intent(in) :: np_smp, numnod
!        type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!      subroutine alloc_type_sort_smp(rhs_tbl)
!        type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!      subroutine alloc_type_marix_list(nnod_4_ele, rhs_tbl, mat_tbl)
!        integer(kind = kint), intent(in) :: nnod_4_ele
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(table_mat_const), intent(inout) :: mat_tbl
!      subroutine dealloc_type_sorted_node(rhs_tbl)
!        type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!      subroutine dealloc_type_sort_smp(rhs_tbl)
!        type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!      subroutine dealloc_type_marix_list(mat_tbl)
!        type(table_mat_const), intent(inout) :: mat_tbl
!
      module t_table_FEM_const
!
      use m_precision
!
      implicit  none
!
!
!>  Structure for FEM construction table
      type tables_4_FEM_assembles
!>    maximum number of element on a node
        integer (kind=kint) :: inod_ele_max
!>    total number of element on a node
        integer (kind=kint) :: num_sort_smp
!
!>    node order by number of elements on each node 4 SMP
        integer(kind=kint), pointer :: node_sort_list_smp(:,:)
!
!>    Minimum number of summuation for each node
        integer (kind=kint) :: nmin_sort_smp
!>    Maximum number of summuation for each node
        integer (kind=kint) :: nmax_sort_smp
!>    Number of summuation for each node
        integer(kind=kint), pointer :: nnod_sort_smp(:)
!>    stack by number of elements on each node
        integer(kind=kint), pointer :: nod_stack_smp(:)
!
!
!>    element ID in the node ID and summation count 4 SMP
        integer(kind=kint), pointer :: iele_sort_smp(:)
!>    pocessor ID in the node ID and summation count 4 SMP
        integer(kind=kint), pointer :: iconn_sort_smp(:)
      end type tables_4_FEM_assembles
!
      type table_mat_const
        integer (kind=kint) :: nnod_1ele
        integer (kind=kint), pointer :: idx_4_mat(:,:)
      end type table_mat_const
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_idx_list_whole_crs_mat                             &
     &         (node, ele, tbl_crs, rhs_tbl, mat_tbl)
!
      use m_machine_parameter
      use t_geometry_data
      use t_crs_matrix
      use set_index_list_4_crs
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(table_mat_const), intent(inout) :: mat_tbl
!
!
      call alloc_type_marix_list(ele%nnod_4_ele, rhs_tbl, mat_tbl)
!
      call s_set_index_list_4_crs                                       &
     &   (tbl_crs, node%numnod, node%internal_node,                     &
     &    ele%numele, ele%nnod_4_ele, ele%ie,                           &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, mat_tbl%idx_4_mat)
!
      end subroutine set_idx_list_whole_crs_mat
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_type_sorted_node(np_smp, numnod, rhs_tbl)
!
      integer(kind = kint), intent(in) :: np_smp, numnod
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!
      allocate( rhs_tbl%node_sort_list_smp(numnod,2) )
!
      allocate( rhs_tbl%nnod_sort_smp(rhs_tbl%inod_ele_max*np_smp)   )
      allocate( rhs_tbl%nod_stack_smp(0:rhs_tbl%inod_ele_max*np_smp) )
!
      if (numnod .gt. 0)  rhs_tbl%node_sort_list_smp = 0
!
      if (rhs_tbl%inod_ele_max .gt. 0)  rhs_tbl%nnod_sort_smp = 0
      rhs_tbl%nod_stack_smp = 0
!
      end subroutine alloc_type_sorted_node
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_sort_smp(rhs_tbl)
!
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!
      allocate( rhs_tbl%iele_sort_smp(rhs_tbl%num_sort_smp))
      allocate( rhs_tbl%iconn_sort_smp(rhs_tbl%num_sort_smp))
!
      if (rhs_tbl%num_sort_smp .gt. 0) then
        rhs_tbl%iele_sort_smp =  0
        rhs_tbl%iconn_sort_smp = 0
      end if
!
      end subroutine alloc_type_sort_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_type_marix_list(nnod_4_ele, rhs_tbl, mat_tbl)
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(inout) :: mat_tbl
!
!
      mat_tbl%nnod_1ele = nnod_4_ele
      allocate(mat_tbl%idx_4_mat(rhs_tbl%num_sort_smp,nnod_4_ele))
      if (rhs_tbl%num_sort_smp .gt. 0) mat_tbl%idx_4_mat = 0
!
      end subroutine alloc_type_marix_list
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_type_sorted_node(rhs_tbl)
!
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!
      deallocate( rhs_tbl%node_sort_list_smp )
!
      deallocate( rhs_tbl%nnod_sort_smp )
      deallocate( rhs_tbl%nod_stack_smp )
!
      end subroutine dealloc_type_sorted_node
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_type_sort_smp(rhs_tbl)
!
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!
      deallocate( rhs_tbl%iele_sort_smp )
      deallocate( rhs_tbl%iconn_sort_smp)
!
      end subroutine dealloc_type_sort_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_type_marix_list(mat_tbl)
!
      type(table_mat_const), intent(inout) :: mat_tbl
!
      deallocate (mat_tbl%idx_4_mat)
!
      end subroutine dealloc_type_marix_list
!
!-----------------------------------------------------------------------
!
      end module t_table_FEM_const
