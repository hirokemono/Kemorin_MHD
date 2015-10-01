!
!     module   m_sorted_node
!.......................................................................
!
!     Written by H. Matsui
!
!       subroutine allocate_sorted_node(numnod)
!       subroutine allocate_sort_smp
!
!       subroutine allocate_marix_list_general(nnod_4_ele)
!       subroutine allocate_marix_list(nnod_4_ele)
!
!       subroutine deallocate_sorted_node
!
!       subroutine deallocate_marix_list
!       subroutine deallocate_marix_list_l
!
      module   m_sorted_node
!
      use m_precision
      use t_table_FEM_const
!
      implicit  none
!
!>  Structure for FEM construction table
      type(tables_4_FEM_assembles) :: rhs_tbl1
!rhs_tbl1%iconn_sort_smp
!
!>  Structure for quad FEM marix table
      type(table_mat_const) :: mat_tbl_q1
!>  Structure for linear FEM marix table
      type(table_mat_const) :: mat_tbl_l1
!
!
!      integer (kind=kint) :: inod_ele_max
!    maximum number of element on a node
!      integer (kind=kint) :: num_sort_smp
!    total number of element on a node
!
!      integer(kind=kint), allocatable :: node_sort_list_smp(:,:)
!    node order by number of elements on each node 4 SMP
!
!      integer (kind=kint) :: nmin_sort_smp
!      integer (kind=kint) :: nmax_sort_smp
!      integer(kind=kint), allocatable :: nnod_sort_smp(:)
!      integer(kind=kint), allocatable :: nod_stack_smp(:)
!    stack by number of elements on each node
!
!
!      integer(kind=kint), allocatable :: iele_sort_smp(:)
!    element ID in the node ID and summation count 4 SMP
!      integer(kind=kint), allocatable :: iconn_sort_smp(:)
!    pocessor ID in the node ID and summation count 4 SMP
!
!
      integer (kind=kint), allocatable :: idx_4_mat(:,:)
      integer (kind=kint), allocatable :: idx_4_l_mat(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine allocate_sorted_node(numnod)
!
       use m_machine_parameter
!
       integer(kind = kint), intent(in) :: numnod
!
!
       allocate(rhs_tbl1%node_sort_list_smp(numnod,2) )
!
       allocate(rhs_tbl1%nnod_sort_smp(rhs_tbl1%inod_ele_max*np_smp))
       allocate(rhs_tbl1%nod_stack_smp(0:rhs_tbl1%inod_ele_max*np_smp))
!
        rhs_tbl1%node_sort_list_smp = 0
!
        rhs_tbl1%nnod_sort_smp = 0
        rhs_tbl1%nod_stack_smp = 0
!
       end subroutine allocate_sorted_node
!
!-----------------------------------------------------------------------
!
       subroutine allocate_sort_smp
!
       allocate( rhs_tbl1%iele_sort_smp(rhs_tbl1%num_sort_smp))
       allocate( rhs_tbl1%iconn_sort_smp(rhs_tbl1%num_sort_smp))
!
       rhs_tbl1%iele_sort_smp = 0
       rhs_tbl1%iconn_sort_smp = 0
!
       end subroutine allocate_sort_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_marix_list_general(nnod_4_ele)
!
      integer(kind = kint), intent(in) :: nnod_4_ele
!
      allocate (idx_4_mat (rhs_tbl1%num_sort_smp,nnod_4_ele))
      idx_4_mat = 0
!
      end subroutine allocate_marix_list_general
!
!-----------------------------------------------------------------------
!
      subroutine allocate_marix_list(nnod_4_ele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_4_ele
!
      allocate (idx_4_mat (rhs_tbl1%num_sort_smp,nnod_4_ele))
      allocate (idx_4_l_mat (rhs_tbl1%num_sort_smp,num_t_linear))
!
      idx_4_mat = 0
      idx_4_l_mat = 0
!
      end subroutine allocate_marix_list
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_sorted_node
!
      deallocate( rhs_tbl1%node_sort_list_smp )
!
      deallocate( rhs_tbl1%nnod_sort_smp )
      deallocate( rhs_tbl1%nod_stack_smp )
!
      deallocate( rhs_tbl1%iele_sort_smp )
      deallocate( rhs_tbl1%iconn_sort_smp )
!
      end subroutine deallocate_sorted_node
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_marix_list
!
       deallocate (idx_4_mat)
!
       end subroutine deallocate_marix_list
!
! ----------------------------------------------
!
       subroutine deallocate_marix_list_l
!
       deallocate (idx_4_l_mat)
!
       end subroutine deallocate_marix_list_l
!
!-----------------------------------------------------------------------
!
      end module m_sorted_node
