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

      use m_precision
!
      implicit  none
!
      integer (kind=kint) :: inod_ele_max
!    maximum number of element on a node
      integer (kind=kint) :: num_sort_smp
!    total number of element on a node
!
      integer(kind=kint), allocatable :: node_sort_list_smp(:,:)
!    node order by number of elements on each node 4 SMP
!
      integer (kind=kint) :: nmin_sort_smp, nmax_sort_smp
      integer(kind=kint), allocatable :: nnod_sort_smp(:)
      integer(kind=kint), allocatable :: nod_stack_smp(:)
!    stack by number of elements on each node
!
!
      integer(kind=kint), allocatable :: iele_sort_smp(:)
!    element ID in the node ID and summation count 4 SMP
      integer(kind=kint), allocatable :: iconn_sort_smp(:)
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
       allocate( node_sort_list_smp(numnod,2) )
!
       allocate( nnod_sort_smp(inod_ele_max*np_smp) )
       allocate( nod_stack_smp(0:inod_ele_max*np_smp))
!
        node_sort_list_smp = 0
!
        nnod_sort_smp = 0
        nod_stack_smp = 0
!
       end subroutine allocate_sorted_node
!
!-----------------------------------------------------------------------
!
       subroutine allocate_sort_smp
!
       allocate( iele_sort_smp(num_sort_smp))
       allocate( iconn_sort_smp(num_sort_smp))
!
       iele_sort_smp = 0
       iconn_sort_smp = 0
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
      allocate (idx_4_mat (num_sort_smp,nnod_4_ele))
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
      allocate (idx_4_mat (num_sort_smp,nnod_4_ele))
      allocate (idx_4_l_mat (num_sort_smp,num_t_linear))
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
      deallocate( node_sort_list_smp )
!
      deallocate( nnod_sort_smp )
      deallocate( nod_stack_smp )
!
      deallocate( iele_sort_smp )
      deallocate( iconn_sort_smp )
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
