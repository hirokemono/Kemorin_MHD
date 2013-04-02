!t_merged_geometry_data.f90
!      module t_merged_geometry_data
!
!      Written by H. Matsui
!      Modified by H. Matsui on Apr., 2012
!
!      subroutine alloc_subdomain_stack(table)
!      subroutine alloc_local_nod_id_tbl(table)
!      subroutine alloc_local_ele_id_tbl(table)
!      subroutine dealloc_subdomain_stack(table)
!      subroutine dealloc_local_nod_id_tbl(table)
!      subroutine dealloc_local_ele_id_tbl(table)
!        integer(kind = kint), intent(in) :: nnod, nele
!        type(merged_stacks), intent(inout) :: table
!
      module t_merged_geometry_data
!
      use m_precision
!
      implicit    none
!
      type merged_stacks
        integer(kind=kint )  :: num_subdomain
!      number of node, element, internal node for each domain
        integer(kind=kint ), pointer :: istack_nod(:)
        integer(kind=kint ), pointer :: istack_ele(:)
        integer(kind=kint ), pointer :: istack_inter(:)
!      number of node, element, internal node stack
        integer(kind=kint ) :: nnod_max
!
        integer(kind=kint ) :: nnod_overlap
        integer(kind=kint ) :: nele_overlap
!
        integer(kind=kint ) :: nnod_merged
        integer(kind=kint ) :: nele_merged
        integer(kind=kint ) :: inter_nod_m
        integer(kind=kint ) :: inter_ele_m
!
        integer(kind=kint ), pointer :: inod_local(:)
        integer(kind=kint ), pointer :: idomain_nod(:)
!
        integer(kind=kint ), pointer :: iele_local(:)
        integer(kind=kint ), pointer :: idomain_ele(:)
      end type merged_stacks
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_subdomain_stack(num_pe, table)
!
      integer(kind = kint), intent(in) :: num_pe
      type(merged_stacks), intent(inout) :: table
!
!
      table%num_subdomain = num_pe
      allocate( table%istack_nod(0:table%num_subdomain)    )
      allocate( table%istack_ele(0:table%num_subdomain)    )
      allocate( table%istack_inter(0:table%num_subdomain) )
      table%istack_nod = 0
      table%istack_ele = 0
      table%istack_inter = 0
!
      table%nnod_max = 0
!
      end subroutine alloc_subdomain_stack
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_local_nod_id_tbl(table)
!
      type(merged_stacks), intent(inout) :: table
!
!
      allocate ( table%inod_local(table%nnod_overlap) )
      allocate ( table%idomain_nod(table%nnod_overlap) )
      table%inod_local  =  0
      table%idomain_nod  = -1
!
      end subroutine alloc_local_nod_id_tbl
!
!------------------------------------------------------------------
!
      subroutine alloc_local_ele_id_tbl(table)
!
      type(merged_stacks), intent(inout) :: table
!
!
      allocate ( table%iele_local(table%nele_overlap) )
      allocate ( table%idomain_ele(table%nele_overlap) )
      table%iele_local =   0
      table%idomain_ele = -1
!
      end subroutine alloc_local_ele_id_tbl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine dealloc_subdomain_stack(table)
!
      type(merged_stacks), intent(inout) :: table
!
!
      deallocate(table%istack_nod, table%istack_ele)
      deallocate(table%istack_inter)
!
      end subroutine dealloc_subdomain_stack
!
!------------------------------------------------------------------
!
      subroutine dealloc_local_nod_id_tbl(table)
!
      type(merged_stacks), intent(inout) :: table
!
      deallocate (table%inod_local, table%idomain_nod)
!
      end subroutine dealloc_local_nod_id_tbl
!
!------------------------------------------------------------------
!
      subroutine dealloc_local_ele_id_tbl(table)
!
      type(merged_stacks), intent(inout) :: table
!
      deallocate (table%iele_local, table%idomain_ele)
!
      end subroutine dealloc_local_ele_id_tbl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_subdomain_stacks(tbl_org, tbl_dst)
!
      type(merged_stacks), intent(in) :: tbl_org
      type(merged_stacks), intent(inout) :: tbl_dst
!
!
      call alloc_subdomain_stack(tbl_org%num_subdomain, tbl_dst)
!
      tbl_dst%istack_nod(0:tbl_org%num_subdomain)                       &
     &                = tbl_org%istack_nod(0:tbl_org%num_subdomain)
      tbl_dst%istack_ele(0:tbl_org%num_subdomain)                       &
     &                = tbl_org%istack_ele(0:tbl_org%num_subdomain)
      tbl_dst%istack_inter(0:tbl_org%num_subdomain)                     &
     &                = tbl_org%istack_inter(0:tbl_org%num_subdomain)
!
      tbl_dst%nnod_max = tbl_org%nnod_max
!
      end subroutine copy_subdomain_stacks
!
!------------------------------------------------------------------
!
      end module t_merged_geometry_data
