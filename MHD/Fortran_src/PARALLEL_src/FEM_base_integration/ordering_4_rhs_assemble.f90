!
!      module ordering_4_rhs_assemble
!
!      programmed by H.Matsui on Oct., 2000 (ver 1.1)
!      Modified by H.Matsui on Oct., 2006
!      Modified by H.Matsui on Dec., 2008
!
!      subroutine sort_node_index
!
!      subroutine count_nele_4_RHS_assemble(numnod, nmax_ele_4_node,    &
!     &          nele_4_node, np_smp, inod_ele_max, nnod_sort_smp)
!         output:  nnod_sort_smp
!      subroutine set_iele_4_RHS_assemble(numnod, nmax_ele_4_node,      &
!     &          nele_4_node, iele_stack_4_node, np_smp, inod_ele_max,  &
!     &          nod_stack_smp, ntot_ele_4_node, iele_4_node,           &
!     &          iconn_4_node, num_sort_smp, node_sort_list_smp,        &
!     &          iele_sort_smp, iconn_sort_smp)
!         output:  iele_sort_smp, iconn_sort_smp, node_sort_list_smp
!
      module ordering_4_rhs_assemble
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sort_node_index
!
      use m_constants
!
      use m_geometry_data
      use m_machine_parameter
      use m_element_id_4_node
      use m_sorted_node
!
      use cal_minmax_and_stacks
!
!
      rhs_tbl1%inod_ele_max = ele_4_nod1%nmax
      call allocate_sorted_node(node1%numnod)
!
!    count number of node to RHS assemble
!
      call count_nele_4_RHS_assemble(node1%numnod,                      &
     &    ele_4_nod1%nmax, ele_4_nod1%nele_4_node, np_smp,              &
     &    rhs_tbl1%inod_ele_max, rhs_tbl1%nnod_sort_smp)
!
!    stacking for SMP
!
      call s_cal_dbl_minmax_and_stacks(np_smp, ele_4_nod1%nmax,         &
     &    rhs_tbl1%nnod_sort_smp, izero, rhs_tbl1%nod_stack_smp,        &
     &    rhs_tbl1%num_sort_smp, rhs_tbl1%nmax_sort_smp,                &
     &    rhs_tbl1%nmin_sort_smp)
!
!       write(*,*) 'num_sort_smp', rhs_tbl1%num_sort_smp
!
      call allocate_sort_smp
!
!       write(*,*) 'allocate smp end'
!
      call set_iele_4_RHS_assemble(node1%numnod, ele_4_nod1%nmax,       &
     &    ele_4_nod1%nele_4_node, ele_4_nod1%istack_4_node,             &
     &    np_smp, rhs_tbl1%inod_ele_max, rhs_tbl1%nod_stack_smp,        &
     &    ele_4_nod1%ntot, ele_4_nod1%iele_4_node,                      &
     &    ele_4_nod1%iconn_4_node, rhs_tbl1%num_sort_smp,               &
     &    rhs_tbl1%node_sort_list_smp, rhs_tbl1%iele_sort_smp,          &
     &    rhs_tbl1%iconn_sort_smp)
!
!       write(*,*) 'set smp for RHS end'
!
      end  subroutine sort_node_index
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_nele_4_RHS_assemble(numnod, nmax_ele_4_node,     &
     &          nele_4_node, np_smp, inod_ele_max, nnod_sort_smp)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: nmax_ele_4_node
      integer(kind = kint), intent(in) :: nele_4_node(numnod)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: inod_ele_max
      integer(kind = kint), intent(inout)                               &
     &             :: nnod_sort_smp(inod_ele_max*np_smp)
!
      integer (kind = kint) :: ii, jj, ip, iproc, inn
      integer (kind = kint) :: inum, inod
!
!    count number of node to RHS assemble
!
      ii = 0
      jj = 0
      do inum = nmax_ele_4_node, 1, -1
        do inod = 1, numnod
          if (nele_4_node(inod) .eq. inum) then
            ii = ii + 1
            ip = mod(ii-ione,np_smp) + 1
            jj = (ii-ip) / np_smp + 1
          end if
        end do
        do iproc = 1, ip
          inn = inum + nmax_ele_4_node*(iproc-1)
          nnod_sort_smp(inn) = jj
        end do
        do iproc = ip+1, np_smp
          inn = inum + nmax_ele_4_node*(iproc-1)
          nnod_sort_smp(inn) = jj - 1
        end do
      end do
!
      end subroutine count_nele_4_RHS_assemble
!
!-----------------------------------------------------------------------
!
      subroutine set_iele_4_RHS_assemble(numnod, nmax_ele_4_node,       &
     &          nele_4_node, iele_stack_4_node, np_smp, inod_ele_max,   &
     &          nod_stack_smp, ntot_ele_4_node, iele_4_node,            &
     &          iconn_4_node, num_sort_smp, node_sort_list_smp,         &
     &          iele_sort_smp, iconn_sort_smp)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: nmax_ele_4_node
      integer(kind = kint), intent(in) :: nele_4_node(numnod)
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: inod_ele_max
      integer(kind = kint), intent(in)                                  &
     &              :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in) :: ntot_ele_4_node
      integer(kind = kint), intent(in) :: iele_4_node(ntot_ele_4_node)
      integer(kind = kint), intent(in) :: iconn_4_node(ntot_ele_4_node)
      integer(kind = kint), intent(in) :: num_sort_smp
!
      integer(kind = kint), intent(inout)                               &
     &              :: node_sort_list_smp(numnod,2)
      integer(kind = kint), intent(inout)                               &
     &              :: iele_sort_smp(num_sort_smp)
      integer(kind = kint), intent(inout)                               &
     &              :: iconn_sort_smp(num_sort_smp)
!
      integer (kind = kint) :: i, ii, jj, ip, is, in, inn
      integer (kind = kint) :: inum, inod
!
!
      ii = 0
      do inum = nmax_ele_4_node, 1, -1
        do inod = 1, numnod
          if ( nele_4_node(inod) .eq. inum ) then
!
            ii = ii + 1
!
            ip = mod(ii-ione,np_smp) + 1
            jj = (ii-ip) / np_smp + 1
            node_sort_list_smp(inod,1) = jj
            node_sort_list_smp(inod,2) = ip
!
            do i = 1, inum
              is = iele_stack_4_node(inod-1) + i
              inn = i + nmax_ele_4_node*(ip-1)
              in = nod_stack_smp(inn-1) + jj
              iele_sort_smp(in) =  iele_4_node(is)
              iconn_sort_smp(in) = iconn_4_node(is)
            end do
!
           end if
         end do
       end do
!
      end subroutine set_iele_4_RHS_assemble
!
!-----------------------------------------------------------------------
!
      end module ordering_4_rhs_assemble
