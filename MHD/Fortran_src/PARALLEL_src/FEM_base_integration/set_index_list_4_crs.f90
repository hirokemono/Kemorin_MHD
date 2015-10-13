!
!      module set_index_list_4_crs
!
!      Written by H. Matsui on Oct., 2006
!
!      subroutine s_set_index_list_4_crs                                &
!     &         (numnod, internal_node, numele, nnod_4_ele, ie,         &
!     &          inod_ele_max, num_sort_smp, nod_stack_smp,             &
!     &          iele_sort_smp, iconn_sort_smp, idx_4_mat)
!
      module set_index_list_4_crs
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_index_list_4_crs                                 &
     &         (numnod, internal_node, numele, nnod_4_ele, ie,          &
     &          inod_ele_max, num_sort_smp, nod_stack_smp,              &
     &          iele_sort_smp, iconn_sort_smp, idx_4_mat)
!
      use m_crs_connect
      use set_off_diagonal
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
!
      integer(kind = kint), intent(in) :: inod_ele_max
      integer(kind = kint), intent(in) :: num_sort_smp
      integer(kind = kint), intent(in)                                  &
     &              :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in) :: iele_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in) :: iconn_sort_smp(num_sort_smp)
!
      integer(kind = kint), intent(inout)                               &
     &              :: idx_4_mat(num_sort_smp,nnod_4_ele)
!
      integer(kind = kint) :: nod1, nod2, mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, istart, iend, in
!
!
      do k2 = 1, nnod_4_ele
!
        do iproc = 1, np_smp
          do inum = 1, inod_ele_max
            inn = inum + inod_ele_max*(iproc-1)
            istart = nod_stack_smp(inn-1)+1
            iend =   nod_stack_smp(inn)
!
            do in = istart, iend
              iele =  iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
              nod1 = ie(iele,iconn)
              nod2 = ie(iele,k2)
!
              call s_set_off_diagonal(internal_node, numnod,            &
     &            tbl1_crs%ntot_l, tbl1_crs%ntot_u, tbl1_crs%istack_l,  &
     &            tbl1_crs%istack_u, tbl1_crs%item_l, tbl1_crs%item_u,  &
     &            nod1, nod2, mat_num)
!
              idx_4_mat(in,k2) = mat_num
            end do
          end do
        end do
!
      end do
!
      end subroutine s_set_index_list_4_crs
!
!-----------------------------------------------------------------------
!
      end module set_index_list_4_crs
