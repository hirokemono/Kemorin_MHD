!
!      module set_index_list_4_crs
!
!      Written by H. Matsui on Oct., 2006
!
!      subroutine set_idx_list_4_whole_crs
!      subroutine s_set_index_list_4_crs(np_smp, numnod, internal_node, &
!     &          numele, nnod_4_ele, ie)
!
      module set_index_list_4_crs
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_idx_list_4_whole_crs
!
      use m_geometry_parameter
      use m_geometry_data
      use m_machine_parameter
      use m_sorted_node
!
      call allocate_marix_list_general(nnod_4_ele)
      call s_set_index_list_4_crs(np_smp, numnod, internal_node,        &
     &          numele, nnod_4_ele, ie)
!
      end subroutine set_idx_list_4_whole_crs
!
!-----------------------------------------------------------------------
!
      subroutine s_set_index_list_4_crs(np_smp, numnod, internal_node,  &
     &          numele, nnod_4_ele, ie)
!
      use m_sorted_node
      use m_crs_connect
      use set_off_diagonal
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
!
      integer(kind = kint) :: nod1, nod2, mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, istart, iend, in
!
      do k2 = 1, nnod_4_ele
!
        do iproc = 1, np_smp
          do inum = 1, inod_ele_max
!
            inn = inum + inod_ele_max*(iproc-1)
            istart = nod_stack_smp(inn-1)+1
            iend = nod_stack_smp(inn)
!
            do in = istart, iend
!
              iele = iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
              nod1 = ie(iele,iconn)
              nod2 = ie(iele,k2)
!
              call s_set_off_diagonal(internal_node, numnod,            &
     &            ntot_crs_l, ntot_crs_u,                               &
     &            istack_crs_l, istack_crs_u, item_crs_l, item_crs_u,   &
     &            nod1, nod2, mat_num)
!
              idx_4_mat(in,k2) = mat_num
!
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
