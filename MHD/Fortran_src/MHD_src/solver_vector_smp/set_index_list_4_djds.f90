!
!      module set_index_list_4_djds
!
!      programmed by H.Matsui and H.Okuda on 2002
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine s_set_index_list_4_djds(np_smp, numnod, internal_node,&
!     &          numele, nnod_4_ele, ie)
!
      module set_index_list_4_djds
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
      subroutine s_set_index_list_4_djds(np_smp, numnod, internal_node, &
     &          numele, nnod_4_ele, ie)
!
      use m_sorted_node
      use m_solver_djds
!
      use set_DJDS_off_diagonal
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
              call s_set_DJDS_off_diagonal (internal_node, numnod,  &
     &            np_smp, NLmax, NUmax, itotal_l, itotal_u,         &
     &            npLX1, npUX1, NHYP, STACKmc, NLmaxHYP, NUmaxHYP,  &
     &            OLDtoNEW, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,       &
     &            indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U, &
     &            PEon, COLORon, nod1, nod2, mat_num)
!
              idx_4_mat(in,k2) = mat_num
!
            end do
          end do
        end do
!
      end do
!
      end subroutine s_set_index_list_4_djds
!
!-----------------------------------------------------------------------
!
      end module set_index_list_4_djds
