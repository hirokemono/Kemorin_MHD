!
!      module set_index_list_4_djds
!
!      programmed by H.Matsui and H.Okuda on 2002
!      Modified by H. Matsui on Oct., 2005
!
!      subroutine set_index_list_4_DJDS_mat_etr                         &
!     &         (node, ele, rhs_tbl, djds_tbl, mat_tbl)
!      subroutine set_index_list_4_DJDS_mat(iele_start, iele_end,       &
!     &          node, ele, rhs_tbl, djds_tbl, mat_tbl)
!        type(node_data), intent(in) :: node
!        type(element_data), intent(in) :: ele
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(DJDS_ordering_table), intent(in) :: djds_tbl
!        type(table_mat_const), intent(inout) :: mat_tbl
!
!      subroutine set_whole_index_list_4_djds                           &
!     &         (numnod, internal_node, numele, nnod_4_ele, ie,         &
!     &          inod_ele_max, num_sort_smp, nod_stack_smp,             &
!     &          iele_sort_smp, iconn_sort_smp, djds_tbl,               &
!     &          nnod_1ele, idx_4_mat)
!      subroutine set_part_index_list_4_djds(iele_start, iele_end,      &
!     &          numnod, internal_node, numele, nnod_4_ele, ie,         &
!     &          inod_ele_max, num_sort_smp, nod_stack_smp,             &
!     &          iele_sort_smp, iconn_sort_smp, djds_tbl,               &
!     &          nnod_1ele, idx_4_mat)
!
      module set_index_list_4_djds
!
      use m_precision
      use m_machine_parameter
!
      use t_solver_djds
!
      use set_idx_4_mat_type
!
      implicit none
!
      private :: set_whole_index_list_4_djds
      private :: set_part_index_list_4_djds
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_DJDS_mat_etr                          &
     &         (node, ele, rhs_tbl, djds_tbl, mat_tbl)
!
      use t_geometry_data
      use t_table_FEM_const
      use t_solver_djds
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      type(table_mat_const), intent(inout) :: mat_tbl
!
!
      call set_whole_index_list_4_djds(node%numnod, node%internal_node, &
     &    ele%numele, ele%nnod_4_ele, ele%ie,                           &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, djds_tbl,                             &
     &    mat_tbl%nnod_1ele, mat_tbl%idx_4_mat)
!
      end subroutine set_index_list_4_DJDS_mat_etr
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_DJDS_mat(iele_start, iele_end,        &
     &          node, ele, rhs_tbl, djds_tbl, mat_tbl)
!
      use t_geometry_data
      use t_table_FEM_const
      use t_solver_djds
!
      integer(kind = kint), intent(in) :: iele_start, iele_end
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      type(table_mat_const), intent(inout) :: mat_tbl
!
!
      call set_part_index_list_4_djds                                   &
     &   (iele_start, iele_end, node%numnod, node%internal_node,        &
     &    ele%numele, ele%nnod_4_ele, ele%ie,                           &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, djds_tbl,                             &
     &    mat_tbl%nnod_1ele, mat_tbl%idx_4_mat)
!
      end subroutine set_index_list_4_DJDS_mat
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_whole_index_list_4_djds                            &
     &         (numnod, internal_node, numele, nnod_4_ele, ie,          &
     &          inod_ele_max, num_sort_smp, nod_stack_smp,              &
     &          iele_sort_smp, iconn_sort_smp, djds_tbl,                &
     &          nnod_1ele, idx_4_mat)
!
      type(DJDS_ordering_table), intent(in) :: djds_tbl
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
      integer(kind = kint), intent(in) :: nnod_1ele
!
      integer(kind = kint), intent(inout)                               &
     &              :: idx_4_mat(num_sort_smp,nnod_1ele)
!
      integer(kind = kint) :: mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2)
      do k2 = 1, nnod_1ele
!
!$omp do private(iproc,inum,inn,ist,ied,in,iele,iconn,mat_num)
        do iproc = 1, np_smp
          do inum = 1, inod_ele_max
            inn = inum + inod_ele_max*(iproc-1)
            ist = nod_stack_smp(inn-1)+1
            ied = nod_stack_smp(inn)
!
            do in = ist, ied
              iele =  iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
!
              call set_DJDS_off_diag_type(numnod, internal_node,        &
     &            djds_tbl, ie(iele,iconn), ie(iele,k2), mat_num)
!
              idx_4_mat(in,k2) = mat_num
            end do
          end do
        end do
!$omp end do nowait
!
      end do
!$omp end parallel
!
      end subroutine set_whole_index_list_4_djds
!
!-----------------------------------------------------------------------
!
      subroutine set_part_index_list_4_djds(iele_start, iele_end,       &
     &          numnod, internal_node, numele, nnod_4_ele, ie,          &
     &          inod_ele_max, num_sort_smp, nod_stack_smp,              &
     &          iele_sort_smp, iconn_sort_smp, djds_tbl,                &
     &          nnod_1ele, idx_4_mat)
!
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      integer(kind = kint), intent(in) :: iele_start, iele_end
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
      integer(kind = kint), intent(in) :: nnod_1ele
!
      integer(kind = kint), intent(inout)                               &
     &              :: idx_4_mat(num_sort_smp,nnod_1ele)
!
      integer(kind = kint) :: mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2)
      do k2 = 1, nnod_1ele
!
!$omp do private(iproc,inum,inn,ist,ied,in,iele,iconn,mat_num)
        do iproc = 1, np_smp
          do inum = 1, inod_ele_max
            inn = inum + inod_ele_max*(iproc-1)
            ist = nod_stack_smp(inn-1)+1
            ied = nod_stack_smp(inn)
!
            do in = ist, ied
              iele =  iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
!
              if(iele.ge.iele_start .and. iele.le.iele_end) then
                call set_DJDS_off_diag_type(numnod, internal_node,      &
     &              djds_tbl, ie(iele,iconn), ie(iele,k2), mat_num)
                idx_4_mat(in,k2) = mat_num
              else
                idx_4_mat(in,k2) = 0
              end if
!
            end do
          end do
        end do
!$omp end do nowait
!
      end do
!$omp end parallel
!
      end subroutine set_part_index_list_4_djds
!
!-----------------------------------------------------------------------
!
      end module set_index_list_4_djds
