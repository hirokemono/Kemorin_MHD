!> @file  set_mesh_for_sleeve_extend.f90
!!      module set_mesh_for_sleeve_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine set_export_4_expanded_mesh(nod_comm, node, ele,      &
!!     &          inod_dbl, iele_dbl, mark_nod, mark_ele,               &
!!     &          ntot_new_nod_export, istack_new_nod_export,           &
!!     &          ntot_new_ele_export, istack_new_ele_export,           &
!!     &          inod_lc_new_export, exp_export_xx,                    &
!!     &          iele_lc_new_export, exp_export_ie)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(node_ele_double_number), intent(in) :: iele_dbl
!!        type(mark_for_each_comm), intent(in)                          &
!!     &                         :: mark_nod(nod_comm%num_neib)
!!        type(mark_for_each_comm), intent(in)                          &
!!     &                         :: mark_ele(nod_comm%num_neib)
!!        integer(kind = kint), intent(in) :: ntot_new_nod_export
!!        integer(kind = kint), intent(in)                              &
!!     &            :: istack_new_nod_export(0:nod_comm%num_neib)
!!        integer(kind = kint), intent(in) :: ntot_new_ele_export
!!        integer(kind = kint), intent(in)                              &
!!     &            :: istack_new_ele_export(0:nod_comm%num_neib)
!!        type(node_data_for_sleeve_ext), intent(inout) :: exp_export_xx
!!        type(ele_data_for_sleeve_ext), intent(inout) :: exp_export_ie
!!        integer(kind = kint), intent(inout)                           &
!!     &            :: inod_lc_new_export(ntot_new_nod_export)
!!        integer(kind = kint), intent(inout)                           &
!!     &            :: iele_lc_new_export(ntot_new_ele_export)
!!
!!      subroutine send_extended_node_position(expand_nod_comm,         &
!!     &          exp_export_xx, exp_import_xx)
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(node_data_for_sleeve_ext), intent(in) :: exp_export_xx
!!        type(node_data_for_sleeve_ext), intent(inout) :: exp_import_xx
!!      subroutine send_extended_element_connect(ele, expand_ele_comm,  &
!!     &          exp_export_ie, exp_import_ie)
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: expand_ele_comm
!!        type(ele_data_for_sleeve_ext), intent(in) :: exp_export_ie
!!        type(ele_data_for_sleeve_ext), intent(inout) :: exp_import_ie
!!@endverbatim
!
      module set_mesh_for_sleeve_extend
!
      use m_precision
      use t_comm_table
      use t_geometry_data
      use t_para_double_numbering
      use t_mesh_for_sleeve_extend
      use t_mark_node_ele_to_extend
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_export_4_expanded_mesh(nod_comm, node, ele,        &
     &          inod_dbl, iele_dbl, mark_nod, mark_ele,                 &
     &          ntot_new_nod_export, istack_new_nod_export,             &
     &          ntot_new_ele_export, istack_new_ele_export,             &
     &          inod_lc_new_export, exp_export_xx,                      &
     &          iele_lc_new_export, exp_export_ie)
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(mark_for_each_comm), intent(in)                              &
     &                         :: mark_nod(nod_comm%num_neib)
      type(mark_for_each_comm), intent(in)                              &
     &                         :: mark_ele(nod_comm%num_neib)
!
      integer(kind = kint), intent(in) :: ntot_new_nod_export
      integer(kind = kint), intent(in)                                  &
     &            :: istack_new_nod_export(0:nod_comm%num_neib)
!
      integer(kind = kint), intent(in) :: ntot_new_ele_export
      integer(kind = kint), intent(in)                                  &
     &            :: istack_new_ele_export(0:nod_comm%num_neib)
!
      type(node_data_for_sleeve_ext), intent(inout) :: exp_export_xx
      type(ele_data_for_sleeve_ext), intent(inout) :: exp_export_ie
      integer(kind = kint), intent(inout)                               &
     &            :: inod_lc_new_export(ntot_new_nod_export)
      integer(kind = kint), intent(inout)                               &
     &            :: iele_lc_new_export(ntot_new_ele_export)
!
      integer(kind = kint), allocatable :: inod_in_comm(:)
      integer(kind = kint) :: i, ist, num, inod, inum, icou, iele, k1
!
!
      allocate(inod_in_comm(node%numnod))
!
      do i = 1, nod_comm%num_neib
!$omp parallel workshare
        inod_in_comm(1:node%numnod) = 0
!$omp end parallel workshare
        ist = nod_comm%istack_export(i-1)
        num = nod_comm%istack_export(i) - nod_comm%istack_export(i-1)
        do inum = 1, num
          inod = nod_comm%item_export(inum+ist)
          inod_in_comm(inod) = -inum
        end do
!
        icou = istack_new_nod_export(i-1)
        do inum = 1, mark_nod(i)%nnod_marked
          inod = mark_nod(i)%idx_marked(inum)
          if(inod_in_comm(inod) .lt. 0) cycle

          icou = icou + 1
          inod_in_comm(inod) =       icou - istack_new_nod_export(i-1)
          exp_export_xx%inod_gl_comm(icou) = node%inod_global(inod)
          exp_export_xx%xx_comm(3*icou-2) =  node%xx(inod,1)
          exp_export_xx%xx_comm(3*icou-1) =  node%xx(inod,2)
          exp_export_xx%xx_comm(3*icou  ) =  node%xx(inod,3)
          inod_lc_new_export(icou) = inod_dbl%index(inod)
          exp_export_xx%irank_comm(icou) = inod_dbl%irank(inod)
          exp_export_xx%distance(icou) =  mark_nod(i)%dist_marked(inum)
        end do
!
        ist = istack_new_ele_export(i-1)
!$omp parallel do private(inum,icou,iele,k1,inod)
        do inum = 1, mark_ele(i)%nnod_marked
          icou = ist + inum
          iele = mark_ele(i)%idx_marked(inum)
          exp_export_ie%iele_gl_comm(icou) = ele%iele_global(iele)
          exp_export_ie%irank_comm(icou) = iele_dbl%irank(iele)
          iele_lc_new_export(icou) =   iele_dbl%index(iele)
!
          do k1 = 1, ele%nnod_4_ele
            inod = ele%ie(iele,k1)
            exp_export_ie%ie_comm(icou,k1) = inod_in_comm(inod)
!            if(inod_in_comm(inod) .eq. 0) write(*,*) my_rank,          &
!     &        'Failed inod_in_comm(inod)', inod,                       &
!     &          inod_dbl%irank(inod), nod_comm%id_neib(i)
          end do
        end do
!$omp end parallel do
      end do
!
      deallocate(inod_in_comm)
!
      end subroutine set_export_4_expanded_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine send_extended_node_position(expand_nod_comm,           &
     &          exp_export_xx, exp_import_xx)
!
      use m_solver_SR
      use reverse_SR_int
      use reverse_SR_int8
      use reverse_SR_real
!
      type(communication_table), intent(in) :: expand_nod_comm
      type(node_data_for_sleeve_ext), intent(in) :: exp_export_xx
      type(node_data_for_sleeve_ext), intent(inout) :: exp_import_xx
!
!
      call comm_items_send_recv                                         &
     &   (expand_nod_comm%num_neib, expand_nod_comm%id_neib,            &
     &    expand_nod_comm%istack_export, expand_nod_comm%istack_import, &
     &    exp_export_xx%irank_comm, SR_sig1, exp_import_xx%irank_comm)
      call real_items_send_recv                                         &
     &   (expand_nod_comm%num_neib, expand_nod_comm%id_neib,            &
     &    expand_nod_comm%istack_export, expand_nod_comm%istack_import, &
     &    exp_export_xx%distance, SR_sig1, exp_import_xx%distance)
!
      call real_items_send_recv_3                                       &
     &   (expand_nod_comm%num_neib, expand_nod_comm%id_neib,            &
     &    expand_nod_comm%istack_export, expand_nod_comm%istack_import, &
     &    exp_export_xx%xx_comm, SR_sig1, exp_import_xx%xx_comm)
      call int8_items_send_recv                                         &
     &   (expand_nod_comm%num_neib, expand_nod_comm%id_neib,            &
     &    expand_nod_comm%istack_export, expand_nod_comm%istack_import, &
     &    exp_export_xx%inod_gl_comm, SR_sig1,                          &
     &    exp_import_xx%inod_gl_comm)
!
      end subroutine send_extended_node_position
!
!  ---------------------------------------------------------------------
!
      subroutine send_extended_element_connect(ele, expand_ele_comm,    &
     &          exp_export_ie, exp_import_ie)
!
      use m_solver_SR
      use reverse_SR_int
      use reverse_SR_int8
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: expand_ele_comm
      type(ele_data_for_sleeve_ext), intent(in) :: exp_export_ie
      type(ele_data_for_sleeve_ext), intent(inout) :: exp_import_ie
!
      integer(kind= kint) :: k1
!
!
      call comm_items_send_recv                                         &
     &   (expand_ele_comm%num_neib, expand_ele_comm%id_neib,            &
     &    expand_ele_comm%istack_export, expand_ele_comm%istack_import, &
     &    exp_export_ie%irank_comm, SR_sig1, exp_import_ie%irank_comm)
!
      call int8_items_send_recv                                         &
     &   (expand_ele_comm%num_neib, expand_ele_comm%id_neib,            &
     &    expand_ele_comm%istack_export, expand_ele_comm%istack_import, &
     &    exp_export_ie%iele_gl_comm, SR_sig1,                          &
     &    exp_import_ie%iele_gl_comm)
      do k1 = 1, ele%nnod_4_ele
        call comm_items_send_recv                                       &
     &   (expand_ele_comm%num_neib, expand_ele_comm%id_neib,            &
     &    expand_ele_comm%istack_export, expand_ele_comm%istack_import, &
     &    exp_export_ie%ie_comm(1,k1), SR_sig1,                         &
     &    exp_import_ie%ie_comm(1,k1))
      end do
!
      end subroutine send_extended_element_connect
!
!  ---------------------------------------------------------------------
!
      end module set_mesh_for_sleeve_extend
