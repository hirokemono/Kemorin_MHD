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
!!     &          inod_lc_new_export, expand_export_position,           &
!!     &          iele_lc_new_export, expand_export_connect)
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
!!        type(node_data_for_sleeve_ext), intent(inout)                 &
!!     &            :: expand_export_position
!!        type(ele_data_for_sleeve_ext), intent(inout)                  &
!!     &            :: expand_export_connect
!!        integer(kind = kint), intent(inout)                           &
!!     &            :: inod_lc_new_export(ntot_new_nod_export)
!!        integer(kind = kint), intent(inout)                           &
!!     &            :: iele_lc_new_export(ntot_new_ele_export)
!!
!!      subroutine send_extended_node_position(expand_nod_comm,         &
!!     &          expand_export_position, expand_import_position)
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(node_data_for_sleeve_ext), intent(in)                    &
!!     &            :: expand_export_position
!!        type(node_data_for_sleeve_ext), intent(inout)                 &
!!     &            :: expand_import_position
!!      subroutine send_extended_element_connect(ele, expand_ele_comm,  &
!!     &          expand_export_connect, expand_import_connect)
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: expand_ele_comm
!!        type(ele_data_for_sleeve_ext), intent(in)                     &
!!     &            :: expand_export_connect
!!        type(ele_data_for_sleeve_ext), intent(inout)                  &
!!     &            :: expand_import_connect
!
!!@endverbatim
!
      module set_mesh_for_sleeve_extend
!
      use m_precision
      use t_comm_table
      use t_geometry_data
      use t_para_double_numbering
      use t_mesh_for_sleeve_extend
      use mark_export_nod_ele_extend
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
     &          inod_lc_new_export, expand_export_position,             &
     &          iele_lc_new_export, expand_export_connect)
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
      type(node_data_for_sleeve_ext), intent(inout)                     &
     &            :: expand_export_position
      type(ele_data_for_sleeve_ext), intent(inout)                      &
     &            :: expand_export_connect
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
          inod = mark_nod(i)%inod_marked(inum)
          if(inod_in_comm(inod) .lt. 0) cycle

          icou = icou + 1
          inod_in_comm(inod) =       icou - istack_new_nod_export(i-1)
          expand_export_position%inod_gl_comm(icou)                     &
     &                                     = node%inod_global(inod)
          expand_export_position%xx_comm(3*icou-2) =  node%xx(inod,1)
          expand_export_position%xx_comm(3*icou-1) =  node%xx(inod,2)
          expand_export_position%xx_comm(3*icou  ) =  node%xx(inod,3)
          inod_lc_new_export(icou) = inod_dbl%index(inod)
          expand_export_position%irank_comm(icou)                       &
     &                             = inod_dbl%irank(inod)
          expand_export_position%distance(icou)                         &
     &                             =  mark_nod(i)%dist_marked(inum)
        end do
!
        ist = istack_new_ele_export(i-1)
!$omp parallel do private(inum,icou,iele,k1,inod)
        do inum = 1, mark_ele(i)%nnod_marked
          icou = ist + inum
          iele = mark_ele(i)%inod_marked(inum)
          expand_export_connect%iele_gl_comm(icou)                      &
     &                             = ele%iele_global(iele)
          expand_export_connect%irank_comm(icou) = iele_dbl%irank(iele)
          iele_lc_new_export(icou) =   iele_dbl%index(iele)
!
          do k1 = 1, ele%nnod_4_ele
            inod = ele%ie(iele,k1)
            expand_export_connect%ie_comm(icou,k1) = inod_in_comm(inod)
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
     &          expand_export_position, expand_import_position)
!
      use m_solver_SR
      use reverse_SR_int
      use reverse_SR_int8
      use reverse_SR_real
!
      type(communication_table), intent(in) :: expand_nod_comm
      type(node_data_for_sleeve_ext), intent(in)                        &
     &            :: expand_export_position
      type(node_data_for_sleeve_ext), intent(inout)                     &
     &            :: expand_import_position
!
!
      call comm_items_send_recv                                         &
     &   (expand_nod_comm%num_neib, expand_nod_comm%id_neib,            &
     &    expand_nod_comm%istack_export, expand_nod_comm%istack_import, &
     &    expand_export_position%irank_comm, SR_sig1,                   &
     &    expand_import_position%irank_comm)
      call real_items_send_recv                                         &
     &   (expand_nod_comm%num_neib, expand_nod_comm%id_neib,            &
     &    expand_nod_comm%istack_export, expand_nod_comm%istack_import, &
     &    expand_export_position%distance, SR_sig1,                     &
     &    expand_import_position%distance)
!
      call real_items_send_recv_3                                       &
     &   (expand_nod_comm%num_neib, expand_nod_comm%id_neib,            &
     &    expand_nod_comm%istack_export, expand_nod_comm%istack_import, &
     &    expand_export_position%xx_comm, SR_sig1,                      &
     &    expand_import_position%xx_comm)
      call int8_items_send_recv                                         &
     &   (expand_nod_comm%num_neib, expand_nod_comm%id_neib,            &
     &    expand_nod_comm%istack_export, expand_nod_comm%istack_import, &
     &    expand_export_position%inod_gl_comm, SR_sig1,                 &
     &    expand_import_position%inod_gl_comm)
!
      end subroutine send_extended_node_position
!
!  ---------------------------------------------------------------------
!
      subroutine send_extended_element_connect(ele, expand_ele_comm,    &
     &          expand_export_connect, expand_import_connect)
!
      use m_solver_SR
      use reverse_SR_int
      use reverse_SR_int8
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: expand_ele_comm
      type(ele_data_for_sleeve_ext), intent(in)                         &
     &            :: expand_export_connect
      type(ele_data_for_sleeve_ext), intent(inout)                      &
     &            :: expand_import_connect
!
      integer(kind= kint) :: k1
!
!
      call comm_items_send_recv                                         &
     &   (expand_ele_comm%num_neib, expand_ele_comm%id_neib,            &
     &    expand_ele_comm%istack_export, expand_ele_comm%istack_import, &
     &    expand_export_connect%irank_comm, SR_sig1,                    &
     &    expand_import_connect%irank_comm)
!
      call int8_items_send_recv                                         &
     &   (expand_ele_comm%num_neib, expand_ele_comm%id_neib,            &
     &    expand_ele_comm%istack_export, expand_ele_comm%istack_import, &
     &    expand_export_connect%iele_gl_comm, SR_sig1,                  &
     &    expand_import_connect%iele_gl_comm)
      do k1 = 1, ele%nnod_4_ele
        call comm_items_send_recv                                       &
     &   (expand_ele_comm%num_neib, expand_ele_comm%id_neib,            &
     &    expand_ele_comm%istack_export, expand_ele_comm%istack_import, &
     &    expand_export_connect%ie_comm(1,k1), SR_sig1,                 &
     &    expand_import_connect%ie_comm(1,k1))
      end do
!
      end subroutine send_extended_element_connect
!
!  ---------------------------------------------------------------------
!
      end module set_mesh_for_sleeve_extend
