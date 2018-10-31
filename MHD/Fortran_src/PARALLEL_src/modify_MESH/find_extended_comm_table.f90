!> @file  find_extended_comm_table.f90
!!      module find_extended_comm_table
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine copy_node_to_extend_buffer(istack_pre,               &
!!     &          org_node, dbl_id1, iflag_node, send_nbuf)
!!        type(node_data), intent(in) :: org_node
!!        type(parallel_double_numbering), intent(in) :: dbl_id1
!!        type(node_buffer_2_extend), intent(inout) :: send_nbuf
!!      subroutine copy_ele_to_extend_buffer(istack_pre,                &
!!     &          org_ele, dbl_ele, dbl_id1, iflag_ele, send_ebuf)
!!        type(element_data), intent(in) :: org_ele
!!        type(parallel_double_numbering), intent(in) :: dbl_id1
!!        type(parallel_double_numbering), intent(in) :: dbl_ele
!!        type(ele_buffer_2_extend), intent(inout) :: send_ebuf
!!
!!      subroutine mark_extended_nod_neib_pe                            &
!!     &         (nprocs, nod_comm, added_comm, recv_nbuf,              &
!!     &          iflag_send, iflag_recv)
!!        type(communication_table), intent(in) :: nod_comm, added_comm
!!        type(node_buffer_2_extend), intent(in) :: recv_nbuf
!!      subroutine count_extended_nod_neib_pe                           &
!!     &         (nprocs, iflag_send, iflag_recv, new_comm)
!!      subroutine set_extended_nod_neib_pe(nprocs, my_rank,            &
!!     &          iflag_send, iflag_recv, nod_comm, new_comm)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(inout) :: new_comm
!!
!!      subroutine count_extended_node_import                           &
!!     &        (recv_nbuf, nod_comm, added_comm, new_comm)
!!      subroutine set_extended_node_import                             &
!!     &        (recv_nbuf, nod_comm, added_comm, new_comm)
!!      subroutine set_extended_node_export(nod_comm, added_comm,       &
!!     &          inod_export_new, irank_export_new, new_comm)
!!        type(communication_table), intent(in) :: nod_comm, added_comm
!!        type(node_buffer_2_extend), intent(in) :: recv_nbuf
!!        type(communication_table), intent(inout) :: new_comm
!!@endverbatim
!
      module find_extended_comm_table
!
      use m_precision
      use m_constants
      use m_phys_constants
      use m_machine_parameter
!
      use t_comm_table
      use t_geometry_data
      use t_para_double_numbering
      use t_work_extend_comm_table
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_node_to_extend_buffer(istack_pre,                 &
     &          org_node, dbl_id1, iflag_node, send_nbuf)
!
      integer(kind = kint), intent(in) :: istack_pre
      type(node_data), intent(in) :: org_node
      type(parallel_double_numbering), intent(in) :: dbl_id1
      integer(kind = kint), intent(in) :: iflag_node(org_node%numnod)
!
      type(node_buffer_2_extend), intent(inout) :: send_nbuf
!
      integer(kind = kint) :: icou, inod
!
      icou = istack_pre
      do inod = 1, org_node%numnod
        if(iflag_node(inod) .gt. 0) then
          icou = icou + 1
          send_nbuf%inod_add(icou) =    dbl_id1%inod_local(inod)
          send_nbuf%irank_add(icou) =   dbl_id1%irank_home(inod)
          send_nbuf%inod_gl_add(icou) = org_node%inod_global(inod)
          send_nbuf%xx_add(icou,1) =    org_node%xx(inod,1)
          send_nbuf%xx_add(icou,2) =    org_node%xx(inod,2)
          send_nbuf%xx_add(icou,3) =    org_node%xx(inod,3)
        end if
      end do
!
      end subroutine copy_node_to_extend_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine copy_ele_to_extend_buffer(istack_pre,                  &
     &          org_ele, dbl_ele, dbl_id1, iflag_ele, send_ebuf)
!
      integer(kind = kint), intent(in) :: istack_pre
      type(element_data), intent(in) :: org_ele
      type(parallel_double_numbering), intent(in) :: dbl_id1
      type(parallel_double_numbering), intent(in) :: dbl_ele
      integer(kind = kint), intent(in) :: iflag_ele(org_ele%numele)
!
      type(ele_buffer_2_extend), intent(inout) :: send_ebuf
!
      integer(kind = kint) :: icou, iele, k1, inod
!
!
        icou = istack_pre
        do iele = 1, org_ele%numele
          if(iflag_ele(iele) .gt. 0) then
            icou = icou + 1
            send_ebuf%iele_lc(icou) =     iele
            send_ebuf%iele_add(icou) =    dbl_ele%inod_local(iele)
            send_ebuf%irank_add(icou) =   dbl_ele%irank_home(iele)
            send_ebuf%iele_gl_add(icou) = org_ele%iele_global(iele)
            do k1 = 1, org_ele%nnod_4_ele
              inod = org_ele%ie(iele,k1)
              send_ebuf%ie_added(icou,k1) = dbl_id1%inod_local(inod)
              send_ebuf%ip_added(icou,k1) = dbl_id1%irank_home(inod)
            end do
          end if
        end do
!
      end subroutine copy_ele_to_extend_buffer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mark_extended_nod_neib_pe                              &
     &         (nprocs, nod_comm, added_comm, recv_nbuf,                &
     &          iflag_send, iflag_recv)
!
      type(communication_table), intent(in) :: nod_comm, added_comm
      type(node_buffer_2_extend), intent(in) :: recv_nbuf
      integer(kind = kint), intent(in) :: nprocs
      integer(kind = kint), intent(inout) :: iflag_send(0:nprocs-1)
      integer(kind = kint), intent(inout) :: iflag_recv(0:nprocs-1)
!
      integer(kind = kint) :: i, ip
!
!
      do i = 1, added_comm%ntot_import
        ip = recv_nbuf%irank_add(i)
        iflag_recv(ip) = 1
      end do
!
      do i = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(i)
        iflag_recv(ip) = -1
      end do
!
      end subroutine mark_extended_nod_neib_pe
!
!  ---------------------------------------------------------------------
!
      subroutine count_extended_nod_neib_pe                             &
     &         (nprocs, iflag_send, iflag_recv, new_comm)
!
      integer(kind = kint), intent(in) :: nprocs
      integer(kind = kint), intent(in) :: iflag_send(0:nprocs-1)
      integer(kind = kint), intent(in) :: iflag_recv(0:nprocs-1)
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: ip
!
!
      new_comm%num_neib = 0
      do ip = 0, nprocs-1
        if(iflag_recv(ip).ne.0 .or. iflag_send(ip).ne.0) then
          new_comm%num_neib = new_comm%num_neib + 1
        end if
      end do
!
      end subroutine count_extended_nod_neib_pe
!
!  ---------------------------------------------------------------------
!
      subroutine set_extended_nod_neib_pe(nprocs, my_rank,              &
     &          iflag_send, iflag_recv, nod_comm, new_comm)
!
      integer(kind = kint), intent(in) :: nprocs, my_rank
      integer(kind = kint), intent(in) :: iflag_send(0:nprocs-1)
      integer(kind = kint), intent(in) :: iflag_recv(0:nprocs-1)
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: i, ip, icou
!
!
      new_comm%id_neib(1:nod_comm%num_neib)                             &
     &              = nod_comm%id_neib(1:nod_comm%num_neib)
      icou = nod_comm%num_neib
      do i = 0, nprocs-1
        ip = mod(i+my_rank,nprocs)
        if(iflag_recv(ip).gt.0 .or. iflag_send(ip).gt.0) then
          icou = icou + 1
          new_comm%id_neib(icou) = ip
        end if
      end do
!
      end subroutine set_extended_nod_neib_pe
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_extended_node_import                             &
     &        (recv_nbuf, nod_comm, added_comm, new_comm)
!
      type(communication_table), intent(in) :: nod_comm, added_comm
      type(node_buffer_2_extend), intent(in) :: recv_nbuf
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: i, ip, inum
!
!
      do i = 1, nod_comm%num_neib
        new_comm%num_import(i)                                          &
     &       = nod_comm%istack_import(i) - nod_comm%istack_import(i-1)
      end do
      do i = nod_comm%num_neib+1, new_comm%num_neib
        new_comm%num_import(i) = 0
      end do
      do i = 1, new_comm%num_neib
        ip = new_comm%id_neib(i)
        do inum = 1, added_comm%ntot_import
          if(recv_nbuf%irank_add(inum).eq.ip                            &
     &         .and. added_comm%item_import(inum).gt. 0) then
            new_comm%num_import(i) = new_comm%num_import(i) + 1
          end if
        end do
      end do
!
      end subroutine count_extended_node_import
!
!  ---------------------------------------------------------------------
!
      subroutine set_extended_node_import                               &
     &        (recv_nbuf, nod_comm, added_comm, new_comm)
!
      type(communication_table), intent(in) :: nod_comm, added_comm
      type(node_buffer_2_extend), intent(in) :: recv_nbuf
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: i, ip, inum, icou, ist, jst, num
!
!
      do i = 1, nod_comm%num_neib
        ist = new_comm%istack_import(i-1)
        jst = nod_comm%istack_import(i-1)
        num = nod_comm%istack_import(i) - nod_comm%istack_import(i-1)
        do inum = 1, num
          new_comm%item_import(ist+inum)                                &
     &       = nod_comm%item_import(jst+inum)
        end do
      end do
!
      do i = 1, nod_comm%num_neib
        ip = new_comm%id_neib(i)
        icou = new_comm%istack_import(i-1)                              &
     &        + nod_comm%istack_import(i) - nod_comm%istack_import(i-1)
        do inum = 1, added_comm%ntot_import
          if(recv_nbuf%irank_add(inum).eq.ip                            &
     &         .and. added_comm%item_import(inum).gt.0) then
            icou = icou + 1
            new_comm%item_import(icou) = added_comm%item_import(inum)
          end if
        end do
      end do
!
      do i = nod_comm%num_neib+1, new_comm%num_neib
        ip = new_comm%id_neib(i)
        icou = new_comm%istack_import(i-1)
        do inum = 1, added_comm%ntot_import
          if(recv_nbuf%irank_add(inum).eq.ip                            &
     &         .and. added_comm%item_import(inum).gt.0) then
            icou = icou + 1
            new_comm%item_import(icou) = added_comm%item_import(inum)
          end if
        end do
      end do
!
      end subroutine set_extended_node_import
!
!  ---------------------------------------------------------------------
!
      subroutine set_extended_node_export(nod_comm, added_comm,         &
     &          inod_export_new, irank_export_new, new_comm)
!
      type(communication_table), intent(in) :: nod_comm, added_comm
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_export_new(new_comm%ntot_export)
      integer(kind = kint), intent(in)                                  &
     &                     :: irank_export_new(new_comm%ntot_export)
!
      integer(kind = kint) :: i, ip, inum, ist, ied, jst, num
!
!
      do i = 1, nod_comm%num_neib
        ist = new_comm%istack_export(i-1)
        jst = nod_comm%istack_export(i-1)
        num = nod_comm%istack_export(i) - nod_comm%istack_export(i-1)
        do inum = 1, num
          new_comm%item_export(ist+inum)                                &
     &       = nod_comm%item_export(jst+inum)
        end do
      end do
      do i = 1, new_comm%num_neib
        ip = new_comm%id_neib(i)
        ist = new_comm%istack_export(i-1) + 1
        ied = new_comm%istack_export(i)
        do inum = ist, ied
          if(irank_export_new(inum) .eq. my_rank) then
            new_comm%item_export(inum) = inod_export_new(inum)
          end if
        end do
      end do
!
      end subroutine set_extended_node_export
!
!  ---------------------------------------------------------------------
!
      end module find_extended_comm_table
