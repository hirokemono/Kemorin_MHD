!>@file   t_local_node_id_in_import.f90
!!@brief  module t_local_node_id_in_import
!!
!!@author H. Matsui
!!@date Programmed on Nov., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!@endverbatim
!
      module t_local_node_id_in_import
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_comm_table
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_new_comm_irev_import                             &
     &         (new_comm, nmax_import, numnod_new, inod_recv,           &
     &          num_rev_import_recv, istack_rev_import_recv)
!
      use cal_minmax_and_stacks
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
      integer(kind = kint), intent(in) :: nmax_import
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_rev_import_recv(nmax_import)
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_rev_import_recv(0:nmax_import)
!
      integer(kind = kint) :: inod, jnum, jnod, ntot_tmp
!
!
      istack_rev_import_recv(0:nmax_import) = 0
      num_rev_import_recv(1:nmax_import) = 0
!
      do jnum = 1, new_comm%ntot_import
        jnod = new_comm%item_import(jnum)
        inod = inod_recv(jnod)
        num_rev_import_recv(inod) = num_rev_import_recv(inod) + 1
      end do
!
      call s_cal_total_and_stacks(nmax_import, num_rev_import_recv,     &
     &    izero, istack_rev_import_recv, ntot_tmp)
!
      do inod = 1, nmax_import
        istack_rev_import_recv(inod) = istack_rev_import_recv(inod-1)   &
     &                                + num_rev_import_recv(inod)
      end do
!
      if(ntot_tmp .ne. new_comm%ntot_import) then
        write(*,*) 'Wrong total number of node stack'
      end if
!
      end subroutine count_new_comm_irev_import
!
! ----------------------------------------------------------------------
!
      subroutine set_new_comm_irev_import(new_comm, nmax_import,        &
     &          istack_rev_import_recv, numnod_new, inod_recv,          &
     &          num_rev_import_recv, irev_import, irank_import_recv)
!
      use quicksort
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: nmax_import
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_rev_import_recv(0:nmax_import)
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: num_rev_import_recv(nmax_import)
      integer(kind = kint), intent(inout)                               &
     &                     :: irev_import(new_comm%ntot_import)
      integer(kind = kint), intent(inout)                               &
     &                     :: irank_import_recv(new_comm%ntot_import)
!
      integer(kind = kint) :: ip, ist, ied, inod, jnum, jnod, jst, num
!
!
!$omp parallel workshare
      num_rev_import_recv(1:nmax_import) = 0
!$omp end parallel workshare
!
      do ip = 1, new_comm%num_neib
        ist = new_comm%istack_import(ip-1) + 1
        ied = new_comm%istack_import(ip  )
        do jnum = ist, ied
          jnod = new_comm%item_import(jnum)
          inod = inod_recv(jnod)
          jst = istack_rev_import_recv(inod-1)
          num = num_rev_import_recv(inod) + 1
!
          irank_import_recv(jst+num) =  new_comm%id_neib(ip)
          irev_import(jst+num) = jnum
          num_rev_import_recv(inod) = num
        end do
      end do
!
      do inod = 1, nmax_import
        ist = istack_rev_import_recv(inod-1) + 1
        ied = istack_rev_import_recv(inod  )
        if((ied-ist) .gt. 1) then
          call quicksort_w_index(new_comm%ntot_import,                  &
     &        irank_import_recv, ist, ied, irev_import)
        end if
      end do
!
      end subroutine set_new_comm_irev_import
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function search_repart_external_node         &
     &                           (inod, ip, id_rank, new_comm,          &
     &                            nmax_import, istack_rev_import_recv,  &
     &                            irev_import, irank_import_recv)
!
      use search_from_list
!
      integer(kind = kint), intent(in) :: ip, inod
      integer(kind = kint), intent(in) :: id_rank
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: nmax_import
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_rev_import_recv(0:nmax_import)
      integer(kind = kint), intent(in)                                  &
     &                     :: irev_import(new_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &                      :: irank_import_recv(new_comm%ntot_import)
!
      integer(kind = kint) :: ie_new
!
      integer(kind = kint) :: jnum, jst, jed, knum
!
!
      if(ip .eq. id_rank) then
        ie_new = inod
      else
        ie_new = 0
        jst = istack_rev_import_recv(inod-1) + 1
        jed = istack_rev_import_recv(inod  )
!
        if((jed-jst) .ge. 0) then
          knum = search_from_sorted_data(ip, jst, jed,              &
     &                  new_comm%ntot_import, irank_import_recv)
          if(knum.ge.jst .and. knum.le.jed) then
            jnum = irev_import(knum)
            ie_new = new_comm%item_import(jnum)
          end if
        end if
      end if
      search_repart_external_node = ie_new
!
      end function search_repart_external_node
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_item_import_recv(new_comm, numnod_new,             &
     &                                inod_recv, item_import_recv)
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in) :: numnod_new
      integer(kind = kint), intent(in) :: inod_recv(numnod_new)
      integer(kind = kint), intent(inout)                               &
     &                     :: item_import_recv(new_comm%ntot_import)
!
      integer(kind = kint) :: jnum, jnod
!
!$omp parallel do private(jnum,jnod)
      do jnum = 1, new_comm%ntot_import
        jnod = new_comm%item_import(jnum)
        item_import_recv(jnum) = inod_recv(jnod)
      end do
!$omp end parallel do
!
      end subroutine set_item_import_recv
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function search_repart_ext_node_old          &
     &                            (inod, ip, id_rank, item_import_recv, &
     &                             new_comm, ie_new)
!
      use search_from_list
!
      integer(kind = kint), intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ip, inod
!
      type(communication_table), intent(in) :: new_comm
      integer(kind = kint), intent(in)                                  &
     &                     :: item_import_recv(new_comm%ntot_import)
!
      integer(kind = kint), intent(inout) :: ie_new
!
      integer(kind = kint) :: inum, ist, ied, jnum
!
!
      if(ip .eq. id_rank) then
        ie_new = inod
      else
        ie_new = 0
        inum = search_from_list_data(ip, ione, new_comm%num_neib,  &
     &                         new_comm%num_neib, new_comm%id_neib)
        ist = 0
        ied = -1
        if(inum.ge.ione .and. inum.le.new_comm%num_neib) then
          ist = new_comm%istack_import(inum-1) + 1
          ied = new_comm%istack_import(inum)
        end if
!
        if(ied .ge. ist) then
          jnum = search_from_sorted_data(inod, ist, ied,           &
     &                      new_comm%ntot_import, item_import_recv)
!
          if(jnum.ge.ist .and. jnum.le.ied) then
            ie_new = new_comm%item_import(jnum)
          end if
        end if
      end if
      search_repart_ext_node_old = ie_new
!
      end function search_repart_ext_node_old
!
! ----------------------------------------------------------------------
!
      end module t_local_node_id_in_import
