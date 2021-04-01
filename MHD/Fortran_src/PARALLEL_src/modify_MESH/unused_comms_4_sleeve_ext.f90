!>@file   unused_comms_4_sleeve_ext.f90
!!@brief  module unused_comms_4_sleeve_ext
!!
!!@author H. Matsui
!!@date Programmed in March, 2021
!
!>@brief  Trim redundant impoert items from sorted import list
!!
!!@verbatim
!!      subroutine check_node_extend_by_return_comm                     &
!!     &         (inod_dbl, add_nod_comm, irank_new_import_trim,        &
!!     &          inod_gl_new_import_trim, xx_new_import_trim)
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(communication_table), intent(in) :: add_nod_comm
!!      subroutine check_ele_extend_by_return_comm(ele, add_ele_comm,   &
!!     &          iele_gl_new_import_trim, ie_new_import_trim)
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: add_ele_comm
!!      subroutine unused_4_element_sleeve_extend(nod_comm, ele,        &
!!     &          add_ele_comm, inod_dbl, mark_ele, istack_new_export,  &
!!     &          ntot_new_ele_export, istack_new_ele_export,           &
!!     &          ntot_new_ele_import, istack_new_ele_import,           &
!!     &          istack_trimmed_ele_import_pe, ntot_trimmed_ele_import,&
!!     &           idx_home_sorted_ele_import)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: add_ele_comm
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(mark_for_each_comm), intent(in)                          &
!!     &           :: mark_ele(nod_comm%num_neib)
!!@endverbatim
      module unused_comms_4_sleeve_ext
!
      use m_precision
      use m_precision
      use calypso_mpi
      use t_geometry_data
      use t_comm_table
      use t_para_double_numbering
      use m_solver_SR
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine check_node_extend_by_return_comm                       &
     &         (inod_dbl, add_nod_comm, irank_new_import_trim,          &
     &          inod_gl_new_import_trim, xx_new_import_trim)
!
      use reverse_SR_int
      use reverse_SR_int8
      use reverse_SR_real
!
      implicit none
!
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(communication_table), intent(in) :: add_nod_comm
!
      integer(kind = kint_gl), intent(in)                               &
     &             :: inod_gl_new_import_trim(add_nod_comm%ntot_import)
      real(kind = kreal), intent(in)                                    &
     &             :: xx_new_import_trim(3*add_nod_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &             :: irank_new_import_trim(add_nod_comm%ntot_import)
!
      integer(kind = kint_gl), allocatable :: inod_gl_new_export_back(:)
      real(kind = kreal), allocatable :: xx_new_export_back(:)
      integer(kind = kint), allocatable :: irank_new_export_back(:)
!
      integer(kind = kint) :: icou, i, inod
!
!
      allocate(irank_new_export_back(add_nod_comm%ntot_export))
      allocate(xx_new_export_back(3*add_nod_comm%ntot_export))
      allocate(inod_gl_new_export_back(add_nod_comm%ntot_export))
!
      call int8_items_send_recv                                         &
     &   (add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%istack_import, add_nod_comm%istack_export,       &
     &    inod_gl_new_import_trim, SR_sig1, inod_gl_new_export_back)
      call real_items_send_recv_3                                       &
     &   (add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%istack_import, add_nod_comm%istack_export,       &
     &    xx_new_import_trim, SR_sig1, xx_new_export_back)
!
      call comm_items_send_recv                                         &
     &   (add_nod_comm%num_neib, add_nod_comm%id_neib,                  &
     &    add_nod_comm%istack_import, add_nod_comm%istack_export,       &
     &    irank_new_import_trim, irank_new_export_back)
!
      icou = 0
      do i = 1, add_nod_comm%ntot_export
        inod = add_nod_comm%item_export(i)
!        write(50+my_rank,*) 'check', i, inod, &
!     &       inod_dbl%irank(inod), irank_new_export_back(i),  &
!     &       inod_dbl%index(inod)
        if(inod_dbl%irank(inod) .ne. irank_new_export_back(i))     &
     &   icou = icou + 1
      end do
      write(*,*) my_rank, 'Failed communication data: ', icou
!
      deallocate(irank_new_export_back, xx_new_export_back)
      deallocate(inod_gl_new_export_back)
!
      end subroutine check_node_extend_by_return_comm
!
!  ---------------------------------------------------------------------
!
      subroutine check_ele_extend_by_return_comm(ele, add_ele_comm,     &
     &          iele_gl_new_import_trim, ie_new_import_trim)
!
      use reverse_SR_int
      use reverse_SR_int8
!
      implicit none
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: add_ele_comm
!
      integer(kind = kint_gl), intent(in)                               &
     &   :: iele_gl_new_import_trim(add_ele_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &   :: ie_new_import_trim(add_ele_comm%ntot_import,ele%nnod_4_ele)
!
      integer(kind = kint_gl), allocatable                              &
     &                        :: iele_gl_new_export_trim(:)
      integer(kind = kint), allocatable :: ie_new_export_trim(:,:)
!
      integer(kind = kint) :: ntot, k1
!      integer(kind = kint) :: icou, inum, iele
!
!
      ntot = add_ele_comm%ntot_export
      allocate(iele_gl_new_export_trim(ntot))
      allocate(ie_new_export_trim(ntot,ele%nnod_4_ele))
!
      call int8_items_send_recv                                         &
     &   (add_ele_comm%num_neib, add_ele_comm%id_neib,                  &
     &    add_ele_comm%istack_import, add_ele_comm%istack_export,       &
     &    iele_gl_new_import_trim, SR_sig1, iele_gl_new_export_trim)
      do k1 = 1, ele%nnod_4_ele
        call comm_items_send_recv                                       &
     &     (add_ele_comm%num_neib, add_ele_comm%id_neib,                &
     &      add_ele_comm%istack_import, add_ele_comm%istack_export,     &
     &     ie_new_import_trim(1,k1), ie_new_export_trim(1,k1))
      end do
!
!      icou = 0
!      do inum = 1, add_ele_comm%ntot_export
!        iele = iele_lc_new_export_trim(inum)
!        if(ele%iele_global(iele) .ne. iele_gl_new_export_trim(inum))   &
!     &     icou = icou + 1
!        write(50+my_rank,*) inum, 'global ele id',                     &
!     &      ele%iele_global(iele), iele_gl_new_export_trim(inum)
!      end do
!      write(*,*) my_rank, 'failed global element ID', icou
!
      deallocate(iele_gl_new_export_trim, ie_new_export_trim)
!
      end subroutine check_ele_extend_by_return_comm
!
!  ---------------------------------------------------------------------
!
      subroutine unused_4_element_sleeve_extend(nod_comm, ele,          &
     &          add_ele_comm, inod_dbl, mark_ele, istack_new_export,    &
     &          ntot_new_ele_export, istack_new_ele_export,             &
     &          ntot_new_ele_import, istack_new_ele_import,             &
     &          istack_trimmed_ele_import_pe, ntot_trimmed_ele_import,  &
     &           idx_home_sorted_ele_import)
!
      use t_mark_node_ele_to_extend
      use reverse_SR_int
      use reverse_SR_int8
!
      implicit none
!
      type(communication_table), intent(in) :: nod_comm
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: add_ele_comm
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(mark_for_each_comm), intent(in)                              &
     &           :: mark_ele(nod_comm%num_neib)
!
      integer(kind = kint), intent(in) :: ntot_new_ele_export
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_new_export(0:nod_comm%num_neib)
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_new_ele_export(0:nod_comm%num_neib)
!
      integer(kind = kint), intent(in) :: ntot_new_ele_import
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_new_ele_import(0:nod_comm%num_neib)
!
      integer(kind = kint), intent(in) :: ntot_trimmed_ele_import
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_trimmed_ele_import_pe(0:nprocs)
      integer(kind = kint), intent(in)                                  &
     &          :: idx_home_sorted_ele_import(ntot_trimmed_ele_import)
!
      integer(kind = kint), allocatable :: item_new_ele_export(:)
      integer(kind = kint), allocatable :: ie_lc_new_export(:,:)
      integer(kind = kint), allocatable :: ie_rank_new_export(:,:)
!
      integer(kind = kint), allocatable :: item_new_ele_import(:)
      integer(kind = kint), allocatable :: ie_lc_new_import(:,:)
      integer(kind = kint), allocatable :: ie_rank_new_import(:,:)
!
      integer(kind = kint), allocatable :: item_new_ele_import_trim(:)
      integer(kind = kint), allocatable :: ie_lc_new_import_trim(:,:)
      integer(kind = kint), allocatable :: ie_rank_new_import_trim(:,:)
!
      integer(kind = kint), allocatable :: item_new_ele_export_trim(:)
      integer(kind = kint), allocatable :: ie_lc_new_export_trim(:,:)
      integer(kind = kint), allocatable :: ie_rank_new_export_trim(:,:)
!
      integer(kind = kint) :: i, icou, ist, iele, k1, inod, inum
      integer(kind = kint) :: irank, jst, jnum, jcou, ntot
!
!
      allocate(item_new_ele_export(ntot_new_ele_export))
      allocate(ie_lc_new_export(ntot_new_ele_export,ele%nnod_4_ele))
      allocate(ie_rank_new_export(ntot_new_ele_export,ele%nnod_4_ele))
      do i = 1, nod_comm%num_neib
        icou = istack_new_export(i-1)
        ist = istack_new_ele_export(i-1)
!$omp parallel do private(inum,icou,iele,k1,inod)
        do inum = 1, mark_ele(i)%num_marked
          icou = ist + inum
          iele = mark_ele(i)%idx_marked(inum)
          item_new_ele_export(icou) = iele
!
          do k1 = 1, ele%nnod_4_ele
            inod = ele%ie(iele,k1)
            ie_lc_new_export(icou,k1) =   inod_dbl%index(inod)
            ie_rank_new_export(icou,k1) = inod_dbl%irank(inod)
          end do
        end do
!$omp end parallel do
      end do

      allocate(item_new_ele_import(ntot_new_ele_import))
      allocate(ie_lc_new_import(ntot_new_ele_import,ele%nnod_4_ele))
      allocate(ie_rank_new_import(ntot_new_ele_import,ele%nnod_4_ele))
!
      call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    istack_new_ele_export, istack_new_ele_import,                 &
     &    item_new_ele_export, item_new_ele_import)
      do k1 = 1, ele%nnod_4_ele
        call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,  &
     &      istack_new_ele_export, istack_new_ele_import,               &
     &      ie_lc_new_export(1,k1), ie_lc_new_import(1,k1))
      end do
      do k1 = 1, ele%nnod_4_ele
        call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,  &
     &      istack_new_ele_export, istack_new_ele_import,               &
     &      ie_rank_new_export(1,k1), ie_rank_new_import(1,k1))
      end do
!
      deallocate(item_new_ele_export)
      deallocate(ie_lc_new_export, ie_rank_new_export)
!
      ntot = add_ele_comm%ntot_import
      allocate(item_new_ele_import_trim(ntot))
      allocate(ie_lc_new_import_trim(ntot,ele%nnod_4_ele))
      allocate(ie_rank_new_import_trim(ntot,ele%nnod_4_ele))
!
      do i = 1, add_ele_comm%num_neib
        irank = add_ele_comm%id_neib(i)
        ist = istack_trimmed_ele_import_pe(irank)
        jst = add_ele_comm%istack_import(i-1)
        do inum = 1, add_ele_comm%num_import(i)
          jcou = inum + jst
          jnum = idx_home_sorted_ele_import(inum+ist)
          item_new_ele_import_trim(jcou) = item_new_ele_import(jnum)
          do k1 = 1, ele%nnod_4_ele
            ie_lc_new_import_trim(jcou,k1)                              &
     &          = ie_lc_new_import(jnum,k1)
            ie_rank_new_import_trim(jcou,k1)                            &
     &          = ie_rank_new_import(jnum,k1)
          end do
        end do
      end do
!
      ntot = add_ele_comm%ntot_export
      allocate(item_new_ele_export_trim(ntot))
      allocate(ie_lc_new_export_trim(ntot,ele%nnod_4_ele))
      allocate(ie_rank_new_export_trim(ntot,ele%nnod_4_ele))

      call comm_items_send_recv                                         &
     &   (add_ele_comm%num_neib, add_ele_comm%id_neib,                  &
     &    add_ele_comm%istack_import, add_ele_comm%istack_export,       &
     &    item_new_ele_import_trim, item_new_ele_export_trim)
      do k1 = 1, ele%nnod_4_ele
        call comm_items_send_recv                                       &
     &     (add_ele_comm%num_neib, add_ele_comm%id_neib,                &
     &      add_ele_comm%istack_import, add_ele_comm%istack_export,     &
     &     ie_lc_new_import_trim(1,k1), ie_lc_new_export_trim(1,k1))
      end do
      do k1 = 1, ele%nnod_4_ele
        call comm_items_send_recv                                       &
     &     (add_ele_comm%num_neib, add_ele_comm%id_neib,                &
     &      add_ele_comm%istack_import, add_ele_comm%istack_export,     &
     &      ie_rank_new_import_trim(1,k1),                              &
     &      ie_rank_new_export_trim(1,k1))
      end do
!
      deallocate(item_new_ele_import_trim)
      deallocate(ie_lc_new_import_trim, ie_rank_new_import_trim)
      deallocate(item_new_ele_export_trim)
      deallocate(ie_lc_new_export_trim, ie_rank_new_export_trim)
!
      end subroutine unused_4_element_sleeve_extend
!
!  ---------------------------------------------------------------------
!
      end module unused_comms_4_sleeve_ext
