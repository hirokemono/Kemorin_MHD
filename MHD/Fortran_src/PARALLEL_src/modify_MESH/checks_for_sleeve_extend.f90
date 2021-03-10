!> @file  checks_for_sleeve_extend.f90
!!      module checks_for_sleeve_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Control parameter for sleeve extension
!!
!!@verbatim
!!      subroutine check_returned_extend_element                        &
!!     &         (iele_dbl, add_ele_comm, trim_import_ie)
!!        type(node_ele_double_number), intent(in) :: iele_dbl
!!        type(communication_table), intent(in) :: add_ele_comm
!!        type(ele_data_for_sleeve_ext), intent(in) :: trim_import_ie
!!
!!      integer(kind = kint) function check_trimmed_import_node         &
!!     &                (org_node, dbl_id2, add_nod_comm,               &
!!     &                 irank_new_import_trim, inod_lc_new_import_trim)
!!        type(node_data) :: org_node
!!        type(node_ele_double_number) :: dbl_id2
!!        type(communication_table) :: add_nod_comm
!!        integer(kind = kint), intent(in)                              &
!!     &      :: irank_new_import_trim(add_nod_comm%ntot_import)
!!        integer(kind = kint), intent(in)                              &
!!     &      :: inod_lc_new_import_trim(add_nod_comm%ntot_import)
!!      integer(kind = kint) function check_idx_home_for_import         &
!!     &                   (expand_nod_comm, irank_nod_new_import,      &
!!     &                    idx_home_for_import)
!!        type(communication_table) :: expand_nod_comm
!!        integer(kind = kint), intent(in)                              &
!!     &      :: idx_home_for_import(expand_nod_comm%ntot_import)
!!        integer(kind = kint), intent(in)                              &
!!     &      :: irank_nod_new_import(expand_nod_comm%ntot_import)
!!        integer(kind = kint) function check_zero_inod_added_import    &
!!     &                            (ntot, inod_added_import)
!!        integer(kind = kint), intent(in) :: ntot
!!        integer(kind = kint), intent(in) :: inod_added_import(ntot)
!!      integer(kind = kint) function check_wrong_inod_added_import     &
!!     &                   (inod_new_dbl, expand_nod_comm,              &
!!     &                    inod_added_import, irank_nod_new_import)
!!        type(node_ele_double_number) :: inod_new_dbl
!!        type(communication_table) :: expand_nod_comm
!!        integer(kind = kint), intent(in)                              &
!!     &      :: inod_added_import(expand_nod_comm%ntot_import)
!!        integer(kind = kint), intent(in)                              &
!!     &      :: irank_nod_new_import(expand_nod_comm%ntot_import)
!!      integer(kind = kint) function check_negative_ie_new_import      &
!!     &                   (ele, expand_ele_comm, ie_new_import)
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: expand_ele_comm
!!        integer(kind = kint), intent(in)                              &
!!     &     :: ie_new_import(expand_ele_comm%ntot_import,ele%nnod_4_ele)
!!      integer(kind = kint) function check_zero_ie_new_import          &
!!     &                   (ele, expand_ele_comm, ie_new_import)
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: expand_ele_comm
!!        integer(kind = kint), intent(in)                              &
!!     &     :: ie_new_import(expand_ele_comm%ntot_import,ele%nnod_4_ele)
!!      integer(kind = kint) function check_trim_import_ele_connect     &
!!     &                   (ele, add_ele_comm, ie_new_import_trim)
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: add_ele_comm
!!        integer(kind = kint), intent(in)                              &
!!     &   :: ie_new_import_trim(add_ele_comm%ntot_import,ele%nnod_4_ele)
!!      integer(kind = kint) function check_expand_nod_import_item      &
!!     &             (inod_new_dbl, expand_nod_comm, add_nod_comm,      &
!!     &              istack_trimmed_import_pe, idx_home_sorted_import, &
!!     &              irank_nod_new_import)
!!        type(node_ele_double_number), intent(in) :: inod_new_dbl
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(communication_table), intent(in) :: add_nod_comm
!!        integer(kind = kint), intent(in)                              &
!!     &   :: istack_trimmed_import_pe(0:nprocs)
!!        integer(kind = kint), intent(in)                              &
!!     &   :: idx_home_sorted_import(istack_trimmed_import_pe(nprocs))
!!        integer(kind = kint), intent(in)                              &
!!     &   :: irank_nod_new_import(expand_nod_comm%ntot_import)
!!@endverbatim
!
      module checks_for_sleeve_extend
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_para_double_numbering
      use t_mesh_for_sleeve_extend
!
      implicit none
!
      private :: check_recieved_ext_ele_export
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine check_returned_extend_element                          &
     &         (iele_dbl, add_ele_comm, trim_import_ie)
!
      use calypso_mpi
!
      use m_solver_SR
      use calypso_mpi_int
      use reverse_SR_int
!
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(communication_table), intent(in) :: add_ele_comm
      type(ele_data_for_sleeve_ext), intent(in) :: trim_import_ie
!
      integer(kind = kint), allocatable :: irank_new_ele_export_trim(:)
!
      integer(kind = kint) :: icou, ntot_gl
!

      allocate(irank_new_ele_export_trim(add_ele_comm%ntot_export))
      call comm_items_send_recv                                         &
     &   (add_ele_comm%num_neib, add_ele_comm%id_neib,                  &
     &    add_ele_comm%istack_import, add_ele_comm%istack_export,       &
     &    trim_import_ie%irank_comm, SR_sig1,                           &
     &    irank_new_ele_export_trim)
!
      icou = check_recieved_ext_ele_export(iele_dbl, add_ele_comm,      &
     &                                      irank_new_ele_export_trim)
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Failed double element ID ',        &
     &                            'from returnrd table', ntot_gl
      deallocate(irank_new_ele_export_trim)
!
      end subroutine check_returned_extend_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_trimmed_import_node           &
     &                (org_node, dbl_id2, add_nod_comm,                 &
     &                 irank_new_import_trim, inod_lc_new_import_trim)
!
      type(node_data) :: org_node
      type(node_ele_double_number) :: dbl_id2
      type(communication_table) :: add_nod_comm
      integer(kind = kint), intent(in)                                  &
     &      :: irank_new_import_trim(add_nod_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_lc_new_import_trim(add_nod_comm%ntot_import)
!
      integer(kind = kint) :: icou, i, jst, inum, inod
!
      icou = 0
      do i = 1, add_nod_comm%num_neib
        jst = add_nod_comm%istack_import(i-1)
        do inum = 1, add_nod_comm%num_import(i)
          inod = inum + org_node%numnod
          if(dbl_id2%irank(inod) .ne. irank_new_import_trim(inum)      &
     &   .or. dbl_id2%index(inod) .ne. inod_lc_new_import_trim(inum))   &
     &      icou = icou + 1
!          write(*,*) my_rank, 'idx_home_for_import', i, &
!     &      dbl_id2%irank(inum+org_node%numnod),                       &
!     &      irank_new_import_trim(inum),                               &
!     &      dbl_id2%index(inum+org_node%numnod),                       &
!     &      inod_lc_new_import_trim(inum)
        end do
      end do
      check_trimmed_import_node = icou

      end function check_trimmed_import_node
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_idx_home_for_import           &
     &                   (expand_nod_comm, irank_nod_new_import,        &
     &                    idx_home_for_import)
!
      type(communication_table) :: expand_nod_comm
      integer(kind = kint), intent(in)                                  &
     &      :: idx_home_for_import(expand_nod_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &      :: irank_nod_new_import(expand_nod_comm%ntot_import)
!
      integer(kind = kint) :: icou, i, isort
!
      icou = 0
      do i = 1, expand_nod_comm%ntot_import
        isort = idx_home_for_import(i)
        if(isort .gt. expand_nod_comm%ntot_import                       &
     &      .or. isort .le. 0) then
          icou = icou + 1
        else if(irank_nod_new_import(i)                                 &
     &        .ne. irank_nod_new_import(isort)                          &
     &   .or. expand_nod_comm%item_import(i)                            &
     &       .ne. expand_nod_comm%item_import(isort)) then
          icou = icou + 1
        end if
      end do
      check_idx_home_for_import = icou

      end function check_idx_home_for_import
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_zero_inod_added_import        &
     &                            (ntot, inod_added_import)
!
      integer(kind = kint), intent(in) :: ntot
      integer(kind = kint), intent(in) :: inod_added_import(ntot)
!
      integer(kind = kint) :: jcou, i
!
      jcou = 0
      do i = 1, ntot
        if(inod_added_import(i) .eq. 0) jcou = jcou + 1
      end do
      check_zero_inod_added_import = jcou

      end function check_zero_inod_added_import
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_wrong_inod_added_import       &
     &                   (inod_new_dbl, expand_nod_comm,                &
     &                    inod_added_import, irank_nod_new_import)
!
      type(node_ele_double_number) :: inod_new_dbl
      type(communication_table) :: expand_nod_comm
      integer(kind = kint), intent(in)                                  &
     &      :: inod_added_import(expand_nod_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &      :: irank_nod_new_import(expand_nod_comm%ntot_import)
!
      integer(kind = kint) :: icou, i, inod
!
      icou = 0
      do i = 1, expand_nod_comm%ntot_import
        inod = inod_added_import(i)
        if(irank_nod_new_import(i) .ne. inod_new_dbl%irank(inod)        &
     &   .or. expand_nod_comm%item_import(i)                            &
     &                             .ne. inod_new_dbl%index(inod)) then
             icou = icou + 1
        end if
      end do
      check_wrong_inod_added_import = icou

      end function check_wrong_inod_added_import
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_recieved_ext_ele_export       &
     &         (iele_dbl, add_ele_comm, irank_new_ele_export_trim)
!
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(communication_table), intent(in) :: add_ele_comm
!
      integer(kind = kint), intent(in)                                  &
     &   :: irank_new_ele_export_trim(add_ele_comm%ntot_export)
!
      integer(kind = kint) :: icou, iele, inum
!
!
      icou = 0
      do inum = 1, add_ele_comm%ntot_export
        iele = add_ele_comm%item_export(inum)
        if(iele_dbl%index(iele) .ne. add_ele_comm%item_export(inum)     &
         .or. iele_dbl%irank(iele) .ne. irank_new_ele_export_trim(inum) &
     &       )  icou = icou + 1
      end do
      check_recieved_ext_ele_export = icou
!
      end function check_recieved_ext_ele_export
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_negative_ie_new_import        &
     &                   (ele, expand_ele_comm, ie_new_import)
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: expand_ele_comm
!
      integer(kind = kint), intent(in)                                  &
     &     :: ie_new_import(expand_ele_comm%ntot_import,ele%nnod_4_ele)
!
      integer(kind = kint) :: icou, k1, inum
!
!
      icou = 0
      do k1 = 1, ele%nnod_4_ele
        do inum = 1, expand_ele_comm%ntot_import
          if(ie_new_import(inum,k1) .le. 0) icou = icou + 1
        end do
      end do
      check_negative_ie_new_import = icou
!
      end function check_negative_ie_new_import
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_zero_ie_new_import            &
     &                   (ele, expand_ele_comm, ie_new_import)
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: expand_ele_comm
!
      integer(kind = kint), intent(in)                                  &
     &     :: ie_new_import(expand_ele_comm%ntot_import,ele%nnod_4_ele)
!
      integer(kind = kint) :: icou, k1, inum
!
!
      icou = 0
      do k1 = 1, ele%nnod_4_ele
        do inum = 1, expand_ele_comm%ntot_import
          if(ie_new_import(inum,k1) .eq. 0) icou = icou + 1
        end do
      end do
      check_zero_ie_new_import = icou
!
      end function check_zero_ie_new_import
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_trim_import_ele_connect       &
     &                   (ele, add_ele_comm, ie_new_import_trim)
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: add_ele_comm
!
      integer(kind = kint), intent(in)                                  &
     &   :: ie_new_import_trim(add_ele_comm%ntot_import,ele%nnod_4_ele)
!
      integer(kind = kint) :: icou, k1, inum
!
!
      icou = 0
      do k1 = 1, ele%nnod_4_ele
        do inum = 1, add_ele_comm%ntot_import
          if(ie_new_import_trim(inum,k1) .le. 0) icou = icou + 1
        end do
      end do
      check_trim_import_ele_connect = icou
!
      end function check_trim_import_ele_connect
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_expand_nod_import_item        &
     &             (inod_new_dbl, expand_nod_comm, add_nod_comm,        &
     &              istack_trimmed_import_pe, idx_home_sorted_import,   &
     &              irank_nod_new_import)
!
      type(node_ele_double_number), intent(in) :: inod_new_dbl
      type(communication_table), intent(in) :: expand_nod_comm
      type(communication_table), intent(in) :: add_nod_comm
!
      integer(kind = kint), intent(in)                                  &
     &   :: istack_trimmed_import_pe(0:nprocs)
      integer(kind = kint), intent(in)                                  &
     &   :: idx_home_sorted_import(istack_trimmed_import_pe(nprocs))
      integer(kind = kint), intent(in)                                  &
     &   :: irank_nod_new_import(expand_nod_comm%ntot_import)
!
      integer(kind = kint) :: i, icou, irank, ist, jst, jed
      integer(kind = kint) :: inum, jnum, inod
!
!
      icou = 0
      do i = 1, add_nod_comm%num_neib
        irank = add_nod_comm%id_neib(i)
        ist = istack_trimmed_import_pe(irank)
        jst = add_nod_comm%istack_import(i-1) + 1
        jed = add_nod_comm%istack_import(i)
        do inum = jst, jed
          jnum = idx_home_sorted_import(inum+ist)
          inod = add_nod_comm%item_import(inum)
          if(inod_new_dbl%irank(inod) .ne. irank_nod_new_import(jnum)   &
     &       .or. inod_new_dbl%index(inod)                              &
     &          .ne. expand_nod_comm%item_import(jnum)) icou = icou + 1
        end do
      end do
      check_expand_nod_import_item = icou
!
      end function check_expand_nod_import_item
!
! ----------------------------------------------------------------------
!
      end module checks_for_sleeve_extend