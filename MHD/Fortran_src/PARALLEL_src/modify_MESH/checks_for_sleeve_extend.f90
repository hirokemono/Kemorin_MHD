!> @file  checks_for_sleeve_extend.f90
!!      module checks_for_sleeve_extend
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Control parameter for sleeve extension
!!
!!@verbatim
!!      subroutine check_appended_node_data                             &
!!     &         (node, expand_nod_comm, add_nod_comm, exp_import_xx,   &
!!     &          ext_nod_trim, trim_import_xx, inod_new_dbl,           &
!!     &          idx_nod_extend_to_trimmed, inod_lc_new_import_trim)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(calypso_comm_table), intent(in) :: add_nod_comm
!!        type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
!!        type(node_data_for_sleeve_ext), intent(in) :: trim_import_xx
!!        type(data_for_trim_import), intent(in) :: ext_nod_trim
!!        type(node_ele_double_number), intent(in) :: inod_new_dbl
!!      subroutine check_expanded_import_node(inod_new_dbl,             &
!!     &          expand_nod_comm, exp_import_xx, inod_added_import)
!!        type(node_ele_double_number), intent(in) :: inod_new_dbl
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
!!        integer(kind = kint), intent(in)                              &
!!     &          :: inod_added_import(expand_nod_comm%ntot_import)
!!      subroutine check_expanded_import_ele                            &
!!     &         (ele, expand_ele_comm, exp_import_ie)
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: expand_ele_comm
!!        type(ele_data_for_sleeve_ext), intent(in) :: exp_import_ie
!!      subroutine check_returned_extend_element                        &
!!     &         (iele_dbl, add_ele_comm, trim_import_ie)
!!        type(node_ele_double_number), intent(in) :: iele_dbl
!!        type(communication_table), intent(in) :: add_ele_comm
!!        type(ele_data_for_sleeve_ext), intent(in) :: trim_import_ie
!!
!!      subroutine check_trim_import_ele_connect                        &
!!     &         (ele, add_ele_comm, ie_new_import_trim)
!!        type(element_data), intent(in) :: ele
!!        type(calypso_comm_table), intent(in) :: add_ele_comm
!!        integer(kind = kint), intent(in)                              &
!!     &   :: ie_new_import_trim(add_ele_comm%ntot_import,ele%nnod_4_ele)
!!@endverbatim
!
      module checks_for_sleeve_extend
!
      use m_precision
      use m_constants
!
      use t_calypso_comm_table
      use t_geometry_data
      use t_comm_table
      use t_calypso_comm_table
      use t_para_double_numbering
      use t_mesh_for_sleeve_extend
!
      implicit none
!
      private :: check_trimmed_import_node
      private :: check_idx_home_for_import
      private :: check_zero_inod_added_import
      private :: check_wrong_inod_added_import
      private :: check_zero_ie_new_import
      private :: check_negative_ie_new_import
      private :: check_recieved_ext_ele_export
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine check_appended_node_data                               &
     &         (node, expand_nod_comm, add_nod_comm, exp_import_xx,     &
     &          ext_nod_trim, trim_import_xx, inod_new_dbl,             &
     &          idx_nod_extend_to_trimmed, inod_lc_new_import_trim)
!
      use t_para_double_numbering
      use t_trim_overlapped_import
      use t_mesh_for_sleeve_extend
!
      use calypso_mpi
      use calypso_mpi_int
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: expand_nod_comm
      type(calypso_comm_table), intent(in) :: add_nod_comm
      type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
      type(node_data_for_sleeve_ext), intent(in) :: trim_import_xx
      type(data_for_trim_import), intent(in) :: ext_nod_trim
      type(node_ele_double_number), intent(in) :: inod_new_dbl
      integer(kind = kint), intent(in)                                  &
     &        :: idx_nod_extend_to_trimmed(expand_nod_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &        :: inod_lc_new_import_trim(add_nod_comm%ntot_import)
!
      integer(kind = kint) :: icou, ntot_gl
!
!
      icou = check_trimmed_import_node                                  &
     &             (node, inod_new_dbl, add_nod_comm,                   &
     &              trim_import_xx%irank_comm, inod_lc_new_import_trim)
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)  'Num. of failed ',                 &
     &        'trimmed_import_node:', ntot_gl
!
      icou = check_idx_home_for_import(expand_nod_comm,                 &
     &    exp_import_xx%irank_comm, idx_nod_extend_to_trimmed)
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Number of Wrong address ',         &
     &         'in idx_nod_extend_to_trimmed ', ntot_gl
!
      end subroutine check_appended_node_data
!
!  ---------------------------------------------------------------------
!
      subroutine check_expanded_import_node(inod_new_dbl,               &
     &          expand_nod_comm, exp_import_xx, inod_added_import)
!
      use calypso_mpi
      use calypso_mpi_int
!
      type(node_ele_double_number), intent(in) :: inod_new_dbl
      type(communication_table), intent(in) :: expand_nod_comm
      type(node_data_for_sleeve_ext), intent(in) :: exp_import_xx
      integer(kind = kint), intent(in)                                  &
     &          :: inod_added_import(expand_nod_comm%ntot_import)
!
      integer(kind = kint) :: icou, ntot_gl
!
!
      icou = check_zero_inod_added_import(expand_nod_comm%ntot_import,  &
     &                                    inod_added_import)
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Number of Zero address ',          &
     &           'in inod_added_import', ntot_gl

      icou = check_wrong_inod_added_import                              &
     &           (inod_new_dbl, expand_nod_comm,                        &
     &            inod_added_import, exp_import_xx%irank_comm)
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*) 'Number of Wrong address ',         &
     &           'in inod_added_import', ntot_gl

      end subroutine check_expanded_import_node
!
!  ---------------------------------------------------------------------
!
      subroutine check_expanded_import_ele                              &
     &         (ele, expand_ele_comm, exp_import_ie)
!
      use calypso_mpi
      use calypso_mpi_int
!
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: expand_ele_comm
      type(ele_data_for_sleeve_ext), intent(in) :: exp_import_ie
!
      integer(kind = kint) :: icou, ntot_gl
!
!
      icou = check_zero_ie_new_import(ele, expand_ele_comm,             &
     &                                exp_import_ie%ie_comm)
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'zero exp_import_ie%ie_comm before fix', ntot_gl
!
      icou = check_negative_ie_new_import(ele, expand_ele_comm,         &
     &                                    exp_import_ie%ie_comm)
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &     'Negative exp_import_ie%ie_comm before fix', ntot_gl
!
      end subroutine check_expanded_import_ele
!
!  ---------------------------------------------------------------------
!
      subroutine check_returned_extend_element                          &
     &         (iele_dbl, add_ele_comm, trim_import_ie)
!
      use calypso_mpi
!
      use calypso_mpi_int
      use reverse_SR_int
!
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(calypso_comm_table), intent(in) :: add_ele_comm
      type(ele_data_for_sleeve_ext), intent(in) :: trim_import_ie
!
      integer(kind = kint), allocatable :: irank_new_ele_export_trim(:)
!
      integer(kind = kint) :: icou, ntot_gl
!

      allocate(irank_new_ele_export_trim(add_ele_comm%ntot_export))
      call comm_items_send_recv                                         &
     &   (add_ele_comm%nrank_import, add_ele_comm%irank_import,         &
     &    add_ele_comm%istack_import, trim_import_ie%irank_comm,        &
     &    add_ele_comm%nrank_export, add_ele_comm%irank_export,         &
     &    add_ele_comm%istack_export, add_ele_comm%iflag_self_copy,     &
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
!
      subroutine check_trim_import_ele_connect                          &
     &         (ele, add_ele_comm, ie_new_import_trim)
!
      use calypso_mpi
!
      use m_solver_SR
      use calypso_mpi_int
!
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: add_ele_comm
!
      integer(kind = kint), intent(in)                                  &
     &   :: ie_new_import_trim(add_ele_comm%ntot_import,ele%nnod_4_ele)
!
      integer(kind = kint) :: icou, k1, inum, ntot_gl
!
!
      icou = 0
      do k1 = 1, ele%nnod_4_ele
        do inum = 1, add_ele_comm%ntot_import
          if(ie_new_import_trim(inum,k1) .le. 0) icou = icou + 1
        end do
      end do
      call calypso_mpi_reduce_one_int(icou, ntot_gl, MPI_SUM, 0)
      if(my_rank .eq. 0) write(*,*)                                     &
     &       'Number of wrong trim_import_ie%ie_comm', ntot_gl
!
      end subroutine check_trim_import_ele_connect
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_trimmed_import_node           &
     &                (org_node, inod_new_dbl, add_nod_comm,            &
     &                 irank_new_import_trim, inod_lc_new_import_trim)
!
      type(node_data), intent(in) :: org_node
      type(node_ele_double_number), intent(in) :: inod_new_dbl
      type(calypso_comm_table), intent(in) :: add_nod_comm
      integer(kind = kint), intent(in)                                  &
     &      :: irank_new_import_trim(add_nod_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_lc_new_import_trim(add_nod_comm%ntot_import)
!
      integer(kind = kint) :: icou, i, jst, inum, inod
!
      icou = 0
      do i = 1, add_nod_comm%nrank_import
        jst = add_nod_comm%istack_import(i-1)
        do inum = 1, add_nod_comm%num_import(i)
          inod = inum + org_node%numnod
          if(inod_new_dbl%irank(inod) .ne. irank_new_import_trim(inum)  &
     &        .or. inod_new_dbl%index(inod)                             &
     &           .ne. inod_lc_new_import_trim(inum)) icou = icou + 1
!          write(*,*) my_rank, 'idx_home_for_import', i,                &
!     &      inod_new_dbl%irank(inum+org_node%numnod),                  &
!     &      irank_new_import_trim(inum),                               &
!     &      inod_new_dbl%index(inum+org_node%numnod),                  &
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
      type(node_ele_double_number), intent(in) :: inod_new_dbl
      type(communication_table), intent(in) :: expand_nod_comm
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
      type(calypso_comm_table), intent(in) :: add_ele_comm
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
      end module checks_for_sleeve_extend
