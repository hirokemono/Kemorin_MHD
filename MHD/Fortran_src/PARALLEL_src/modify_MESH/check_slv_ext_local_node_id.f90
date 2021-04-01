!> @file  check_slv_ext_local_node_id.f90
!!      module check_slv_ext_local_node_id
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node and element to extend export table
!!
!!@verbatim
!!      subroutine s_check_slv_ext_local_node_id                        &
!!     &         (org_node, nod_comm, mark_nod, expand_nod_comm,        &
!!     &          add_nod_comm, sort_nod_import, ext_nod_trim,          &
!!     &          expand_import_position, trimmed_import_position,      &
!!     &          idx_trimmed_to_sorted, inod_lc_new_import_trim)
!!        type(node_data), intent(in) :: org_node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(communication_table), intent(in) :: add_nod_comm
!!        type(mark_for_each_comm), intent(in)                          &
!!     &                          :: mark_nod(nod_comm%num_neib)
!!        type(sort_data_for_sleeve_trim), intent(in) :: sort_nod_import
!!        type(node_data_for_sleeve_ext), intent(in)                    &
!!     &                          :: expand_import_position
!!        type(node_data_for_sleeve_ext), intent(in)                    &
!!     &                          :: trimmed_import_position
!!        type(data_for_trim_import), intent(in) :: ext_nod_trim
!!        integer(kind = kint), intent(in)                              &
!!     &      :: idx_trimmed_to_sorted(expand_nod_comm%ntot_import)
!!        integer(kind = kint), intent(in)                              &
!!     &      :: inod_lc_new_import_trim(add_nod_comm%ntot_import)
!!@endverbatim
!
      module check_slv_ext_local_node_id
!
      use m_precision
      use calypso_mpi
!
      use t_geometry_data
      use t_comm_table
      use t_para_double_numbering
      use t_mesh_for_sleeve_extend
      use t_sort_data_for_sleeve_trim
      use t_trim_overlapped_import
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
      subroutine s_check_slv_ext_local_node_id                          &
     &         (org_node, nod_comm, mark_nod, expand_nod_comm,          &
     &          add_nod_comm, sort_nod_import, ext_nod_trim,            &
     &          expand_import_position, trimmed_import_position,        &
     &          idx_trimmed_to_sorted, inod_lc_new_import_trim)
!
      use reverse_SR_int
!
      type(node_data), intent(in) :: org_node
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: expand_nod_comm
      type(communication_table), intent(in) :: add_nod_comm
      type(mark_for_each_comm), intent(in) :: mark_nod(nod_comm%num_neib)
      type(sort_data_for_sleeve_trim), intent(in) :: sort_nod_import
      type(node_data_for_sleeve_ext), intent(in)                        &
     &                          :: expand_import_position
      type(node_data_for_sleeve_ext), intent(in)                        &
     &                          :: trimmed_import_position
      type(data_for_trim_import), intent(in) :: ext_nod_trim
!
      integer(kind = kint), intent(in)                                  &
     &      :: inod_lc_new_import_trim(add_nod_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &      :: idx_trimmed_to_sorted(expand_nod_comm%ntot_import)
!
      integer(kind = kint), allocatable :: item_new_export(:)
      integer(kind = kint), allocatable :: item_new_import(:)
!
!
      allocate(item_new_export(expand_nod_comm%ntot_export))
      call set_item_new_export(nod_comm, org_node, mark_nod,            &
     &    expand_nod_comm, item_new_export)
!
      allocate(item_new_import(expand_nod_comm%ntot_import))
      call comm_items_send_recv(nod_comm%num_neib, nod_comm%id_neib,    &
     &    expand_nod_comm%istack_export, expand_nod_comm%istack_import, &
     &    item_new_export, item_new_import)
      deallocate(item_new_export)
!
      call check_sort_nod_import(nod_comm, expand_nod_comm,             &
     &    expand_import_position, sort_nod_import, ext_nod_trim,        &
     &    idx_trimmed_to_sorted, item_new_import)
!
      call check_trimmed_import_item(expand_nod_comm, add_nod_comm,     &
     &    trimmed_import_position, ext_nod_trim, item_new_import,       &
     &    inod_lc_new_import_trim)
      deallocate(item_new_import)
!
      end subroutine s_check_slv_ext_local_node_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_item_new_export(nod_comm, node, mark_nod,          &
     &          expand_nod_comm, item_new_export)
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: expand_nod_comm
      type(node_data), intent(in) :: node
      type(mark_for_each_comm), intent(in)                              &
     &                         :: mark_nod(nod_comm%num_neib)
!
      integer(kind = kint), intent(inout)                               &
     &            :: item_new_export(expand_nod_comm%ntot_export)
!
      integer(kind = kint), allocatable :: inod_in_comm(:)
      integer(kind = kint) :: i, ist, num, inod, inum, icou
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
        icou = expand_nod_comm%istack_export(i-1)
        do inum = 1, mark_nod(i)%num_marked
          inod = mark_nod(i)%idx_marked(inum)
          if(inod_in_comm(inod) .lt. 0) cycle

          icou = icou + 1
          inod_in_comm(inod) = icou-expand_nod_comm%istack_export(i-1)
          item_new_export(icou) = inod
        end do
      end do
!
      deallocate(inod_in_comm)
!
      end subroutine set_item_new_export
!
!  ---------------------------------------------------------------------
!
      subroutine check_sort_nod_import(nod_comm, expand_nod_comm,       &
     &          expand_import_position, sort_nod_import, ext_nod_trim,  &
     &          idx_trimmed_to_sorted, item_new_import)
!
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: expand_nod_comm
      type(node_data_for_sleeve_ext), intent(in)                        &
     &                          :: expand_import_position
      type(sort_data_for_sleeve_trim), intent(in) :: sort_nod_import
      type(data_for_trim_import), intent(in) :: ext_nod_trim
!
      integer(kind = kint), intent(in)                                  &
     &      :: idx_trimmed_to_sorted(expand_nod_comm%ntot_import)
!
      integer(kind = kint), intent(in)                                  &
     &      :: item_new_import(expand_nod_comm%ntot_import)
!
      integer(kind = kint) :: i, inod, inum, jst, jed
!
!
!
      write(80+my_rank,*) 'check neib', nod_comm%id_neib
      write(80+my_rank,*) 'check expand_nod_comm%istack_import',        &
     &                   expand_nod_comm%istack_import
      do i = 1, expand_nod_comm%ntot_import
        inod = item_new_import(i)
        write(80+my_rank,*) 'item_new_import', i, inod,                 &
     &       expand_nod_comm%item_import(i),                            &
     &       expand_import_position%irank_comm(i)
      end do

      write(70+my_rank,*) 'check neib', nod_comm%id_neib
      write(70+my_rank,*) 'check ext_nod_trim%istack_trimmed_pe',       &
     &                   ext_nod_trim%istack_trimmed_pe
      do inum = 1, ext_nod_trim%ntot_trimmed
        inod = ext_nod_trim%idx_trimmed_to_sorted(inum)
        jst = ext_nod_trim%istack_trimmed_item(inum-1) + 1
        jed = ext_nod_trim%istack_trimmed_item(inum)
        write(70+my_rank,*) 'item_new_import', inum, inod,              &
     &       expand_nod_comm%item_import(inod),                         &
     &       expand_import_position%irank_comm(inod),                   &
     &       expand_nod_comm%item_import(                               &
     &                        sort_nod_import%iref_lc_import(jst:jed)), &
     &       expand_import_position%irank_comm(                         &
     &                        sort_nod_import%iref_lc_import(jst:jed))
      end do
!
      end subroutine check_sort_nod_import
!
!  ---------------------------------------------------------------------
!
      subroutine check_trimmed_import_item                              &
     &         (expand_nod_comm, add_nod_comm,                          &
     &          trimmed_import_position, ext_nod_trim,                  &
     &          item_new_import, inod_lc_new_import_trim)
!
      type(communication_table) :: expand_nod_comm
      type(communication_table) :: add_nod_comm
      type(node_data_for_sleeve_ext), intent(in)                        &
     &      :: trimmed_import_position
      type(data_for_trim_import), intent(in) :: ext_nod_trim
!
      integer(kind = kint), intent(in)                                  &
     &      :: item_new_import(expand_nod_comm%ntot_import)
      integer(kind = kint), intent(in)                                  &
     &      :: inod_lc_new_import_trim(add_nod_comm%ntot_import)
!
      integer(kind = kint) :: i, irank, ist, jst, num, inod
      integer(kind = kint) :: inum, jcou, jnum
!
      integer(kind = kint), allocatable :: item_new_import_trim(:)
!
!
      allocate(item_new_import_trim(add_nod_comm%ntot_import))
      do i = 1, add_nod_comm%num_neib
        irank = add_nod_comm%id_neib(i)
        ist = ext_nod_trim%istack_trimmed_pe(irank)
        jst = add_nod_comm%istack_import(i-1)
        num = add_nod_comm%istack_import(i) - ist
        do inum = 1, num
          jcou = inum + jst
          jnum = ext_nod_trim%idx_trimmed_to_sorted(inum+ist)
          item_new_import_trim(jcou) = item_new_import(jnum)
        end do
      end do
!
      write(60+my_rank,*) 'check neib', add_nod_comm%id_neib
      write(60+my_rank,*) 'check stack', add_nod_comm%istack_import
      do i = 1, add_nod_comm%ntot_import
        inod = add_nod_comm%item_import(i)
        write(60+my_rank,*) 'trimmed_import_position%irank_comm',       &
     &                      i, inod, inod_lc_new_import_trim(i),        &
     &                      trimmed_import_position%irank_comm(i),      &
     &                      item_new_import_trim(i)
      end do
      deallocate(item_new_import_trim)
!
      end subroutine check_trimmed_import_item
!
!  ---------------------------------------------------------------------
!
      end module check_slv_ext_local_node_id
