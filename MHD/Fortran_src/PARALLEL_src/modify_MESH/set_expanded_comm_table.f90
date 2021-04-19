!>@file   set_expanded_comm_table.f90
!!@brief  module set_expanded_comm_table
!!
!!@author H. Matsui
!!@date Programmed in March, 2021
!
!>@brief  Set 
!!
!!@verbatim
!!      subroutine find_home_import_item_by_trim                        &
!!     &         (nprocs, ntot_new_import, index_4_import_tmp,          &
!!     &          ntot_trimmed_nod_import, istack_trimmed_import_pe,    &
!!     &          istack_trimmed_import_item, idx_home_sorted_import,   &
!!     &          idx_home_for_import, num_miss)
!!        integer, intent(in) :: nprocs
!!        integer(kind = kint), intent(in) :: ntot_new_import
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: index_4_import_tmp(ntot_new_import)
!!        integer(kind = kint), intent(in) :: ntot_trimmed_nod_import
!!        integer(kind= kint), intent(in)                               &
!!     &         :: istack_trimmed_import_pe(0:nprocs)
!!        integer(kind= kint), intent(in)                               &
!!     &         :: istack_trimmed_import_item(0:ntot_trimmed_nod_import)
!!        integer(kind = kint), intent(in)                              &
!!     &              :: idx_home_sorted_import(ntot_trimmed_nod_import)
!!        integer(kind = kint), intent(inout)                           &
!!     &              :: idx_home_for_import(ntot_new_import)
!!        integer(kind = kint), intent(inout) :: num_miss
!!      subroutine count_import_item_for_extend                         &
!!     &         (nprocs, istack_trimmed_import_pe,                     &
!!     &          num_added_neib, id_added_neib, num_added_import)
!!        integer, intent(in) :: nprocs
!!        integer(kind = kint), intent(in)                              &
!!     &      :: istack_trimmed_import_pe(0:nprocs)
!!        integer(kind = kint), intent(in) :: num_added_neib
!!        integer(kind = kint), intent(in)                              &
!!     &        :: id_added_neib(num_added_neib)
!!        integer(kind = kint), intent(inout)                           &
!!     &        :: num_added_import(num_added_neib)
!!      subroutine set_import_item_for_extend                           &
!!     &         (node, nod_comm, expand_nod_comm, ext_nod_trim,        &
!!     &          num_added_neib, id_added_neib,                        &
!!     &          istack_added_import, ntot_added_import,               &
!!     &          inod_lc_new_import_trim, item_added_import)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(communication_table), intent(in) :: expand_nod_comm
!!        type(data_for_trim_import), intent(in) :: ext_nod_trim
!!        integer(kind = kint), intent(in) :: num_added_neib
!!        integer(kind = kint), intent(in) :: ntot_added_import
!!        integer(kind = kint), intent(in)                              &
!!     &      :: id_added_neib(num_added_neib)
!!        integer(kind = kint), intent(in)                              &
!!     &      :: istack_added_import(0:num_added_neib)
!!        integer(kind = kint), intent(inout)                           &
!!     &      :: inod_lc_new_import_trim(ntot_added_import)
!!        integer(kind = kint), intent(inout)                           &
!!     &      :: item_added_import(ntot_added_import)
!!@endverbatim
      module set_expanded_comm_table
!
      use m_precision
!
      use t_geometry_data
      use t_comm_table
      use t_mesh_for_sleeve_extend
      use t_sort_data_for_sleeve_trim
      use t_trim_overlapped_import
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine find_home_import_item_by_trim                          &
     &         (nprocs, ntot_new_import, sort_import, ext_trim,         &
     &          idx_home_for_import, num_miss)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ntot_new_import
!
      type(sort_data_for_sleeve_trim), intent(in) :: sort_import
      type(data_for_trim_import), intent(in) :: ext_trim
!
      integer(kind = kint), intent(inout)                               &
     &              :: idx_home_for_import(ntot_new_import)
      integer(kind = kint), intent(inout) :: num_miss
!
      integer(kind = kint) :: ist, ied, inum, ip
      integer(kind = kint) :: jst, jed, jnum, jnod
!
!
!$omp parallel workshare
      idx_home_for_import(1:ntot_new_import) = 0
!$omp end parallel workshare
!
      do ip = 1, nprocs
        ist = ext_trim%istack_trimmed_pe(ip-1) + 1
        ied = ext_trim%istack_trimmed_pe(ip)
        do inum = ist, ied
          jst = ext_trim%istack_trimmed_item(inum-1) + 1
          jed = ext_trim%istack_trimmed_item(inum)
!
          if(ext_trim%idx_trimmed_to_sorted(inum) .lt. 0) then
            do jnum = jst, jed
              jnod = sort_import%isorted_to_org(jnum)
              if(jnod .gt. 0) then
                idx_home_for_import(jnod)                               &
     &                    = ext_trim%idx_trimmed_to_sorted(inum)
              end if
            end do
          else
            do jnum = jst, jed
              jnod = sort_import%isorted_to_org(jnum)
              idx_home_for_import(jnod)                                 &
     &           = ext_trim%idx_trimmed_to_sorted(inum)
            end do
          end if
!
        end do
      end do
!
      num_miss = 0
      do inum = 1, ntot_new_import
        if(idx_home_for_import(inum) .eq. 0) num_miss = num_miss + 1
      end do
!
      end subroutine find_home_import_item_by_trim
!
!  ---------------------------------------------------------------------
!
      subroutine count_import_item_for_extend_org(ext_nod_trim,         &
     &          num_added_neib, id_added_neib, num_added_import)
!
      type(data_for_trim_import), intent(in) :: ext_nod_trim
!
      integer(kind = kint), intent(in) :: num_added_neib
      integer(kind = kint), intent(in)                                  &
     &        :: id_added_neib(num_added_neib)
 !
      integer(kind = kint), intent(inout)                               &
     &        :: num_added_import(num_added_neib)
!
      integer(kind = kint) :: i, irank
!
!
      do i = 1, num_added_neib
        irank = id_added_neib(i)
        num_added_import(i) = ext_nod_trim%istack_trimmed_pe(irank+1)   &
     &                       - ext_nod_trim%istack_trimmed_pe(irank)
      end do
!
      end subroutine count_import_item_for_extend_org
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_import_item_for_extend(nod_comm, ext_nod_trim,   &
     &          num_added_neib, id_added_neib, num_added_import)
!
      type(communication_table), intent(in) :: nod_comm
      type(data_for_trim_import), intent(in) :: ext_nod_trim
!
      integer(kind = kint), intent(in) :: num_added_neib
      integer(kind = kint), intent(in)                                  &
     &        :: id_added_neib(num_added_neib)
 !
      integer(kind = kint), intent(inout)                               &
     &        :: num_added_import(num_added_neib)
!
      integer(kind = kint) :: i, irank, j0, j, jrank, num
!
!
      do i = 1, num_added_neib
        irank = id_added_neib(i)
        num_added_import(i) = ext_nod_trim%istack_trimmed_pe(irank+1)   &
     &                       - ext_nod_trim%istack_trimmed_pe(irank)
!
        do j0 = 1, nod_comm%num_neib
          j = mod(i+j0-2,nod_comm%num_neib) + 1
          jrank = nod_comm%id_neib(j)
          if(irank .eq. jrank) then
            num = nod_comm%istack_import(j)                             &
     &           - nod_comm%istack_import(j-1)
            num_added_import(i) = num_added_import(i) - num
            exit
          end if
        end do
      end do
!
      end subroutine count_import_item_for_extend
!
!  ---------------------------------------------------------------------
!
      subroutine set_import_item_for_extend                             &
     &         (node, nod_comm, expand_nod_comm, ext_nod_trim,          &
     &          num_added_neib, id_added_neib,                          &
     &          istack_added_import, ntot_added_import,                 &
     &          idx_home_for_import, inod_lc_new_import_trim,           &
     &          item_added_import, inod_added_import)
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(communication_table), intent(in) :: expand_nod_comm
      type(data_for_trim_import), intent(in) :: ext_nod_trim
!
      integer(kind = kint), intent(in) :: num_added_neib
      integer(kind = kint), intent(in) :: ntot_added_import
      integer(kind = kint), intent(in)                                  &
     &      :: id_added_neib(num_added_neib)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_added_import(0:num_added_neib)
      integer(kind = kint), intent(in)                                  &
     &      :: idx_home_for_import(expand_nod_comm%ntot_import)
!
      integer(kind = kint), intent(inout)                               &
     &      :: inod_lc_new_import_trim(ntot_added_import)
      integer(kind = kint), intent(inout)                               &
     &      :: item_added_import(ntot_added_import)
      integer(kind = kint), intent(inout)                               &
     &      :: inod_added_import(expand_nod_comm%ntot_import)
!
      integer(kind = kint) :: i, irank, ist, num
      integer(kind = kint) :: inum, jcou, jnum, isort
!
!
!$omp parallel workshare
      inod_added_import(1:expand_nod_comm%ntot_import) = 0
!$omp end parallel workshare
!
      do i = 1, num_added_neib
        irank = id_added_neib(i)
        ist = ext_nod_trim%istack_trimmed_pe(irank)
        num = ext_nod_trim%istack_trimmed_pe(irank+1) - ist
        jcou = istack_added_import(i-1)
        do inum = 1, num
          jnum = ext_nod_trim%idx_trimmed_to_sorted(inum+ist)
!
          if(jnum .gt. 0) then
            jcou = jcou + 1
            item_added_import(jcou) = jcou + node%numnod
            inod_lc_new_import_trim(jcou)                               &
     &              = expand_nod_comm%item_import(jnum)
            inod_added_import(jnum) = jcou
          end if
!
        end do
      end do
!
!$omp parallel do private(jnum,isort)
      do jnum = 1, expand_nod_comm%ntot_import
        if(inod_added_import(jnum) .eq. 0) then
          isort = idx_home_for_import(jnum)
          if(isort .lt. 0) then
            inod_added_import(jnum) = nod_comm%item_import(-isort)
          else
            inod_added_import(jnum) = inod_added_import(isort)
          end if
        end if
      end do
!$omp end parallel do
!
      end subroutine set_import_item_for_extend
!
!  ---------------------------------------------------------------------
!
      end module set_expanded_comm_table
