!>@file   append_comm_items.f90
!!@brief  module append_comm_items
!!
!!@author H. Matsui
!!@date Programmed in March, 2021, 2007
!
!>@brief  Append and construct communicatino table
!!
!!@verbatim
!!      subroutine count_merged_comm_table(nprocs, ineib_org, ineib_add,&
!!     &          num_neib_org, istack_comm_org,                        &
!!     &          nrank_neib_add, istack_comm_add,                      &
!!     &          num_new_neib, id_new_neib, num_new_import)
!!
!!      subroutine append_merged_import_item                            &
!!     &         (org_comm, add_comm, nprocs, ineib_org, ineib_add,     &
!!     &          num_new_neib, id_new_neib, istack_new_import,         &
!!     &          ntot_new_import, item_new_import)
!!      subroutine append_merged_export_item                            &
!!     &         (org_comm, add_comm, nprocs, ineib_org, ineib_add,     &
!!     &          num_new_neib, id_new_neib, istack_new_export,         &
!!     &          ntot_new_export, item_new_export)
!!        type(communication_table), intent(in) :: org_comm
!!        type(calypso_comm_table), intent(in) ::  add_comm
!!@endverbatim
!
      module append_comm_items
!
      use m_precision
      use t_comm_table
      use t_calypso_comm_table
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_merged_comm_table(nprocs, ineib_org, ineib_add,  &
     &          num_neib_org, istack_comm_org,                          &
     &          nrank_neib_add, istack_comm_add,                        &
     &          num_new_neib, id_new_neib, num_new_import)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add(nprocs)
!
      integer(kind = kint), intent(in) :: num_neib_org
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_comm_org(0:num_neib_org)
      integer(kind = kint), intent(in) :: nrank_neib_add
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_comm_add(0:nrank_neib_add)
!
      integer(kind = kint), intent(in) :: num_new_neib
      integer(kind = kint), intent(in) :: id_new_neib(num_new_neib)
!
      integer(kind = kint), intent(inout)                               &
     &                      :: num_new_import(num_new_neib)
!
      integer(kind = kint) :: i, irank, ineib, jneib
!
!
!$omp parallel do private(i,irank,ineib,jneib)
      do i = 1, num_new_neib
        irank = id_new_neib(i)
        ineib = ineib_org(irank+1)
        jneib = ineib_add(irank+1)
        num_new_import(i) = 0
        if(ineib .gt. 0) then
          num_new_import(i) = num_new_import(i)                         &
     &              + istack_comm_org(ineib) - istack_comm_org(ineib-1)
        end if
        if(jneib .gt. 0) then
          num_new_import(i) = num_new_import(i)                         &
     &              + istack_comm_add(jneib) - istack_comm_add(jneib-1)
        end if
      end do
!$omp end parallel do
!
      end subroutine count_merged_comm_table
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine append_merged_import_item                              &
     &         (org_comm, add_comm, nprocs, ineib_org, ineib_add,       &
     &          num_new_neib, id_new_neib, istack_new_import,           &
     &          ntot_new_import, item_new_import)
!
      type(communication_table), intent(in) :: org_comm
      type(calypso_comm_table), intent(in) ::  add_comm
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add(nprocs)
      integer(kind = kint), intent(in) :: num_new_neib, ntot_new_import
      integer(kind = kint), intent(in) :: id_new_neib(num_new_neib)
      integer(kind = kint), intent(in)                                  &
     &                      :: istack_new_import(0:num_new_neib)
      integer(kind = kint), intent(inout)                               &
     &                      :: item_new_import(ntot_new_import)
!
      integer :: i, irank, ineib, jneib, jst, ist, num, inum
!
!
!$omp parallel do private(i,irank,ineib,jneib,jst,ist,num,inum)
      do i = 1, num_new_neib
        irank = id_new_neib(i)
        ineib = ineib_org(irank+1)
        jneib = ineib_add(irank+1)

        jst = istack_new_import(i-1)
        if(ineib .gt. 0) then
          ist = org_comm%istack_import(ineib-1)
          num = org_comm%istack_import(ineib) - ist
          do inum = 1, num
            item_new_import(inum+jst) = org_comm%item_import(inum+ist)
          end do
          jst = jst + num
        end if
        if(jneib .gt. 0) then
          ist = add_comm%istack_import(jneib-1)
          num = add_comm%istack_import(jneib) - ist
          do inum = 1, num
            item_new_import(inum+jst) = add_comm%item_import(inum+ist)
          end do
        end if
      end do
!$omend p parallel do
!
      end subroutine append_merged_import_item
!
! ----------------------------------------------------------------------
!
      subroutine append_merged_export_item                              &
     &         (org_comm, add_comm, nprocs, ineib_org, ineib_add,       &
     &          num_new_neib, id_new_neib, istack_new_export,           &
     &          ntot_new_export, item_new_export)
!
      type(communication_table), intent(in) :: org_comm
      type(calypso_comm_table), intent(in) :: add_comm
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: ineib_org(nprocs)
      integer(kind = kint), intent(in) :: ineib_add(nprocs)
      integer(kind = kint), intent(in) :: num_new_neib, ntot_new_export
      integer(kind = kint), intent(in) :: id_new_neib(num_new_neib)
      integer(kind = kint), intent(in)                                  &
     &                      :: istack_new_export(0:num_new_neib)
      integer(kind = kint), intent(inout)                               &
     &                      :: item_new_export(ntot_new_export)
!
      integer :: i, irank, ineib, jneib, jst, ist, num, inum
!
!
!$omp parallel do private(i,irank,ineib,jneib,jst,ist,num,inum)
      do i = 1, num_new_neib
        irank = id_new_neib(i)
        ineib = ineib_org(irank+1)
        jneib = ineib_add(irank+1)

        jst = istack_new_export(i-1)
        if(ineib .gt. 0) then
          ist = org_comm%istack_export(ineib-1)
          num = org_comm%istack_export(ineib) - ist
          do inum = 1, num
            item_new_export(inum+jst) = org_comm%item_export(inum+ist)
          end do
          jst = jst + num
        end if
        if(jneib .gt. 0) then
          ist = add_comm%istack_export(jneib-1)
          num = add_comm%istack_export(jneib) - ist
          do inum = 1, num
            item_new_export(inum+jst) = add_comm%item_export(inum+ist)
          end do
        end if
      end do
!$omend p parallel do
!
      end subroutine append_merged_export_item
!
! ----------------------------------------------------------------------
!
      end module append_comm_items
