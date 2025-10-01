!>@file   compare_calypso_comm_tables.f90
!!@brief  module compare_calypso_comm_tables
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief Compare FEM mesh structures
!!
!!@verbatim
!!      subroutine compare_node_comm_types                              &
!!     &         (id_rank, org_comm, new_comm, icount_error)
!!        type(calypso_comm_table), intent(in) :: org_comm
!!        type(calypso_comm_table), intent(in) :: new_comm
!!@endverbatim
!
      module compare_calypso_comm_tables
!
      use m_precision
      use m_machine_parameter
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_compare_calypso_comm_tables                          &
     &         (id_rank, org_comm, new_comm, icount_error)
!
      use t_calypso_comm_table
!
      integer, intent(in) :: id_rank
      type(calypso_comm_table), intent(in) :: org_comm
      type(calypso_comm_table), intent(in) :: new_comm
      integer(kind = kint), intent(inout) :: icount_error
!
      integer(kind = kint) :: i
!
!
      icount_error = 0
      if(new_comm%nrank_import .ne. org_comm%nrank_import) then
          write(*,*) 'nrank_import', id_rank,                           &
     &            new_comm%nrank_import, org_comm%nrank_import
          icount_error = icount_error + 1
      end if
      if(new_comm%nrank_export .ne. org_comm%nrank_export) then
          write(*,*) 'nrank_export', id_rank,                           &
     &            new_comm%nrank_export, org_comm%nrank_export
          icount_error = icount_error + 1
      end if
      if(new_comm%iflag_self_copy .ne. org_comm%iflag_self_copy) then
          write(*,*) 'iflag_self_copy', id_rank,                        &
     &            new_comm%iflag_self_copy, org_comm%iflag_self_copy
          icount_error = icount_error + 1
      end if
!
      if(new_comm%ntot_export .ne. org_comm%ntot_export) then
            write(*,*) 'ntot_export',                                   &
     &      id_rank, new_comm%ntot_export, org_comm%ntot_export
          icount_error = icount_error + 1
      end if
      if(new_comm%ntot_import .ne. org_comm%ntot_import) then
            write(*,*) 'ntot_import',                                   &
     &      id_rank, new_comm%ntot_import, org_comm%ntot_import
          icount_error = icount_error + 1
      end if
!
      do i = 1, org_comm%nrank_import
        if(new_comm%irank_import(i) .ne. org_comm%irank_import(i)) then
             write(*,*) 'irank_import(i)', id_rank, i,                  &
     &       new_comm%irank_import(i), org_comm%irank_import(i)
          icount_error = icount_error + 1
        end if
        if(new_comm%istack_import(i)                                    &
     &        .ne. org_comm%istack_import(i))  then
             write(*,*) 'istack_import(i)', id_rank, i,                 &
     &       new_comm%istack_import(i), org_comm%istack_import(i)
          icount_error = icount_error + 1
        end if
      end do
!
      do i = 1, org_comm%nrank_export
        if(new_comm%irank_export(i) .ne. org_comm%irank_export(i)) then
             write(*,*) 'irank_export(i)', id_rank, i,                  &
     &       new_comm%irank_export(i), org_comm%irank_export(i)
          icount_error = icount_error + 1
        end if
        if(new_comm%istack_export(i)                                    &
     &        .ne. org_comm%istack_export(i)) then
             write(*,*) 'istack_export(i)', id_rank, i,                 &
     &       new_comm%istack_export(i), org_comm%istack_export(i)
          icount_error = icount_error + 1
        end if
      end do
!
      do i = 1, org_comm%ntot_export
        if(new_comm%item_export(i) .ne. org_comm%item_export(i)) then
             write(*,*) 'item_export(i)', id_rank, i,                   &
     &       new_comm%item_export(i), org_comm%item_export(i)
          icount_error = icount_error + 1
        end if
      end do
      do i = 1, org_comm%ntot_import
        if(new_comm%item_import(i) .ne. org_comm%item_import(i)) then
             write(*,*) 'item_import(i)', id_rank, i,                   &
     &       new_comm%item_import(i), org_comm%item_import(i)
          icount_error = icount_error + 1
        end if
      end do
!
      end subroutine s_compare_calypso_comm_tables
!
!-----------------------------------------------------------------------
!
      end module compare_calypso_comm_tables
