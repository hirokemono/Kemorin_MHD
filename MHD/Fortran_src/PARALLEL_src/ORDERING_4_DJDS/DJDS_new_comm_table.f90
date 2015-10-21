!
!     module DJDS_new_comm_table
!
!      Written by H. Matsui on Jan., 2006
!
!!      subroutine set_new_comm_table_type(NP, nod_comm, djds_tbl)
!!      subroutine set_new_comm_table(NP, OLDtoNEW, NEIBPETOT,          &
!!                STACK_EXPORT, NOD_EXPORT_org, NOD_EXPORT_NEW)
!
      module DJDS_new_comm_table
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_new_comm_table_type(NP, nod_comm, djds_tbl)
!
      use t_comm_table
      use t_solver_djds
!
      integer(kind = kint), intent(in) :: NP
      type(communication_table), intent(in) :: nod_comm
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      call alloc_type_new_comm_table(nod_comm%ntot_export, djds_tbl)
!
      call set_new_comm_table(NP, djds_tbl%OLDtoNEW,                    &
     &    nod_comm%num_neib, nod_comm%istack_export,                    &
     &    nod_comm%item_export, djds_tbl%NOD_EXPORT_NEW)
!
      end subroutine set_new_comm_table_type
!
!-----------------------------------------------------------------------
!
      subroutine set_new_comm_table(NP, OLDtoNEW, NEIBPETOT,            &
                STACK_EXPORT, NOD_EXPORT_org, NOD_EXPORT_NEW)
!
      integer(kind = kint), intent(in) :: NP, NEIBPETOT
      integer(kind = kint), intent(in) :: OLDtoNEW(NP)
!
      integer(kind=kint ), intent(in) :: STACK_EXPORT(0:NEIBPETOT)
      integer(kind=kint ), intent(in)                                   &
     &        :: NOD_EXPORT_org(STACK_EXPORT(NEIBPETOT))
      integer(kind=kint ), intent(inout)                                &
     &        :: NOD_EXPORT_NEW(STACK_EXPORT(NEIBPETOT))
!
      integer (kind = kint) :: i, k, in
!
      if (NEIBPETOT .gt. 0) then
        do k= 1, STACK_EXPORT(NEIBPETOT)
          i= NOD_EXPORT_org(k)
          in= OLDtoNEW(i)
          NOD_EXPORT_NEW(k)= in
        end do
     end if
!
      end subroutine set_new_comm_table
!
!-----------------------------------------------------------------------
!
      end module DJDS_new_comm_table
