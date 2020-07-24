!>@file   t_solver_RMA.f90
!!@brief  module t_solver_RMA
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!!@n     Modified in July, 2020
!
!>@brief  Work area for data communications by RMA
!!
!!@verbatim
!!      subroutine init_work_4_RMA                                      &
!!     &         (NEIBPETOT, NEIBPE, STACK_IMPORT, import_a)
!!
!!      subroutine init_window_4_RMA(NB, NEIBPETOT, STACK_IMPORT, RMA_r)
!!      subroutine delete_window_4_RMA(RMA_r)
!!      subroutine resize_wsend_RMA(nb, ntot_send, RMA_r)
!!      subroutine deallocate_wsend_RMA(RMA_r)
!!        type(RMA_real_buffer), intent(inout) :: RMA_r
!!@endverbatim
!
      module t_solver_RMA
!
      use calypso_mpi
      use m_precision
!
      implicit none
!
!>      Structure for RMA of real data
      type RMA_real_buffer
!>        Import table for RMA
        integer(kind=MPI_ADDRESS_KIND), allocatable :: import_a(:)
!
!>        Integer flag to initialized send buffer
        integer(kind = kint) :: iflag_ws =  -1
!>        work array for communication (send)
        real(kind = kreal), allocatable:: WS_RMA(:)
!>        Integer flag to initialized recieve buffer
        integer(kind = kint) :: iflag_win =  0
!>        work array for communication (receive)
        real(kind = kreal), allocatable:: WRecieve(:)
!
!>        Window flag
        integer :: win
!>        Window flag
        integer :: inbr_group
!>        Window flag
        integer :: nbr_group
      end type RMA_real_buffer
!
      private :: allocate_wsend_RMA
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_work_4_RMA                                        &
     &         (NEIBPETOT, NEIBPE, STACK_IMPORT, import_a)
!
      use t_solver_SR
!
      integer(kind=kint ), intent(in) ::  NEIBPETOT
!        total neighboring pe count
      integer(kind=kint ), intent(in) :: NEIBPE(NEIBPETOT)
!        neighboring pe id                        (i-th pe)
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
!        imported node count for each neighbor pe (i-th pe)
!
      integer(kind=MPI_ADDRESS_KIND), intent(inout)                     &
     &                               :: import_a(NEIBPETOT)
!
      type(send_recv_status) :: RMA_sig
      integer(kind = kint) :: ip
!      integer :: group
!C
!C-- INIT...... only for th first time
      call resize_SR_flag(NEIBPETOT, NEIBPETOT, RMA_sig)
!
!  send address for put
!
      do ip = 1, NEIBPETOT
        call MPI_ISEND(STACK_IMPORT(ip-1), 1, CALYPSO_INTEGER,          &
     &      int(NEIBPE(ip)), 0, CALYPSO_COMM, RMA_sig%req1(ip),         &
     &      ierr_MPI)
      end do
!C
!C-- RECEIVE
!
      do ip = 1, NEIBPETOT
       call MPI_IRECV(import_a(ip), 1, CALYPSO_INTEGER,                 &
     &     int(NEIBPE(ip)), 0, CALYPSO_COMM, RMA_sig%req2(ip),          &
     &     ierr_MPI)
      end do

      call MPI_WAITALL                                                  &
     &   (int(NEIBPETOT), RMA_sig%req2, RMA_sig%sta2, ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(NEIBPETOT), RMA_sig%req1, RMA_sig%sta1, ierr_MPI)
!
!        call MPI_COMM_GROUP(CALYPSO_COMM, group, ierr_MPI)
!        call MPI_GROUP_INCL(group, NEIBPETOT, NEIBPE,                  &
!     &                      RMA_r%nbr_group, ierr_MPI)
!        call MPI_GROUP_free(group, ierr_MPI)
!
      call dealloc_SR_flag(RMA_sig)
!
      end subroutine init_work_4_RMA
!
! ----------------------------------------------------------------------
!
      subroutine init_window_4_RMA(NB, NEIBPETOT, STACK_IMPORT, RMA_r)
!
      integer(kind=kint ), intent(in) :: NB
      integer(kind=kint ), intent(in) :: NEIBPETOT
      integer(kind=kint ), intent(in) :: STACK_IMPORT(0:NEIBPETOT)
!
      type(RMA_real_buffer), intent(inout) :: RMA_r
!
      integer(kind=MPI_ADDRESS_KIND) :: size_window
!
!
      allocate(RMA_r%import_a(NEIBPETOT))
      allocate(RMA_r%WRecieve(NB*STACK_IMPORT(NEIBPETOT)))
      size_window = NB*STACK_IMPORT(NEIBPETOT) * kreal
!
      call MPI_WIN_CREATE(RMA_r%WRecieve, size_window, kreal,           &
     &    MPI_INFO_NULL, CALYPSO_COMM, RMA_r%win, ierr_MPI)
!
      RMA_r%iflag_win = NB*STACK_IMPORT(NEIBPETOT)
!
      end subroutine init_window_4_RMA
!
! ----------------------------------------------------------------------
!
      subroutine delete_window_4_RMA(RMA_r)
!
      type(RMA_real_buffer), intent(inout) :: RMA_r
!
!
      call MPI_WIN_FREE(RMA_r%win)
      deallocate(RMA_r%WRecieve, RMA_r%import_a)
!
      RMA_r%iflag_win = 0
!
      end subroutine delete_window_4_RMA
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine resize_wsend_RMA(nb, ntot_send, RMA_r)
!
      integer(kind=kint), intent(in)   ::  nb, ntot_send
!
      type(RMA_real_buffer), intent(inout) :: RMA_r
!
      if (RMA_r%iflag_ws .lt. 0) then
        call allocate_wsend_RMA(nb, ntot_send, RMA_r)
!
      else if (RMA_r%iflag_ws .ge. 0                                    &
     &       .and. RMA_r%iflag_ws .lt. (nb*ntot_send) ) then
        call deallocate_wsend_RMA(RMA_r)
        call allocate_wsend_RMA(nb, ntot_send, RMA_r)
!
      end if
!
      end subroutine resize_wsend_RMA
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_wsend_RMA(RMA_r)
!
      type(RMA_real_buffer), intent(inout) :: RMA_r
!
      deallocate (RMA_r%WS_RMA)
      RMA_r%iflag_ws = -1
!
      end subroutine deallocate_wsend_RMA
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_wsend_RMA(nb, ntot_send, RMA_r)
!
      integer(kind=kint), intent(in)   ::  nb, ntot_send
      type(RMA_real_buffer), intent(inout) :: RMA_r
!
      RMA_r%iflag_ws = nb * ntot_send
      allocate (RMA_r%WS_RMA(RMA_r%iflag_ws))
!
      end subroutine allocate_wsend_RMA
!
! ----------------------------------------------------------------------
!
      end module t_solver_RMA
