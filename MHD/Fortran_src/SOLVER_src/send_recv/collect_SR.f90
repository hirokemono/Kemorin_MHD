!>@file   collect_SR.f90
!!@brief  module collect_SR
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!@n      using reverse import table
!!
!!@verbatim
!!      subroutine dealloc_collect_SR_flag
!!
!!      subroutine count_collect_SR_num(NP, istack_NP)
!!        integer(kind = kint), intent(in) :: NP
!!        integer(kind = kint), intent(inout) :: istack_NP(0:nprocs)
!!
!!      subroutine collect_send_recv_N                                  &
!!     &         (dest_rank, NB, NP, X, istack_NP, X_dest, SR_sig)
!!      integer, intent(in) :: dest_rank
!!      integer(kind = kint), intent(in) :: NB, NP
!!      integer(kind = kint), intent(in) :: istack_NP(0:nprocs)
!!      real(kind = kreal), intent(in) :: X(NB*NP)
!!      real(kind = kreal), intent(inout)                               &
!!     &                   :: X_dest(NB*istack_NP(nprocs))
!!        type(send_recv_status), intent(inout) :: SR_sig
!!      subroutine collect_send_recv_int                                &
!!     &         (dest_rank, NP, iX, istack_NP, X_dest)
!!      integer, intent(in) :: dest_rank
!!      integer(kind = kint), intent(in) :: NP
!!      integer(kind = kint), intent(in) :: istack_NP(0:nprocs)
!!      integer(kind = kint), intent(in) :: iX(NP)
!!      integer(kind = kint), intent(inout) :: iX_dest(istack_NP(nprocs))
!!@endverbatim
!
      module collect_SR
!
      use m_precision
      use calypso_mpi
      use t_solver_SR
!
      implicit none
!
      type(send_recv_status), save, private :: SR_sig_c
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_collect_SR_flag
!
      call dealloc_SR_flag(SR_sig_c)
!
      subroutine dealloc_collect_SR_flag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_collect_SR_num(NP, istack_NP)
!
      integer(kind = kint), intent(in) :: NP
!
      integer(kind = kint), intent(inout) :: istack_NP(0:nprocs)
!
      integer(kind = kint), allocatable :: nums_NP(:)
      integer(kind = kint) :: ntmp(1)
      integer :: id_rank, nneib_recv, ip
!
!
      call resize_SR_flag(1, nprocs, SR_sig_c)
!
      ntmp(1) = NP
      call MPI_ISEND(ntmp, 1, CALYPSO_INTEGER,                          &
     &    0, 0, CALYPSO_COMM, SR_sig_c%req1(1), ierr_MPI)
      if (my_rank .eq. 0) then
        allocate(nums_NP(nprocs))
!
        do ip = 1, nprocs
          id_rank = int(ip - 1)
          call MPI_IRECV(nums_NP(ip), 1, CALYPSO_INTEGER,               &
     &        id_rank, 0, CALYPSO_COMM, SR_sig_c%req2(ip), ierr_MPI)
        end do
        call MPI_WAITALL                                                &
     &     (nprocs, SR_sig_c%req2(1), SR_sig_c%sta2(1,1), ierr_MPI)
      end if
      call MPI_WAITALL(1, SR_sig_c%req1(1), SR_sig_c%sta1(1,1),         &
     &                 ierr_MPI)
!
      if (my_rank .eq. 0) then
        istack_NP(0) = 0
        do ip = 1, nprocs
          istack_NP(ip) = istack_NP(ip-1) + nums_NP(ip)
        end do
!
        deallocate(nums_NP)
      end if
!
      end subroutine count_collect_SR_num
!
! ----------------------------------------------------------------------
!
      subroutine collect_send_recv_N                                    &
     &         (dest_rank, NB, NP, X, istack_NP, X_dest)
!
      integer, intent(in) :: dest_rank
      integer(kind = kint), intent(in) :: NB, NP
!
      integer(kind = kint), intent(in) :: istack_NP(0:nprocs)
      real(kind = kreal), intent(in) :: X(NB*NP)
!
      real(kind = kreal), intent(inout) :: X_dest(NB*istack_NP(nprocs))
!
      integer(kind = kint) :: ist, ip
      integer :: num, id_rank
      
!
!
      call resize_SR_flag(1, nprocs, SR_sig_c)
!
      num = int(NB * NP)
      call MPI_ISEND(X, num, CALYPSO_REAL, dest_rank,                   &
     &               0, CALYPSO_COMM, SR_sig_c%req1(ione), ierr_MPI)
!
      if (my_rank .eq. dest_rank) then
        do ip = 1, nprocs
          id_rank = int(ip - 1)
          ist = NB*istack_NP(ip-1) + 1
          num = int(NB * (istack_NP(ip) - istack_NP(ip-1)))
          call MPI_IRECV(X_dest(ist), num, CALYPSO_REAL, id_rank,       &
     &                   0, CALYPSO_COMM, SR_sig_c%req2(ip), ierr_MPI)
        end do
        call MPI_WAITALL                                                &
     &     (nprocs, SR_sig_c%req2(1), SR_sig_c%sta2(1,1), ierr_MPI)
      end if
      call MPI_WAITALL(1, SR_sig_c%req1(1), SR_sig_c%sta1(1,1),         &
     &                 ierr_MPI)
!
      end subroutine collect_send_recv_N
!
! ----------------------------------------------------------------------
!
      subroutine collect_send_recv_int                                  &
     &         (dest_rank, NP, iX, istack_NP, iX_dest)
!
      integer, intent(in) :: dest_rank
      integer(kind = kint), intent(in) :: NP
!
      integer(kind = kint), intent(in) :: istack_NP(0:nprocs)
      integer(kind = kint), intent(in) :: iX(NP)
!
      integer(kind = kint), intent(inout) :: iX_dest(istack_NP(nprocs))
!
      integer(kind = kint) :: ist, ip
      integer :: num, id_rank
!
!
      call resize_SR_flag(1, nprocs, SR_sig_c)
!
      call MPI_ISEND(iX, int(NP), CALYPSO_INTEGER, dest_rank,           &
     &               0, CALYPSO_COMM, SR_sig_c%req1(ione), ierr_MPI)
!
      if (my_rank .eq. dest_rank) then
        do ip = 1, nprocs
          id_rank = int(ip - 1)
          ist = istack_NP(ip-1) + 1
          num = int(istack_NP(ip) - istack_NP(ip-1))
          call MPI_IRECV(iX_dest(ist), num, CALYPSO_INTEGER, id_rank,   &
     &                   0, CALYPSO_COMM, SR_sig_c%req2(ip), ierr_MPI)
        end do
        call MPI_WAITALL                                                &
     &     (nprocs, SR_sig_c%req2(1), SR_sig_c%sta2(1,1), ierr_MPI)
      end if
      call MPI_WAITALL(1, SR_sig_c%req1(1), SR_sig_c%sta1(1,1),         &
     &                 ierr_MPI)
!
      end subroutine collect_send_recv_int
!
! ----------------------------------------------------------------------
!
      end module collect_SR
