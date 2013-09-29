!m_solver_count_time.f90
!
!      module m_solver_count_time
!
!      subroutine reset_solver_time
!      subroutine count_time(iflag_op, time_kind)
!
      module m_solver_count_time
!
      use m_precision
!
      implicit none
!
      real(kind=kreal) :: COMMtime, COMPtime
      real(kind=kreal) :: S1_TIME, E1_TIME
      real(kind=kreal) :: START_TIME, END_TIME
      real(kind=kreal) :: R1
!
      integer(kind=kint), parameter, private :: TIME_KINDS = 20
      real(kind=kreal), private :: time_table(TIME_KINDS)
      real(kind=kreal), private :: start_table(TIME_KINDS)
      data time_table/TIME_KINDS*0/
      data start_table/TIME_KINDS*0/
!
      integer(kind = kint), parameter :: ireset_timer = 0
      integer(kind = kint), parameter :: istart_timer = 1
      integer(kind = kint), parameter :: istop_timer =  2
      integer(kind = kint), parameter :: iprint_timer = 3
!
    !C prt_ech_PE records time_kind at which each PE's time is needed.
      logical, parameter, private :: prt_ech_PE(TIME_KINDS) = .FALSE.
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine reset_solver_time
!
      COMMtime= 0.d0
      COMPtime= 0.d0
!
      end subroutine reset_solver_time
!
!  ---------------------------------------------------------------------
!
      subroutine count_time(iflag_op, time_kind)
!
      use calypso_mpi
!
      integer(kind=kint), intent(in) :: iflag_op
      integer(kind=kint), intent(in) :: time_kind
      integer(kind=kint) :: i, num_of_kinds
      real   (kind=kreal) :: recv_time, send_time, out_time

    !C num_of_kinds must be less than or equal to TIME_KINDS
      num_of_kinds = 15

    !C iflag_op:1 start 2 stop 3 print time 0 reset time_table
      if     (iflag_op .eq. 0) then
        time_table(time_kind) = 0
!
      else if(iflag_op .eq. 1) then
        start_table(time_kind) = MPI_WTIME()
!
      else if(iflag_op .eq. 2) then
       if(start_table(time_kind) .ne. 0) THEN
         time_table(time_kind) = time_table(time_kind)                  &
     &                 + MPI_WTIME() - start_table(time_kind)
          start_table(time_kind) = 0
       end if
!
      else if(iflag_op .eq. 3) then
        send_time = time_table(time_kind)
        recv_time = 0
        CALL MPI_REDUCE(send_time, recv_time, 1, CALYPSO_REAL,          &
     &                   MPI_SUM, 0, CALYPSO_COMM, ierr_MPI)
        CALL MPI_COMM_SIZE(CALYPSO_COMM, nprocs, ierr_MPI)
        IF(my_rank .eq. 0) THEN
          out_time = recv_time / NPROCS
          WRITE(*,'("***", i3, ":", 1pe16.6, " sec")')                  &
     &         time_kind, out_time
       end if
!
      else if(iflag_op .eq. 4) then
!
        CALL MPI_COMM_SIZE(CALYPSO_COMM, NPROCS, ierr_MPI)
        IF(my_rank .eq. 0) THEN
          WRITE(*,'(a)') "#  2:sr_aggre"
          WRITE(*,'(a)') "#  3:sr_elem"
          WRITE(*,'(a)') "#  4:RAP"
          WRITE(*,'(a)') "#  5:neib"
          WRITE(*,'(a)') "#  6:agr"
          WRITE(*,'(a)') "#  7:sgs"
          WRITE(*,'(a)') "#  8:sm_agr"
          WRITE(*,'(a)') "#  9:v_cycle"
          WRITE(*,'(a)') "# 10:calculation"
          WRITE(*,'(a)') "# 11:matrix transform"
          WRITE(*,'(a)') "# 12:ParMetis"
          WRITE(*,'(a)') "# 13:matrix redistribution"
          WRITE(*,'(a)') "# 14:Total time in matrix dist"
          WRITE(*,'(a)') "# 15:calculation in Redist()"
       end if
       DO i = 1, num_of_kinds
          send_time = time_table(i)
          recv_time = 0

          CALL MPI_barrier(CALYPSO_COMM, ierr_MPI)
!
          IF(prt_ech_PE(i)) WRITE(*,'( i3,a3,i3,a1,1pe16.6,a4)')        &
     &         my_rank,"--#", i, ":", send_time, " sec"
          CALL MPI_REDUCE(send_time, recv_time, 1,                      &
     &         CALYPSO_REAL, MPI_SUM, 0, CALYPSO_COMM, ierr_MPI)

          IF(my_rank == 0) THEN
             out_time = recv_time / NPROCS
             WRITE(*,'("***", i3, ":", 1pe16.6, " sec")') i, out_time
          end if
       end do
!
      end if
!
      end subroutine count_time
!
!  ---------------------------------------------------------------------
!
      end module m_solver_count_time
