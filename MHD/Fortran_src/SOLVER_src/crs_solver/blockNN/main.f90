
      program ICCG_SOLVER

      use calypso_mpi
!
      use solverNN

      implicit REAL*8 (A-H,O-Z)
      parameter (kint=4, kreal=8)
      integer(kind=kint )                  ::  N
      integer(kind=kint )                  ::  NP
      integer(kind=kint )                  ::  NPL
      integer(kind=kint )                  ::  NPU

      integer(kind=kint ) ::  NB = 3

      real   (kind=kreal), dimension(:,:,:), allocatable ::  AU, AL
      real   (kind=kreal), dimension(:,:,:), allocatable ::  D

      integer(kind=kint ), dimension(:), allocatable ::  INU, INL
      integer(kind=kint ), dimension(:), allocatable ::  IAU, IAL

      real   (kind=kreal), dimension(:), allocatable ::  B
      real   (kind=kreal), dimension(:), allocatable ::  X

      integer(kind=kint )                  :: NEIBPETOT
      integer(kind=kint ), pointer         :: NEIBPE      (:)
      integer(kind=kint ), pointer         :: STACK_IMPORT(:)
      integer(kind=kint ), pointer         :: NOD_IMPORT  (:)
      integer(kind=kint ), pointer         :: STACK_EXPORT(:)
      integer(kind=kint ), pointer         :: NOD_EXPORT  (:)

      integer :: ERROR

      character(len=kchara)                :: PRECOND, METHOD
      character(len=80)                    :: LINE

      integer(kind=kint ), dimension(:), allocatable :: IW1 
      integer(kind=kint ), dimension(:), allocatable :: IW2 

      integer PETOT

      character*80 MATfile(8)

      real*8,  dimension(10) :: REALARRAY
      integer, dimension(10) ::  INTARRAY
      integer                ::  PRESET

      real*4 time, t1,t2,t3,t4,t5, t6

!C
!C-- init. MPI

      call MPI_INIT      (ierr)
      call MPI_COMM_SIZE (MPI_COMM_WORLD, PETOT, ierr )
      call MPI_COMM_RANK (MPI_COMM_WORLD, my_rank, ierr )
      call MPI_COMM_DUP  (MPI_COMM_WORLD, CALYPSO_COMM, ierr)

!C
!C-- CNTL DATA
      if (PETOT.eq.1) then
        MATfile(1)= '1PE/matIN.0'
       else
        MATfile(1)= '4PE/matIN.0'
        MATfile(2)= '4PE/matIN.1'
        MATfile(3)= '4PE/matIN.2'
        MATfile(4)= '4PE/matIN.3'
      endif

       INTARRAY= 0
      REALARRAY= 0.d0

      if (my_rank.eq.0) then
        write (*,*) 'INPUT FILE NAME'
        open (11,file='inp1', status='unknown')
          read (11,'(a20)') METHOD
          read (11,'(a20)') PRECOND
          read (11,*)  INTARRAY(2)

!     convergence radius
          REALARRAY(1)= 1.d-12
!     Iteration terminate count
          INTARRAY (1)= 100
!     coeficient for diagonal
          REALARRAY(2)= 1.d0
!     sigma...what??
          REALARRAY(3)= 0.d0

        close (11)
      endif

      call MPI_BARRIER(CALYPSO_COMM,ierr)
      call MPI_BCAST  (METHOD  ,20,      CALYPSO_CHARACTER,             &
     &                   0, CALYPSO_COMM, ierr)
      call MPI_BCAST  (PRECOND  ,20,     CALYPSO_CHARACTER,             &
     &                   0, CALYPSO_COMM, ierr)
      call MPI_BCAST  (REALARRAY(1), 10, CALYPSO_REAL,                  &
     &                 0, CALYPSO_COMM, ierr)
      call MPI_BCAST  (INTARRAY(1) , 10, CALYPSO_INTEGER,               &
     &                 0, CALYPSO_COMM, ierr)
      call MPI_BARRIER(CALYPSO_COMM,ierr)

!
!C 
!C +-------------+
!C | MATRIX file |
!C +-------------+
!C===
      open (15, file=MATfile(my_rank+1), status='unknown')

      read (15,'(10i8)') N, NP, NPL, NPU, NB, NEIBPETOT

      allocate (INL(0:NP), INU(0:NP), IAL(NPL), IAU(NPU))
      allocate (AL(NB,NB,NPL), AU(NB,NB,NPU), D(NB,NB,NP))
      allocate ( B(NB*NP),      X(NB*NP))

      read (15,'(10i8)') (INL(k), k= 1, NP)
      read (15,'(10i8)') (INU(k), k= 1, NP)
      read (15,'(10i8)') (IAL(k), k= 1, NPL)
      read (15,'(10i8)') (IAU(k), k= 1, NPU)

      do  k= 1, NPL
      do j1= 1, NB
        read (15,'(5e27.20)') (AL(j1,j2,k),j2= 1,NB)
      enddo
      enddo

      do  k= 1, NPU
      do j1= 1, NB
        read (15,'(5e27.20)') (AU(j1,j2,k),j2= 1,NB)
      enddo
      enddo

      do  k= 1, NP
      do j1= 1, NB
        read (15,'(5e27.20)') (D(j1,j2,k),j2= 1, NB), B(3*(k-1)+j1)
      enddo
      enddo

      INU(0)= 0
      INL(0)= 0

      allocate (NEIBPE(NEIBPETOT))
      if (PETOT.ne.1) then
        read (15,'(10i8)') (NEIBPE(k), k= 1, NEIBPETOT)

        allocate (STACK_IMPORT(0:NEIBPETOT))
        allocate (STACK_EXPORT(0:NEIBPETOT))

        STACK_IMPORT(0)= 0
        STACK_EXPORT(0)= 0

        read (15,'(10i8)') (STACK_IMPORT(k), k= 1, NEIBPETOT)
        read (15,'(10i8)') (STACK_EXPORT(k), k= 1, NEIBPETOT)

        NPSI= STACK_IMPORT(NEIBPETOT)
        NPSE= STACK_EXPORT(NEIBPETOT)

        allocate (NOD_IMPORT(NPSI))
        allocate (NOD_EXPORT(NPSE))
        read (15,'(10i8)') (NOD_IMPORT(k), k= 1, NPSI)
        read (15,'(10i8)') (NOD_EXPORT(k), k= 1, NPSE)
      endif
      close (15)
!C===

!C
!C-- ICCG computation

      call MPI_BARRIER  (CALYPSO_COMM,ierr)
      STARTTIME= MPI_WTIME()
      im1= -1000000
 
      PRESET= 2

      do i= 1, 3*NP
        X(i)= 0.d0
      enddo

      call  solveNN                                                     &
     &                  (N, NP, NB, NPL, NPU,                           &
     &                   D, AL, INL, IAL, AU, INU, IAU, B, X, PRESET,   &
     &                   NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,   &
     &                                      STACK_EXPORT, NOD_EXPORT,   &
     &                   my_rank, ITERactual, ERROR,                    &
     &                   METHOD, PRECOND, INTARRAY, REALARRAY         )


      if (my_rank.eq.0) then
      do i= 1, N
      write (*,'(i8,3(1pe16.6))') i,X(3*i-2),X(3*i-1),X(3*i)
      enddo
      endif

      if (my_rank.eq.0) write (*,*) ITERactual, "  iters"

      ENDTIME= MPI_WTIME()

      call MPI_BARRIER  (CALYPSO_COMM,ierr)

      if (my_rank.eq.0) then
        RTIME= ENDTIME-STARTTIME
        write (*, '("*** ELAPCE TIME", 1pe16.6, " sec.")') RTIME
      endif

      call MPI_FINALIZE(ierr)
      end program ICCG_SOLVER




