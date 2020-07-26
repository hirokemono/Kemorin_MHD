
      program ICCG_SOLVER

      use m_precision
      use calypso_mpi
      use calypso_mpi_int
      use calypso_mpi_real
      use calypso_mpi_char
      use transfer_to_long_integers
!
      use m_solver_SR
      use solverNN

      implicit REAL*8 (A-H,O-Z)
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

      integer(kind=kint ) :: ERROR

      character(len=kchara)                :: PRECOND, METHOD

      character*80 MATfile(8)

      real*8,  dimension(10) :: REALARRAY
      integer(kind=kint ), dimension(10) ::  INTARRAY
      integer(kind=kint ) ::  PRESET
      integer(kind=kint ) :: ITERactual
      real*8 RTIME, STARTTIME, ENDTIME
!
      integer(kind=kint ) ::  i, j1, im1, k, NPSI, NPSE
!
!C
!C-- init. MPI

      call calypso_MPI_init
!C
!C-- CNTL DATA
      if (nprocs.eq.1) then
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
          read (11,*) METHOD
          read (11,*) PRECOND
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

      call MPI_BARRIER(CALYPSO_COMM,ierr_MPI)
      call calypso_mpi_bcast_character(METHOD  ,cast_long(20), 0)
      call calypso_mpi_bcast_character(PRECOND  ,cast_long(20), 0)
      call calypso_mpi_bcast_real(REALARRAY(1), cast_long(10), 0)
      call calypso_mpi_bcast_int(INTARRAY(1) , cast_long(10), 0)
      call MPI_BARRIER(CALYPSO_COMM,ierr_MPI)

!
!C 
!C +-------------+
!C | MATRIX file |
!C +-------------+
!C===
      open (15, file=MATfile(my_rank+1), status='unknown')

      read (15,'(10i16)') N, NP, NPL, NPU, NB, NEIBPETOT

      allocate (INL(0:NP), INU(0:NP), IAL(NPL), IAU(NPU))
      allocate (AL(NB,NB,NPL), AU(NB,NB,NPU), D(NB,NB,NP))
      allocate ( B(NB*NP),      X(NB*NP))

      read (15,'(10i16)') (INL(k), k= 1, NP)
      read (15,'(10i16)') (INU(k), k= 1, NP)
      read (15,'(10i16)') (IAL(k), k= 1, NPL)
      read (15,'(10i16)') (IAU(k), k= 1, NPU)

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
      if (nprocs.ne.1) then
        read (15,'(10i16)') (NEIBPE(k), k= 1, NEIBPETOT)

        allocate (STACK_IMPORT(0:NEIBPETOT))
        allocate (STACK_EXPORT(0:NEIBPETOT))

        STACK_IMPORT(0)= 0
        STACK_EXPORT(0)= 0

        read (15,'(10i16)') (STACK_IMPORT(k), k= 1, NEIBPETOT)
        read (15,'(10i16)') (STACK_EXPORT(k), k= 1, NEIBPETOT)

        NPSI= STACK_IMPORT(NEIBPETOT)
        NPSE= STACK_EXPORT(NEIBPETOT)

        allocate (NOD_IMPORT(NPSI))
        allocate (NOD_EXPORT(NPSE))
        read (15,'(10i16)') (NOD_IMPORT(k), k= 1, NPSI)
        read (15,'(10i16)') (NOD_EXPORT(k), k= 1, NPSE)
      endif
      close (15)
!C===

!C
!C-- ICCG computation
      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)
      STARTTIME = MPI_WTIME()
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
     &                   ITERactual, ERROR,                             &
     &                   METHOD, PRECOND, INTARRAY, REALARRAY,          &
     &                   SR_sig1, SR_r1)


      if (my_rank.eq.0) then
      do i= 1, N
      write (*,'(i16,3(1pe16.6))') i,X(3*i-2),X(3*i-1),X(3*i)
      enddo
      endif

      if (my_rank.eq.0) write (*,*) ITERactual, "  iters"

      ENDTIME= MPI_WTIME()

      call MPI_BARRIER  (CALYPSO_COMM,ierr_MPI)

      if (my_rank.eq.0) then
        RTIME= ENDTIME - STARTTIME
        write (*, '("*** ELAPCE TIME", 1pe16.6, " sec.")') RTIME
      endif

      call MPI_FINALIZE(ierr_MPI)
      end program ICCG_SOLVER




