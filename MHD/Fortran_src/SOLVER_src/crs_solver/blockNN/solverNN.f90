!
!  module solverNN

!
!C*** 
!C*** module solverNN
!C***
!
      module solverNN
!
      use m_precision
!
      implicit REAL*8(A-H,O-Z)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!C
!C--- init_solverNN

        subroutine  init_solverNN (ERROR)
! \beginSUBROUTINE
!      initialize solver subsystems for nb*nb Block Matrix
!    \begin{flushright}     
!     coded by K.Nakajima (RIST) on DEC. 1999 (ver 3.0)
!    \end{flushright}     
! \endSUBROUTINE
        integer(kind=kint )                  , intent(inout) :: ERROR
        ERROR= 0
        end subroutine init_solverNN

!C
!C--- solve
      subroutine  solveNN                                               &
     &                  (N, NP, NB, NPL, NPU,                           &
     &                   D, AL, INL, IAL, AU, INU, IAU, B, X, PRESET,   &
     &                   NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,   &
     &                                      STACK_EXPORT, NOD_EXPORT,   &
     &                   ITERactual, ERROR, METHOD, PRECOND,            &
     &                   INTARRAY, REALARRAY)

! \beginSUBROUTINE
!      solver subsystem entry for nb*nb Block Matrix
!    \begin{flushright}     
!     coded by K.Nakajima (RIST) on DEC. 1999 (ver 3.0)
!    \end{flushright}     

      use calypso_mpi
!
      use solver_CG_N
      use solver_GPBiCG_N
      use solver_BiCGSTAB_N


! ......................................................................
      integer(kind=kint )                  , intent(in)   ::  N
! \beginARG       number of internal nodes (exclude external node)
      integer(kind=kint )                  , intent(in)   ::  NP
! \beginARG       number of nodes          (include external node)
      integer(kind=kint )                  , intent(in)   ::  NPL
! \beginARG       array length of IAL, AL
      integer(kind=kint )                  , intent(in)   ::  NPU
! \beginARG       array length of IAU, AU
      real   (kind=kreal), dimension(NB,NB,NP), intent(inout)::  D
! \beginARG       diagonal matrix value            (i-th dof)
      real   (kind=kreal), dimension(NB,NB,NPL),intent(inout):: AL
! \beginARG       upper triangular matrix value    (i-th term)
      integer(kind=kint ), dimension(0:NP) , intent(in)    ::  INL
! \beginARG       last term count at each freedom  (i-th node)
      integer(kind=kint ), dimension(NPL)  , intent(in)    ::  IAL
! \beginARG       node number at each term         (i-th term)
      real   (kind=kreal), dimension(NB,NB,NPU),intent(inout):: AU
! \beginARG       lower triangular matrix value    (i-th term)
      integer(kind=kint ), dimension(0:NP) ,intent(in)     ::  INU
! \beginARG       last term count at each freedom  (i-th node)
      integer(kind=kint ), dimension(NPU)  , intent(in)    ::  IAU
! \beginARG       node number at each term         (i-th term)
      real   (kind=kreal), dimension(NB*NP) , intent(inout)::  B
! \beginARG       right hand load vector
      real   (kind=kreal), dimension(NB*NP) , intent(inout)::  X
! \beginARG       solution vector
      integer(kind=kint )                  , intent(in)   ::  PRESET
! \beginARG       preconditioning RESET flag (n:0, y:1)
      integer(kind=kint )                  , intent(in)   ::  NEIBPETOT
! \beginARG       total neighboring pe count
      integer(kind=kint ), dimension(NEIBPETOT)   :: NEIBPE
! \beginARG       neighboring pe id                        (i-th pe)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_IMPORT
! \beginARG       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(STACK_IMPORT(NEIBPETOT))           &
     &       :: NOD_IMPORT
! \beginARG       imported degree of freedom               (i-th node)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_EXPORT
! \beginARG       exported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(STACK_EXPORT(NEIBPETOT))           &
     &       :: NOD_EXPORT
! \beginARG       exported node                            (i-th node)
      integer(kind=kint )                  , intent(out)  :: ITERactual
! \beginARG       actual iteration number
      integer(kind=kint )                  , intent(inout):: ERROR
! \beginARG       error flag (0:normal termination, -1: iter. over)
      character(len=kchara)                , intent(inout):: METHOD
! \beginARG       solver method name
      character(len=kchara)                , intent(inout):: PRECOND
! \beginARG       precondition method name
      integer(kind=kint ), dimension(:)    , intent(inout):: INTARRAY
! \beginARG       interger array for solver parameters
      real   (kind=kreal), dimension(:)    , intent(inout):: REALARRAY
! \beginARG       real     array for solver parameters
      real   (kind=kreal)                                 :: SIGMA
! \beginARG       solver parameters
! \endSUBROUTINE

      integer(kind=kint) :: ITER, FLAGmethod, FLAGprecond

!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
      FLAGmethod = 0
      FLAGprecond= 0

      if ( ((PRECOND(1:1).eq.'I').or.(PRECOND(1:1).eq.'i')) .and.       &
     &     ((PRECOND(2:2).eq.'C').or.(PRECOND(2:2).eq.'c')) ) then
        PRECOND= 'IC'
        FLAGprecond= 1
      endif

      if ( ((PRECOND(1:1).eq.'I').or.(PRECOND(1:1).eq.'i')) .and.       &
     &     ((PRECOND(2:2).eq.'L').or.(PRECOND(2:2).eq.'l')) .and.       &
     &     ((PRECOND(3:3).eq.'U').or.(PRECOND(3:3).eq.'u')) ) then
        PRECOND= 'ILU'
        FLAGprecond= 1
      endif

      if ( ((PRECOND(1:1).eq.'S').or.(PRECOND(1:1).eq.'s')) .and.       &
     &     ((PRECOND(2:2).eq.'S').or.(PRECOND(2:2).eq.'s')) .and.       &
     &     ((PRECOND(3:3).eq.'O').or.(PRECOND(3:3).eq.'o')) .and.       &
     &     ((PRECOND(4:4).eq.'R').or.(PRECOND(4:4).eq.'r')) ) then
        PRECOND= 'SSOR'
        FLAGprecond= 1
      endif

      if ( ((PRECOND(1:1).eq.'D').or.(PRECOND(1:1).eq.'d')) .and.       &
     &     ((PRECOND(2:2).eq.'I').or.(PRECOND(2:2).eq.'i')) .and.       &
     &     ((PRECOND(3:3).eq.'A').or.(PRECOND(3:3).eq.'a')) ) then
        PRECOND= 'DIAG'
        FLAGprecond= 1
      endif

      if ( ((PRECOND(1:1).eq.'B').or.(PRECOND(1:1).eq.'b')) .and.       &
     &     ((PRECOND(2:2).eq.'L').or.(PRECOND(2:2).eq.'l')) .and.       &
     &     ((PRECOND(3:3).eq.'O').or.(PRECOND(3:3).eq.'o')) .and.       &
     &     ((PRECOND(4:4).eq.'C').or.(PRECOND(4:4).eq.'c')) ) then
        PRECOND= 'BLOCK'
        FLAGprecond= 1
      endif

      ITER   = INTARRAY (1)
      NREST  = INTARRAY (2) * NB

      RESID     = REALARRAY (1)
      SIGMA_DIAG= REALARRAY (2)
      SIGMA     = REALARRAY (3)

      ERROR= 0

!C
!C-- DIAG. check
      do i= 1, N
      do k= 1, NB
        if (D(k,k,i).eq.0.d0) ERROR= 300
      enddo
      enddo

!C
!C-- RHS check
      BNRM20= 0.d0
      do j= 1, N
        jj= NB*j-NB
        do k= 1, NB
          BNRM20= BNRM20 + B(jj+k)**2
        enddo
      enddo

      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr)
      if (BNRM2.eq.0.d0) ERROR= 320
!C===

!C
!C +--------+
!C | SOLVER |
!C +--------+
!C===
      if (ERROR.eq.0) then
      if ( ((METHOD(1:1).eq.'C').or.(METHOD(1:1).eq.'c')) .and.         &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) ) then
      FLAGmethod = 1
      call CG_N (N, NP, NB, NPL, NPU, D, AL, INL, IAL, AU, INU, IAU,    &
     &           B, X, PRECOND, SIGMA_DIAG, SIGMA, RESID, ITER,  ERROR, &
     &         NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &                            STACK_EXPORT, NOD_EXPORT, PRESET)
      endif
      endif

      if (ERROR.eq.0) then
      if ( ((METHOD(1:1).eq.'B').or.(METHOD(1:1).eq.'b')) .and.         &
     &     ((METHOD(2:2).eq.'I').or.(METHOD(2:2).eq.'i')) .and.         &
     &     ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.         &
     &     ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) .and.         &
     &     ((METHOD(5:5).eq.'S').or.(METHOD(5:5).eq.'s')) ) then
      FLAGmethod = 1
      call BiCGSTAB_N                                                   &
     &          (N, NP, NB, NPL, NPU, D, AL, INL, IAL, AU, INU, IAU,    &
     &           B, X, PRECOND, SIGMA_DIAG, SIGMA, RESID, ITER,  ERROR, &
     &         NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &                            STACK_EXPORT, NOD_EXPORT, PRESET)
      endif
      endif

      if (ERROR.eq.0) then
      if ( ((METHOD(1:1).eq.'G').or.(METHOD(1:1).eq.'g')) .and.         &
     &     ((METHOD(2:2).eq.'P').or.(METHOD(2:2).eq.'p')) .and.         &
     &     ((METHOD(3:3).eq.'B').or.(METHOD(3:3).eq.'b')) .and.         &
     &     ((METHOD(4:4).eq.'I').or.(METHOD(4:4).eq.'i')) .and.         &
     &     ((METHOD(5:5).eq.'C').or.(METHOD(5:5).eq.'c')) ) then
      FLAGmethod = 1
      call GPBiCG_N                                                     &
     &          (N, NP, NB, NPL, NPU, D, AL, INL, IAL, AU, INU, IAU,    &
     &           B, X, PRECOND, SIGMA_DIAG, SIGMA, RESID, ITER,  ERROR, &
     &         NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &                            STACK_EXPORT, NOD_EXPORT, PRESET)
      endif
      endif

!      if (ERROR.eq.0) then
!      if ( ((METHOD(1:1).eq.'G').or.(METHOD(1:1).eq.'g')) .and.         &
!     &     ((METHOD(2:2).eq.'M').or.(METHOD(2:2).eq.'m')) .and.         &
!     &     ((METHOD(3:3).eq.'R').or.(METHOD(3:3).eq.'r')) .and.         &
!     &     ((METHOD(4:4).eq.'E').or.(METHOD(4:4).eq.'e')) .and.         &
!     &     ((METHOD(5:5).eq.'S').or.(METHOD(5:5).eq.'s')) ) then
!      FLAGmethod = 1
!      call GMRES_N                                                      &
!     &       (N, NP, NB, NPL, NPU, D, AL, INL, IAL, AU, INU, IAU, B, X,  &
!     &        PRECOND, SIGMA_DIAG, SIGMA, NREST, RESID, ITER,  ERROR,  &
!     &         NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,            &
!     &                            STACK_EXPORT, NOD_EXPORT, PRESET)
!      endif
!      endif

!      if (ERROR.eq.0) then
!      if ( ((METHOD(1:1).eq.'B').or.(METHOD(1:1).eq.'b')) .and.         &
!     &     ((METHOD(2:2).eq.'L').or.(METHOD(2:2).eq.'l')) .and.         &
!     &     ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.         &
!     &     ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) ) then
!      FLAGmethod = 1
!
!      FLAGprecond= -1
!        if ( ((PRECOND(1:1).eq.'I').or.(PRECOND(1:1).eq.'i')) .and.     &
!     &       ((PRECOND(2:2).eq.'L').or.(PRECOND(2:2).eq.'l')) .and.     &
!     &       ((PRECOND(3:3).eq.'U').or.(PRECOND(4:4).eq.'u')) ) then
!          PRECOND= 'BILU0'
!          FLAGprecond= 1
!        endif
!        if ( ((PRECOND(1:1).eq.'I').or.(PRECOND(1:1).eq.'i')) .and.     &
!     &       ((PRECOND(2:2).eq.'C').or.(PRECOND(4:4).eq.'c')) ) then
!          PRECOND= 'BILU0'
!          FLAGprecond= 1
!        endif
!        if ( ((PRECOND(1:1).eq.'D').or.(PRECOND(1:1).eq.'d')) .and.     &
!     &       ((PRECOND(2:2).eq.'I').or.(PRECOND(2:2).eq.'i')) .and.     &
!     &       ((PRECOND(3:3).eq.'A').or.(PRECOND(3:3).eq.'a')) .and.     &
!     &       ((PRECOND(4:4).eq.'G').or.(PRECOND(4:4).eq.'g')) ) then
!          PRECOND= 'BILU0'
!          FLAGprecond= 1
!        endif
!        if ( ((PRECOND(1:1).eq.'B').or.(PRECOND(1:1).eq.'b')) .and.     &
!     &       ((PRECOND(2:2).eq.'L').or.(PRECOND(2:2).eq.'l')) .and.     &
!     &       ((PRECOND(3:3).eq.'O').or.(PRECOND(3:3).eq.'o')) .and.     &
!     &       ((PRECOND(4:4).eq.'C').or.(PRECOND(4:4).eq.'c')) .and.     &
!     &       ((PRECOND(5:5).eq.'K').or.(PRECOND(5:5).eq.'k')) ) then
!          PRECOND= 'BILU0'
!          FLAGprecond= 1
!        endif
!        if ( ((PRECOND(1:1).eq.'S').or.(PRECOND(1:1).eq.'s')) .and.     &
!     &       ((PRECOND(2:2).eq.'S').or.(PRECOND(2:2).eq.'s')) .and.     &
!     &       ((PRECOND(3:3).eq.'O').or.(PRECOND(3:3).eq.'o')) .and.     &
!     &       ((PRECOND(4:4).eq.'R').or.(PRECOND(4:4).eq.'r')) ) then
!          PRECOND= 'BILU0'
!          FLAGprecond= 1
!        endif
!        if ( ((PRECOND(1:1).eq.'B').or.(PRECOND(1:1).eq.'b')) .and.     &
!     &       ((PRECOND(2:2).eq.'S').or.(PRECOND(2:2).eq.'s')) .and.     &
!     &       ((PRECOND(3:3).eq.'S').or.(PRECOND(3:3).eq.'s')) .and.     &
!     &       ((PRECOND(4:4).eq.'O').or.(PRECOND(4:4).eq.'o')) .and.     &
!     &       ((PRECOND(5:5).eq.'R').or.(PRECOND(5:5).eq.'r')) ) then
!          PRECOND= 'BILU0'
!          FLAGprecond= 1
!        endif
!        if ( ((PRECOND(1:1).eq.'B').or.(PRECOND(1:1).eq.'b')) .and.     &
!     &       ((PRECOND(2:2).eq.'I').or.(PRECOND(2:2).eq.'i')) .and.     &
!     &       ((PRECOND(3:3).eq.'L').or.(PRECOND(3:3).eq.'l')) .and.     &
!     &       ((PRECOND(4:4).eq.'U').or.(PRECOND(4:4).eq.'u')) .and.     &
!     &       ((PRECOND(5:5).eq.'0'))) then
!          PRECOND= 'BILU0'
!          FLAGprecond= 1
!        endif
!        if ( ((PRECOND(1:1).eq.'B').or.(PRECOND(1:1).eq.'b')) .and.     &
!     &       ((PRECOND(2:2).eq.'I').or.(PRECOND(2:2).eq.'i')) .and.     &
!     &       ((PRECOND(3:3).eq.'C').or.(PRECOND(3:3).eq.'c')) .and.     &
!     &       ((PRECOND(4:4).eq.'0'))) then
!          PRECOND= 'BILU0'
!          FLAGprecond= 1
!        endif
!       
!        if ( ((PRECOND(1:1).eq.'B').or.(PRECOND(1:1).eq.'b')) .and.     &
!     &       ((PRECOND(2:2).eq.'I').or.(PRECOND(2:2).eq.'i')) .and.     &
!     &       ((PRECOND(3:3).eq.'L').or.(PRECOND(3:3).eq.'l')) .and.     &
!     &       ((PRECOND(4:4).eq.'U').or.(PRECOND(4:4).eq.'u')) .and.     &
!     &       ((PRECOND(5:5).eq.'1'))) then
!          PRECOND= 'BILU1'
!          FLAGprecond= 1
!        endif
!        if ( ((PRECOND(1:1).eq.'B').or.(PRECOND(1:1).eq.'b')) .and.     &
!     &       ((PRECOND(2:2).eq.'I').or.(PRECOND(2:2).eq.'i')) .and.     &
!     &       ((PRECOND(3:3).eq.'C').or.(PRECOND(3:3).eq.'c')) .and.     &
!     &       ((PRECOND(4:4).eq.'1'))) then
!          PRECOND= 'BILU1'
!          FLAGprecond= 1
!        endif
!       
!        if ( ((PRECOND(1:1).eq.'B').or.(PRECOND(1:1).eq.'b')) .and.     &
!     &       ((PRECOND(2:2).eq.'I').or.(PRECOND(2:2).eq.'i')) .and.     &
!     &       ((PRECOND(3:3).eq.'L').or.(PRECOND(3:3).eq.'l')) .and.     &
!     &       ((PRECOND(4:4).eq.'U').or.(PRECOND(4:4).eq.'u')) .and.     &
!     &       ((PRECOND(5:5).eq.'2'))) then
!          PRECOND= 'BILU2'
!          FLAGprecond= 1
!        endif
!        if ( ((PRECOND(1:1).eq.'B').or.(PRECOND(1:1).eq.'b')) .and.     &
!     &       ((PRECOND(2:2).eq.'I').or.(PRECOND(2:2).eq.'i')) .and.     &
!     &       ((PRECOND(3:3).eq.'C').or.(PRECOND(3:3).eq.'c')) .and.     &
!     &       ((PRECOND(4:4).eq.'2'))) then
!          PRECOND= 'BILU2'
!          FLAGprecond= 1
!        endif
!       
!      if (FLAGprecond.ne.1) goto 100
!
!      if (NEIBPETOT.eq.0) iterPREmax= 1
!      if (NEIBPETOT.ne.0) iterPREmax= 2
!      call BLCG_N                                                      &
!     &          (N, NP, NB, NPL, NPU, D, AL, INL, IAL, AU, INU, IAU,   &
!     &           B, X, PRECOND, SIGMA_DIAG, SIGMA, RESID, ITER,  ERROR,&
!     &           iterPREmax,                                           &
!     &         NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,            &
!     &                            STACK_EXPORT, NOD_EXPORT, PRESET)
!      endif
!      endif

  100 continue
      ITERactual= ITER
!C===

!C
!C +-------+
!C | ERROR |
!C +-------+
!C===
      if (ERROR.eq.0.and.FLAGmethod.eq.0) ERROR= 302
      if (ERROR.gt.0) then
        if (my_rank.eq.0) then
          write (*,'(//,"#### GeoFEM SOLVER abort CODE=", i8,/)') ERROR
        endif
        call MPI_FINALIZE(ierr)
        stop
      endif

      if (ERROR.eq.0.and.FLAGprecond.eq.0) ERROR= -301
      if (ERROR.lt.0) then
        if (my_rank.eq.0) then
          write (*,'(//,"#### GeoFEM SOLVER warn. CODE=", i8,/)') ERROR
        endif
      endif
!C===
      end subroutine solveNN
      end module solverNN
