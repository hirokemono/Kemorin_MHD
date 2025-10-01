!
!  module solver_GPBiCG
!
!
!C*** 
!C*** module solver_GPBiCG
!C***
!
!!      subroutine GPBiCG                                               &
!!     &                 (N, NP,  NPL, NPU,                             &
!!     &                  D,  AL, INL, IAL, AU, INU, IAU,               &
!!     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,             &
!!     &                  EPS,  ITER, ERROR, NEIBPETOT, NEIBPE,         &
!!     &                  STACK_IMPORT, NOD_IMPORT,                     &
!!     &                  STACK_EXPORT, NOD_EXPORT, NSET,               &
!!     &                  SR_sig, SR_r, PRECtime, COMPtime, COMMtime)
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!     GPBiCG solves the linear system Ax = b using the
!!     GPBiCG iterative method with preconditioning.
!!
!!     coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!!     Modified by H. Matsui on jul. 2005 (ver 1.0)
!

      module solver_GPBiCG
!
      use m_precision
      use t_solver_SR
!
      implicit none
!
      real   (kind=kreal), dimension(:), allocatable, private :: ALUG

      real(kind = kreal), allocatable :: W(:,:)
      private :: W
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!C
!C*** GPBiCG
!C
      subroutine GPBiCG(N, NP,  NPL, NPU,                               &
     &                  D,  AL, INL, IAL, AU, INU, IAU,                 &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  EPS,  ITER, ERROR, NEIBPETOT, NEIBPE,           &
     &                  STACK_IMPORT, NOD_IMPORT,                       &
     &                  STACK_EXPORT, NOD_EXPORT, NSET,                 &
     &                  SR_sig, SR_r, PRECtime, COMPtime, COMMtime)

      use calypso_mpi
!
      use m_constants
      use m_GPBiCG_constants
      use solver_SR
      use crs_matrix_calcs_11
      use crs_norm_products_11
      use incomplete_cholesky_crs_11
      use precond_crs_incomplete_lu
!

      real   (kind=kreal),                   intent(inout)::  EPS
      real   (kind=kreal),                   intent(in   )::  SIGMA_DIAG
      real   (kind=kreal),                   intent(in   )::  SIGMA
      integer(kind=kint ),                   intent(inout)::  ITER
      integer(kind=kint ),                   intent(inout)::  ERROR

      integer(kind=kint )                  , intent(in)   :: NSET

      integer(kind=kint ), intent(in   )::  NP, N, NPL, NPU
      real   (kind=kreal), dimension(NP )  , intent(inout)::  D
      real   (kind=kreal), dimension(NP )  , intent(inout)::  B
      real   (kind=kreal), dimension(NP )  , intent(inout)::  X

      real   (kind=kreal), dimension(NPU)  , intent(inout)::  AU
      real   (kind=kreal), dimension(NPU)  , intent(inout)::  AL

      integer(kind=kint ), dimension(0:NP)   , intent(in) ::  INU
      integer(kind=kint ), dimension(0:NP)   , intent(in) ::  INL
      integer(kind=kint ), dimension(  NPU)  , intent(in) ::  IAU
      integer(kind=kint ), dimension(  NPL)  , intent(in) ::  IAL
      character(len=kchara)                  , intent(in) :: PRECOND

      integer(kind=kint ), intent(in) :: NEIBPETOT
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
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!>      Elapsed time for solver preconditioning
      real(kind = kreal), intent(inout) :: PRECtime
!>      Elapsed time for solver iteration
      real(kind = kreal), intent(inout) :: COMPtime
!>      Elapsed time for communication
      real(kind = kreal), intent(inout) :: COMMtime
!
      real(kind = kreal) :: START_TIME, S1_TIME, S2_TIME
!
      integer(kind = kint) :: iterPRE
!
      real(kind=kreal) :: RESID, TOL
      real(kind=kreal) :: ALPHA, BETA, QSI, ETA
      real(kind=kreal) :: BNRM2(1),  DNRM2(1),  RHO(1),  RHO1(1)
      real(kind=kreal) :: BNRM20(1), DNRM20(1), RHO0(1), RHO10(1)
      real(kind=kreal) :: COEF1(1)
      real(kind=kreal) :: COEF10(1)
      real(kind=kreal) :: C0(5), CG(5)
      real(kind=kreal) :: EQ(2)
!
      integer(kind=kint ) :: IFLAG, MONITORFLAG
      integer(kind=kint ) :: MAXIT
      data IFLAG/0/

      open (12,file='test.lis',status='unknown')

!C
!C-- INIT.

      MAXIT = ITER
      TOL   = EPS
      S1_TIME= MPI_WTIME()

      allocate(W(NP,nWK_GPBiCG))
      W = 0.0d0

      MONITORFLAG = ERROR
      ERROR= 0
      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 101
        return
      endif

      if (IFLAG.eq.0) then
        allocate (ALUG   (NP))
        IFLAG= 1
      endif

      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, D)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!C
!C +-------------------+
!C | ILU decomposition |
!C +-------------------+
!C===
      if (NSET.ge.1) then
        S2_TIME= MPI_WTIME()
        if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
          call precond_crs_ilu(N, NP, NPL, NPU, D, AL, INL,IAL,         &
     &        INU, AU, ALUG, SIGMA, SIGMA_DIAG)
        else if (PRECOND(1:4).eq.'SSOR' .or. PRECOND(1:4).eq.'DIAG') then
          call precond_crs_ssor(N, NP, D, ALUG, SIGMA_DIAG)
        end if
        PRECtime = MPI_WTIME() - S2_TIME
      end if
!C===

!C
!C +----------------------+
!C | {r}= {b} - [A]{xini} |
!C +----------------------+
!C===
      call subtruct_crs_matvec_11 (NP, N, NPL, NPU, INL, INU,           &
     &    IAL, IAU, D, AL, AU, W(1,R), B, X)
!
      W(1:N,RT)= W(1:N,R)
      W(1:N, T)= 0.d0
      W(1:N,T0)= 0.d0
      W(1:N,W1)= 0.d0
!
      call cal_local_sproduct_and_norm_1(NP, N,                         &
     &           B, W(1,R), W(1,RT), RHO0(1), BNRM20(1))

      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE (RHO0  , RHO,   1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

      if (BNRM2(1) .eq. 0.d0) BNRM2(1) = 1.d0
!C===

!C
!C*************************************************************** ITERATIVE PROC.
!C
      do iter= 1, MAXIT
!C
!C-- INIT.
        W(1:N,WK)=  W(1:N,R)
!C
!C +----------------+
!C | {r}= [Minv]{r} |
!C +----------------+
!C===

!C
!C-- INTERFACE data EXCHANGE
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,R) )
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!C
!C-- incomplete CHOLESKY
      
        if (PRECOND(1:2).eq.'IC'  .or.                                  &
     &      PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
          call i_cholesky_crs_11(NP, N, NPL, NPU,                       &
     &        INL, INU, IAL, IAU, ALUG, AL, AU, W(1,R) )
!C
!C-- diagonal
        else if (PRECOND(1:4).eq.'DIAG') then
          W(1:N,R)=  W(1:N,R) * ALUG(1:N)
        end if
!C===

!C
!C +----------------------------------+
!C | {p} = {r} + BETA * ( {p} - {u} ) |
!C +----------------------------------+
!C===
        if (iter.gt.1) then
          W(1:N,P)= W(1:N,R) + BETA*( W(1:N,P)-W(1:N,U))
        else
          W(1:N,P)= W(1:N,R)
        end if
!C===

!C
!C +--------------------------------+
!C | ALPHA= {r_tld}{r}/{r_tld} A{p} |
!C +--------------------------------+
!C===

!C
!C-- calc. {p_tld}= A{p}
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,P) )
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

        call cal_crs_matvec_11(NP, N, NPL, NPU, INL, INU, IAL, IAU,     &
     &      D, AL, AU, W(1,PT), W(1,P) )
!C
!C-- calc. ALPHA

        call cal_local_s_product_1(NP, N, W(1,RT), W(1,PT), RHO10(1))
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (RHO10, RHO1, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
        ALPHA= RHO(1) / RHO1(1)
!C===

!C
!C +------------------------------------------+
!C | {y}= {t} - {r} - ALPHA{w} + ALPHA{p_tld} |
!C | {t}= {r}                  - ALPHA{p_tld} |
!C +------------------------------------------+
!C===
        W(1:N,Y) = W(1:N, T) - W(1:N,WK)                                &
     &             + ALPHA * (-W(1:N,W1)+W(1:N,PT))
        W(1:N,T) = W(1:N,WK)                                            &
     &             - ALPHA * W(1:N,PT)
!C===

!C
!C +-----------------------+
!C | {t_tld}= [A][Minv]{t} |
!C +-----------------------+
!C===

!C
!C-- calc. {t_tld} and {t0} by [M] inversion
!C         {WZ}   = [Minv]{p_tld} 
!C
        call SOLVER_SEND_RECVx3(NP, NEIBPETOT, NEIBPE,                  &
     &      STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,         &
     &      SR_sig, SR_r, W(1,PT), W(1,T), W(1,T0))

        W(1:NP,TT) = W(1:NP,T)
        W(1:NP,WZ) = W(1:NP,PT)
!C
!C-- incomplete CHOLESKY

        if (PRECOND(1:2).eq.'IC'  .or.                                  &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
          call i_cholesky_crs_3x11(NP, N, NPL, NPU, INL, INU, IAL, IAU, &
     &      ALUG, AL, AU, W(1,TT), W(1,WZ), W(1,T0) )
!C
!C-- diagonal
        else if (PRECOND(1:4).eq.'DIAG') then
          W(1:N,TT)=  W(1:N,TT) * ALUG(1:N)
          W(1:N,WZ)=  W(1:N,WZ) * ALUG(1:N)
          W(1:N,T0)=  W(1:N,T0) * ALUG(1:N)
        end if
!C===

!C
!C-- calc. [A]{t_tld}
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV                                           &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, W(1,TT) )
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

        call cal_crs_matvec_11(NP, N, NPL, NPU, INL, INU, IAL, IAU,     &
     &      D, AL, AU, W(1,WK), W(1,TT) )
!C===

!C
!C +-------------------+
!C | calc. QSI and ETA |
!C +-------------------+
!C===
!
        call cal_5_products_norm_1(NP, N,                               &
     &          W(1,Y), W(1,T), W(1,TT), CG, C0)

        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (C0, CG,  5, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)

        if (iter.eq.1) then
          EQ(1)= CG(2)/CG(5)
          EQ(2)= 0.d0
         else
          EQ(1)= (CG(1)*CG(2)-CG(3)*CG(4)) / (CG(5)*CG(1)-CG(4)*CG(4))
          EQ(2)= (CG(5)*CG(3)-CG(4)*CG(2)) / (CG(5)*CG(1)-CG(4)*CG(4))
        endif

        QSI= EQ(1)
        ETA= EQ(2)
!C===
!C
!C +----------------------------------------------------------+
!C | {u} = QSI [Minv]{pt} + ETA([Minv]{t0}-[Minv]{r}+BETA*{u} |
!C | {z} = QSI [Minv]{r}  + ETA*{z} - ALPHA*{u}               |
!C +----------------------------------------------------------+
!C===
        W(1:N,U) = QSI * W(1:N,WZ)                                      &
     &             + ETA * (W(1:N,T0) - W(1:N,R) + BETA*W(1:N,U))
        W(1:N,Z) = QSI * W(1:N, R) + ETA*W(1:N,Z) - ALPHA*W(1:N,U)
!C===

!C
!C +--------------------+
!C | update {x},{r},{w} |
!C +--------------------+
!C===

        X (1:N)   =  X(1:N)   + ALPHA*W(1:N,P) +     W(1:N,Z)
        W(1:N,R) = W(1:N,T) - ETA  *W(1:N,Y) - QSI*W(1:N,TT)
        W(1:N,T0)= W(1:N,T)
!
        call cal_local_sproduct_norm_1(NP, N, W(1,R), W(1,RT),          &
     &      COEF10(1), DNRM20(1))

        START_TIME= MPI_WTIME()
        call MPI_allREDUCE  (DNRM20, DNRM2, 1, CALYPSO_REAL,            &
     &                     MPI_SUM, CALYPSO_COMM, ierr_MPI)
        call MPI_allREDUCE  (COEF10, COEF1, 1, CALYPSO_REAL,            &
     &                     MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
!
!C +---------------------------------+
!C | BETA = ALPHA*COEF1 / (QSI*RHO)  |
!C | {w} = {tt} + BETA*{pt}          |
!C +---------------------------------+
!
        BETA = ALPHA * COEF1(1) / (QSI * RHO(1))
        W(1:N,W1)= W(1:N,TT) + BETA*W(1:N,PT)
!
        RESID= dsqrt(DNRM2(1) / BNRM2(1))
        RHO(1)  = COEF1(1)
!
        if (my_rank.eq.0 .and. MONITORFLAG.eq.1)                        &
     &    write (*, 1000) ITER, RESID
 1000   format ('GPBICG_11: ', i5, 1pe16.6)

      if (RESID.le.TOL   ) exit
      if ( ITER.eq.MAXIT ) ERROR= -100
!C===
      enddo

      deallocate(W)

!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV                                             &
     &   ( NP, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,             &
     &     STACK_EXPORT, NOD_EXPORT, SR_sig, SR_r, X)
      COMMtime = COMMtime + (MPI_WTIME() - START_TIME)
      COMPtime = MPI_WTIME() - S1_TIME

      deallocate (W)

      end subroutine GPBiCG
!
!-----------------------------------------------------------------------
!
      end module     solver_GPBiCG
