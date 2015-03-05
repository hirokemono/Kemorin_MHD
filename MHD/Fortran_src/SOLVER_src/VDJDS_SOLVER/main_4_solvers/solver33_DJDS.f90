!
!  module solver33_DJDS.f90
!
!C*** 
!C*** module solver33_DJDS
!C***
!
!      subroutine  init_solver33_DJDS                                   &
!     &         (NP, PEsmpTOT, METHOD, PRECOND, ERROR)
!
!      subroutine  solve33_DJDS_kemo                                    &
!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                    &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT,                             &
!     &           METHOD, PRECOND, ITERactual)
!
!      subroutine  init_solve33_DJDS_kemo                               &
!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                    &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT,                             &
!     &           METHOD, PRECOND, ITERactual)
!
!      solver subsystem entry for 3*3 Block Matrix with DJDS ordering
!      Kenorin's special
!
      module solver33_DJDS
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter, private :: iterPREmax = 1
      integer(kind = kint), parameter, private :: NB = 3
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!C
!C--- init_solver33
      subroutine  init_solver33_DJDS                                    &
     &         (NP, PEsmpTOT, METHOD, PRECOND, ERROR)
!
      use calypso_mpi
!
      use m_flags_4_solvers
      use solver_VBiCGSTAB33_DJDS_SMP
      use solver_VBiCGSTABnn_DJDS_SMP
      use solver_VGPBiCG33_DJDS_SMP
      use solver_VGPBiCGnn_DJDS_SMP
      use solver_VCG33_DJDS_SMP
      use solver_VCG33_DJDS_SMP_d
      use solver_VCGnn_DJDS_SMP
      use solver_GAUS_ZIDL33_DJDS
      use solver_JACOBI33_DJDS
!
      integer(kind=kint ), intent(in) :: NP, PEsmpTOT
      character(len=kchara) , intent(in):: METHOD
      character(len=kchara) , intent(in):: PRECOND
      integer(kind=kint), intent(inout) :: ERROR
!
      integer :: ierror
!
!
      ERROR = 0
!C
!C-- BiCGSTAB using n*n solver
      if(solver_iflag(METHOD) .eq. iflag_bicgstab_NN) then
        call init_VBiCGSTABnn_DJDS_SMP                                  &
     &     (NP, NB, PEsmpTOT, PRECOND, iterPREmax)
!C
!C-- BiCGSTAB
      else if(solver_iflag(METHOD) .eq. iflag_bicgstab) then
        call init_VBiCGSTAB33_DJDS_SMP                                  &
     &     (NP, PEsmpTOT, PRECOND, iterPREmax)
!C
!C-- GPBiCG using n*n solver
      else if(solver_iflag(METHOD) .eq. iflag_gpbicg_NN) then
        call init_VGPBiCGnn_DJDS_SMP                                    &
     &     (NP, NB, PEsmpTOT, PRECOND, iterPREmax)
!
!C
!C-- GPBiCG
      else if(solver_iflag(METHOD) .eq. iflag_gpbicg) then
        call init_VGPBiCG33_DJDS_SMP(NP, PEsmpTOT, PRECOND, iterPREmax)
!
!C
!C-- CG_only diagonal component
      else if ( ((METHOD(1:1).eq.'C').or.(METHOD(1:1).eq.'c')) .and.    &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.         &
     &      (METHOD(3:3).eq.'_')                          .and.         &
     &     ((METHOD(4:4).eq.'D').or.(METHOD(4:4).eq.'d')) .and.         &
     &     ((METHOD(5:5).eq.'I').or.(METHOD(5:5).eq.'i')) .and.         &
     &     ((METHOD(6:6).eq.'A').or.(METHOD(6:6).eq.'a')) .and.         &
     &     ((METHOD(7:7).eq.'G').or.(METHOD(7:7).eq.'g')) ) then
        call init_VCG33_DJDS_SMP_d(NP, PEsmpTOT, PRECOND, iterPREmax)
!C
!C-- CG
      else if(solver_iflag(METHOD) .eq. iflag_cg_NN) then
        call init_VCGnn_DJDS_SMP(NP, NB, PEsmpTOT, PRECOND, iterPREmax)
!
!C
!C-- CG
      else if(solver_iflag(METHOD) .eq. iflag_cg) then
        call init_VCG33_DJDS_SMP(NP, PEsmpTOT, PRECOND, iterPREmax)
!
!C-- Gauss-Zeidel
      else if(solver_iflag(METHOD) .eq. iflag_gausszeidel) then
        call init_VGAUSS_ZEIDEL33_DJDS_SMP(NP, PEsmpTOT)
!C
!C-- Jacobi
      else if(solver_iflag(METHOD) .eq. iflag_jacobi) then
        call init_VJACOBI33_DJDS_SMP(NP, PEsmpTOT)
      else
        ERROR = 1
      end if
!
!C-- ERROR
      if (ERROR.gt.0) then
        ierror = int(ERROR)
        if (my_rank.eq.0) then
          write (*,'(//,"#### GeoFEM SOLVER abort CODE=", i8,/)') ERROR
        end if
        call MPI_FINALIZE(ierror)
        stop
      end if

      end subroutine init_solver33_DJDS
!
!  ---------------------------------------------------------------------
!C
!C--- solve
      subroutine  solve33_DJDS_kemo                                     &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                     &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT,                              &
     &           METHOD, PRECOND, ITERactual)
!
      use calypso_mpi
!
      use solver_VBiCGSTAB33_DJDS_SMP
      use solver_VBiCGSTABnn_DJDS_SMP
      use solver_VGPBiCG33_DJDS_SMP
      use solver_VGPBiCGnn_DJDS_SMP
      use solver_VCG33_DJDS_SMP
      use solver_VCG33_DJDS_SMP_d
      use solver_VCGnn_DJDS_SMP
      use solver_GAUS_ZIDL33_DJDS
      use solver_JACOBI33_DJDS
!
! ......................................................................
      integer(kind=kint )                  , intent(in)   ::  N
! \beginARG       number of internal nodes (exclude external node)
      integer(kind=kint )                  , intent(in)   ::  NP
! \beginARG       number of nodes          (include external node)
      integer(kind=kint )                  , intent(in)   ::  NL, NU
! \beginARG
      integer(kind=kint )                  , intent(in)   ::  NPL, NPU
! \beginARG       array length of IAL, AL, IAU, and AU
      integer(kind=kint )                  , intent(in)   ::  PEsmpTOT
! \beginARG    Number of processor in each node
      integer(kind=kint )                  , intent(in)   ::  NVECT
! \beginARG       Number of color

      real   (kind=kreal), dimension(9*NP), intent(in)::  D
! \beginARG       diagonal matrix value            (i-th dof)

      real   (kind=kreal), dimension(9*NPL),intent(in)::  AL
! \beginARG       upper triangular matrix value    (i-th term)
      integer(kind=kint),dimension(0:NL*NVECT*PEsmpTOT),intent(in)::INL
! \beginARG       last term count at each freedom  (i-th node)
      integer(kind=kint ), dimension(NPL)  , intent(in)   ::  IAL
! \beginARG       node number at each term         (i-th term)

      real   (kind=kreal), dimension(9*NPU),intent(in)::  AU
! \beginARG       lower triangular matrix value    (i-th term)
      integer(kind=kint),dimension(0:NU*NVECT*PEsmpTOT),intent(in)::INU
! \beginARG       last term count at each freedom  (i-th node)
      integer(kind=kint), dimension(NPU)  , intent(in)   ::  IAU
! \beginARG       node number at each term         (i-th term)

      real   (kind=kreal), dimension(3*NP) , intent(inout)::  B
! \beginARG       right hand load vector
      real   (kind=kreal), dimension(3*NP) , intent(inout)::  X
! \beginARG       solution vector

      real   (kind=kreal), dimension(9*N  ) :: ALU_L, ALU_U
! \beginARG      coefficirnts for preconditioning

      integer(kind=kint), dimension(0:NVECT):: IVECT
! \beginARG      ordering table
      integer(kind=kint), dimension(NVECT)  :: NLhyp, NUhyp 
! \beginARG      ordering table
      integer(kind=kint), dimension(0:PEsmpTOT*NVECT) :: STACKmc
! \beginARG      ordering table
      integer(kind=kint), dimension(0:PEsmpTOT      ) :: STACKmcG
! \beginARG      ordering table
      integer(kind=kint )                  , intent(in)   ::  ITER
! \beginARG      ordering table
      integer(kind=kint), dimension(NP)     :: NtoO
! \beginARG      ordering table
      integer(kind=kint), dimension(NP)     :: OtoN_L, NtoO_U, LtoU
! \beginARG      ordering table
      integer(kind=kint), dimension(NP)     :: OtoN_U
! \beginARG
!
      integer(kind=kint )                  , intent(in)   ::  NEIBPETOT
! \beginARG       total neighboring pe count
      integer(kind=kint ), dimension(NEIBPETOT) :: NEIBPE 
! \beginARG       neighboring pe id                        (i-th pe)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_IMPORT
! \beginARG       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(STACK_IMPORT(NEIBPETOT))           &
     &        :: NOD_IMPORT
! \beginARG       imported degree of freedom               (i-th node)
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_EXPORT
! \beginARG       exported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), dimension(STACK_EXPORT(NEIBPETOT))           &
     &        :: NOD_EXPORT
! \beginARG       exported node                            (i-th node)
!
      real   (kind=kreal)                  , intent(in)   :: EPS
! \beginARG
      integer(kind=kint )                  , intent(out)  :: ITERactual
! \beginARG       actual iteration number
      character(len=kchara)                , intent(in):: METHOD
! \beginARG       solver method name
      character(len=kchara)                , intent(in):: PRECOND
! \beginARG       precondition method name
      integer(kind=kint )                  , intent(inout)   ::  IER
! \beginARG
!
      integer(kind=kint ) :: ITR
      integer :: ierror
!
!
      ITR = ITER
!C
!C-- BiCGSTAB using n*n solver
      if      ( ((METHOD(1:1).eq.'B').or.(METHOD(1:1).eq.'b')) .and.    &
     &          ((METHOD(2:2).eq.'I').or.(METHOD(2:2).eq.'i')) .and.    &
     &          ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.    &
     &          ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) .and.    &
     &          ((METHOD(5:5).eq.'S').or.(METHOD(5:5).eq.'s')) .and.    &
     &          ((METHOD(6:6).eq.'T').or.(METHOD(6:6).eq.'t')) .and.    &
     &          ((METHOD(7:7).eq.'A').or.(METHOD(7:7).eq.'a')) .and.    &
     &          ((METHOD(8:8).eq.'B').or.(METHOD(8:8).eq.'b')) .and.    &
     &           (METHOD(9:9).eq.'_') .and.                             &
     &        ((METHOD(10:10).eq.'N').or.(METHOD(10:10).eq.'n')) .and.  &
     &        ((METHOD(11:11).eq.'N').or.(METHOD(11:11).eq.'n')) ) then
!
       call solve_VBiCGSTABnn_DJDS_SMP                                  &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C
!C-- BiCGSTAB
      else if ( ((METHOD(1:1).eq.'B').or.(METHOD(1:1).eq.'b')) .and.    &
     &          ((METHOD(2:2).eq.'I').or.(METHOD(2:2).eq.'i')) .and.    &
     &          ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.    &
     &          ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) .and.    &
     &          ((METHOD(5:5).eq.'S').or.(METHOD(5:5).eq.'s')) ) then
!
       call solve_VBiCGSTAB33_DJDS_SMP                                  &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C
!C-- GPBiCG using n*n solver
      else if ( ((METHOD(1:1).eq.'G').or.(METHOD(1:1).eq.'g')) .and.    &
     &          ((METHOD(2:2).eq.'P').or.(METHOD(2:2).eq.'p')) .and.    &
     &          ((METHOD(3:3).eq.'B').or.(METHOD(3:3).eq.'b')) .and.    &
     &          ((METHOD(4:4).eq.'I').or.(METHOD(4:4).eq.'i')) .and.    &
     &          ((METHOD(5:5).eq.'C').or.(METHOD(5:5).eq.'c')) .and.    &
     &          ((METHOD(6:6).eq.'G').or.(METHOD(6:6).eq.'g')) .and.    &
     &           (METHOD(7:7).eq.'_') .and.                             &
     &          ((METHOD(8:8).eq.'N').or.(METHOD(8:8).eq.'n')) .and.    &
     &          ((METHOD(9:9).eq.'N').or.(METHOD(9:9).eq.'n')) ) then
!
       call solve_VGPBiCGnn_DJDS_SMP                                    &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C
!C-- GPBiCG
      else if ( ((METHOD(1:1).eq.'G').or.(METHOD(1:1).eq.'g')) .and.    &
     &          ((METHOD(2:2).eq.'P').or.(METHOD(2:2).eq.'p')) .and.    &
     &          ((METHOD(3:3).eq.'B').or.(METHOD(3:3).eq.'b')) .and.    &
     &          ((METHOD(4:4).eq.'I').or.(METHOD(4:4).eq.'i')) .and.    &
     &          ((METHOD(5:5).eq.'C').or.(METHOD(5:5).eq.'c')) .and.    &
     &          ((METHOD(6:6).eq.'G').or.(METHOD(6:6).eq.'g')) ) then
!
       call solve_VGPBiCG33_DJDS_SMP                                    &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C
!C-- CG_only diagonal component
      else if ( ((METHOD(1:1).eq.'C').or.(METHOD(1:1).eq.'c')) .and.    &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.         &
     &      (METHOD(3:3).eq.'_')                          .and.         &
     &     ((METHOD(4:4).eq.'D').or.(METHOD(4:4).eq.'d')) .and.         &
     &     ((METHOD(5:5).eq.'I').or.(METHOD(5:5).eq.'i')) .and.         &
     &     ((METHOD(6:6).eq.'A').or.(METHOD(6:6).eq.'a')) .and.         &
     &     ((METHOD(7:7).eq.'G').or.(METHOD(7:7).eq.'g')) ) then
!
        call solve_VCG33_DJDS_SMP_d                                     &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!C
!C-- CG
      else if ( ((METHOD(1:1).eq.'C').or.(METHOD(1:1).eq.'c')) .and.    &
     &          ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.    &
     &           (METHOD(3:3).eq.'_') .and.                             &
     &          ((METHOD(4:4).eq.'N').or.(METHOD(4:4).eq.'n')) .and.    &
     &          ((METHOD(5:5).eq.'N').or.(METHOD(5:5).eq.'n')) ) then
!
        call solve_VCGnn_DJDS_SMP                                       &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C
!C-- CG
      else if ( ((METHOD(1:1).eq.'C').or.(METHOD(1:1).eq.'c')) .and.    &
     &          ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) ) then
!
        call solve_VCG33_DJDS_SMP                                       &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C-- Gauss-Zeidel

      else if ( ((METHOD(1:1).eq.'G').or.(METHOD(1:1).eq.'g')) .and.    &
     &          ((METHOD(2:2).eq.'A').or.(METHOD(2:2).eq.'a')) .and.    &
     &          ((METHOD(3:3).eq.'U').or.(METHOD(3:3).eq.'u')) .and.    &
     &          ((METHOD(4:4).eq.'S').or.(METHOD(4:4).eq.'s')) .and.    &
     &          ((METHOD(5:5).eq.'S').or.(METHOD(5:5).eq.'s')) ) then
!
        call solve_VGAUSS_ZEIDEL33_DJDS_SMP                             &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!C
!C-- Jacobi

      else if ( ((METHOD(1:1).eq.'J').or.(METHOD(1:1).eq.'j')) .and.    &
     &          ((METHOD(2:2).eq.'A').or.(METHOD(2:2).eq.'a')) .and.    &
     &          ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.    &
     &          ((METHOD(4:4).eq.'O').or.(METHOD(4:4).eq.'o')) .and.    &
     &          ((METHOD(5:5).eq.'B').or.(METHOD(5:5).eq.'b')) .and.    &
     &          ((METHOD(6:6).eq.'I').or.(METHOD(6:6).eq.'i')) ) then
!
        call solve_VJACOBI33_DJDS_SMP                                   &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!
      end if
!
!
      ITERactual= ITR
!C
!C-- ERROR
      if (IER.gt.0) then
        ierror = int(IER)
        if (my_rank.eq.0) then
          write (*,'(//,"#### GeoFEM SOLVER abort CODE=", i8,/)') IER
        endif
        call MPI_FINALIZE(ierror)
        stop
      end if

      end subroutine solve33_DJDS_kemo
!
!  ---------------------------------------------------------------------
!C
!C--- init and solve
      subroutine  init_solve33_DJDS_kemo                                &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                     &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT,                              &
     &           METHOD, PRECOND, ITERactual)

!      solver subsystem entry for 3*3 Block Matrix with DJDS ordering
!      Kenorin's special
!
!     coded by K.Nakajima (RIST) on DEC. 1999 (ver 3.0)
!     Modified by Kemorin (U. of Chicago) on Sep. 2002 (ver X.0)

      use calypso_mpi
!
      use solver_VBiCGSTAB33_DJDS_SMP
      use solver_VBiCGSTABnn_DJDS_SMP
      use solver_VGPBiCG33_DJDS_SMP
      use solver_VGPBiCGnn_DJDS_SMP
      use solver_VCG33_DJDS_SMP
      use solver_VCG33_DJDS_SMP_d
      use solver_VCGnn_DJDS_SMP
      use solver_GAUS_ZIDL33_DJDS
      use solver_JACOBI33_DJDS

      integer(kind=kint )                  , intent(in)   ::  N
      integer(kind=kint )                  , intent(in)   ::  NP
      integer(kind=kint )                  , intent(in)   ::  NL, NU
      integer(kind=kint )                  , intent(in)   ::  NPL, NPU
      integer(kind=kint )                  , intent(in)   ::  PEsmpTOT
      integer(kind=kint )                  , intent(in)   ::  NVECT

      real   (kind=kreal), dimension(9*NP), intent(in)::  D
      real   (kind=kreal), dimension(9*NPL),intent(in)::  AL
      integer(kind=kint),dimension(0:NL*NVECT*PEsmpTOT),intent(in)::INL
      integer(kind=kint ), dimension(NPL)  , intent(in)   ::  IAL

      real   (kind=kreal), dimension(9*NPU),intent(in)::  AU
      integer(kind=kint),dimension(0:NU*NVECT*PEsmpTOT),intent(in)::INU
      integer(kind=kint), dimension(NPU)  , intent(in)   ::  IAU

      real   (kind=kreal), dimension(3*NP) , intent(inout)::  B
      real   (kind=kreal), dimension(3*NP) , intent(inout)::  X

      real   (kind=kreal), dimension(9*N  ) :: ALU_L, ALU_U

      integer(kind=kint), dimension(0:NVECT):: IVECT
      integer(kind=kint), dimension(NVECT)  :: NLhyp, NUhyp 
      integer(kind=kint), dimension(0:PEsmpTOT*NVECT) :: STACKmc
      integer(kind=kint), dimension(0:PEsmpTOT      ) :: STACKmcG
      integer(kind=kint )                  , intent(in)   ::  ITER
      integer(kind=kint), dimension(NP)     :: NtoO
      integer(kind=kint), dimension(NP)     :: OtoN_L, NtoO_U, LtoU
      integer(kind=kint), dimension(NP)     :: OtoN_U
!
      integer(kind=kint )                  , intent(in)   ::  NEIBPETOT
      integer(kind=kint ), dimension(NEIBPETOT) :: NEIBPE 
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_IMPORT
      integer(kind=kint ), dimension(STACK_IMPORT(NEIBPETOT))           &
     &        :: NOD_IMPORT
      integer(kind=kint ), dimension(0:NEIBPETOT) :: STACK_EXPORT
      integer(kind=kint ), dimension(STACK_EXPORT(NEIBPETOT))           &
     &        :: NOD_EXPORT
!
      real   (kind=kreal)                  , intent(in)   :: EPS
      integer(kind=kint )                  , intent(out)  :: ITERactual
      character(len=kchara)                , intent(in):: METHOD
      character(len=kchara)                , intent(in):: PRECOND
      integer(kind=kint )                  , intent(inout)   ::  IER
!
      integer(kind=kint ) :: ITR
      integer :: ierror
!
!
      ITR = ITER
!C
!C-- BiCGSTAB using n*n solver
      if      ( ((METHOD(1:1).eq.'B').or.(METHOD(1:1).eq.'b')) .and.    &
     &          ((METHOD(2:2).eq.'I').or.(METHOD(2:2).eq.'i')) .and.    &
     &          ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.    &
     &          ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) .and.    &
     &          ((METHOD(5:5).eq.'S').or.(METHOD(5:5).eq.'s')) .and.    &
     &          ((METHOD(6:6).eq.'T').or.(METHOD(6:6).eq.'t')) .and.    &
     &          ((METHOD(7:7).eq.'A').or.(METHOD(7:7).eq.'a')) .and.    &
     &          ((METHOD(8:8).eq.'B').or.(METHOD(8:8).eq.'b')) .and.    &
     &           (METHOD(9:9).eq.'_') .and.                             &
     &        ((METHOD(10:10).eq.'N').or.(METHOD(10:10).eq.'n')) .and.  &
     &        ((METHOD(11:11).eq.'N').or.(METHOD(11:11).eq.'n')) ) then
!
       call VBiCGSTABnn_DJDS_SMP                                        &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C
!C-- BiCGSTAB
      else if ( ((METHOD(1:1).eq.'B').or.(METHOD(1:1).eq.'b')) .and.    &
     &          ((METHOD(2:2).eq.'I').or.(METHOD(2:2).eq.'i')) .and.    &
     &          ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.    &
     &          ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) .and.    &
     &          ((METHOD(5:5).eq.'S').or.(METHOD(5:5).eq.'s')) ) then
!
       call VBiCGSTAB33_DJDS_SMP                                        &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C
!C-- GPBiCG using n*n solver
      else if ( ((METHOD(1:1).eq.'G').or.(METHOD(1:1).eq.'g')) .and.    &
     &          ((METHOD(2:2).eq.'P').or.(METHOD(2:2).eq.'p')) .and.    &
     &          ((METHOD(3:3).eq.'B').or.(METHOD(3:3).eq.'b')) .and.    &
     &          ((METHOD(4:4).eq.'I').or.(METHOD(4:4).eq.'i')) .and.    &
     &          ((METHOD(5:5).eq.'C').or.(METHOD(5:5).eq.'c')) .and.    &
     &          ((METHOD(6:6).eq.'G').or.(METHOD(6:6).eq.'g')) .and.    &
     &           (METHOD(7:7).eq.'_') .and.                             &
     &          ((METHOD(8:8).eq.'N').or.(METHOD(8:8).eq.'n')) .and.    &
     &          ((METHOD(9:9).eq.'N').or.(METHOD(9:9).eq.'n')) ) then
!
       call VGPBiCGnn_DJDS_SMP                                          &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C
!C-- GPBiCG
      else if ( ((METHOD(1:1).eq.'G').or.(METHOD(1:1).eq.'g')) .and.    &
     &          ((METHOD(2:2).eq.'P').or.(METHOD(2:2).eq.'p')) .and.    &
     &          ((METHOD(3:3).eq.'B').or.(METHOD(3:3).eq.'b')) .and.    &
     &          ((METHOD(4:4).eq.'I').or.(METHOD(4:4).eq.'i')) .and.    &
     &          ((METHOD(5:5).eq.'C').or.(METHOD(5:5).eq.'c')) .and.    &
     &          ((METHOD(6:6).eq.'G').or.(METHOD(6:6).eq.'g')) ) then
!
       call VGPBiCG33_DJDS_SMP                                          &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C
!C-- CG_only diagonal component
      else if ( ((METHOD(1:1).eq.'C').or.(METHOD(1:1).eq.'c')) .and.    &
     &     ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.         &
     &      (METHOD(3:3).eq.'_')                          .and.         &
     &     ((METHOD(4:4).eq.'D').or.(METHOD(4:4).eq.'d')) .and.         &
     &     ((METHOD(5:5).eq.'I').or.(METHOD(5:5).eq.'i')) .and.         &
     &     ((METHOD(6:6).eq.'A').or.(METHOD(6:6).eq.'a')) .and.         &
     &     ((METHOD(7:7).eq.'G').or.(METHOD(7:7).eq.'g')) ) then
!
        call VCG33_DJDS_SMP_d                                           &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!C
!C-- CG
      else if ( ((METHOD(1:1).eq.'C').or.(METHOD(1:1).eq.'c')) .and.    &
     &          ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) .and.    &
     &           (METHOD(3:3).eq.'_') .and.                             &
     &          ((METHOD(4:4).eq.'N').or.(METHOD(4:4).eq.'n')) .and.    &
     &          ((METHOD(5:5).eq.'N').or.(METHOD(5:5).eq.'n')) ) then
!
        call VCGnn_DJDS_SMP                                             &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C
!C-- CG
      else if ( ((METHOD(1:1).eq.'C').or.(METHOD(1:1).eq.'c')) .and.    &
     &          ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) ) then
!
        call VCG33_DJDS_SMP                                             &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!C-- Gauss-Zeidel

      else if ( ((METHOD(1:1).eq.'G').or.(METHOD(1:1).eq.'g')) .and.    &
     &          ((METHOD(2:2).eq.'A').or.(METHOD(2:2).eq.'a')) .and.    &
     &          ((METHOD(3:3).eq.'U').or.(METHOD(3:3).eq.'u')) .and.    &
     &          ((METHOD(4:4).eq.'S').or.(METHOD(4:4).eq.'s')) .and.    &
     &          ((METHOD(5:5).eq.'S').or.(METHOD(5:5).eq.'s')) ) then
!
        call VGAUSS_ZEIDEL33_DJDS_SMP                                   &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!C
!C-- Jacobi

      else if ( ((METHOD(1:1).eq.'J').or.(METHOD(1:1).eq.'j')) .and.    &
     &          ((METHOD(2:2).eq.'A').or.(METHOD(2:2).eq.'a')) .and.    &
     &          ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.    &
     &          ((METHOD(4:4).eq.'O').or.(METHOD(4:4).eq.'o')) .and.    &
     &          ((METHOD(5:5).eq.'B').or.(METHOD(5:5).eq.'b')) .and.    &
     &          ((METHOD(6:6).eq.'I').or.(METHOD(6:6).eq.'i')) ) then
!
        call VJACOBI33_DJDS_SMP                                         &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!
      end if
!
!
      ITERactual= ITR
!C
!C-- ERROR
      if (IER.gt.0) then
        ierror = int(IER)
        if (my_rank.eq.0) then
          write (*,'(//,"#### GeoFEM SOLVER abort CODE=", i8,/)') IER
        endif
        call MPI_FINALIZE(ierror)
        stop
      endif

      end subroutine init_solve33_DJDS_kemo
!
!  ---------------------------------------------------------------------
!
      end module solver33_DJDS


