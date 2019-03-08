!
!  module solverNN_DJDS
!
!C*** 
!C*** module solverNN_DJDS
!C***
!
!        subroutine  init_solverNN_DJDS                                 &
!     &         (NP, NB, PEsmpTOT, METHOD, PRECOND, IER)
!
!      subroutine  solveNN_DJDS_kemo                                    &
!     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,         &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                    &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT,                             &
!     &           METHOD, PRECOND, ITERactual)
!
!      subroutine  init_solveNN_DJDS_kemo                               &
!     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,         &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                    &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT,                             &
!     &           METHOD, PRECOND, ITERactual)
!
      module solverNN_DJDS
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter, private :: iterPREmax = 1
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
!C--- init_solverNN
        subroutine  init_solverNN_DJDS                                  &
     &         (NP, NB, PEsmpTOT, METHOD, PRECOND, IER)
!
      use calypso_mpi
!
      use m_flags_4_solvers
      use solver_VBiCGSTABnn_DJDS_SMP
      use solver_VGPBiCGnn_DJDS_SMP
      use solver_VCGnn_DJDS_SMP
      use solver_GAUS_ZIDLnn_DJDS
      use solver_JACOBInn_DJDS
!
      integer(kind=kint ), intent(in) :: NP, NB, PEsmpTOT
      character(len=kchara) , intent(in):: METHOD
      character(len=kchara) , intent(in):: PRECOND
      integer(kind=kint), intent(inout) :: IER
!
      integer :: ierror
!C
!C
      IER = 0
!
!C-- BiCGSTAB
      if(solver_iflag(METHOD) .eq. iflag_bicgstab) then
        call init_VBiCGSTABnn_DJDS_SMP                                  &
     &     (NP, NB, PEsmpTOT, PRECOND, iterPREmax)
!C
!C-- GPBiCG using n*n solver
      else if(solver_iflag(METHOD) .eq. iflag_gpbicg) then
        call init_VGPBiCGnn_DJDS_SMP                                    &
     &     (NP, NB, PEsmpTOT, PRECOND, iterPREmax)
!C
!C-- CG
      else if(solver_iflag(METHOD) .eq. iflag_cg) then
        call init_VCGnn_DJDS_SMP(NP, NB, PEsmpTOT, PRECOND, iterPREmax)
!
!C-- Gauss-Zeidel
      else if(solver_iflag(METHOD) .eq. iflag_gausszeidel) then
        call init_VGAUSS_ZEIDELnn_DJDS_SMP(NP, NB, PEsmpTOT)
!C
!C-- Jacobi
      else if(solver_iflag(METHOD) .eq. iflag_jacobi) then
        call init_VJACOBInn_DJDS_SMP(NP, NB, PEsmpTOT)
!
      else
        IER = 1
      end if
!
!C-- ERROR
      if (IER.gt.0) then
        ierror = int(IER)
        if (my_rank.eq.0) then
          write (*,'(//,"#### GeoFEM SOLVER abort CODE=", i8,/)') IER
        end if
        call MPI_FINALIZE(ierror)
        stop
      end if
!
      end subroutine init_solverNN_DJDS
!
!  ---------------------------------------------------------------------
!C
!C--- solve
      subroutine  solveNN_DJDS_kemo                                     &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                     &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT,                              &
     &           METHOD, PRECOND, ITERactual)

! \beginSUBROUTINE
!      solver subsystem entry for N*N Block Matrix with DJDS ordering
!      Kenorin's special
!    \begin{flushright}     
!     coded by K.Nakajima (RIST) on DEC. 1999 (ver 3.0)
!     Modified by Kemorin (RIST) on Sep. 2002 (ver X.0)
!    \end{flushright}     

      use calypso_mpi
!
      use m_flags_4_solvers
      use solver_VBiCGSTABnn_DJDS_SMP
      use solver_VGPBiCGnn_DJDS_SMP
      use solver_VCGnn_DJDS_SMP
      use solver_GAUS_ZIDLnn_DJDS
      use solver_JACOBInn_DJDS
!
! ......................................................................
      integer(kind=kint )                  , intent(in)   ::  NB
! \beginARG       number of internal nodes (exclude external node)
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

      real   (kind=kreal), dimension(NB*NB*NP), intent(in)::  D
! \beginARG       diagonal matrix value            (i-th dof)

      real   (kind=kreal), dimension(NB*NB*NPL),intent(in)::  AL
! \beginARG       upper triangular matrix value    (i-th term)
      integer(kind=kint),dimension(0:NL*NVECT*PEsmpTOT),intent(in)::INL
! \beginARG       last term count at each freedom  (i-th node)
      integer(kind=kint ), dimension(NPL)  , intent(in)   ::  IAL
! \beginARG       node number at each term         (i-th term)

      real   (kind=kreal), dimension(NB*NB*NPU),intent(in)::  AU
! \beginARG       lower triangular matrix value    (i-th term)
      integer(kind=kint),dimension(0:NU*NVECT*PEsmpTOT),intent(in)::INU
! \beginARG       last term count at each freedom  (i-th node)
      integer(kind=kint), dimension(NPU)  , intent(in)   ::  IAU
! \beginARG       node number at each term         (i-th term)

      real   (kind=kreal), dimension(NB*NP) , intent(inout)::  B
! \beginARG       right hand load vector
      real   (kind=kreal), dimension(NB*NP) , intent(inout)::  X
! \beginARG       solution vector

      real   (kind=kreal), dimension(NB*NB*N  ) :: ALU_L, ALU_U
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
!C
!C-- BiCGSTAB
      if(solver_iflag(METHOD) .eq. iflag_bicgstab) then
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
!C-- GPBiCG using n*n solver
      else if(solver_iflag(METHOD) .eq. iflag_gpbicg) then
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
!C-- CG
      else if(solver_iflag(METHOD) .eq. iflag_cg) then
        call solve_VCGnn_DJDS_SMP                                       &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!
!C-- Gauss-Zeidel
      else if(solver_iflag(METHOD) .eq. iflag_gausszeidel) then
        call solve_VGAUSS_ZEIDELnn_DJDS_SMP                             &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!C
!C-- Jacobi
      else if(solver_iflag(METHOD) .eq. iflag_jacobi) then
        call solve_VJACOBInn_DJDS_SMP                                   &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
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

      end subroutine solveNN_DJDS_kemo
!
!  ---------------------------------------------------------------------
!C
!C--- solve with init
      subroutine  init_solveNN_DJDS_kemo                                &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                     &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT,                              &
     &           METHOD, PRECOND, ITERactual)

! \beginSUBROUTINE
!      solver subsystem entry for N*N Block Matrix with DJDS ordering
!      Kenorin's special
!    \begin{flushright}     
!     coded by K.Nakajima (RIST) on DEC. 1999 (ver 3.0)
!     Modified by Kemorin (RIST) on Sep. 2002 (ver X.0)
!    \end{flushright}     

      use calypso_mpi
!
      use m_flags_4_solvers
      use solver_VBiCGSTABnn_DJDS_SMP
      use solver_VGPBiCGnn_DJDS_SMP
      use solver_VCGnn_DJDS_SMP
      use solver_GAUS_ZIDLnn_DJDS
      use solver_JACOBInn_DJDS
!
      integer(kind=kint )                  , intent(in)   ::  NB
      integer(kind=kint )                  , intent(in)   ::  N
      integer(kind=kint )                  , intent(in)   ::  NP
      integer(kind=kint )                  , intent(in)   ::  NL, NU
      integer(kind=kint )                  , intent(in)   ::  NPL, NPU
      integer(kind=kint )                  , intent(in)   ::  PEsmpTOT
      integer(kind=kint )                  , intent(in)   ::  NVECT

      real   (kind=kreal), dimension(NB*NB*NP), intent(in)::  D
      real   (kind=kreal), dimension(NB*NB*NPL),intent(in)::  AL
      integer(kind=kint),dimension(0:NL*NVECT*PEsmpTOT),intent(in)::INL
      integer(kind=kint ), dimension(NPL)  , intent(in)   ::  IAL

      real   (kind=kreal), dimension(NB*NB*NPU),intent(in)::  AU
      integer(kind=kint),dimension(0:NU*NVECT*PEsmpTOT),intent(in)::INU
      integer(kind=kint), dimension(NPU)  , intent(in)   ::  IAU

      real   (kind=kreal), dimension(NB*NP) , intent(inout)::  B
      real   (kind=kreal), dimension(NB*NP) , intent(inout)::  X

      real   (kind=kreal), dimension(NB*NB*N  ) :: ALU_L, ALU_U

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
!C
!C-- BiCGSTAB
      if(solver_iflag(METHOD) .eq. iflag_bicgstab) then
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
!C-- GPBiCG using n*n solver
      else if(solver_iflag(METHOD) .eq. iflag_gpbicg) then
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
!C-- CG
      else if(solver_iflag(METHOD) .eq. iflag_cg) then
        call VCGnn_DJDS_SMP                                             &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax)
!
!
!C-- Gauss-Zeidel
      else if(solver_iflag(METHOD) .eq. iflag_gausszeidel) then
        call VGAUSS_ZEIDELnn_DJDS_SMP                                   &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT, PRECOND)
!C
!C-- Jacobi
      else if(solver_iflag(METHOD) .eq. iflag_jacobi) then
        call VJACOBInn_DJDS_SMP                                         &
     &         ( N, NP, NB, NL, NU, NPL, NPU, NVECT, PEsmpTOT,          &
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

      end subroutine init_solveNN_DJDS_kemo
!
!  ---------------------------------------------------------------------
!
      end module solverNN_DJDS
