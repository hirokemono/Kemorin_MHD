!
!  module solver_DJDS.f90
!
!C*** 
!C*** module solver_DJDS
!C***
!
!      subroutine  init_solver_DJDS                                     &
!     &         (NP, PEsmpTOT, METHOD, PRECOND, ERROR)
!
!      subroutine  solve_DJDS_kemo                                      &
!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                    &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT,                             &
!     &           METHOD, PRECOND, ITERactual, SR_sig, SR_r)
!
!      subroutine  init_solve_DJDS_kemo                                 &
!     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,             &
!     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,               &
!     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,          &
!     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,             &
!     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                    &
!     &           STACK_IMPORT, NOD_IMPORT,                             &
!     &           STACK_EXPORT, NOD_EXPORT,                             &
!     &           METHOD, PRECOND, ITERactual, SR_sig, SR_r)
!
! \beginSUBROUTINE
!      solver subsystem entry for scalar Matrix with DJDS ordering
!      Kenorin's special
!    \begin{flushright}     
!     coded by K.Nakajima (RIST) on DEC. 1999 (ver 3.0)
!     Modified by Kemorin (RIST) on Sep. 2002 (ver X.0)
!    \end{flushright}     
!
      module solver_DJDS
!
      use m_precision
      use t_solver_SR
      use m_solver_count_time
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
!C
!C--- init_solver
      subroutine  init_solver_DJDS                                      &
     &         (NP, PEsmpTOT, METHOD, PRECOND, IER)
!
      use calypso_mpi
!
      use m_flags_4_solvers
      use solver_VCG11_DJDS_SMP
      use solver_VBiCGSTAB11_DJDS_SMP
      use solver_VGPBiCG11_DJDS_SMP
      use solver_GAUS_ZIDL11_DJDS
      use solver_JACOBI11_DJDS

      integer(kind=kint ), intent(in) :: NP, PEsmpTOT
      character(len=kchara) , intent(in):: METHOD
      character(len=kchara) , intent(in):: PRECOND
      integer(kind=kint), intent(inout) :: IER
      integer :: ierror
!
!>      Elapsed time for initialization
      real(kind = kreal) :: INITtime
!
      IER = 0
!
!C-- BiCGSTAB
      if(solver_iflag(METHOD) .eq. iflag_bicgstab) then
        call init_VBiCGSTAB11_DJDS_SMP(NP, PEsmpTOT, PRECOND,           &
     &                                 iterPREmax, INITtime)
!C
!C-- GPBiCG
      else if(solver_iflag(METHOD) .eq. iflag_gpbicg) then
        call init_VGPBiCG11_DJDS_SMP(NP, PEsmpTOT, PRECOND,             &
     &                               iterPREmax, INITtime)
!C
!C-- CG
      else if(solver_iflag(METHOD) .eq. iflag_cg) then
        call init_VCG11_DJDS_SMP(NP, PEsmpTOT, PRECOND,                 &
     &                           iterPREmax, INITtime)
!C
!C-- GAuss-Zeidel
      else if(solver_iflag(METHOD) .eq. iflag_gausszeidel) then
        call init_VGAUSS_ZEIDEL11_DJDS_SMP(NP, PEsmpTOT, INITtime)
!C
!C-- Jacobi
      else if(solver_iflag(METHOD) .eq. iflag_jacobi) then
        call init_VJACOBI11_DJDS_SMP(NP, PEsmpTOT, INITtime)
      else
        IER = 1
      end if
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

      end subroutine init_solver_DJDS
!
!  ---------------------------------------------------------------------
!
!C
!C--- solve
      subroutine  solve_DJDS_kemo                                       &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                     &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT,                              &
     &           METHOD, PRECOND, ITERactual, SR_sig, SR_r)
!
      use calypso_mpi
!
      use m_flags_4_solvers
      use solver_VCG11_DJDS_SMP
      use solver_VBiCGSTAB11_DJDS_SMP
      use solver_VGPBiCG11_DJDS_SMP
      use solver_GAUS_ZIDL11_DJDS
      use solver_JACOBI11_DJDS

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
!

      real   (kind=kreal), dimension(NP ), intent(in)::  D
! \beginARG       diagonal matrix value            (i-th dof)

      real   (kind=kreal), dimension(NPL),intent(in)::  AL
! \beginARG       upper triangular matrix value    (i-th term)
      integer(kind=kint),dimension(0:NL*NVECT*PEsmpTOT),intent(in)::INL
! \beginARG       last term count at each freedom  (i-th node)
      integer(kind=kint ), dimension(NPL)  , intent(in)   ::  IAL
! \beginARG       node number at each term         (i-th term)

      real   (kind=kreal), dimension(NPU),intent(in)::  AU
! \beginARG       lower triangular matrix value    (i-th term)
      integer(kind=kint),dimension(0:NU*NVECT*PEsmpTOT),intent(in)::INU
! \beginARG       last term count at each freedom  (i-th node)
      integer(kind=kint), dimension(NPU)  , intent(in)   ::  IAU
! \beginARG       node number at each term         (i-th term)

      real   (kind=kreal), dimension(NP ) , intent(inout)::  B
! \beginARG       right hand load vector
      real   (kind=kreal), dimension(NP ) , intent(inout)::  X
! \beginARG       solution vector

      real   (kind=kreal), dimension(N  ) :: ALU_L, ALU_U
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
      integer(kind=kint )                  , intent(inout)::  IER
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind=kint ) :: ITR
      integer :: ierror
!
      ITR = ITER

!C
!C-- BiCGSTAB
      if(solver_iflag(METHOD) .eq. iflag_bicgstab) then
       call solve_VBiCGSTAB11_DJDS_SMP                                  &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax, SR_sig, SR_r, COMPtime, COMMtime)
!
!C
!C-- GPBiCG
      else if(solver_iflag(METHOD) .eq. iflag_gpbicg) then
       call solve_VGPBiCG11_DJDS_SMP                                    &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax, SR_sig, SR_r, COMPtime, COMMtime)
!
!C
!C-- CG
      else if(solver_iflag(METHOD) .eq. iflag_cg) then
        call solve_VCG11_DJDS_SMP                                       &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax, SR_sig, SR_r, COMPtime, COMMtime)
!
!C
!C-- GAuss-Zeidel
      else if(solver_iflag(METHOD) .eq. iflag_gausszeidel) then
        call solve_VGAUSS_ZEIDEL11_DJDS_SMP                             &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, SR_sig, SR_r, COMPtime, COMMtime)
!C
!C-- Jacobi
      else if(solver_iflag(METHOD) .eq. iflag_jacobi) then
        call solve_VJACOBI11_DJDS_SMP                                   &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, SR_sig, SR_r, COMPtime, COMMtime)
      end if

      ITERactual= ITR
!C
!C-- ERROR
      R1= 100.d0 * ( 1.d0 - COMMtime/COMPtime )
      if (my_rank.eq.0) then
        open(41,file='solver_11.dat',position='append')
        write (41,'(i7,1p3e16.6)') ITER, COMPtime, COMMtime, R1
        close(41)
      end if
!
      if (IER.gt.0) then
        ierror = int(IER)
        if (my_rank.eq.0) then
          write (*,'(//,"#### GeoFEM SOLVER abort CODE=", i8,/)') IER
        endif
        call MPI_FINALIZE(ierror)
        stop
      endif

      end subroutine solve_DJDS_kemo
!
!  ---------------------------------------------------------------------
!C
!C--- solve
      subroutine  init_solve_DJDS_kemo                                  &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITER, IER, NEIBPETOT, NEIBPE,                     &
     &           STACK_IMPORT, NOD_IMPORT,                              &
     &           STACK_EXPORT, NOD_EXPORT,                              &
     &           METHOD, PRECOND, ITERactual, SR_sig, SR_r)
!
      use calypso_mpi
!
      use m_flags_4_solvers
      use solver_VCG11_DJDS_SMP
      use solver_VBiCGSTAB11_DJDS_SMP
      use solver_VGPBiCG11_DJDS_SMP
      use solver_GAUS_ZIDL11_DJDS
      use solver_JACOBI11_DJDS

      integer(kind=kint )                  , intent(in)   ::  N
      integer(kind=kint )                  , intent(in)   ::  NP
      integer(kind=kint )                  , intent(in)   ::  NL, NU
      integer(kind=kint )                  , intent(in)   ::  NPL, NPU
      integer(kind=kint )                  , intent(in)   ::  PEsmpTOT
      integer(kind=kint )                  , intent(in)   ::  NVECT
!

      real   (kind=kreal), dimension(NP ), intent(in)::  D
      real   (kind=kreal), dimension(NPL),intent(in)::  AL
      integer(kind=kint),dimension(0:NL*NVECT*PEsmpTOT),intent(in)::INL
      integer(kind=kint ), dimension(NPL)  , intent(in)   ::  IAL
      real   (kind=kreal), dimension(NPU),intent(in)::  AU
      integer(kind=kint),dimension(0:NU*NVECT*PEsmpTOT),intent(in)::INU
      integer(kind=kint), dimension(NPU)  , intent(in)   ::  IAU

      real   (kind=kreal), dimension(NP ) , intent(inout)::  B
      real   (kind=kreal), dimension(NP ) , intent(inout)::  X

      real   (kind=kreal), dimension(N  ) :: ALU_L, ALU_U

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
      integer(kind=kint )                  , intent(inout)::  IER
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind=kint ) :: ITR
      integer :: ierror
!>      Elapsed time for initialization
      real(kind = kreal) :: INITtime
!
      ITR = ITER

!C
!C-- BiCGSTAB
      if(solver_iflag(METHOD) .eq. iflag_bicgstab) then
       call VBiCGSTAB11_DJDS_SMP                                        &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax, SR_sig, SR_r,                     &
     &           INITtime, COMPtime, COMMtime)
!
!C
!C-- GPBiCG
      else if(solver_iflag(METHOD) .eq. iflag_gpbicg) then
       call VGPBiCG11_DJDS_SMP                                          &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax, SR_sig, SR_r,                     &
     &           INITtime, COMPtime, COMMtime)
!
!C
!C-- CG
      else if(solver_iflag(METHOD) .eq. iflag_cg) then
        call VCG11_DJDS_SMP                                             &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, iterPREmax, SR_sig, SR_r,                     &
     &           INITtime, COMPtime, COMMtime)
!
!C
!C-- GAuss-Zeidel
      else if(solver_iflag(METHOD) .eq. iflag_gausszeidel) then
        call VGAUSS_ZEIDEL11_DJDS_SMP                                   &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, SR_sig, SR_r, INITtime, COMPtime, COMMtime)
!C
!C-- Jacobi
      else if(solver_iflag(METHOD) .eq. iflag_jacobi) then
        call VJACOBI11_DJDS_SMP                                         &
     &         ( N, NP, NL, NU, NPL, NPU, NVECT, PEsmpTOT,              &
     &           STACKmcG, STACKmc, NLhyp, NUhyp, IVECT,                &
     &           NtoO, OtoN_L, OtoN_U, NtoO_U, LtoU, D, B, X,           &
     &           INL, INU, IAL, IAU, AL, AU, ALU_L, ALU_U,              &
     &           EPS, ITR, IER, NEIBPETOT, NEIBPE,                      &
     &           STACK_IMPORT, NOD_IMPORT, STACK_EXPORT, NOD_EXPORT,    &
     &           PRECOND, SR_sig, SR_r, INITtime, COMPtime, COMMtime)
      end if

      ITERactual= ITR
!C
!C-- ERROR
      R1= 100.d0 * ( 1.d0 - COMMtime/COMPtime )
      if (my_rank.eq.0) then
        open(41,file='solver_11.dat',position='append')
        write (41,'(i7,1p3e16.6)') ITER, COMPtime, COMMtime, R1
        close(41)
      end if
!
      if (IER.gt.0) then
        ierror = int(IER)
        if (my_rank.eq.0) then
          write (*,'(//,"#### GeoFEM SOLVER abort CODE=", i8,/)') IER
        endif
        call MPI_FINALIZE(ierror)
        stop
      endif

      end subroutine init_solve_DJDS_kemo
!
!  ---------------------------------------------------------------------
!
      end module solver_DJDS
