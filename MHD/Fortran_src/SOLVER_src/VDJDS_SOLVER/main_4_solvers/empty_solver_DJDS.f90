!
!  module empty_solver_DJDS.f90
!
!C*** 
!C*** module empty_solver_DJDS
!C***
!
!      subroutine  empty_solve_DJDS_kemo                                &
!     &         (EPS, ITER, ITERactual, IER, my_rank, SOLVER_COMM,      &
!     &          METHOD)
!
      module empty_solver_DJDS
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!C
!C--- through for empty domain
      subroutine  empty_solve_DJDS_kemo                                 &
     &         (EPS, ITER, ITERactual, IER, my_rank, SOLVER_COMM,       &
     &          METHOD)

      use empties_VCG_DJDS_SMP

      integer(kind=kint ), intent(in) :: SOLVER_COMM
      integer(kind=kint ), intent(in) :: my_rank, ITER
      real   (kind=kreal), intent(in) :: EPS
      character(len=kchara ), intent(in):: METHOD
      integer(kind=kint ), intent(inout)  :: ITERactual
      integer(kind=kint ), intent(inout) ::  IER
!
      integer(kind=kint ) :: ITR
!
      ITR = ITER
!C
!C-- BiCGSTAB
      if ( ((METHOD(1:1).eq.'B').or.(METHOD(1:1).eq.'b')) .and.         &
     &          ((METHOD(2:2).eq.'I').or.(METHOD(2:2).eq.'i')) .and.    &
     &          ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.    &
     &          ((METHOD(4:4).eq.'G').or.(METHOD(4:4).eq.'g')) .and.    &
     &          ((METHOD(5:5).eq.'S').or.(METHOD(5:5).eq.'s')) ) then
!
       call empty_VBiCGSTAB_DJDS_SMP                                    &
     &         (EPS, ITR, IER, my_rank, SOLVER_COMM)
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
       call empty_VGPBiCG_DJDS_SMP                                      &
     &         (EPS, ITR, IER, my_rank, SOLVER_COMM)
!
!C
!C-- CG
      else if ( ((METHOD(1:1).eq.'C').or.(METHOD(1:1).eq.'c')) .and.    &
     &          ((METHOD(2:2).eq.'G').or.(METHOD(2:2).eq.'g')) ) then
!
        call empty_VCG_DJDS_SMP                                         &
     &         (EPS, ITR, IER, my_rank, SOLVER_COMM)
!
!C-- Gauss-Zeidel

      else if ( ((METHOD(1:1).eq.'G').or.(METHOD(1:1).eq.'g')) .and.    &
     &          ((METHOD(2:2).eq.'A').or.(METHOD(2:2).eq.'a')) .and.    &
     &          ((METHOD(3:3).eq.'U').or.(METHOD(3:3).eq.'u')) .and.    &
     &          ((METHOD(4:4).eq.'S').or.(METHOD(4:4).eq.'s')) .and.    &
     &          ((METHOD(5:5).eq.'S').or.(METHOD(5:5).eq.'s')) ) then
!
        call empty_VGAUSS_ZEIDEL_DJDS_SMP                               &
     &         (EPS, ITR, IER, my_rank, SOLVER_COMM)
!C
!C-- Jacobi

      else if ( ((METHOD(1:1).eq.'J').or.(METHOD(1:1).eq.'j')) .and.    &
     &          ((METHOD(2:2).eq.'A').or.(METHOD(2:2).eq.'a')) .and.    &
     &          ((METHOD(3:3).eq.'C').or.(METHOD(3:3).eq.'c')) .and.    &
     &          ((METHOD(4:4).eq.'O').or.(METHOD(4:4).eq.'o')) .and.    &
     &          ((METHOD(5:5).eq.'B').or.(METHOD(5:5).eq.'b')) .and.    &
     &          ((METHOD(6:6).eq.'I').or.(METHOD(6:6).eq.'i')) ) then
!
        call empty_VGAUSS_ZEIDEL_DJDS_SMP                               &
     &         (EPS, ITR, IER, my_rank, SOLVER_COMM)
!
      end if
!
!
      ITERactual = ITR
!C
!C-- ERROR
      if (IER.gt.0) then
        if (my_rank.eq.0) then
          write (*,'(//,"#### GeoFEM SOLVER abort CODE=", i8,/)') IER
        endif
        call MPI_FINALIZE(IER)
        stop
      endif

      end subroutine empty_solve_DJDS_kemo
!
!  ---------------------------------------------------------------------
!
      end module empty_solver_DJDS


