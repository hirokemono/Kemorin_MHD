!Cempties_VCG_DJDS_SMP.f90
!C*** 
!C*** module empties_VCG_DJDS_SMP
!C***
!
!      Written by H. Matsui on Sep. 2011
!
!      subroutine empty_VCG_DJDS_SMP(EPS, ITR, IER)
!        subroutine solve_VCG11_DJDS_SMP
!        subroutine solve_VCG33_DJDS_SMP
!        subroutine solve_VCGnn_DJDS_SMP
!
!      subroutine empty_VGPBiCG_DJDS_SMP(EPS, ITR, IER)
!        subroutine solve_VGPBiCGnn_DJDS_SMP
!        subroutine solve_VGPBiCG33_DJDS_SMP
!        subroutine solve_VGPBiCG11_DJDS_SMP
!
!      subroutine empty_VBiCGSTAB_DJDS_SMP(EPS, ITR, IER)
!        subroutine solve_VBiCGSTAB11_DJDS_SMP
!        subroutine solve_VBiCGSTAB33_DJDS_SMP
!        subroutine solve_VBiCGSTABnn_DJDS_SMP
!
!      subroutine empty_VGAUSS_ZEIDEL_DJDS_SMP(EPS, ITR, IER)
!        subroutine solve_VGAUSS_ZEIDEL11_DJDS_SMP
!        subroutine solve_VGAUSS_ZEIDEL33_DJDS_SMP
!        subroutine solve_VGAUSS_ZEIDELnn_DJDS_SMP
!        subroutine solve_VJACOBI11_DJDS_SMP
!        subroutine solve_VJACOBI33_DJDS_SMP
!        subroutine solve_VJACOBInn_DJDS_SMP
!
      module empties_VCG_DJDS_SMP
!
      use m_precision
      use m_constants
!
      implicit none
!C
      real(kind = kreal), private  :: S1_TIME, START_TIME, END_TIME
      real(kind = kreal), private  :: COMMtime
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine empty_VCG_DJDS_SMP(EPS, ITR, IER)

      use calypso_mpi
!
      real   (kind=kreal), intent(in) :: EPS
      integer(kind=kint ), intent(inout) :: ITR, IER

      integer(kind=kint ) :: iter, MAXIT
      real(kind = kreal) :: RESID, BNRM2, DNRM2, TOL
      real(kind = kreal) :: RHO, C1
!
!C
      MAXIT= ITR
      TOL  = EPS
      S1_TIME= MPI_WTIME()
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (ZERO, BNRM2, 1, CALYPSO_REAL,                 &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===
      do iter= 1, MAXIT
!C===
!C
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (ZERO, RHO, 1, CALYPSO_REAL,                 &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (ZERO, C1, 1, CALYPSO_REAL,                  &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!C===
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (ZERO, DNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
        RESID= dsqrt(DNRM2/BNRM2)
!
        ITR = ITER
!
        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
        end if
      end do
!
!C===
   30 continue

      end subroutine empty_VCG_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine empty_VGPBiCG_DJDS_SMP(EPS, ITR, IER)
!
      use calypso_mpi
!
      real   (kind=kreal), intent(in) :: EPS
      integer(kind=kint ), intent(inout) :: ITR, IER

      integer(kind=kint ) :: iter, MAXIT
      real(kind = kreal) :: RESID, BNRM2, DNRM2, TOL
      real(kind = kreal) :: RHO, RHO1, COEF1, C0(5), CG(5)
!
!

      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (zero, BNRM2, 1, CALYPSO_REAL,                 &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE (zero, RHO, 1, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!
!C===
!C************************************************* Conjugate Gradient Iteration
!
      do iter= 1, MAXIT
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (zero, RHO1, 1, CALYPSO_REAL,                &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!C
!C +-------------------+
!C | calc. QSI and ETA |
!C +-------------------+
!C===
!
        C0 = 0.0d0
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (C0, CG,  5, CALYPSO_REAL,                   &
     &                     MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!C
!C +--------------------+
!C | update {x},{r},{w} |
!C +--------------------+
!C===
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE  (zero, DNRM2, 1, CALYPSO_REAL,              &
     &                     MPI_SUM, CALYPSO_COMM, ierr_MPI)
        call MPI_allREDUCE  (zero, COEF1, 1, CALYPSO_REAL,              &
     &                     MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!
        RESID= dsqrt(DNRM2/BNRM2)
!
        ITR = ITER
!
        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
        end if
!
      end do
!
!C===
   30 continue
!
      end subroutine empty_VGPBiCG_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine empty_VBiCGSTAB_DJDS_SMP(EPS, ITR, IER)
!
      use calypso_mpi
!
      real   (kind=kreal), intent(in) :: EPS
      integer(kind=kint ), intent(inout) :: ITR, IER

      integer(kind=kint ) :: iter, MAXIT
      real(kind = kreal) :: RESID, BNRM2, DNRM2, TOL
      real(kind = kreal) :: RHO, C2, C0(2), CG(2)
!
!C
!C +---------------+
!C | BNORM2 = B^2  |
!C +---------------+
!C===

      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (zero, BNRM2, 1, CALYPSO_REAL,                 &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===
      do iter= 1, MAXIT
!C
!C +-------------------+
!C | {RHO}= {r}{r_tld} |
!C +-------------------+
!C===
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (zero, RHO, 1, CALYPSO_REAL,                 &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!C
!C +----------------+
!C | ALPHA = RHO/C2 |
!C +----------------+
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (zero, C2, 1, CALYPSO_REAL,                  &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!C
!C +----------------------------+
!C | OMEGA= ({t}{s}) / ({t}{t}) |
!C +----------------------------+
!C===
        C0 = 0.0d0
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (C0, CG, 2, CALYPSO_REAL, MPI_SUM,           &
     &                    CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME

!C
!C +----------------+
!C | update {x},{r} |
!C +----------------+
!C===
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (zero, DNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
        RESID= dsqrt(DNRM2/BNRM2)

        ITR = ITER

        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
        end if
      end do

!C===
   30 continue

      end subroutine empty_VBiCGSTAB_DJDS_SMP
!
!  ---------------------------------------------------------------------
!C
      subroutine empty_VGAUSS_ZEIDEL_DJDS_SMP(EPS, ITR, IER)
!
      use calypso_mpi
!
      real   (kind=kreal), intent(in) :: EPS
      integer(kind=kint ), intent(inout) :: ITR, IER

      integer(kind=kint ) :: iter, MAXIT
      real(kind = kreal) :: RESID, BNRM2, DNRM2, TOL

!C
!C +---------------+
!C | BNORM2 = B^2  |
!C +---------------+
!C===
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (zero, BNRM2, 1, CALYPSO_REAL,                 &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===
      do iter= 1, MAXIT
!C
!C +---------------+
!C | DNRM2 = B^2   |
!C +---------------+
!C===
         START_TIME= MPI_WTIME()
         call MPI_allREDUCE (zero, DNRM2, 1, CALYPSO_REAL,              &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
         END_TIME= MPI_WTIME()
         COMMtime = COMMtime + END_TIME - START_TIME
!
         RESID= dsqrt(DNRM2/BNRM2)
         ITR = ITER

        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
        end if
      end do

!C===
   30 continue
!
      end subroutine empty_VGAUSS_ZEIDEL_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module empties_VCG_DJDS_SMP
