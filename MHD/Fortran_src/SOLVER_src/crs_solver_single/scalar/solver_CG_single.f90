!
!  module solver_CG_single
!
!C*** 
!C*** module solver_CG_single
!C***
!
!      subroutine CG_sgl(N, NPL, NPU, D,  AL, INL, IAL, AU, INU, IAU,   &
!     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,              &
!     &                  EPS,  ITER, ERROR, my_rank, NSET)
!
!     CG solves the linear system Ax = b using the
!     Conjugate Gradient iterative method with preconditioning.
!
!     coded by K.Nakajima (RIST) on jul. 1999 (ver 1.0)
!     modified by H. Matsui on jul. 2008
!
      module solver_CG_single
!
      use m_precision
      use m_constants
!
      implicit none
!
      real(kind=kreal), allocatable, private :: ALUG(:)
      real(kind=kreal), allocatable, private :: SCALE(:)
!
      real(kind = kreal), allocatable :: W(:,:)
      private :: W
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!C
!C*** CG
!C
      subroutine CG_sgl(N, NPL, NPU, D,  AL, INL, IAL, AU, INU, IAU,    &
     &                  B,  X, PRECOND, SIGMA_DIAG,SIGMA,               &
     &                  EPS, ITER, ERROR, my_rank, NSET)
!
      use m_work_4_CG
      use crs_matrix_calcs_11
      use incomplete_cholesky_crs_11
      use precond_crs_incomplete_lu
      use calcs_4_crs_CG11
      use crs_norm_products_11
!
      integer(kind=kint ),                   intent(in   )::  N
      integer(kind=kint ),                   intent(in   )::  NPL
      integer(kind=kint ),                   intent(in   )::  NPU
!
      real   (kind=kreal),                   intent(inout)::  EPS
      real   (kind=kreal),                   intent(in   )::  SIGMA_DIAG
      real   (kind=kreal),                   intent(in   )::  SIGMA
      integer(kind=kint ),                   intent(inout)::  ITER
      integer(kind=kint ),                   intent(inout)::  ERROR
      integer(kind=kint ),                   intent(in   )::  my_rank

      integer(kind=kint )                  , intent(in)   :: NSET

      real   (kind=kreal), dimension(N )  , intent(inout)::  D
      real   (kind=kreal), dimension(N )  , intent(inout)::  B
      real   (kind=kreal), dimension(N )  , intent(inout)::  X

      real   (kind=kreal), dimension(NPU)  , intent(inout)::  AU
      real   (kind=kreal), dimension(NPU)  , intent(inout)::  AL

      integer(kind=kint ), dimension(0:N)   , intent(in) ::  INU
      integer(kind=kint ), dimension(0:N)   , intent(in) ::  INL
      integer(kind=kint ), dimension(  NPU)  , intent(in) ::  IAU
      integer(kind=kint ), dimension(  NPL)  , intent(in) ::  IAL
      character(len=kchara)                  , intent(in) :: PRECOND

      integer(kind=kint ) :: MAXIT, IFLAG, MONITORFLAG
      data IFLAG/0/
!C
!C-- INIT. 
      MONITORFLAG = ERROR
      ERROR= 0

      if (IFLAG.eq.0 .and. NSET.eq.0) then
        ERROR= 101
        return
      endif


      if (IFLAG.eq.0) then
        allocate (ALUG   (N))
        allocate (SCALE(N))
        IFLAG= 1
        SCALE(1:N)= 1.d0
      end if

      allocate(W(N,nWK_CG))
      W = 0.0d0
!
      MAXIT  = ITER
      TOL   = EPS

      if (NSET.ge.1) then
!C
!C-- SCALING
        if (NSET.eq.2) then
          SCALE(1:N)= 1.d0 / dsqrt(dabs(D(1:N)))
!
          call mat_scaling_crs_ilu(N, N, NPL, NPU, D, AL, INL, IAL,     &
     &         INU, IAU, AU, SCALE)
        end if
!C--

!C
!C +-------------------+
!C | ILU decomposition |
!C +-------------------+
!C===
        ALUG(1:N)= 0.d0
        if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
          call precond_crs_ilu(N, N, NPL, NPU, D, AL, INL,IAL,          &
     &      INU, AU, ALUG, SIGMA, SIGMA_DIAG)
        else if (PRECOND(1:4).eq.'SSOR'                                 &
     &       .or. PRECOND(1:4).eq.'DIAG') then
          call precond_crs_ssor(N, N, D, ALUG, SIGMA_DIAG)
        end if
!C===
      end if

!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===
      if(NSET.eq.2) B(1:N)= B(1:N) * SCALE(1:N)
!C
      call subtruct_crs_matvec_11 (N, N, NPL, NPU, INL, INU, IAL, IAU,  &
     &    D, AL, AU, W(1,R), B, X)
!
!
      call cal_local_norm_1(N, N, B, BNRM20)
      BNRM2 = BNRM20

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
      ITER = 0
!C===
      do iter= 1, MAXIT
!C
!C************************************************* Conjugate Gradient Iteration

!C
!C +----------------+
!C | {z}= [Minv]{r} |
!C +----------------+
!C===
!C-- incomplete CHOLESKY
      
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
          W(1:N,Z) =    W(1:N,R)
          call i_cholesky_crs_11(N, N, NPL, NPU,                        &
     &        INL, INU, IAL, IAU, ALUG, AL, AU, W(1,Z) )
      endif

!C
!C-- diagonal
        if (PRECOND(1:4).eq.'DIAG') then
          W(1:N,Z)=  W(1:N,R) * ALUG(1:N)
        endif
!C===

!C
!C +---------------+
!C | {RHO}= {r}{z} |
!C +---------------+
!C===
        call cal_local_s_product_1(N, N, W(1,R), W(1,Z), RHO0)
        RHO = RHO0
!C===

!C
!C +-----------------------------+
!C | {p} = {z} if      ITER=1    |
!C | BETA= RHO / RHO1  otherwise |
!C +-----------------------------+
!C===
        if ( ITER.eq.1 ) then
          W(1:N,P)= W(1:N,Z)
        else
          call crs_z_plus_beta_p_11(N, N, W(1,P), W(1,Z), RHO, RHO1)
        endif
!C===

!C
!C +-------------+
!C | {q}= [A]{p} |
!C +-------------+
!C===        

        call cal_crs_matvec_11(N, N, NPL, NPU, INL, INU, IAL, IAU,      &
     &      D, AL, AU, W(1,Q), W(1,P) )
!C===

!C
!C +---------------------+
!C | ALPHA= RHO / {p}{q} |
!C +---------------------+
!C===

        call cal_local_s_product_1(N, N, W(1,P), W(1,Q), C10)
        C1 = C10
        ALPHA= RHO / C1
        RHO1 = RHO
!C===

!C
!C +----------------------+
!C | {x}= {x} + ALPHA*{p} |
!C | {r}= {r} - ALPHA*{q} |
!C +----------------------+
!C===
        X(1:N) =     X(1:N) + ALPHA * W(1:N,P)
        W(1:N,R) = W(1:N,R) - ALPHA * W(1:N,Q)
!
        call cal_local_norm_1(N, N,  W(1,R), DNRM20)
!
        DNRM2 = DNRM20
        RESID= dsqrt(DNRM2/BNRM2)

        if (my_rank.eq.0 .and. MONITORFLAG.eq.1)                        &
     &    write (*, 1000) ITER, RESID
 1000   format ('CG_11: ', i5, 1pe16.6)

        if ( RESID.le.TOL   ) exit
        if ( ITER .eq.MAXIT ) ERROR= -100
      enddo
!C===
      deallocate(W)

!C
!C-- INTERFACE data EXCHANGE
      if(NSET.eq.2) then
        X(1:N)= X(1:N) * SCALE(1:N)
        B(1:N)= B(1:N) / SCALE(1:N)
      end if

      deallocate (W)

      end subroutine CG_sgl
!
!  ---------------------------------------------------------------------
!
      end module     solver_CG_single
