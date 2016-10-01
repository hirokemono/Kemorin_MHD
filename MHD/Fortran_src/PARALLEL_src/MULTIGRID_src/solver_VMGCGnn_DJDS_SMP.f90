!C
!C*** 
!C*** module solver_VMGCGnn_DJDS_SMP
!C***
!
!      Written by Kengo Nakajima on May., 2001
!      Modified by H. Matsui on Nov. 2005
!
!C
!C***
!C***  VMGCGnn_DJDS_SMP
!C***
!      subroutine VMGCGnn_DJDS_SMP(num_MG_level, MG_comm, MG_itp,       &
!     &          djds_tbl, matNN, MG_vect, PEsmpTOT, NP, NB, B, X,      &
!     &          ITR, iter_mid, iter_lowest, EPS, EPS_MG,               &
!     &          PRECOND, METHOD_MG, PRECOND_MG, IER, iterPREmax)
!
!      subroutine init_VMGCGnn_DJDS_SMP(NP, NB, PEsmpTOT,               &
!     &          PRECOND, METHOD_MG, PRECOND_MG, iterPREmax)
!      subroutine solve_VMGCGnn_DJDS_SMP(num_MG_level, MG_comm, MG_itp, &
!     &          djds_tbl, matNN, MG_vect, PEsmpTOT, NP, NB, B, X,      &
!     &          ITR, iter_mid, iter_lowest, EPS, EPS_MG,               &
!     &          PRECOND, METHOD_MG, PRECOND_MG, IER)
!      integer(kind = kint), intent(in) :: num_MG_level
!      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
!      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
!      type(DJDS_MATRIX), intent(in) ::         matNN(0:num_MG_level)
!      type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
!      integer(kind = kint), intent(in) :: PEsmpTOT
!      integer(kind = kint), intent(in) :: NP
!      real(kind = kreal), intent(in), target :: B(NB*NP)
!      real(kind = kreal), intent(inout), target :: X(NB*NP)
!      type(vectors_4_solver), intent(inout) :: MG_vect(0:num_MG_level)
!
!      character (len=kchara), intent(in) :: PRECOND
!      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
!      integer(kind = kint), intent(in) :: iter_mid,  iter_lowest
!      integer(kind=kint ), intent(inout) :: ITR, IER
!      real(kind = kreal), intent(in) :: EPS
!      real(kind = kreal), intent(in) :: EPS_MG
!
!C     VCG_DJDS_SMP solves the linear system Ax = b 
!C     using the Conjugate Gradient iterative method with preconditioning.
!C     Elements are ordered in descending Jagged Diagonal Storage
!C     for Vector Processing and Cyclic Ordering for SMP Parallel Computation
!C
      module solver_VMGCGnn_DJDS_SMP
!
      use m_precision
!
      implicit none
!
       real(kind = kreal), allocatable :: W(:,:)
       private :: W
       private :: verify_work_4_matvecnn
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_matvecnn(NP, NB, nwk)
!
      integer(kind = kint), intent(in) :: NB, NP, nwk
!
!
      if(allocated(W) .eqv. .false.) then
        allocate ( W(NB*NP,nwk) )
        W = 0.0d0
      else if(size(W) .lt. (NB*nwk*NP)) then
        deallocate (W)
        allocate ( W(NB*NP,nwk) )
        W = 0.0d0
      end if
!
      end subroutine verify_work_4_matvecnn
!
!  ---------------------------------------------------------------------
!C
      subroutine VMGCGnn_DJDS_SMP(num_MG_level, MG_comm, MG_itp,        &
     &          djds_tbl, matNN, MG_vect, PEsmpTOT, NP, NB, B, X,       &
     &          ITR, iter_mid, iter_lowest, EPS, EPS_MG,                &
     &          PRECOND, METHOD_MG, PRECOND_MG, IER, iterPREmax)
!
      use t_comm_table
      use t_interpolate_table
      use t_solver_djds
      use t_vector_for_solver
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) ::         matNN(0:num_MG_level)
      type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP, NB
      real(kind = kreal), intent(inout), target :: B(NP,NB)
      real(kind = kreal), intent(inout), target :: X(NP,NB)
      type(vectors_4_solver), intent(inout) :: MG_vect(0:num_MG_level)
!
      character (len=kchara), intent(in) :: PRECOND
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      integer(kind = kint), intent(in) :: iter_mid,  iter_lowest
      real(kind = kreal), intent(in) :: EPS
      real(kind = kreal), intent(in) :: EPS_MG
      integer(kind=kint ), intent(inout) :: ITR, IER
      integer(kind=kint ), intent(in)  :: iterPREmax
!
!
      call init_VMGCGnn_DJDS_SMP(NP, NB, PEsmpTOT,                      &
     &          PRECOND, METHOD_MG, PRECOND_MG, iterPREmax)
!C
      call solve_VMGCGnn_DJDS_SMP(num_MG_level, MG_comm, MG_itp,        &
     &          djds_tbl, matNN, MG_vect, PEsmpTOT, NP, NB, B, X,       &
     &          ITR, iter_mid, iter_lowest, EPS, EPS_MG,                &
     &          PRECOND, METHOD_MG, PRECOND_MG, IER)
!
      end subroutine VMGCGnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!C
      subroutine init_VMGCGnn_DJDS_SMP(NP, NB, PEsmpTOT,                &
     &          PRECOND, METHOD_MG, PRECOND_MG, iterPREmax)
!
      use m_work_4_CG
!
      use incomplete_cholesky_nn
      use MGCGnn_V_cycle
!
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP, NB
      character (len=kchara), intent(in) :: PRECOND
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      integer(kind=kint ), intent(in)  :: iterPREmax
!
!
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
        if(iterPREmax .ge. 1) ntotWK_CG = ntotWK_CG + 2
      end if
!
      call verify_work_4_matvecnn(NP,NB, ntotWK_CG)
      call init_MGCGnn_V_cycle(NP, NB, PEsmpTOT, METHOD_MG, PRECOND_MG)
!
      end subroutine init_VMGCGnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!C
      subroutine solve_VMGCGnn_DJDS_SMP(num_MG_level, MG_comm, MG_itp,  &
     &          djds_tbl, matNN, MG_vect, PEsmpTOT, NP, NB, B, X,       &
     &          ITR, iter_mid, iter_lowest, EPS, EPS_MG,                &
     &          PRECOND, METHOD_MG, PRECOND_MG, IER)
!
      use calypso_mpi
      use solver_SR_N
!
      use t_comm_table
      use t_interpolate_table
      use t_solver_djds
      use t_vector_for_solver
!
      use m_work_4_CG
      use m_solver_count_time
!
      use cal_norm_products_nn
      use vector_calc_solver_nn
      use djds_matrix_calcs_nn
      use incomplete_cholesky_nn
      use diagonal_scaling_nn
      use jacobi_precondition_nn
      use gauss_zeidel_nn
      use calcs_4_CG
      use MGCGnn_V_cycle
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) ::         matNN(0:num_MG_level)
      type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP, NB
      real(kind = kreal), intent(inout), target :: B(NP,NB)
      real(kind = kreal), intent(inout), target :: X(NP,NB)
      type(vectors_4_solver), intent(inout) :: MG_vect(0:num_MG_level)
!
      character (len=kchara), intent(in) :: PRECOND
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      integer(kind = kint), intent(in) :: iter_mid,  iter_lowest
      real(kind = kreal), intent(in) :: EPS
      real(kind = kreal), intent(in) :: EPS_MG
      integer(kind=kint ), intent(inout) :: ITR, IER
!
!
      integer(kind=kint ) :: N
      integer(kind=kint ) :: NEIBPETOT
      real(kind=kreal), pointer :: D(:)
      real(kind=kreal), pointer :: AL(:)
      real(kind=kreal), pointer :: AU(:)

      integer(kind=kint ), pointer :: NEIBPE(:)
      integer(kind=kint ), pointer :: STACK_IMPORT(:)
      integer(kind=kint ), pointer :: NOD_IMPORT(:)
      integer(kind=kint ), pointer :: STACK_EXPORT(:)
!
      integer(kind=kint ) :: iter, MAXIT
!
!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
!
      NEIBPETOT =     MG_comm(0)%num_neib
      NEIBPE =>       MG_comm(0)%id_neib
      STACK_IMPORT => MG_comm(0)%istack_import
      STACK_EXPORT => MG_comm(0)%istack_export
      NOD_IMPORT =>   MG_comm(0)%item_import
!
      N =      matNN(0)%internal_diag
      D =>     matNN(0)%D
      AL =>    matNN(0)%AL
      AU =>    matNN(0)%AU
!
      MAXIT= ITR
      TOL  = EPS
      S1_TIME= MPI_WTIME()
!
!$omp workshare
      W(1:NB*NP,1:ntotWK_CG) = 0.0d0
!$omp end workshare
!
      call reset_solver_time
!
!C
!C-- change B,X

      call change_order_2_solve_bxn(NP, NB, PEsmpTOT,                   &
     &    djds_tbl(0)%STACKmcG, djds_tbl(0)%NEWtoOLD, B, X, W(1,iWK))

      call clear_vector_solve_11(NP, W(1,3) )

!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, djds_tbl(0)%NOD_EXPORT_NEW, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===
!
       call subtruct_matvec_nn                                          &
     &    (NP, NB, djds_tbl(0)%NLmax, djds_tbl(0)%NUmax,                &
     &     djds_tbl(0)%itotal_l, djds_tbl(0)%itotal_u,                  &
     &     djds_tbl(0)%npLX1, djds_tbl(0)%npUX1, djds_tbl(0)%NHYP,      &
     &     PEsmpTOT, djds_tbl(0)%STACKmcG, djds_tbl(0)%STACKmc,         &
     &     djds_tbl(0)%NLmaxHYP, djds_tbl(0)%NUmaxHYP,                  &
     &     djds_tbl(0)%OLDtoNEW_DJDS_L, djds_tbl(0)%OLDtoNEW_DJDS_U,    &
     &     djds_tbl(0)%NEWtoOLD_DJDS_U, djds_tbl(0)%LtoU,               &
     &     djds_tbl(0)%indexDJDS_L, djds_tbl(0)%indexDJDS_U,            &
     &     djds_tbl(0)%itemDJDS_L, djds_tbl(0)%itemDJDS_U, D, AL, AU,   &
     &     W(1,R), B, X, W(1,iWK))
!
!C
!C +---------------+
!C | BNORM2 = B^2  |
!C +---------------+
!C===
      call cal_local_norm_n                                             &
     &   (NP, NB, PEsmpTOT, djds_tbl(0)%STACKmcG, B, BNRM20)
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

      if (BNRM2.eq.0.d0) BNRM2= 1.d0
!C===
      do iter= 1, MAXIT
!C
!C************************************************* Conjugate Gradient Iteration

!C
!C +----------------+
!C | {z}= [Minv]{r} |
!C +----------------+
!C===
!
      call clear_vector_solve_nn(NP, NB, W(1,Z) )
!
!C
!C-- Multigtrid preconditioning
!
       call s_MGCGnn_V_cycle(num_MG_level, MG_comm, MG_itp,             &
     &     djds_tbl, matNN, MG_vect, PEsmpTOT, NP, NB, W(1,R), W(1,Z),  &
     &     iter_mid, iter_lowest, EPS_MG, METHOD_MG, PRECOND_MG, IER,   &
     &     W(1,1))
!
!C
!C +---------------+
!C | {RHO}= {r}{z} |
!C +---------------+
!C===
!
      call cal_local_s_product_n(NP, NB, PEsmpTOT,                      &
     &    djds_tbl(0)%STACKmcG, W(1,R), W(1,Z), RHO0)
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (RHO0, RHO, 1, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!C===

!C
!C +-----------------------------+
!C | {p} = {z} if      ITER=1    |
!C | BETA= RHO / RHO1  otherwise |
!C | {p} = {z} + BETA*{p}        |
!C +-----------------------------+
!C===
        if ( ITER.eq.1 ) then
          call copy_internal_vect_n_smp(NP, NB, PEsmpTOT,               &
     &        djds_tbl(0)%STACKmcG, W(1,P), W(1,Z) )
        else
          call djds_z_plus_beta_p_nn(NP, NB, PEsmpTOT,                  &
     &        djds_tbl(0)%STACKmcG, W(1,P), W(1,Z), RHO, RHO1)
        endif
!C===
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, djds_tbl(0)%NOD_EXPORT_NEW, W(1,P))
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!C
!C +-------------+
!C | {q}= [A]{p} |
!C +-------------+
!C===        
!
      call cal_matvec_nn                                                &
     &   (NP, NB, djds_tbl(0)%NLmax, djds_tbl(0)%NUmax,                 &
     &    djds_tbl(0)%itotal_l, djds_tbl(0)%itotal_u,                   &
     &    djds_tbl(0)%npLX1, djds_tbl(0)%npUX1, djds_tbl(0)%NHYP,       &
     &    PEsmpTOT, djds_tbl(0)%STACKmcG, djds_tbl(0)%STACKmc,          &
     &    djds_tbl(0)%NLmaxHYP, djds_tbl(0)%NUmaxHYP,                   &
     &    djds_tbl(0)%OLDtoNEW_DJDS_L, djds_tbl(0)%OLDtoNEW_DJDS_U,     &
     &    djds_tbl(0)%NEWtoOLD_DJDS_U, djds_tbl(0)%LtoU,                &
     &    djds_tbl(0)%indexDJDS_L, djds_tbl(0)%indexDJDS_U,             &
     &    djds_tbl(0)%itemDJDS_L, djds_tbl(0)%itemDJDS_U, D, AL, AU,    &
     &    W(1,Q), W(1,P), W(1,iWK))
!
!C===
!C +---------------------+
!C | ALPHA= RHO / {p}{q} |
!C +---------------------+
!C===
!
        call cal_local_s_product_n(NP, NB, PEsmpTOT,                    &
     &      djds_tbl(0)%STACKmcG, W(1,P), W(1,Q), C10)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (C10, C1, 1, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
        ALPHA= RHO / C1
!C===

!C
!C +----------------------+
!C | {x}= {x} + ALPHA*{p} |
!C | {r}= {r} - ALPHA*{q} |
!C | norm= {r}^2          |
!C +----------------------+
!C===
!
        call djds_x_and_residual_CG_nn                                  &
     &     (NP, NB, PEsmpTOT, djds_tbl(0)%STACKmcG,                     &
     &      DNRM20, X, W(1,R), W(1,P), W(1,Q), ALPHA)
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (DNRM20, DNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
      RESID= dsqrt(DNRM2/BNRM2)

      if (IER.eq.1 .and. my_rank.eq.0) write (12,'(a23,i5,1p2e16.6)')   &
     &            'solver_VMGCGnn_DJDS_SMP: ', ITER, RESID
      ITR = ITER

        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
        end if

        RHO1 = RHO

      enddo

!C===
   30 continue

!C
!C== change B,X
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_N                                           &
     &   ( NP, NB, NEIBPETOT, NEIBPE, STACK_IMPORT, NOD_IMPORT,         &
     &     STACK_EXPORT, djds_tbl(0)%NOD_EXPORT_NEW, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C== change B,X

      call back_2_original_order_bxn                                    &
     &   (NP, NB, djds_tbl(0)%NEWtoOLD, B, X, W(1,iWK))

      IER = 0
      E1_TIME= MPI_WTIME()
      COMPtime= E1_TIME - S1_TIME
      R1= 100.d0 * ( 1.d0 - COMMtime/COMPtime )
      if (my_rank.eq.0) then
        open(41,file='solver_11.dat',position='append')
        write (41,'(i7,1p3e16.6)') ITER, COMPtime, COMMtime, R1
        close(41)
      endif

      return
      end subroutine solve_VMGCGnn_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module     solver_VMGCGnn_DJDS_SMP
