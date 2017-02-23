!C
!C*** 
!C*** module solver_VMGCG11_DJDS_SMP
!C***
!
!      Written by Kengo Nakajima on May., 2001
!      Modified by H. Matsui on Nov. 2005
!
!C
!C***
!C***  VMGCG11_DJDS_SMP
!C***
!      subroutine VMGCG11_DJDS_SMP(num_MG_level, MG_comm, MG_itp,       &
!     &          djds_tbl, mat11, MG_vect, PEsmpTOT, NP, B, X,          &
!     &          MAXIT, ITR, iter_mid, iter_lowest, EPS, EPS_MG,        &
!     &          PRECOND, METHOD_MG, PRECOND_MG, IER, iterPREmax)
!
!      subroutine init_VMGCG11_DJDS_SMP(NP, PEsmpTOT, PRECOND,          &
!     &           METHOD_MG, PRECOND_MG, iterPREmax)
!      subroutine solve_VMGCG11_DJDS_SMP(num_MG_level, MG_comm, MG_itp, &
!     &          djds_tbl, mat11, MG_vect, PEsmpTOT, NP, B, X,          &
!     &          MAXIT, ITR, iter_mid, iter_lowest, EPS, EPS_MG,        &
!     &          PRECOND, METHOD_MG, PRECOND_MG, IER)
!      integer(kind = kint), intent(in) :: num_MG_level
!      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
!      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
!      type(DJDS_MATRIX), intent(in) ::         mat11(0:num_MG_level)
!      type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
!      integer(kind = kint), intent(in) :: PEsmpTOT
!      integer(kind = kint), intent(in) :: NP
!      real(kind = kreal), intent(in), target :: B(NP)
!      real(kind = kreal), intent(inout), target :: X(NP)
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
      module solver_VMGCG11_DJDS_SMP
!
      use m_precision
!
      implicit none
!
       real(kind = kreal), allocatable :: W(:,:)
       private :: W
       private :: verify_work_4_matvec11
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_matvec11(NP, nwk)
!
      integer(kind = kint), intent(in) :: NP, nwk
!
!
      if(allocated(W) .eqv. .false.) then
        allocate ( W(NP,nwk) )
        W = 0.0d0
      else if(size(W) .lt. (nwk*NP)) then
        deallocate (W)
        allocate ( W(NP,nwk) )
        W = 0.0d0
      end if
!
      end subroutine verify_work_4_matvec11
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!C
      subroutine VMGCG11_DJDS_SMP(num_MG_level, MG_comm, MG_itp,        &
     &          djds_tbl, mat11, MG_vect, PEsmpTOT, NP, B, X,           &
     &          MAXIT, ITR, iter_mid, iter_lowest, EPS, EPS_MG,         &
     &          PRECOND, METHOD_MG, PRECOND_MG, IER, iterPREmax)
!
      use calypso_mpi
!
      use t_comm_table
      use t_interpolate_table
      use t_solver_djds
      use t_vector_for_solver
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) ::         mat11(0:num_MG_level)
      type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP
      real(kind = kreal), intent(inout), target :: B(NP)
      real(kind = kreal), intent(inout), target :: X(NP)
      type(vectors_4_solver), intent(inout) :: MG_vect(0:num_MG_level)
!
      character (len=kchara), intent(in) :: PRECOND
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      integer(kind = kint), intent(in) :: iter_mid,  iter_lowest
      real(kind = kreal), intent(in) :: EPS
      real(kind = kreal), intent(in) :: EPS_MG
      integer(kind=kint ), intent(in) :: MAXIT
      integer(kind=kint ), intent(inout) :: ITR, IER
      integer(kind=kint ), intent(in)  :: iterPREmax
!
!
      call init_VMGCG11_DJDS_SMP(NP, PEsmpTOT, PRECOND,                 &
     &    METHOD_MG, PRECOND_MG, iterPREmax)
!
      call solve_VMGCG11_DJDS_SMP(num_MG_level, MG_comm, MG_itp,        &
     &          djds_tbl, mat11, MG_vect, PEsmpTOT, NP, B, X,           &
     &          MAXIT, ITR, iter_mid, iter_lowest, EPS, EPS_MG,         &
     &          PRECOND, METHOD_MG, PRECOND_MG, IER)
!
      end subroutine VMGCG11_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      subroutine init_VMGCG11_DJDS_SMP(NP, PEsmpTOT, PRECOND,           &
     &           METHOD_MG, PRECOND_MG, iterPREmax)
!
      use m_work_4_CG
      use djds_matrix_calcs_11
      use incomplete_cholesky_11
      use MGCG11_V_cycle
!
      integer(kind=kint ), intent(in) :: NP, PEsmpTOT
      character (len=kchara), intent(in) :: PRECOND
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      integer(kind=kint ), intent(in)  :: iterPREmax
!
!   allocate work arrays
!
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
        if(iterPREmax .ge. 1) ntotWK_CG = ntotWK_CG + 2
      end if
!
      call verify_work_4_matvec11(NP, ntotWK_CG)
      call init_MGCG11_V_cycle(NP, PEsmpTOT, METHOD_MG, PRECOND_MG)
!
      end subroutine init_VMGCG11_DJDS_SMP
!
!  ---------------------------------------------------------------------
!C
      subroutine solve_VMGCG11_DJDS_SMP(num_MG_level, MG_comm, MG_itp,  &
     &          djds_tbl, mat11, MG_vect, PEsmpTOT, NP, B, X,           &
     &          MAXIT, ITR, iter_mid, iter_lowest, EPS, EPS_MG,         &
     &          PRECOND, METHOD_MG, PRECOND_MG, IER)
!
      use calypso_mpi
      use solver_SR
!
      use t_comm_table
      use t_interpolate_table
      use t_solver_djds
      use t_vector_for_solver
!
      use m_work_4_CG
      use m_solver_count_time
!
      use djds_norm_products_11
      use vector_calc_solver_11
      use djds_matrix_calcs_11
      use incomplete_cholesky_11
      use diagonal_scaling_11
      use jacobi_precondition_11
      use gauss_zeidel_11
      use calcs_4_CG
      use MGCG11_V_cycle
      use solver_DJDS11_struct
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) ::         mat11(0:num_MG_level)
      type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP
      real(kind = kreal), intent(inout), target :: B(NP)
      real(kind = kreal), intent(inout), target :: X(NP)
      type(vectors_4_solver), intent(inout) :: MG_vect(0:num_MG_level)
!
      character (len=kchara), intent(in) :: PRECOND
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      integer(kind = kint), intent(in) :: iter_mid,  iter_lowest
      real(kind = kreal), intent(in) :: EPS
      real(kind = kreal), intent(in) :: EPS_MG
      integer(kind=kint ), intent(in) :: MAXIT
      integer(kind=kint ), intent(inout) :: ITR, IER
!
!
      integer(kind=kint ) :: iter
!
!C +-------+
!C | INIT. |
!C +-------+
!C===
!
      TOL  = EPS
      S1_TIME= MPI_WTIME()
!
!$omp workshare
      W(1:NP,1:ntotWK_CG) = 0.0d0
!$omp end workshare
!
      call reset_solver_time
!
!C
!C-- change B,X

      write(*,*) 'change_order_2_solve_bx1'
       call change_order_2_solve_bx1                                    &
     &    (NP, PEsmpTOT, djds_tbl(0)%STACKmcG, djds_tbl(0)%NEWtoOLD,    &
     &     B, X, W(1,iWK))

      write(*,*) 'clear_vector_solve_11'
       call clear_vector_solve_11(NP, W(1,3) )

!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV                                             &
     &   ( NP, MG_comm(0)%num_neib, MG_comm(0)%id_neib,                 &
     &     MG_comm(0)%istack_import, MG_comm(0)%item_import,            &
     &     MG_comm(0)%istack_export, djds_tbl(0)%NOD_EXPORT_NEW, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===
!
      write(*,*) 'subtruct_matvec_11'
       call subtruct_matvec_11                                          &
     &    (NP, djds_tbl(0)%NLmax, djds_tbl(0)%NUmax,                    &
     &     djds_tbl(0)%itotal_l, djds_tbl(0)%itotal_u,                  &
     &     djds_tbl(0)%npLX1, djds_tbl(0)%npUX1, djds_tbl(0)%NHYP,      &
     &     PEsmpTOT, djds_tbl(0)%STACKmcG, djds_tbl(0)%STACKmc,         &
     &     djds_tbl(0)%NLmaxHYP, djds_tbl(0)%NUmaxHYP,                  &
     &     djds_tbl(0)%OLDtoNEW_DJDS_L, djds_tbl(0)%OLDtoNEW_DJDS_U,    &
     &     djds_tbl(0)%NEWtoOLD_DJDS_U, djds_tbl(0)%LtoU,               &
     &     djds_tbl(0)%indexDJDS_L, djds_tbl(0)%indexDJDS_U,            &
     &     djds_tbl(0)%itemDJDS_L, djds_tbl(0)%itemDJDS_U,              &
     &     mat11(0)%aiccg(1), mat11(0)%aiccg(mat11%istart_l),           &
     &     mat11(0)%aiccg(mat11%istart_u), W(1,R), B, X, W(1,iWK))
!
!C
!C +---------------+
!C | BNORM2 = B^2  |
!C +---------------+
!C===
      call djds_local_norm_1                                            &
     &   (NP, PEsmpTOT, djds_tbl(0)%STACKmcG, B, BNRM20)
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
       call clear_vector_solve_11(NP, W(1,Z) )
!
!C
!C-- Multigtrid preconditioning
!
        call s_MGCG11_V_cycle(num_MG_level, MG_comm, MG_itp,            &
     &          djds_tbl, mat11, MG_vect, PEsmpTOT, NP, W(1,R), W(1,Z), &
     &          iter_mid, iter_lowest, EPS_MG,                          &
     &          METHOD_MG, PRECOND_MG, IER, W(1,1))
      write(*,*) 'IER', IER
!
!C
!C +---------------+
!C | {RHO}= {r}{z} |
!C +---------------+
!C===
!
      write(*,*) 'djds_local_s_product_1'
!       do ist = 1, NP
!         write(*,*)  ist, W(ist,R), W(ist,Z)
!       end do
      call djds_local_s_product_1(NP, PEsmpTOT, djds_tbl(0)%STACKmcG,   &
     &           W(1,R), W(1,Z), RHO0)
        write(*,*)  'RHO0', RHO0
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
          call copy_internal_vect_1_smp                                 &
     &       (NP, PEsmpTOT, djds_tbl(0)%STACKmcG, W(1,P), W(1,Z) )
        else
          call djds_z_plus_beta_p_11(NP, PEsmpTOT,                      &
     &        djds_tbl(0)%STACKmcG, W(1,P), W(1,Z), RHO, RHO1)
        endif
!C===
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV                                             &
     &   (NP, MG_comm(0)%num_neib, MG_comm(0)%id_neib,                  &
     &    MG_comm(0)%istack_import, MG_comm(0)%item_import,             &
     &    MG_comm(0)%istack_export, djds_tbl(0)%NOD_EXPORT_NEW, W(1,P))
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!C
!C +-------------+
!C | {q}= [A]{p} |
!C +-------------+
!C===        
!
      call cal_matvec_11                                                &
     &   (NP, djds_tbl(0)%NLmax, djds_tbl(0)%NUmax,                     &
     &    djds_tbl(0)%itotal_l, djds_tbl(0)%itotal_u,                   &
     &    djds_tbl(0)%npLX1, djds_tbl(0)%npUX1, djds_tbl(0)%NHYP,       &
     &    PEsmpTOT, djds_tbl(0)%STACKmcG, djds_tbl(0)%STACKmc,          &
     &    djds_tbl(0)%NLmaxHYP, djds_tbl(0)%NUmaxHYP,                   &
     &    djds_tbl(0)%OLDtoNEW_DJDS_L, djds_tbl(0)%OLDtoNEW_DJDS_U,     &
     &    djds_tbl(0)%NEWtoOLD_DJDS_U, djds_tbl(0)%LtoU,                &
     &    djds_tbl(0)%indexDJDS_L, djds_tbl(0)%indexDJDS_U,             &
     &    djds_tbl(0)%itemDJDS_L, djds_tbl(0)%itemDJDS_U,               &
     &    mat11(0)%aiccg(1), mat11(0)%aiccg(mat11%istart_l),            &
     &    mat11(0)%aiccg(mat11%istart_u), W(1,Q), W(1,P), W(1,iWK))
!
!C===
!C +---------------------+
!C | ALPHA= RHO / {p}{q} |
!C +---------------------+
!C===
!
        call djds_local_s_product_1(NP, PEsmpTOT,                       &
     &      djds_tbl(0)%STACKmcG, W(1,P), W(1,Q), C10)
        write(*,*)  'RHO, C10', RHO, C10
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (C10, C1, 1, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
        ALPHA= RHO / C1
!C===

!       do ist = 1, NP
!         write(*,*)  ist, W(ist,P), W(ist,Q)
!       end do
!C
!C +----------------------+
!C | {x}= {x} + ALPHA*{p} |
!C | {r}= {r} - ALPHA*{q} |
!C | norm= {r}^2          |
!C +----------------------+
!C===
!
        call djds_x_and_residual_CG_11                                  &
     &     (NP, PEsmpTOT, djds_tbl(0)%STACKmcG, DNRM20,                 &
     &      X, W(1,R), W(1,P), W(1,Q), ALPHA)
!       do ist = 1, NP
!         write(*,*)  ist, X(ist), W(ist,R)
!       end do
      write(*,*) 'DNRM20', DNRM20
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (DNRM20, DNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
      RESID= dsqrt(DNRM2/BNRM2)

      write(*,*) 'RESID', RESID, TOL
      write(*,*) 'ITER', ITER, MAXIT, IER
      if (IER.eq.1 .and. my_rank.eq.0) write (12,'(a23,i5,1p2e16.6)')   &
     &            'solver_VMGCG11_DJDS_SMP: ', ITER, RESID
      ITR = ITER

        if ( RESID.le.TOL   ) go to 30
        if ( ITER .eq. MAXIT) then
          IER = -300
          exit
        end if

        RHO1 = RHO

      end do

!C===
   30 continue
      write(*,*) 'exit loop',  RESID, TOL
!C
!C== change B,X
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV                                             &
     &   ( NP, MG_comm(0)%num_neib, MG_comm(0)%id_neib,                 &
     &     MG_comm(0)%istack_import, MG_comm(0)%item_import,            &
     &     MG_comm(0)%istack_export, djds_tbl(0)%NOD_EXPORT_NEW, X)
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME

!C
!C== change B,X

       call back_2_original_order_bx1(NP, djds_tbl(0)%NEWtoOLD,         &
     &     B, X, W(1,iWK))

      IER = 0
      E1_TIME= MPI_WTIME()
      COMPtime= E1_TIME - S1_TIME
      R1= 100.d0 * ( 1.d0 - COMMtime/COMPtime )
      if (my_rank.eq.0) then
        open(41,file='solver_11.dat',position='append')
        write (41,'(i7,1p3e16.6)') ITER, COMPtime, COMMtime, R1
        close(41)
      end if

      return
      end subroutine solve_VMGCG11_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module     solver_VMGCG11_DJDS_SMP
