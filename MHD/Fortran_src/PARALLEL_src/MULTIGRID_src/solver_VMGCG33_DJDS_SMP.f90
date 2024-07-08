!C
!C*** 
!C*** module solver_VMGCG33_DJDS_SMP
!C***
!
!      Written by Kengo Nakajima on May., 2001
!      Modified by H. Matsui on Nov. 2005
!
!C
!C***
!C***  VMGCG33_DJDS_SMP
!C***
!!      subroutine VMGCG33_DJDS_SMP(num_MG_level, MG_comm, MG_itp,      &
!!     &          djds_tbl, mat33, MG_vect, PEsmpTOT, NP, B, X,         &
!!     &          MAXIT, ITR, iter_mid, iter_lowest, EPS, EPS_MG,       &
!!     &          PRECOND, METHOD_MG, PRECOND_MG, IER, iterPREmax,      &
!!     &          SR_sig, SR_r, INITtime, COMPtime, COMPtime_CG,        &
!!     &          VCYCLEtime, COMMtime_MG)
!!
!!      subroutine init_VMGCG33_DJDS_SMP(NP, PEsmpTOT,                  &
!!     &          PRECOND, METHOD_MG, PRECOND_MG, iterPREmax, INITtime)
!!      subroutine solve_VMGCG33_DJDS_SMP(num_MG_level, MG_comm, MG_itp,&
!!     &          djds_tbl, mat33, MG_vect, PEsmpTOT, NP, B, X,         &
!!     &          MAXIT, ITR, iter_mid, iter_lowest, EPS, EPS_MG,       &
!!     &          PRECOND, METHOD_MG, PRECOND_MG, IER, SR_sig, SR_r)
!!      integer(kind = kint), intent(in) :: num_MG_level
!!      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
!!      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
!!      type(DJDS_MATRIX), intent(in) ::         mat33(0:num_MG_level)
!!      type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!!
!!      integer(kind = kint), intent(in) :: PEsmpTOT
!!      integer(kind = kint), intent(in) :: NP
!!      real(kind = kreal), intent(in), target :: B(3*NP)
!!      real(kind = kreal), intent(inout), target :: X(3*NP)
!!      type(vectors_4_solver), intent(inout) :: MG_vect(0:num_MG_level)
!!
!!      character (len=kchara), intent(in) :: PRECOND
!!      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
!!      integer(kind = kint), intent(in) :: iter_mid,  iter_lowest
!!      integer(kind=kint ), intent(in) :: MAXIT
!!      integer(kind=kint ), intent(inout) :: ITR, IER
!!      real(kind = kreal), intent(in) :: EPS
!!      real(kind = kreal), intent(in) :: EPS_MG
!!
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!C     VCG_DJDS_SMP solves the linear system Ax = b 
!C     using the Conjugate Gradient iterative method with preconditioning.
!C     Elements are ordered in descending Jagged Diagonal Storage
!C     for Vector Processing and Cyclic Ordering for SMP Parallel Computation
!C
      module solver_VMGCG33_DJDS_SMP
!
      use m_precision
      use t_solver_SR
!
      implicit none
!
      type MGCG33_work
        real(kind = kreal), allocatable :: W2(:,:)
      end type MGCG33_work
!
      integer(kind=kint), parameter, private :: nWK_CG =  4
      integer(kind = kint), private :: ntotWK_CG = nWK_CG + 3
      real(kind = kreal), allocatable, private :: W(:,:)

      private :: verify_work_4_matvec33
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine verify_work_4_matvec33(NP, nwk)
!
      integer(kind = kint), intent(in) :: NP, nwk
!
!
      if(allocated(W) .eqv. .false.) then
        allocate ( W(3*NP,nwk) )
        W = 0.0d0
      else if(size(W) .lt. (3*nwk*NP)) then
        deallocate (W)
        allocate ( W(3*NP,nwk) )
        W = 0.0d0
      end if
!
      end subroutine verify_work_4_matvec33
!
!  ---------------------------------------------------------------------
!C
      subroutine VMGCG33_DJDS_SMP(num_MG_level, MG_comm, MG_itp,        &
     &          djds_tbl, mat33, MG_vect, PEsmpTOT, NP, B, X,           &
     &          MAXIT, ITR, iter_mid, iter_lowest, EPS, EPS_MG,         &
     &          PRECOND, METHOD_MG, PRECOND_MG, IER, iterPREmax,        &
     &          SR_sig, SR_r, INITtime, COMPtime, COMPtime_CG,          &
     &          VCYCLEtime, COMMtime_MG)
!
      use calypso_mpi
      use solver_SR_3
!
      use t_comm_table
      use t_interpolate_table
      use t_solver_djds
      use t_vector_for_solver
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) ::         mat33(0:num_MG_level)
      type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP
      real(kind = kreal), intent(inout), target :: B(3*NP)
      real(kind = kreal), intent(inout), target :: X(3*NP)
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
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      real(kind = kreal), intent(inout) :: INITtime
      real(kind = kreal), intent(inout) :: COMPtime,   COMPtime_CG
      real(kind = kreal), intent(inout) :: VCYCLEtime, COMMtime_MG
!
      call init_VMGCG33_DJDS_SMP(NP, PEsmpTOT,                          &
     &    PRECOND, METHOD_MG, PRECOND_MG, iterPREmax, INITtime)
!
      call solve_VMGCG33_DJDS_SMP(num_MG_level, MG_comm, MG_itp,        &
     &          djds_tbl, mat33, MG_vect, PEsmpTOT, NP, B, X,           &
     &          MAXIT, ITR, iter_mid, iter_lowest, EPS, EPS_MG,         &
     &          PRECOND, METHOD_MG, PRECOND_MG, IER, SR_sig, SR_r,      &
     &          COMPtime, COMPtime_CG, VCYCLEtime, COMMtime_MG)
!
      end subroutine VMGCG33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!C
      subroutine init_VMGCG33_DJDS_SMP(NP, PEsmpTOT,                    &
     &          PRECOND, METHOD_MG, PRECOND_MG, iterPREmax, INITtime)
!
      use MGCG33_V_cycle
!
      integer(kind = kint), intent(in) :: NP, PEsmpTOT
      character (len=kchara), intent(in) :: PRECOND
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      integer(kind=kint ), intent(in)  :: iterPREmax
      real(kind = kreal), intent(inout) :: INITtime
!
!
      ntotWK_CG = nWK_CG + 3
      if (PRECOND(1:2).eq.'IC'  .or.                                    &
     &    PRECOND(1:3).eq.'ILU' .or. PRECOND(1:4).eq.'SSOR') then
        if(iterPREmax .ge. 1) ntotWK_CG = ntotWK_CG + 2
      end if
!
      call verify_work_4_matvec33(NP, ntotWK_CG)
      call init_MGCG33_V_cycle(NP, PEsmpTOT, METHOD_MG, PRECOND_MG,     &
     &                         INITtime)
!
      end subroutine init_VMGCG33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!C
      subroutine solve_VMGCG33_DJDS_SMP(num_MG_level, MG_comm, MG_itp,  &
     &          djds_tbl, mat33, MG_vect, PEsmpTOT, NP, B, X,           &
     &          MAXIT, ITR, iter_mid, iter_lowest, EPS, EPS_MG,         &
     &          PRECOND, METHOD_MG, PRECOND_MG, IER, SR_sig, SR_r,      &
     &          COMPtime, COMPtime_CG, VCYCLEtime, COMMtime_MG)
!
      use calypso_mpi
!
      use solver_SR_3
!
      use t_comm_table
      use t_interpolate_table
      use t_solver_djds
      use t_vector_for_solver
!
      use m_CG_constants
!
      use cal_norm_products_33
      use vector_calc_solver_33
      use djds_matrix_calcs_33
      use incomplete_cholesky_33
      use diagonal_scaling_33
      use jacobi_precondition_33
      use gauss_zeidel_33
      use calcs_4_CG
      use MGCG33_V_cycle
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) ::         mat33(0:num_MG_level)
      type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP
      real(kind = kreal), intent(inout), target :: B(3*NP)
      real(kind = kreal), intent(inout), target :: X(3*NP)
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
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      real(kind = kreal), intent(inout) :: COMPtime,   COMPtime_CG
      real(kind = kreal), intent(inout) :: VCYCLEtime, COMMtime_MG
!
      real(kind=kreal) :: START_TIME, S1_TIME, R1
!
      integer(kind = kint) :: iterPRE
!
      real(kind=kreal) :: RESID, TOL
      real(kind=kreal) :: BNRM2,  DNRM2,  C1,  RHO, RHO1, ALPHA
      real(kind=kreal) :: BNRM20, DNRM20, C10, RHO0
!
      integer(kind=kint ) :: iter
!
!
!C
!C +-------+
!C | INIT. |
!C +-------+
!C===
!
      TOL  = EPS
      S1_TIME= MPI_WTIME()
      COMMtime_MG = 0.0d0
      VCYCLEtime = 0.0d0
!
!$omp workshare
      W(1:3*NP,1:ntotWK_CG) = 0.0d0
!$omp end workshare
!C
!C-- change B,X

       call change_order_2_solve_bx3                                    &
     &    (NP, PEsmpTOT, djds_tbl(0)%STACKmcG, djds_tbl(0)%NEWtoOLD,    &
     &     B, X, W(1,iWK))

       call clear_vector_solve_33(NP, W(1,3) )

!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_3                                           &
     &   (NP, MG_comm(0)%num_neib, MG_comm(0)%id_neib,                  &
     &    MG_comm(0)%istack_import, MG_comm(0)%item_import,             &
     &    MG_comm(0)%istack_export, djds_tbl(0)%NOD_EXPORT_NEW,         &
     &    SR_sig, SR_r, X)
      COMMtime_MG = COMMtime_MG + MPI_WTIME() - START_TIME

!C
!C +-----------------------+
!C | {r0}= {b} - [A]{xini} |
!C +-----------------------+
!C===
!
       call subtruct_matvec_33                                          &
     &    (NP, djds_tbl(0)%NLmax, djds_tbl(0)%NUmax,                    &
     &     djds_tbl(0)%itotal_l, djds_tbl(0)%itotal_u,                  &
     &     djds_tbl(0)%npLX1, djds_tbl(0)%npUX1, djds_tbl(0)%NHYP,      &
     &     PEsmpTOT, djds_tbl(0)%STACKmcG, djds_tbl(0)%STACKmc,         &
     &     djds_tbl(0)%NLmaxHYP, djds_tbl(0)%NUmaxHYP,                  &
     &     djds_tbl(0)%OLDtoNEW_DJDS_L,djds_tbl(0)%OLDtoNEW_DJDS_U,     &
     &     djds_tbl(0)%NEWtoOLD_DJDS_U, djds_tbl(0)%LtoU,               &
     &     djds_tbl(0)%indexDJDS_L, djds_tbl(0)%indexDJDS_U,            &
     &     djds_tbl(0)%itemDJDS_L, djds_tbl(0)%itemDJDS_U,              &
     &     mat33(0)%aiccg(1), mat33(0)%aiccg(mat33%istart_l),           &
     &     mat33(0)%aiccg(mat33%istart_u), W(1,R), B, X, W(1,iWK))
!
!C
!C +---------------+
!C | BNORM2 = B^2  |
!C +---------------+
!C===
      call cal_local_norm_3                                             &
     &   (NP, PEsmpTOT, djds_tbl(0)%STACKmcG, B, BNRM20)
!
      START_TIME= MPI_WTIME()
      call MPI_allREDUCE (BNRM20, BNRM2, 1, CALYPSO_REAL,               &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
      COMMtime_MG = COMMtime_MG + MPI_WTIME() - START_TIME

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
        call clear_vector_solve_33(NP, W(1,Z) )
!
!C
!C-- Multigtrid preconditioning
!
        START_TIME = MPI_WTIME()
        call s_MGCG33_V_cycle(num_MG_level, MG_comm, MG_itp,            &
     &      djds_tbl, mat33, MG_vect, PEsmpTOT, NP, W(1,R), W(1,Z),     &
     &      iter_mid, iter_lowest, EPS_MG, METHOD_MG, PRECOND_MG,       &
     &      IER, ntotWK_CG, W(1,1), SR_sig, SR_r,                       &
     &      COMPtime_CG, COMMtime_MG)
        VCYCLEtime = VCYCLEtime + (MPI_WTIME() - START_TIME)
!
!C
!C +---------------+
!C | {RHO}= {r}{z} |
!C +---------------+
!C===
!
        call cal_local_s_product_3(NP, PEsmpTOT, djds_tbl(0)%STACKmcG,  &
     &                             W(1,R), W(1,Z), RHO0)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE(RHO0, RHO, 1, CALYPSO_REAL,                  &
     &                     MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime_MG = COMMtime_MG + MPI_WTIME() - START_TIME
!C===
!C
!C +-----------------------------+
!C | {p} = {z} if      ITER=1    |
!C | BETA= RHO / RHO1  otherwise |
!C | {p} = {z} + BETA*{p}        |
!C +-----------------------------+
!C===
        if ( ITER.eq.1 ) then
          call copy_internal_vect_3_smp(NP, PEsmpTOT,                   &
     &       djds_tbl(0)%STACKmcG, W(1,P), W(1,Z) )
        else
          call djds_z_plus_beta_p_33(NP, PEsmpTOT,                      &
     &        djds_tbl(0)%STACKmcG, W(1,P), W(1,Z), RHO, RHO1)
        endif
!C===
!C
!C-- INTERFACE data EXCHANGE
        START_TIME= MPI_WTIME()
        call SOLVER_SEND_RECV_3                                         &
     &     (NP, MG_comm(0)%num_neib, MG_comm(0)%id_neib,                &
     &      MG_comm(0)%istack_import, MG_comm(0)%item_import,           &
     &      MG_comm(0)%istack_export, djds_tbl(0)%NOD_EXPORT_NEW,       &
     &      SR_sig, SR_r, W(1,P))
        COMMtime_MG = COMMtime_MG + MPI_WTIME() - START_TIME
!C
!C +-------------+
!C | {q}= [A]{p} |
!C +-------------+
!C===        
!
      call cal_matvec_33                                                &
     &   (NP, djds_tbl(0)%NLmax, djds_tbl(0)%NUmax,                     &
     &    djds_tbl(0)%itotal_l, djds_tbl(0)%itotal_u,                   &
     &    djds_tbl(0)%npLX1, djds_tbl(0)%npUX1, djds_tbl(0)%NHYP,       &
     &    PEsmpTOT, djds_tbl(0)%STACKmcG, djds_tbl(0)%STACKmc,          &
     &    djds_tbl(0)%NLmaxHYP, djds_tbl(0)%NUmaxHYP,                   &
     &    djds_tbl(0)%OLDtoNEW_DJDS_L, djds_tbl(0)%OLDtoNEW_DJDS_U,     &
     &    djds_tbl(0)%NEWtoOLD_DJDS_U, djds_tbl(0)%LtoU,                &
     &    djds_tbl(0)%indexDJDS_L, djds_tbl(0)%indexDJDS_U,             &
     &    djds_tbl(0)%itemDJDS_L, djds_tbl(0)%itemDJDS_U,               &
     &    mat33(0)%aiccg(1), mat33(0)%aiccg(mat33%istart_l),            &
     &    mat33(0)%aiccg(mat33%istart_u), W(1,Q), W(1,P), W(1,iWK))
!
!C===
!C +---------------------+
!C | ALPHA= RHO / {p}{q} |
!C +---------------------+
!C===
!
        call cal_local_s_product_3(NP, PEsmpTOT, djds_tbl(0)%STACKmcG,  &
     &      W(1,P), W(1,Q), C10)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (C10, C1, 1, CALYPSO_REAL,                   &
     &                    MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime_MG = COMMtime_MG + MPI_WTIME() - START_TIME
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
        call djds_x_and_residual_CG_33                                  &
     &     (NP, PEsmpTOT, djds_tbl(0)%STACKmcG,                         &
     &      DNRM20, X, W(1,R), W(1,P), W(1,Q), ALPHA)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE(DNRM20, DNRM2, 1, CALYPSO_REAL,              &
     &                     MPI_SUM, CALYPSO_COMM, ierr_MPI)
        COMMtime_MG = COMMtime_MG + MPI_WTIME() - START_TIME
        RESID= dsqrt(DNRM2/BNRM2)

        if(IER.eq.1 .and. my_rank.eq.0) write (12,'(a23,i5,1p2e16.6)')  &
     &            'solver_VMGCG33_DJDS_SMP: ', ITER, RESID
        ITR = ITER

        if ( RESID.le.TOL   ) goto 30
        if ( ITER .eq. MAXIT ) then
          IER = -300
          exit
        end if

        RHO1 = RHO

      end do

!C===
   30 continue

!C
!C== change B,X
!C
!C-- INTERFACE data EXCHANGE
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_3                                           &
     &   (NP, MG_comm(0)%num_neib, MG_comm(0)%id_neib,                  &
     &    MG_comm(0)%istack_import, MG_comm(0)%item_import,             &
     &    MG_comm(0)%istack_export, djds_tbl(0)%NOD_EXPORT_NEW,         &
     &    SR_sig, SR_r, X)
      COMMtime_MG = COMMtime_MG + MPI_WTIME() - START_TIME

!C
!C== change B,X

      call back_2_original_order_bx3(NP, djds_tbl(0)%NEWtoOLD,          &
     &                               B, X, W(1,iWK))
      COMPtime= MPI_WTIME() - S1_TIME

      IER = 0
!
      R1= 100.d0 * (1.d0 - COMMtime_MG/COMPtime)
      if(my_rank .eq. 0) then
        open(41,file='solver_11.dat',position='append')
        write (41,'(i7,1p3e16.6)') ITR, COMPtime, COMMtime_MG, R1
        close(41)
      end if
!
      return
      end subroutine solve_VMGCG33_DJDS_SMP
!
!  ---------------------------------------------------------------------
!
      end module     solver_VMGCG33_DJDS_SMP
