!MGCGnn_V_cycle.f90
!      module MGCGnn_V_cycle
!
!     Written by Kemorin
!
!      subroutine init_MGCGnn_V_cycle(NP, NB, PEsmpTOT,                 &
!     &          METHOD_MG, PRECOND_MG)
!
!      subroutine s_MGCGnn_V_cycle(num_MG_level, MG_comm, MG_itp,       &
!     &          djds_tbl, matNN, MG_vect, PEsmpTOT, NP, NB, B, X,      &
!     &          iter_mid, iter_lowest, EPS_MG,                         &
!     &          METHOD_MG, PRECOND_MG, IER)
!       integer(kind = kint), intent(in) :: num_MG_level
!       type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
!       type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
!       type(DJDS_MATRIX), intent(in) ::         mat11(0:num_MG_level)
!       type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
!       integer(kind = kint), intent(in) :: PEsmpTOT
!       integer(kind = kint), intent(in) :: NP, NB
!       real(kind = kreal), intent(in), target :: B(NP)
!
!       real(kind = kreal), intent(inout), target :: X(NP)
!       type(vectors_4_solver), intent(inout) :: MG_vect(0:num_MG_level)
!
!       character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
!       integer(kind = kint), intent(in) :: iter_mid,  iter_lowest
!       real(kind = kreal), intent(in) :: EPS_MG
!
!       integer(kind = kint), intent(inout) :: IER
!
      module MGCGnn_V_cycle
!
      use m_precision
!
      use m_solver_count_time
      use t_interpolate_table
      use t_solver_djds
      use t_vector_for_solver
      use m_work_4_MGCGnn
!
      implicit none
!
      logical, private :: print_residual_on_each_level = .true.
!
       private :: cal_residualnn_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_MGCGnn_V_cycle(NP, NB, PEsmpTOT,                  &
     &          METHOD_MG, PRECOND_MG)
!
      use m_constants
      use solver_DJDSnn_struct
!
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP, NB
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      integer(kind = kint) :: ierr
!
!
      call initNN_DJDS_struct(NP, NB, PEsmpTOT, METHOD_MG, PRECOND_MG,  &
     &   ierr)
!
      end subroutine init_MGCGnn_V_cycle
!
!  ---------------------------------------------------------------------
!
      subroutine s_MGCGnn_V_cycle(num_MG_level, MG_comm, MG_itp,        &
     &          djds_tbl, matNN, MG_vect, PEsmpTOT, NP, NB, B, X,       &
     &          iter_mid, iter_lowest, EPS_MG,                          &
     &          METHOD_MG, PRECOND_MG, IER)
!
      use calypso_mpi
!
      use m_constants
      use t_comm_table
      use solver_DJDSnn_struct
      use interpolate_type_N
      use empty_solver_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) ::         matNN(0:num_MG_level)
      type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP, NB
      real(kind = kreal), intent(in) :: B(NB*NP)
!
      real(kind = kreal), intent(inout) :: X(NB*NP)
      type(vectors_4_solver), intent(inout) :: MG_vect(0:num_MG_level)
!
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      real(kind = kreal), intent(in) :: EPS_MG
      integer(kind = kint), intent(in) :: iter_mid,  iter_lowest
      integer(kind = kint), intent(inout) :: IER
!
      integer(kind = kint) :: NP_f, NP_c
      integer(kind = kint) :: i, iter_res, ierr
      real(kind = kreal) :: resd
!
!
!$omp parallel do
      do i = 1, NB*NP
        MG_vect(0)%b_vec(i) = B(i)
        MG_vect(0)%x_vec(i) = X(i)
      end do
!$omp end parallel do
!
      call back_2_original_order_bxn(NP, NB, djds_tbl(0)%NEWtoOLD,      &
     &    MG_vect(0)%b_vec, MG_vect(0)%x_vec)
!
!C restrict the residual vector
      DO i = 0, num_MG_level-1
        NP_f = matNN(i  )%num_diag
        NP_c = matNN(i+1)%num_diag
        call s_interpolate_type_N(NP_f, NP_c, NB, MG_comm(i+1),         &
     &      MG_itp(i+1)%f2c, MG_vect(i)%b_vec, MG_vect(i+1)%b_vec,      &
     &      PEsmpTOT)
        MG_vect(i+1)%x_vec(1:NP_c) = zero
      end do
!
!C calculate residual
      if(print_residual_on_each_level) Then
        call cal_residualnn_type(djds_tbl(0), matNN(0), MG_vect(0),     &
     &      PEsmpTOT, NB, resd)
        if(my_rank .eq. 0) write(*,*) '0-th level, pre ', resd
      end if
!
      do i = 0, num_MG_level-1
!
        NP_f = matNN(i  )%num_diag
        NP_c = matNN(i+1)%num_diag
        ierr = IER
        if(NP_f.gt.0) then
          call solveNN_DJDS_struct(NB, PEsmpTOT, MG_comm(i),            &
     &      djds_tbl(i), matNN(i),NP_f, MG_vect(i)%b_vec,               &
     &      MG_vect(i)%x_vec, METHOD_MG, PRECOND_MG, ierr,              &
     &      EPS_MG, iter_mid, iter_res)
         else
          call empty_solve_DJDS_kemo(EPS_MG, iter_mid, iter_res, ierr,  &
     &        METHOD_MG)
         end if
!
        call s_interpolate_type_N(NP_f, NP_c, NB, MG_comm(i+1),         &
     &      MG_itp(i+1)%f2c, MG_vect(i)%x_vec, MG_vect(i+1)%x_vec,      &
     &      PEsmpTOT)
      end do
!
!    at the coarsest level
!
      i = num_MG_level
      NP_f = matNN(i  )%num_diag
      ierr = IER
      if(NP_f.gt.0) then
        call solveNN_DJDS_struct(NB, PEsmpTOT, MG_comm(i),              &
     &      djds_tbl(i), matNN(i), NP_f, MG_vect(i)%b_vec,              &
     &      MG_vect(i)%x_vec, METHOD_MG, PRECOND_MG, ierr,              &
     &      EPS_MG, iter_lowest, iter_res)
      else
        call empty_solve_DJDS_kemo(EPS_MG, iter_lowest, iter_res, ierr, &
     &      METHOD_MG)
      end if
!
!
      do i = num_MG_level-1, 0, -1
        NP_f = matNN(i  )%num_diag
        NP_c = matNN(i+1)%num_diag
        call s_interpolate_type_N(NP_c, NP_f, NB, MG_comm(i),           &
     &       MG_itp(i+1)%c2f, MG_vect(i+1)%x_vec, MG_vect(i)%x_vec,     &
     &       PEsmpTOT)
!
!C calculate residual
        if(print_residual_on_each_level) Then
          call cal_residualnn_type(djds_tbl(i), matNN(i), MG_vect(i),   &
     &        PEsmpTOT, NB, resd)
          if(my_rank .eq. 0) write(*,*) i, 'th level, pre ', resd
        end if
!
        ierr = IER
        if(NP_f.gt.0) then
          call solveNN_DJDS_struct(NB, PEsmpTOT, MG_comm(i),            &
     &      djds_tbl(i), matNN(i),NP_f, MG_vect(i)%b_vec,               &
     &      MG_vect(i)%x_vec, METHOD_MG, PRECOND_MG, ierr,              &
     &      EPS_MG, iter_lowest, iter_res)
         else
          call empty_solve_DJDS_kemo(EPS_MG, iter_lowest, iter_res,     &
     &        ierr, METHOD_MG)
         end if
      end do
!
      call change_order_2_solve_bxn(NP, NB, PEsmpTOT,                   &
     &    djds_tbl(0)%STACKmcG, djds_tbl(0)%NEWtoOLD,                   &
     &    MG_vect(0)%b_vec, MG_vect(0)%x_vec)
!
!$omp parallel do
      do i = 1, NB*NP
        X(i) = MG_vect(0)%x_vec(i)
      end do
!$omp end parallel do
!
      end subroutine s_MGCGnn_V_cycle
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_residualnn_type(djds_tbl, matNN, MG_vect,          &
     &          PEsmpTOT, NB, resd)
!
      use calypso_mpi
!
      use m_constants
      use m_work_4_MGCGnn
      use djds_matrix_calcs_nn
      use cal_norm_products_nn
!
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(in) ::      matNN
!
      type(vectors_4_solver), intent(inout) :: MG_vect
      integer(kind = kint), intent(in) :: NB, PEsmpTOT
      real(kind = kreal), intent(inout) :: resd
!
!
      call change_order_2_solve_bxn(matNN%num_diag, NB,  PEsmpTOT,      &
            djds_tbl%STACKmcG, djds_tbl%NEWtoOLD,                       &
     &      MG_vect%b_vec, MG_vect%x_vec)
!
!C calculate residual
        call subtruct_matvec_nn                                         &
     &       (matNN%num_diag, NB, djds_tbl%NLmax, djds_tbl%NUmax,       &
     &       djds_tbl%itotal_l, djds_tbl%itotal_u,                      &
     &       djds_tbl%npLX1, djds_tbl%npUX1, djds_tbl%NHYP, PEsmpTOT,   &
     &       djds_tbl%STACKmcG, djds_tbl%STACKmc,                       &
     &       djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,                      &
     &       djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,        &
     &       djds_tbl%NEWtoOLD_DJDS_U, djds_tbl%LtoU,                   &
     &       djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                &
     &       djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                  &
     &       matNN%D, matNN%AL, matNN%AU, W(1,ZQ),                      &
     &       MG_vect%b_vec, MG_vect%x_vec)
!
      call back_2_original_order_bxn(matNN%num_diag, NB,                &
     &    djds_tbl%NEWtoOLD, MG_vect%b_vec, MG_vect%x_vec)
!
        BNRM20=zero
        call cal_local_norm_n(matNN%num_diag, NB, PEsmpTOT,             &
     &      djds_tbl%STACKmcG, W(1,ZQ), BNRM20, DNRMsmp)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (BNRM20, resd, 1, CALYPSO_REAL,              &
     &        MPI_SUM, CALYPSO_COMM, ierr)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!
      end subroutine cal_residualnn_type
!
!  ---------------------------------------------------------------------
!
      end module MGCGnn_V_cycle
