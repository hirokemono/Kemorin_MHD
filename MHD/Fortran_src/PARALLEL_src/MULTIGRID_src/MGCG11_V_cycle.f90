!MGCG11_V_cycle.f90
!      module MGCG11_V_cycle
!
!     Written by Kemorin
!
!      subroutine init_MGCG11_V_cycle(NP, PEsmpTOT,                     &
!     &          METHOD_MG, PRECOND_MG)
!      subroutine s_MGCG11_V_cycle(num_MG_level, MG_comm, MG_itp,       &
!     &          djds_tbl, mat11, MG_vect, PEsmpTOT, NP, B, X,          &
!     &          iter_mid, iter_lowest, EPS_MG,                         &
!     &          METHOD_MG, PRECOND_MG, IER, W)
!       integer(kind = kint), intent(in) :: num_MG_level
!       type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
!       type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
!       type(DJDS_MATRIX), intent(in) ::         mat11(0:num_MG_level)
!       type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
!       integer(kind = kint), intent(in) :: PEsmpTOT
!       integer(kind = kint), intent(in) :: NP
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
      module MGCG11_V_cycle
!
      use m_precision
!
      use m_solver_count_time
      use t_interpolate_table
      use t_solver_djds
      use t_vector_for_solver
      use djds_matrix_calcs_11
!
      implicit none
!
      logical, private :: print_residual_on_each_level = .true.
!
       private :: cal_residual11_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_MGCG11_V_cycle(NP, PEsmpTOT,                      &
     &          METHOD_MG, PRECOND_MG)
!
      use m_constants
      use solver_DJDS11_struct
!
      integer(kind = kint), intent(in) :: NP, PEsmpTOT
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      integer(kind = kint) :: ierr
!
!
      call init_DJDS11_struct(NP, PEsmpTOT, METHOD_MG, PRECOND_MG,      &
     &    ierr)
!
      end subroutine init_MGCG11_V_cycle
!
!  ---------------------------------------------------------------------
!
      subroutine s_MGCG11_V_cycle(num_MG_level, MG_comm, MG_itp,        &
     &          djds_tbl, mat11, MG_vect, PEsmpTOT, NP, B, X,           &
     &          iter_mid, iter_lowest, EPS_MG,                          &
     &          METHOD_MG, PRECOND_MG, IER, W)
!
      use calypso_mpi
!
      use m_constants
      use m_work_4_CG
      use t_comm_table
      use solver_DJDS11_struct
      use interpolate_by_type
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) ::         mat11(0:num_MG_level)
      type(MG_itp_table), intent(in) ::        MG_itp(num_MG_level)
!
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP
      real(kind = kreal), intent(in) :: B(NP)
!
      real(kind = kreal), intent(inout) :: X(NP)
      type(vectors_4_solver), intent(inout) :: MG_vect(0:num_MG_level)
!
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      integer(kind = kint), intent(in) :: iter_mid,  iter_lowest
      real(kind = kreal), intent(in) :: EPS_MG
!
      integer(kind = kint), intent(inout) :: IER
      real(kind = kreal), intent(inout) :: W(NP*ntotWK_CG)
!
!
      integer(kind = kint) :: NP_f, NP_c
      integer(kind = kint) :: i, j, iter_res, ierr
      real(kind = kreal) :: resd
!
!
!$omp parallel do
      do i = 1, NP
        MG_vect(0)%b_vec(i) = B(i)
        MG_vect(0)%x_vec(i) = X(i)
      end do
!$omp end parallel do
!
      call back_2_original_order_bx1(NP, djds_tbl(0)%NEWtoOLD,          &
     &    MG_vect(0)%b_vec, MG_vect(0)%x_vec, W(1))
!
!C restrict the residual vector
      DO i = 0, num_MG_level-1
        NP_f = mat11(i  )%num_diag
        NP_c = mat11(i+1)%num_diag
        ierr = IER
        call interpolate_type_1(NP_f, NP_c, MG_comm(i+1),               &
     &      MG_itp(i+1)%f2c, MG_vect(i)%b_vec, MG_vect(i+1)%b_vec,      &
     &      PEsmpTOT)
        MG_vect(i+1)%x_vec(1:NP_c) = zero
      end do
!
!
!C calculate residual
      if(print_residual_on_each_level) Then
        call cal_residual11_type(djds_tbl(0), mat11(0), MG_vect(0),     &
     &      PEsmpTOT, resd, W(1))
        if(my_rank .eq. 0) write(*,*) '0-th level, pre ', resd
      end if
!
      do i = 0, num_MG_level-1
!
        NP_f = mat11(i  )%num_diag
        NP_c = mat11(i+1)%num_diag
        ierr = IER
!
        write(*,*) 'solve_DJDS11_struct', i, my_rank
        call solve_DJDS11_struct(PEsmpTOT, MG_comm(i),                  &
     &      djds_tbl(i), mat11(i), NP_f, MG_vect(i)%b_vec,              &
     &      MG_vect(i)%x_vec, METHOD_MG, PRECOND_MG, ierr,              &
     &      EPS_MG, iter_mid, iter_res)
!
!        write(*,*) 'j, MG_vect(i)%x_vec(j)', i
!        do j = 1, NP_f
!          write(*,*) j, MG_vect(i)%x_vec(j), MG_vect(i)%b_vec(j)
!        end do
!
        write(*,*) 'interpolate_type_1 restriction', i
        call interpolate_type_1(NP_f, NP_c, MG_comm(i+1),               &
     &      MG_itp(i+1)%f2c, MG_vect(i)%x_vec, MG_vect(i+1)%x_vec,      &
     &      PEsmpTOT)
      end do
!
!    at the coarsest level
!
!      write(*,*) 'j, MG_vect(i)%x_vec(j)', i
!      do j = 1, NP_c
!        write(*,*) j, MG_vect(i)%x_vec(j)
!      end do
!
      i = num_MG_level
      NP_c = mat11(i  )%num_diag
      ierr = IER
!
      write(*,*) 'solve_DJDS11_struct', i
      call solve_DJDS11_struct(PEsmpTOT, MG_comm(i),                    &
     &      djds_tbl(i), mat11(i), NP_c, MG_vect(i)%b_vec,              &
     &      MG_vect(i)%x_vec, METHOD_MG, PRECOND_MG, ierr,              &
     &      EPS_MG, iter_lowest, iter_res)
!
!
      do i = num_MG_level-1, 0, -1
        NP_f = mat11(i  )%num_diag
        NP_c = mat11(i+1)%num_diag
        ierr = IER
        write(*,*) 'interpolate_type_1 interpolation', i
        call interpolate_type_1(NP_c, NP_f, MG_comm(i),                 &
     &      MG_itp(i+1)%c2f, MG_vect(i+1)%x_vec, MG_vect(i)%x_vec,      &
     &      PEsmpTOT)
!
!        write(*,*) 'j, MG_vect(i)%x_vec(j)', i
!        do j = 1, NP_f
!          write(*,*) j, MG_vect(i)%x_vec(j)
!        end do
!
!C calculate residual
        if(print_residual_on_each_level) Then
        write(*,*) 'cal_residual11_type', i
          call cal_residual11_type(djds_tbl(i), mat11(i), MG_vect(i),   &
     &      PEsmpTOT, resd, W(1))
          if(my_rank .eq. 0) write(*,*) i, 'th level, pre ', resd
        end if
!
        write(*,*) 'solve_DJDS11_struct', i
        call solve_DJDS11_struct(PEsmpTOT, MG_comm(i),                  &
     &      djds_tbl(i), mat11(i), NP_f, MG_vect(i)%b_vec,              &
     &      MG_vect(i)%x_vec, METHOD_MG, PRECOND_MG, ierr,              &
     &      EPS_MG, iter_lowest, iter_res)
!
!        write(*,*) 'j, MG_vect(i)%x_vec(j)', i
!        do j = 1, NP_f
!          write(*,*) j, MG_vect(i)%x_vec(j), MG_vect(i)%b_vec(j)
!        end do
      end do
!
      call change_order_2_solve_bx1(NP, PEsmpTOT, djds_tbl(0)%STACKmcG, &
     &    djds_tbl(0)%NEWtoOLD, MG_vect(0)%b_vec, MG_vect(0)%x_vec,     &
     &    W(1))
!
!$omp parallel do
      do i = 1, NP
        X(i) = MG_vect(0)%x_vec(i)
      end do
!$omp end parallel do
!
      end subroutine s_MGCG11_V_cycle
!
!  ---------------------------------------------------------------------
!
      subroutine cal_residual11_type(djds_tbl, mat11, MG_vect,          &
     &          PEsmpTOT, resd, W)
!
      use calypso_mpi
!
      use m_constants
      use m_work_4_CG
      use djds_norm_products_11
!
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(in) ::      mat11
!
      type(vectors_4_solver), intent(inout) :: MG_vect
      integer(kind = kint), intent(in) :: PEsmpTOT
      real(kind = kreal), intent(inout) :: resd
      real(kind = kreal), intent(inout) :: W(mat11%num_diag,ntotWK_CG)
!
!
      call change_order_2_solve_bx1(mat11%num_diag, PEsmpTOT,           &
            djds_tbl%STACKmcG, djds_tbl%NEWtoOLD,                       &
     &      MG_vect%b_vec, MG_vect%x_vec, W(1,iWK))
!
!C calculate residual
        call subtruct_matvec_11                                         &
     &       (mat11%num_diag, djds_tbl%NLmax, djds_tbl%NUmax,           &
     &       djds_tbl%itotal_l, djds_tbl%itotal_u,                      &
     &       djds_tbl%npLX1, djds_tbl%npUX1, djds_tbl%NHYP, PEsmpTOT,   &
     &       djds_tbl%STACKmcG, djds_tbl%STACKmc,                       &
     &       djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,                      &
     &       djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,        &
     &       djds_tbl%NEWtoOLD_DJDS_U, djds_tbl%LtoU,                   &
     &       djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                &
     &       djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                  &
     &       mat11%D, mat11%AL,  mat11%AU, W(1,ZQ),                     &
     &       MG_vect%b_vec, MG_vect%x_vec, W(1,iWK))
!
      call back_2_original_order_bx1(mat11%num_diag, djds_tbl%NEWtoOLD, &
     &    MG_vect%b_vec, MG_vect%x_vec, W(1,iWK))
!
        BNRM20=zero
        call djds_local_norm_1(mat11%num_diag, PEsmpTOT,                &
     &      djds_tbl%STACKmcG, W(1,ZQ), BNRM20)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (BNRM20, resd, 1, CALYPSO_REAL,              &
     &        MPI_SUM, CALYPSO_COMM, ierr_MPI)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!
      end subroutine cal_residual11_type
!
!  ---------------------------------------------------------------------
!
      end module MGCG11_V_cycle
