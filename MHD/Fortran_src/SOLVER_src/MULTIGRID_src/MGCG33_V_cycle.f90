!MGCG33_V_cycle.f90
!      module MGCG33_V_cycle
!
!     Written by Kemorin
!
!      subroutine init_MGCG33_V_cycle(NP, PEsmpTOT,                     &
!     &          METHOD_MG, PRECOND_MG, my_rank)
!
!      subroutine s_MGCG33_V_cycle(num_MG_level, MG_comm, MG_itp,       &
!     &          djds_tbl, mat33, MG_vect, PEsmpTOT, NP, B, X,          &
!     &          iter_mid, iter_lowest, EPS_MG, my_rank,                &
!     &          METHOD_MG, PRECOND_MG, IER)
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
!       integer(kind = kint), intent(in) :: my_rank
!       character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
!       integer(kind = kint), intent(in) :: iter_mid,  iter_lowest
!       real(kind = kreal), intent(in) :: EPS_MG
!
!       integer(kind = kint), intent(inout) :: IER
!
      module MGCG33_V_cycle
!
      use m_precision
!
      use m_solver_count_time
      use t_interpolate_table
      use t_solver_djds
      use t_vector_for_solver
!
      use djds_matrix_calcs_33
!
      implicit none
!
      logical, private :: print_residual_on_each_level = .true.
!
       private :: cal_residual33_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_MGCG33_V_cycle(NP, PEsmpTOT,                      &
     &          METHOD_MG, PRECOND_MG, my_rank)
!
      use m_constants
      use solver_DJDS33_struct
!
      integer(kind = kint), intent(in) :: NP, PEsmpTOT, my_rank
      character(len=kchara), intent(in) :: METHOD_MG, PRECOND_MG
      integer(kind = kint) :: ierr
!
!
      call init33_DJDS_struct(NP, PEsmpTOT, METHOD_MG, PRECOND_MG,      &
     &    my_rank, ierr)
!
      end subroutine init_MGCG33_V_cycle
!
!  ---------------------------------------------------------------------
!
      subroutine s_MGCG33_V_cycle(num_MG_level, MG_comm, MG_itp,        &
     &          djds_tbl, mat33, MG_vect, PEsmpTOT, NP, B, X,           &
     &          iter_mid, iter_lowest, EPS_MG, my_rank,                 &
     &          METHOD_MG, PRECOND_MG, IER)
!
      use calypso_mpi
!
      use m_constants
      use t_comm_table
      use solver_DJDS33_struct
      use interpolate_type_3
      use empty_solver_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(communication_table), intent(in) :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in) :: djds_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) ::         mat33(0:num_MG_level)
      type(MG_itp_table), intent(in) ::       MG_itp(num_MG_level)
!
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP
      real(kind = kreal), intent(in) :: B(3*NP)
!
      real(kind = kreal), intent(inout) :: X(3*NP)
      type(vectors_4_solver), intent(inout) :: MG_vect(0:num_MG_level)
!
      integer(kind = kint), intent(in) :: my_rank
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
        if(my_rank .eq. 0) write(*,*) 'copy vector'
!$omp parallel do
      do i = 1, 3*NP
        MG_vect(0)%b_vec(i) = B(i)
        MG_vect(0)%x_vec(i) = X(i)
      end do
!$omp end parallel do
!
        if(my_rank .eq. 0) write(*,*) 'back_2_original_order_bx3'
      call back_2_original_order_bx3(NP, djds_tbl(0)%NEWtoOLD,          &
     &    MG_vect(0)%b_vec, MG_vect(0)%x_vec)
!
!C restrict the residual vector
      DO i = 0, num_MG_level-1
        NP_f = mat33(i  )%num_diag
        NP_c = mat33(i+1)%num_diag
        if(my_rank .eq. 0) write(*,*) 's_interpolate_type_3 level ', i
        call s_interpolate_type_3(NP_f, NP_c, MG_comm(i+1),             &
     &      MG_itp(i+1)%f2c, MG_vect(i)%b_vec, MG_vect(i+1)%b_vec,      &
     &      PEsmpTOT)
        MG_vect(i+1)%x_vec(1:NP_c) = zero
      end do
!
!C calculate residual
      if(print_residual_on_each_level) Then
        call cal_residual33_type(djds_tbl(0), mat33(0), MG_vect(0),     &
     &      PEsmpTOT, resd)
        if(my_rank .eq. 0) write(*,*) '0-th level, pre ', resd
      end if
!
      do i = 0, num_MG_level-1
!
        NP_f = mat33(i  )%num_diag
        NP_c = mat33(i+1)%num_diag
        ierr = IER
        if(NP_f.gt.0) then
          call solve33_DJDS_struct(PEsmpTOT, MG_comm(i),                &
     &      djds_tbl(i), mat33(i), NP_f, MG_vect(i)%b_vec,              &
     &      MG_vect(i)%x_vec, METHOD_MG, PRECOND_MG, ierr,              &
     &      EPS_MG, iter_mid, iter_res, my_rank)
        else
          call empty_solve_DJDS_kemo(EPS_MG, iter_mid, iter_res, ierr,  &
     &        my_rank, METHOD_MG)
        end if
!
        call s_interpolate_type_3(NP_f, NP_c, MG_comm(i+1),             &
     &      MG_itp(i+1)%f2c, MG_vect(i)%x_vec, MG_vect(i+1)%x_vec,      &
     &       PEsmpTOT)
      end do
!
!    at the coarsest level
!
      i = num_MG_level
      NP_f = mat33(i  )%num_diag
      ierr = IER
      if(NP_f.gt.0) then
        call solve33_DJDS_struct(PEsmpTOT, MG_comm(i),                  &
     &      djds_tbl(i), mat33(i), NP_f, MG_vect(i)%b_vec,              &
     &      MG_vect(i)%x_vec, METHOD_MG, PRECOND_MG, ierr,              &
     &      EPS_MG, iter_lowest, iter_res, my_rank)
      else
        call empty_solve_DJDS_kemo(EPS_MG, iter_lowest, iter_res, ierr, &
     &        my_rank, METHOD_MG)
       end if
!
!
      do i = num_MG_level-1, 0, -1
        NP_f = mat33(i  )%num_diag
        NP_c = mat33(i+1)%num_diag
        call s_interpolate_type_3(NP_c, NP_f, MG_comm(i),               &
     &      MG_itp(i+1)%c2f,  MG_vect(i+1)%x_vec, MG_vect(i)%x_vec,     &
     &      PEsmpTOT)
!
!C calculate residual
        if(print_residual_on_each_level) Then
          call cal_residual33_type(djds_tbl(i), mat33(i), MG_vect(i),   &
     &      PEsmpTOT, resd)
          if(my_rank .eq. 0) write(*,*) i, 'th level, pre ', resd
        end if
!
        ierr = IER
        if(NP_f.gt.0) then
          call solve33_DJDS_struct(PEsmpTOT, MG_comm(i),                &
     &      djds_tbl(i), mat33(i), NP_f, MG_vect(i)%b_vec,              &
     &      MG_vect(i)%x_vec, METHOD_MG, PRECOND_MG, ierr,              &
     &      EPS_MG, iter_lowest, iter_res, my_rank)
        else
          call empty_solve_DJDS_kemo(EPS_MG, iter_lowest, iter_res,     &
     &        ierr, my_rank, METHOD_MG)
        end if
      end do
!
      call change_order_2_solve_bx3(NP, PEsmpTOT, djds_tbl(0)%STACKmcG, &
     &    djds_tbl(0)%NEWtoOLD, MG_vect(0)%b_vec, MG_vect(0)%x_vec)
!
!$omp parallel do
      do i = 1, 3*NP
        X(i) = MG_vect(0)%x_vec(i)
      end do
!$omp end parallel do
!
      end subroutine s_MGCG33_V_cycle
!
!  ---------------------------------------------------------------------
!
      subroutine cal_residual33_type(djds_tbl, mat33, MG_vect,          &
     &          PEsmpTOT, resd)
!
      use calypso_mpi
!
      use m_constants
      use m_work_4_MGCG33
      use cal_norm_products_33
!
      type(DJDS_ordering_table), intent(in) :: djds_tbl
      type(DJDS_MATRIX), intent(in) ::      mat33
!
      type(vectors_4_solver), intent(inout) :: MG_vect
      integer(kind = kint), intent(in) :: PEsmpTOT
      real(kind = kreal), intent(inout) :: resd
!
!
      call change_order_2_solve_bx3(mat33%num_diag, PEsmpTOT,           &
            djds_tbl%STACKmcG, djds_tbl%NEWtoOLD,                       &
     &      MG_vect%b_vec, MG_vect%x_vec)
!
!C calculate residual
        call subtruct_matvec_33                                         &
     &       (mat33%num_diag, djds_tbl%NLmax, djds_tbl%NUmax,           &
     &       djds_tbl%itotal_l, djds_tbl%itotal_u,                      &
     &       djds_tbl%npLX1, djds_tbl%npUX1, djds_tbl%NHYP, PEsmpTOT,   &
     &       djds_tbl%STACKmcG, djds_tbl%STACKmc,                       &
     &       djds_tbl%NLmaxHYP, djds_tbl%NUmaxHYP,                      &
     &       djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,        &
     &       djds_tbl%NEWtoOLD_DJDS_U, djds_tbl%LtoU,                   &
     &       djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                &
     &       djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                  &
     &       mat33%D, mat33%AL,  mat33%AU, W(1,ZQ),                     &
     &       MG_vect%b_vec, MG_vect%x_vec)
!
      call back_2_original_order_bx3(mat33%num_diag, djds_tbl%NEWtoOLD, &
     &    MG_vect%b_vec, MG_vect%x_vec)
!
        BNRM20=zero
        call cal_local_norm_3(mat33%num_diag, PEsmpTOT,                 &
     &      djds_tbl%STACKmcG, W(1,ZQ), BNRM20, DNRMsmp)
!
        START_TIME= MPI_WTIME()
        call MPI_allREDUCE (BNRM20, resd, 1, MPI_DOUBLE_PRECISION,      &
     &        MPI_SUM, CALYPSO_COMM, ierr)
        END_TIME= MPI_WTIME()
        COMMtime = COMMtime + END_TIME - START_TIME
!
      end subroutine cal_residual33_type
!
!  ---------------------------------------------------------------------
!
      end module MGCG33_V_cycle
