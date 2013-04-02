!
!      module vector_calc_solver_11
!
!     Written by Kemorin
!
!      subroutine copy_internal_vector_1(N, NP, S, V)
!      subroutine copy_vector_11(NP, S, V)
!      subroutine copy_vector_2x11(NP, S1, S2, V1, V2)
!      subroutine add_vector_11(NP, S, V)
!      subroutine add_vector_3x11(NP, S1, S2, S3, V1, V2, V3)
!      subroutine clear_vector_solve_11(NP, S)
!      subroutine clear_vector_solve_3x11(NP, S1, S2, S3)
!      subroutine clear_external_solve_11(N, NP, S)
!      subroutine clear_external_solve_3x11(N, NP, S1, S2, S3)
!
      module vector_calc_solver_11
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
!
      subroutine copy_internal_vector_1(N, NP, S, V)
!
       integer(kind = kint), intent(in) :: N, NP
       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: i
!
!
!$omp parallel do
       do i= 1, N
         S(i) = V(i)
       enddo
!$omp end parallel do
!
      end subroutine copy_internal_vector_1
!
!  ---------------------------------------------------------------------
!
      subroutine copy_vector_11(NP, S, V)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: i
!
!
!$omp parallel do
!voption indep (S,V)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
           S(i) = V(i)
      enddo
!$omp end parallel do
!
      end subroutine copy_vector_11
!
!  ---------------------------------------------------------------------
!
      subroutine copy_vector_2x11(NP, S1, S2, V1, V2)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(in) :: V1(NP), V2(NP)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP)
!
       integer (kind = kint) :: i
!
!
!$omp parallel do
!voption indep (S1,S2,V1,V2)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
           S1(i) = V1(i)
           S2(i) = V2(i)
      enddo
!$omp end parallel do
!
      end subroutine copy_vector_2x11
!
!  ---------------------------------------------------------------------
!
      subroutine add_vector_11(NP, S, V)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(in) :: V(NP)
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: i
!
!
!$omp parallel do
!voption indep (S,V)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
           S(i) = S(i) + V(i)
      enddo
!$omp end parallel do
!
      end subroutine add_vector_11
!
!  ---------------------------------------------------------------------
!
      subroutine add_vector_3x11(NP, S1, S2, S3, V1, V2, V3)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(in) :: V1(NP), V2(NP), V3(NP)
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer (kind = kint) :: i
!
!
!$omp parallel do
!voption indep (S1,S2,S3,V1,V2,V3)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
           S1(i) = S1(i) + V1(i)
           S2(i) = S2(i) + V2(i)
           S3(i) = S3(i) + V3(i)
      enddo
!$omp end parallel do
!
      end subroutine add_vector_3x11
!
!  ---------------------------------------------------------------------
!
      subroutine clear_vector_solve_11(NP, S)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: i
!
!
!$omp parallel do
!voption indep (S)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
        S(i) = 0.0d0
      enddo
!$omp end parallel do
!
      end subroutine clear_vector_solve_11
!
!  ---------------------------------------------------------------------
!
      subroutine clear_vector_solve_3x11(NP, S1, S2, S3)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer (kind = kint) :: i
!
!
!$omp parallel do
!voption indep (S1,S2,S3)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
        S1(i) = 0.0d0
        S2(i) = 0.0d0
        S3(i) = 0.0d0
      enddo
!$omp end parallel do
!
      end subroutine clear_vector_solve_3x11
!
!  ---------------------------------------------------------------------
!
      subroutine clear_external_solve_11(N, NP, S)
!
       integer(kind = kint), intent(in) :: N, NP
       real(kind = kreal), intent(inout) :: S(NP)
!
       integer (kind = kint) :: i
!
!
!$omp parallel do
!voption indep (S)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= N+1, NP
        S(i) = 0.0d0
      enddo
!$omp end parallel do
!
      end subroutine clear_external_solve_11
!
!  ---------------------------------------------------------------------
!
      subroutine clear_external_solve_3x11(N, NP, S1, S2, S3)
!
       integer(kind = kint), intent(in) :: N, NP
       real(kind = kreal), intent(inout) :: S1(NP), S2(NP), S3(NP)
!
       integer (kind = kint) :: i
!
!
!$omp parallel do
!voption indep (S1,S2,S3)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= N+1, NP
        S1(i) = 0.0d0
        S2(i) = 0.0d0
        S3(i) = 0.0d0
      enddo
!$omp end parallel do
!
      end subroutine clear_external_solve_3x11
!
!  ---------------------------------------------------------------------
!
      end module vector_calc_solver_11
