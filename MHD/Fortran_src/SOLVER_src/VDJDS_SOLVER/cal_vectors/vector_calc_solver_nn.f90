!
!      module vector_calc_solver_nn
!
!     Written by Kemorin
!
!      subroutine copy_internal_vector_n(N, NP, NB, S, V)
!      subroutine copy_vector_nn(NP, NB, S, V)
!      subroutine copy_vector_2xnn(NP, NB, S1, S2, V1, V2)
!      subroutine add_vector_nn(NP, NB, S, V)
!      subroutine add_vector_3xnn(NP, NB, S1, S2, S3, V1, V2, V3)
!      subroutine clear_vector_solve_nn(NP, NB, S)
!      subroutine clear_vector_solve_3xnn(NP, NB, S1, S2, S3)
!      subroutine clear_external_solve_nn(N, NP, NB, S)
!      subroutine clear_external_solve_3xnn(N, NP, NB, S1, S2, S3)
!
      module vector_calc_solver_nn
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
      subroutine copy_internal_vector_n(N, NP, NB, S, V)
!
       integer(kind = kint), intent(in) :: N, NP, NB
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: i
!
      if(NP .le. 0) return
!
!$omp parallel do
       do i= 1, NB*N
         S(i) = V(i)
       enddo
!$omp end parallel do
!
      end subroutine copy_internal_vector_n
!
!  ---------------------------------------------------------------------
!
      subroutine copy_vector_nn(NP, NB, S, V)
!
       integer(kind = kint), intent(in) :: NP, NB
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: i
!
      if(NP .le. 0) return
!
!$omp parallel do
!voption indep (S,V)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NB*NP
           S(i) = V(i)
      enddo
!$omp end parallel do
!
      end subroutine copy_vector_nn
!
!  ---------------------------------------------------------------------
!
      subroutine copy_vector_2xnn(NP, NB, S1, S2, V1, V2)
!
       integer(kind = kint), intent(in) :: NP, NB
       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
!
       integer (kind = kint) :: i
!
      if(NP .le. 0) return
!
!$omp parallel do
!voption indep (S1,S2,V1,V2)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NB*NP
           S1(i) = V1(i)
           S2(i) = V2(i)
      end do
!$omp end parallel do
!
      end subroutine copy_vector_2xnn
!
!  ---------------------------------------------------------------------
!
      subroutine add_vector_nn(NP, NB, S, V)
!
       integer(kind = kint), intent(in) :: NP, NB
       real(kind = kreal), intent(in) :: V(NB*NP)
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: i
!
      if(NP .le. 0) return
!
!$omp parallel do
!voption indep (S,V)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NB*NP
           S(i) = S(i) + V(i)
      enddo
!$omp end parallel do
!
      end subroutine add_vector_nn
!
!  ---------------------------------------------------------------------
!
      subroutine add_vector_3xnn(NP, NB, S1, S2, S3, V1, V2, V3)
!
       integer(kind = kint), intent(in) :: NP, NB
       real(kind = kreal), intent(in) :: V1(NB*NP), V2(NB*NP)
       real(kind = kreal), intent(in) :: V3(NB*NP)
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: i
!
      if(NP .le. 0) return
!
!$omp parallel do
!voption indep (S1,S2,S3,V1,V2,V3)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NB*NP
           S1(i) = S1(i) + V1(i)
           S2(i) = S2(i) + V2(i)
           S3(i) = S3(i) + V3(i)
      enddo
!$omp end parallel do
!
      end subroutine add_vector_3xnn
!
!  ---------------------------------------------------------------------
!
      subroutine clear_vector_solve_nn(NP, NB, S)
!
       integer(kind = kint), intent(in) :: NP, NB
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: i
!
      if(NP .le. 0) return
!
!$omp parallel do
!voption indep (S)
!OCL VECTOR, NOVREC
!cdir nodep
       do i= 1, NB*NP
         S(i) = 0.0d0
       enddo
!$omp end parallel do
!
      end subroutine clear_vector_solve_nn
!
!  ---------------------------------------------------------------------
!
      subroutine clear_vector_solve_3xnn(NP, NB, S1, S2, S3)
!
       integer(kind = kint), intent(in) :: NP, NB
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: i
!
      if(NP .le. 0) return
!
!$omp parallel do
!voption indep (S1,S2,S3)
!OCL VECTOR, NOVREC
!cdir nodep
       do i= 1, NB*NP
         S1(i) = 0.0d0
         S2(i) = 0.0d0
         S3(i) = 0.0d0
       enddo
!$omp end parallel do
!
      end subroutine clear_vector_solve_3xnn
!
!  ---------------------------------------------------------------------
!
      subroutine clear_external_solve_nn(N, NP, NB, S)
!
       integer(kind = kint), intent(in) :: N, NP, NB
       real(kind = kreal), intent(inout) :: S(NB*NP)
!
       integer (kind = kint) :: i
!
!
      if(NP .le. 0) return
!
!$omp parallel do
!voption indep (S)
!OCL VECTOR, NOVREC
!cdir nodep
       do i= NB*N+1, NB*NP
         S(i) = 0.0d0
       enddo
!$omp end parallel do
!
      end subroutine clear_external_solve_nn
!
!  ---------------------------------------------------------------------
!
      subroutine clear_external_solve_3xnn(N, NP, NB, S1, S2, S3)
!
       integer(kind = kint), intent(in) :: N, NP, NB
       real(kind = kreal), intent(inout) :: S1(NB*NP), S2(NB*NP)
       real(kind = kreal), intent(inout) :: S3(NB*NP)
!
       integer (kind = kint) :: i
!
!
      if(NP .le. 0) return
!
!$omp parallel do
!voption indep (S1,S2,S3)
!OCL VECTOR, NOVREC
!cdir nodep
       do i= NB*N+1, NB*NP
         S1(i) = 0.0d0
         S2(i) = 0.0d0
         S3(i) = 0.0d0
       enddo
!$omp end parallel do
!
      end subroutine clear_external_solve_3xnn
!
!  ---------------------------------------------------------------------
!
      end module vector_calc_solver_nn
