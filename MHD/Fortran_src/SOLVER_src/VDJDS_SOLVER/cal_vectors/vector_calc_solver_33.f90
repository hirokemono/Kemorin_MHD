!
!      module vector_calc_solver_33
!
!     Written by Kemorin
!
!      subroutine copy_internal_vector_3(N, NP, S, V)
!      subroutine copy_vector_33(NP, S, V)
!      subroutine copy_vector_2x33(NP, S1, S2, V1, V2)
!      subroutine add_vector_33(NP, S, V)
!      subroutine add_vector_3x33(NP, S1, S2, S3, V1, V2, V3)
!      subroutine clear_vector_solve_33(NP, S)
!      subroutine clear_vector_solve_3x33(NP, S1, S2, S3)
!      subroutine clear_external_solve_33(N, NP, S)
!      subroutine clear_external_solve_3x33(N, NP, S1, S2, S3)
!
      module vector_calc_solver_33
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
      subroutine copy_internal_vector_3(N, NP, S, V)
!
       integer(kind = kint), intent(in) :: N, NP
       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: i
!
!
!$omp parallel do
       do i= 1, N
         S(3*i-2) = V(3*i-2)
         S(3*i-1) = V(3*i-1)
         S(3*i  ) = V(3*i  )
       enddo
!$omp end parallel do
!
      end subroutine copy_internal_vector_3
!
!  ---------------------------------------------------------------------
!
      subroutine copy_vector_33(NP, S, V)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: i
!
!$omp parallel do
!voption indep (S,V)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
           S(3*i-2) = V(3*i-2)
           S(3*i-1) = V(3*i-1)
           S(3*i  ) = V(3*i  )
      enddo
!$omp end parallel do
!
      end subroutine copy_vector_33
!
!  ---------------------------------------------------------------------
!
      subroutine copy_vector_2x33(NP, S1, S2, V1, V2)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP)
!
       integer (kind = kint) :: i
!
!$omp parallel do
!voption indep (S1,S2,V1,V2)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
           S1(3*i-2) = V1(3*i-2)
           S1(3*i-1) = V1(3*i-1)
           S1(3*i  ) = V1(3*i  )
           S2(3*i-2) = V2(3*i-2)
           S2(3*i-1) = V2(3*i-1)
           S2(3*i  ) = V2(3*i  )
      enddo
!$omp end parallel do
!
      end subroutine copy_vector_2x33
!
!  ---------------------------------------------------------------------
!
      subroutine add_vector_33(NP, S, V)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(in) :: V(3*NP)
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: i
!
!$omp parallel do
!voption indep (S,V)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
           S(3*i-2) = S(3*i-2) + V(3*i-2)
           S(3*i-1) = S(3*i-1) + V(3*i-1)
           S(3*i  ) = S(3*i  ) + V(3*i  )
      enddo
!$omp end parallel do
!
      end subroutine add_vector_33
!
!  ---------------------------------------------------------------------
!
      subroutine add_vector_3x33(NP, S1, S2, S3, V1, V2, V3)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(in) :: V1(3*NP), V2(3*NP), V3(3*NP)
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP), S3(3*NP)
!
       integer (kind = kint) :: i
!
!$omp parallel do
!voption indep (S1,S2,S3,V1,V2,V3)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
           S1(3*i-2) = S1(3*i-2) + V1(3*i-2)
           S1(3*i-1) = S1(3*i-1) + V1(3*i-1)
           S1(3*i  ) = S1(3*i  ) + V1(3*i  )
           S2(3*i-2) = S2(3*i-2) + V2(3*i-2)
           S2(3*i-1) = S2(3*i-1) + V2(3*i-1)
           S2(3*i  ) = S2(3*i  ) + V2(3*i  )
           S3(3*i-2) = S3(3*i-2) + V3(3*i-2)
           S3(3*i-1) = S3(3*i-1) + V3(3*i-1)
           S3(3*i  ) = S3(3*i  ) + V3(3*i  )
      enddo
!$omp end parallel do
!
      end subroutine add_vector_3x33
!
!  ---------------------------------------------------------------------
!
      subroutine clear_vector_solve_33(NP, S)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: i
!
!$omp parallel do
!voption indep (S)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
           S(3*i-2) = 0.0d0
           S(3*i-1) = 0.0d0
           S(3*i  ) = 0.0d0
         enddo
!$omp end parallel do
!
      end subroutine clear_vector_solve_33
!
!  ---------------------------------------------------------------------
!
      subroutine clear_vector_solve_3x33(NP, S1, S2, S3)
!
       integer(kind = kint), intent(in) :: NP
       real(kind = kreal), intent(inout) :: S1(3*NP), S2(3*NP), S3(3*NP)
!
       integer (kind = kint) :: i
!
!$omp parallel do
!voption indep (S1,S2,S3)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= 1, NP
           S1(3*i-2) = 0.0d0
           S1(3*i-1) = 0.0d0
           S1(3*i  ) = 0.0d0
           S2(3*i-2) = 0.0d0
           S2(3*i-1) = 0.0d0
           S2(3*i  ) = 0.0d0
           S3(3*i-2) = 0.0d0
           S3(3*i-1) = 0.0d0
           S3(3*i  ) = 0.0d0
         enddo
!$omp end parallel do
!
      end subroutine clear_vector_solve_3x33
!
!  ---------------------------------------------------------------------
!
      subroutine clear_external_solve_33(N, NP, S)
!
       integer(kind = kint), intent(in) :: N, NP
       real(kind = kreal), intent(inout) :: S(3*NP)
!
       integer (kind = kint) :: i
!
!$omp parallel do
!voption indep (S)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= N+1, NP
        S(3*i-2) = 0.0d0
        S(3*i-1) = 0.0d0
        S(3*i  ) = 0.0d0
      enddo
!$omp end parallel do
!
      end subroutine clear_external_solve_33
!
!  ---------------------------------------------------------------------
!
      subroutine clear_external_solve_3x33(N, NP, S1, S2, S3)
!
       integer(kind = kint), intent(in) :: N, NP
       real(kind = kreal), intent(inout) :: S1(3*NP), S3(3*NP), S2(3*NP)
!
       integer (kind = kint) :: i
!
!$omp parallel do
!voption indep (S1,S2,S3)
!OCL VECTOR, NOVREC
!cdir nodep
      do i= N+1, NP
        S1(3*i-2) = 0.0d0
        S1(3*i-1) = 0.0d0
        S1(3*i  ) = 0.0d0
        S2(3*i-2) = 0.0d0
        S2(3*i-1) = 0.0d0
        S2(3*i  ) = 0.0d0
        S3(3*i-2) = 0.0d0
        S3(3*i-1) = 0.0d0
        S3(3*i  ) = 0.0d0
      enddo
!$omp end parallel do
!
      end subroutine clear_external_solve_3x33
!
!  ---------------------------------------------------------------------
!
      end module vector_calc_solver_33
