!
!     module copy_for_MHD_solvers
!
!        programmed by H.Matsui on June 2010
!
!!      subroutine copy_ff_to_rhs33                                     &
!!     &         (numnod, inod_smp_stack, ff, b_vec, x_vec)
!!      subroutine copy_ff_to_rhs11                                     &
!!     &         (numnod, inod_smp_stack, ff, b_vec, x_vec)
!!      subroutine copy_ff_potential_to_rhs(numnod, inod_smp_stack,     &
!!     &          ncomp_nod, i_field, d_nod, ff, b_vec, x_vec)
!!      subroutine copy_solver_vec_to_vector(numnod, inod_smp_stack,    &
!!     &          ncomp_nod, i_field, x_vec, d_nod)
!!      subroutine copy_solver_vec_to_scalar(numnod, inod_smp_stack,    &
!!     &          ncomp_nod, i_field, x_vec, d_nod)
!
      module copy_for_MHD_solvers
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_ff_to_rhs33                                       &
     &         (numnod, inod_smp_stack, ff, b_vec, x_vec)
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: ff(numnod,3)
!
      real(kind = kreal), intent(inout) :: b_vec(3*numnod)
      real(kind = kreal), intent(inout) :: x_vec(3*numnod)
!
      integer (kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1)+1
        ied = inod_smp_stack(ip)
!cdir nodep
        do inod = ist, ied
          b_vec(3*inod-2) = ff(inod,1)
          b_vec(3*inod-1) = ff(inod,2)
          b_vec(3*inod  ) = ff(inod,3)
          x_vec(3*inod-2) = ff(inod,1)
          x_vec(3*inod-1) = ff(inod,2)
          x_vec(3*inod  ) = ff(inod,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ff_to_rhs33
!
!-----------------------------------------------------------------------
!
      subroutine copy_ff_to_rhs11                                       &
     &         (numnod, inod_smp_stack, ff, b_vec, x_vec)
!
      integer (kind = kint), intent(in) :: numnod
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: ff(numnod,3)
!
      real(kind = kreal), intent(inout) :: b_vec(numnod)
      real(kind = kreal), intent(inout) :: x_vec(numnod)
!
      integer (kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1)+1
        ied = inod_smp_stack(ip)
!cdir nodep
        do inod = ist, ied
          b_vec(inod) = ff(inod,1)
          x_vec(inod) = ff(inod,1)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ff_to_rhs11
!
!-----------------------------------------------------------------------
!
      subroutine copy_ff_potential_to_rhs(numnod, inod_smp_stack,       &
     &          ncomp_nod, i_field, d_nod, ff, b_vec, x_vec)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_field
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: d_nod(numnod,ncomp_nod)
      real(kind = kreal), intent(in) :: ff(numnod,3)
!
      real(kind = kreal), intent(inout) :: b_vec(numnod)
      real(kind = kreal), intent(inout) :: x_vec(numnod)
!
      integer (kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1)+1
        ied = inod_smp_stack(ip)
!cdir nodep
        do inod = ist, ied
          b_vec(inod) = ff(inod,1)
          x_vec(inod) = d_nod(inod,i_field)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ff_potential_to_rhs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_solver_vec_to_vector(numnod, inod_smp_stack,      &
     &          ncomp_nod, i_field, x_vec, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_field
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(inout) :: x_vec(3*numnod)
!
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1)+1
        ied = inod_smp_stack(ip)
!cdir nodep
        do inod = ist, ied
          d_nod(inod,i_field  ) = x_vec(3*inod-2)
          d_nod(inod,i_field+1) = x_vec(3*inod-1)
          d_nod(inod,i_field+2) = x_vec(3*inod  )
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_solver_vec_to_vector
!
!-----------------------------------------------------------------------
!
      subroutine copy_solver_vec_to_scalar(numnod, inod_smp_stack,      &
     &          ncomp_nod, i_field, x_vec, d_nod)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod, i_field
      integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(inout) :: x_vec(numnod)
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: ip, ist, ied, inod
!
!
!$omp parallel do private(ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1)+1
        ied = inod_smp_stack(ip)
!cdir nodep
        do inod = ist, ied
          d_nod(inod,i_field  ) = x_vec(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_solver_vec_to_scalar
!
!-----------------------------------------------------------------------
!
      end module copy_for_MHD_solvers
