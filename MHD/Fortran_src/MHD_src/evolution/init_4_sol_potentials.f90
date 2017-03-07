!
!      module init_4_sol_potentials
!
!      Written by H. Matsui on June, 2005
!
!      subroutine init_4_sol_k_potential(inod_smp_stack)
!      subroutine init_4_sol_m_potential(inod_smp_stack)
!
      module init_4_sol_potentials
!
      use m_precision
!
      use m_machine_parameter
      use m_t_step_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sol_potential(numnod, inod_smp_stack,             &
     &          coef_p, ncomp_nod, i_p_phi, i_press, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, ncomp_nod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: i_p_phi, i_press
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: iproc, inod, ist, ied
!
!$omp parallel do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!cdir nodep
        do inod = ist, ied
          d_nod(inod,i_p_phi) = - coef_p * dt * d_nod(inod,i_press)
          d_nod(inod,i_press) = 0.0d0
        end do
      end do
!$omp end parallel do
!
       end subroutine init_sol_potential
!
! -----------------------------------------------------------------------
!
      end module init_4_sol_potentials
