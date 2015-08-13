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
      use m_physical_property
      use m_t_int_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_4_sol_k_potential(inod_smp_stack)
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer (kind = kint) :: iproc, inod
!
!$omp parallel do private(inod)
      do iproc = 1, np_smp
!cdir nodep
        do inod = inod_smp_stack(iproc-1)+1, inod_smp_stack(iproc)
          d_nod(inod,iphys%i_p_phi) = - coef_press * dt                 &
     &                           * d_nod(inod,iphys%i_press)
          d_nod(inod,iphys%i_press) = 0.0d0
        end do
      end do
!$omp end parallel do
!
       end subroutine init_4_sol_k_potential
!
! -----------------------------------------------------------------------
!
      subroutine init_4_sol_m_potential(inod_smp_stack)
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer (kind = kint) :: iproc, inod
!
!$omp parallel do private(inod)
      do iproc = 1, np_smp
!cdir nodep
        do inod = inod_smp_stack(iproc-1)+1, inod_smp_stack(iproc)
          d_nod(inod,iphys%i_m_phi) = - coef_mag_p * dt                 &
     &                                 * d_nod(inod,iphys%i_mag_p)
          d_nod(inod,iphys%i_mag_p) = 0.0d0
        end do
      end do
!$omp end parallel do
!
       end subroutine init_4_sol_m_potential
!
! -----------------------------------------------------------------------
!
      end module init_4_sol_potentials
