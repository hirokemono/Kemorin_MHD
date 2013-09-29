!
!     module cal_solver_MHD
!
!        programmed by H.Matsui on June 2010
!
!      subroutine cal_sol_velo_pre_crank
!      subroutine cal_sol_mod_po
!      subroutine cal_sol_vect_p_pre_crank
!      subroutine cal_sol_magne_pre_crank
!      subroutine cal_sol_mag_po
!      subroutine cal_sol_energy_crank(i_fld)
!      subroutine cal_sol_d_scalar_crank(i_fld)
!
      module cal_solver_MHD
!
      use m_precision
!
      implicit none
!
      private :: copy_ff_to_rhs33, copy_ff_to_rhs11
      private :: copy_solver_vec_to_vector, copy_solver_vec_to_scalar
      private :: copy_ff_potential_to_rhs
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_velo_pre_crank
!
      use m_node_phys_address
      use solver_MGCG_MHD
!
!
      call copy_ff_to_rhs33
      call solver_MGCG_velo
!
      call copy_solver_vec_to_vector(iphys%i_velo)
!
      end subroutine cal_sol_velo_pre_crank
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_mod_po
!
      use m_node_phys_address
      use solver_MGCG_MHD
!
!
      call copy_ff_potential_to_rhs(iphys%i_p_phi)
      call solver_MGCG_press
!
      call copy_solver_vec_to_scalar(iphys%i_p_phi)
!
      end subroutine cal_sol_mod_po
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_pre_crank
!
      use m_geometry_parameter
      use m_node_phys_address
      use m_node_phys_data
      use solver_MGCG_MHD
!
!
      call copy_ff_to_rhs33
      call solver_MGCG_magne
!
      call copy_solver_vec_to_vector(iphys%i_vecp)
      d_nod(1:numnod,iphys%i_m_phi) = 0.0d0
!
      end subroutine cal_sol_vect_p_pre_crank
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_magne_pre_crank
!
      use calypso_mpi
      use m_geometry_parameter
      use m_node_phys_address
      use solver_MGCG_MHD
!
!
      call copy_ff_to_rhs33
      call solver_MGCG_magne
!
      call copy_solver_vec_to_vector(iphys%i_magne)
!
      end subroutine cal_sol_magne_pre_crank
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_mag_po
!
      use m_node_phys_address
      use solver_MGCG_MHD
!
!
      call copy_ff_potential_to_rhs(iphys%i_m_phi)
      call solver_MGCG_magne_p
!
      call copy_solver_vec_to_scalar(iphys%i_m_phi)
!
      end subroutine cal_sol_mag_po
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sol_energy_crank(i_fld)
!
      use solver_MGCG_MHD
!
      integer (kind = kint), intent(in) :: i_fld
!
!
      call copy_ff_to_rhs11
      call solver_MGCG_temp
!
      call copy_solver_vec_to_scalar(i_fld)
!
      end subroutine cal_sol_energy_crank
!
!-----------------------------------------------------------------------
!
      subroutine cal_sol_d_scalar_crank(i_fld)
!
      use solver_MGCG_MHD
!
      integer (kind = kint), intent(in) :: i_fld
!
!
      call copy_ff_to_rhs11
      call solver_MGCG_d_scalar
!
      call copy_solver_vec_to_scalar(i_fld)
!
      end subroutine cal_sol_d_scalar_crank
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_ff_to_rhs33
!
      use calypso_mpi
      use m_array_for_send_recv
      use m_machine_parameter
      use m_geometry_parameter
      use m_finite_element_matrix
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
      subroutine copy_ff_to_rhs11
!
      use calypso_mpi
      use m_array_for_send_recv
      use m_machine_parameter
      use m_geometry_parameter
      use m_finite_element_matrix
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
          b_vec(inod) =    ff(inod,1)
          x_vec(inod) = ff(inod,1)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_ff_to_rhs11
!
!-----------------------------------------------------------------------
!
      subroutine copy_ff_potential_to_rhs(i_field)
!
      use calypso_mpi
      use m_array_for_send_recv
      use m_machine_parameter
      use m_geometry_parameter
      use m_node_phys_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: i_field
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
      subroutine copy_solver_vec_to_vector(i_field)
!
      use calypso_mpi
      use m_array_for_send_recv
      use m_machine_parameter
      use m_geometry_parameter
      use m_node_phys_data
!
      integer (kind = kint), intent(in) :: i_field
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
      subroutine copy_solver_vec_to_scalar(i_field)
!
      use calypso_mpi
      use m_array_for_send_recv
      use m_machine_parameter
      use m_geometry_parameter
      use m_node_phys_data
!
      integer (kind = kint), intent(in) :: i_field
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
      end module cal_solver_MHD
