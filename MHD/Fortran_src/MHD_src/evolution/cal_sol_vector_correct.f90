!
!      module cal_sol_vector_correct
!
!      Written by H. Matsui on March, 2006
!
!      subroutine cal_sol_velo_co
!      subroutine cal_sol_vect_p_co
!      subroutine cal_sol_magne_co
!
      module cal_sol_vector_correct
!
      use m_precision
!
      implicit none
!
      private :: cal_sol_vector_co
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_co
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_phys_constants
      use m_node_phys_address
      use m_node_phys_data
      use m_finite_element_matrix
!
      integer (kind = kint) :: iproc, inod, nd
!
!
!$omp parallel do private(nd,inod)
      do iproc = 1, np_smp
        do nd = 1, n_vector
!cdir nodep
          do inod = inter_smp_stack(iproc-1)+1, inter_smp_stack(iproc)
!
            d_nod(inod,iphys%i_velo+nd-1)                               &
     &           = d_nod(inod,iphys%i_velo+nd-1)                        &
     &             + ml_fl(inod)*ff(inod,nd)
            d_nod(inod,iphys%i_p_phi) = 0.0d0
!
          end do
        end do
      end do
!$omp end parallel do
!
!
      end subroutine cal_sol_velo_co
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_vect_p_co
!
      use m_node_phys_address
!
      call cal_sol_vector_co(iphys%i_vecp)
!
      end subroutine cal_sol_vect_p_co
!
! ----------------------------------------------------------------------
!
      subroutine cal_sol_magne_co
!
      use m_node_phys_address
!
      call cal_sol_vector_co(iphys%i_magne)
!
      end subroutine cal_sol_magne_co
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------!
      subroutine cal_sol_vector_co(i_field)
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_phys_constants
      use m_node_phys_data
      use m_finite_element_matrix
!
      integer (kind = kint), intent(in) :: i_field
!
      integer (kind = kint) :: iproc, inod, nd, icomp
!
!
!$omp parallel do private(nd,inod,icomp)
      do iproc = 1, np_smp
        do nd=1, n_vector
          icomp = i_field + nd - 1
!cdir nodep
          do inod = inter_smp_stack(iproc-1)+1, inter_smp_stack(iproc)
!
            d_nod(inod,icomp) = d_nod(inod,icomp)                       &
     &                         + ml(inod)*ff(inod,nd)
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_vector_co
!
! ----------------------------------------------------------------------
!
      end module cal_sol_vector_correct
