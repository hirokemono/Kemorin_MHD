!
!     module cal_sol_pressure_MHD
!
!      Written by H. Matsui on June, 2005
!
!      subroutine cal_sol_pressure
!      subroutine cal_sol_pressure_w_mag_ene
!      subroutine cal_sol_pressure_mcv
!      subroutine cal_sol_pressure_crank
!      subroutine cal_sol_pressure_rotate
!
      module cal_sol_pressure_MHD
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_geometry_parameter
      use m_t_int_parameter
      use m_physical_property
      use m_finite_element_matrix
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_pressure
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint) :: iproc, inod
!
!
!$omp parallel do private(inod)
      do iproc = 1, np_smp
!cdir nodep
        do inod = inter_smp_stack(iproc-1)+1, inter_smp_stack(iproc)
          d_nod(inod,iphys%i_press) =  d_nod(inod,iphys%i_press)        &
     &           - acoef_press * ddt * d_nod(inod,iphys%i_p_phi)
       end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_pressure
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_pressure_w_mag_ene
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer(kind = kint) :: iproc, inod
      real(kind = kreal) :: mag_ene
!       
!
!$omp parallel do private (mag_ene,inod)
      do iproc = 1, np_smp
!cdir nodep
        do inod = inter_smp_stack(iproc-1)+1, inter_smp_stack(iproc)
!
          mag_ene = half                                                &
     &      * (d_nod(inod,iphys%i_magne  )*d_nod(inod,iphys%i_magne  )  &
     &       + d_nod(inod,iphys%i_magne+1)*d_nod(inod,iphys%i_magne+1)  &
     &       + d_nod(inod,iphys%i_magne+2)*d_nod(inod,iphys%i_magne+2))

          d_nod(inod,iphys%i_p_phi) =  - coef_lor * dt * mag_ene        &
     &               - coef_press * dt * d_nod(inod,iphys%i_press)
          d_nod(inod,iphys%i_press)= - acoef_press * coef_lor*mag_ene
!
       end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_pressure_w_mag_ene
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_pressure_mcv
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint) :: iproc, inod
      real(kind = kreal) :: mag_ene
!       
!
!$omp parallel do private (mag_ene,inod)
      do iproc = 1, np_smp
!cdir nodep
        do inod = inter_smp_stack(iproc-1)+1, inter_smp_stack(iproc)
!
          mag_ene = half                                                &
     &             * ((d_nod(inod,iphys%i_magne  )+ex_magne(1))**2      &
     &              + (d_nod(inod,iphys%i_magne+1)+ex_magne(2))**2      &
     &              + (d_nod(inod,iphys%i_magne+2)+ex_magne(3))**2 )
!
          d_nod(inod,iphys%i_p_phi) = - coef_lor * dt * mag_ene         &
     &              - coef_press * dt * d_nod(inod,iphys%i_press)
          d_nod(inod,iphys%i_press)= - acoef_press * coef_lor * mag_ene
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_pressure_mcv
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_pressure_crank
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint) :: iproc, inod
!
!$omp parallel do private(inod)
      do iproc = 1, np_smp
!cdir nodep
        do inod = inter_smp_stack(iproc-1)+1, inter_smp_stack(iproc)
          d_nod(inod,iphys%i_press) =  d_nod(inod,iphys%i_press)        &
     &       - acoef_press * ddt * ( d_nod(inod,iphys%i_p_phi)          &
     &       - half * coef_d_velo * ff(inod,1)*ml_fl(inod) )
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_pressure_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_pressure_rotate
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind=kint) :: iproc, inod
      real(kind = kreal) :: kin_ene
!
!
!$omp parallel do private (inod,kin_ene)
      do iproc = 1, np_smp
!cdir nodep
        do inod = inter_smp_stack(iproc-1)+1, inter_smp_stack(iproc)
!
          kin_ene = half                                                &
     &      * ( d_nod(inod,iphys%i_velo  )*d_nod(inod,iphys%i_velo  )   &
     &        + d_nod(inod,iphys%i_velo+1)*d_nod(inod,iphys%i_velo+1)   &
     &        + d_nod(inod,iphys%i_velo+2)*d_nod(inod,iphys%i_velo+2))

          d_nod(inod,iphys%i_press) =  d_nod(inod,iphys%i_press)        &
     &                                - kin_ene
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sol_pressure_rotate
!
! -----------------------------------------------------------------------
!
      end module cal_sol_pressure_MHD
