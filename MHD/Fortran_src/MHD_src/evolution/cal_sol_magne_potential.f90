!
!     module cal_sol_magne_potential
!
!      Written by H. Matsui on June, 2005
!
!      subroutine cal_sol_m_potential
!      subroutine cal_sol_m_potential_crank
!
      module cal_sol_magne_potential
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_parameter
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
      subroutine cal_sol_m_potential
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint) :: iproc, inod, ist, ied
!
!
!$omp parallel do private(inod, ist, ied)
       do iproc = 1, np_smp
         ist = inter_smp_stack(iproc-1)+1
         ied = inter_smp_stack(iproc)
!cdir nodep
         do inod = ist, ied
           d_nod(inod,iphys%i_mag_p) = - d_nod(inod,iphys%i_m_phi)      &
     &                                  + d_nod(inod,iphys%i_mag_p)
         end do
       end do
!$omp end parallel do
!
!       write(50+my_rank,*) ' magne_p_ins'
!       do inod = 1, numnod
!         write(50+my_rank,*)  d_nod(inod,iphys%i_mag_p)
!       end do
!
      end subroutine cal_sol_m_potential
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_m_potential_crank
!
      use m_node_phys_address
      use m_node_phys_data
!
      integer (kind = kint) :: inod, iproc, ist, ied
!
!$omp parallel do private(inod, ist, ied)
       do iproc = 1, np_smp
         ist = inter_smp_stack(iproc-1)+1
         ied = inter_smp_stack(iproc)
!cdir nodep
         do inod = ist, ied
           d_nod(inod,iphys%i_mag_p) =  d_nod(inod,iphys%i_mag_p)       &
     &                                 - d_nod(inod,iphys%i_m_phi)
!     &                                - half * ak_d_magne(1)           &
!     &                                 * ff(inod,1)*ml(inod)
         end do
       end do
!$omp end parallel do
!
!cdir concur
!       do iproc = 1, np_smp
!!$omp parallel do private(inod)
!        do inum = inter_cd_smp_stack(iproc-1)+1, inter_cd_smp_stack(iproc)
!          inod = inod_conduct(inum)
!          d_nod(inod,iphys%i_mag_p)=  + d_nod(inod,iphys%i_mag_p)      &
!     &               - 0.5d0 * ak_d_magne(1) * ff(inod,1)*ml(inod)
!         end do
!       end do
!!$omp end parallel do
!
!
      end subroutine cal_sol_m_potential_crank
!
! -----------------------------------------------------------------------
!
      end module cal_sol_magne_potential
