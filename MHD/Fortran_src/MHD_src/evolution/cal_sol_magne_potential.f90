!
!     module cal_sol_magne_potential
!
!      Written by H. Matsui on June, 2005
!
!!      subroutine cal_sol_m_potential(numnod, inter_smp_stack,         &
!!     &          ncomp_nod, i_m_phi, i_mag_p, d_nod)
!!      subroutine cal_sol_m_potential_crank(numnod, inter_smp_stack,   &
!!     &          ncomp_nod, i_m_phi, i_mag_p, d_nod)
!
      module cal_sol_magne_potential
!
      use m_precision
!
      use m_machine_parameter
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
      subroutine cal_sol_m_potential(numnod, inter_smp_stack,           &
     &          ncomp_nod, i_m_phi, i_mag_p, d_nod)
!
      integer(kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: i_m_phi, i_mag_p
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer(kind = kint) :: iproc, inod, ist, ied
!
!
!$omp parallel do private(inod, ist, ied)
       do iproc = 1, np_smp
         ist = inter_smp_stack(iproc-1)+1
         ied = inter_smp_stack(iproc)
!cdir nodep
         do inod = ist, ied
           d_nod(inod,i_mag_p) = - d_nod(inod,i_m_phi)                  &
     &                            + d_nod(inod,i_mag_p)
         end do
       end do
!$omp end parallel do
!
!       write(50+my_rank,*) ' magne_p_ins'
!       do inod = 1, node1%numnod
!         write(50+my_rank,*)  d_nod(inod,i_mag_p)
!       end do
!
      end subroutine cal_sol_m_potential
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_m_potential_crank(numnod, inter_smp_stack,     &
     &          ncomp_nod, i_m_phi, i_mag_p, d_nod)
!
      integer(kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: i_m_phi, i_mag_p
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inod, iproc, ist, ied
!
!$omp parallel do private(inod, ist, ied)
       do iproc = 1, np_smp
         ist = inter_smp_stack(iproc-1)+1
         ied = inter_smp_stack(iproc)
!cdir nodep
         do inod = ist, ied
           d_nod(inod,i_mag_p) =  d_nod(inod,i_mag_p)                   &
     &                                 - d_nod(inod,i_m_phi)
!     &                                - half * ak_d_magne(1)           &
!     &                                 * ff(inod,1)*ml(inod)
         end do
       end do
!$omp end parallel do
!
!       do iproc = 1, np_smp
!         ist = inter_cd_smp_stack(iproc-1)+1
!         ied = inter_cd_smp_stack(iproc)
!!$omp parallel do private(inod)
!         do inod = ist, ied
!          inod = inod_conduct(inum)
!          d_nod(inod,i_mag_p)=  + d_nod(inod,i_mag_p)                  &
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
