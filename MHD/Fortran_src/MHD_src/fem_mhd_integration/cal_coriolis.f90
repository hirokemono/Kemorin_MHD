!
!     module cal_coriolis
!
!        programmed by H.Matsui on July 2000 (ver 1.1)
!        modified by H. Matsui on Aug., 2007
!
!!      subroutine cal_coriolis_nod(numnod, inod_smp_stack,             &
!!     &          angular, coef_cor, ml_o_fl,                           &
!!     &          ncomp_nod, i_velo, d_nod, ff)
!
      module cal_coriolis
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_coriolis_nod(numnod, inod_smp_stack,               &
     &          angular, coef_cor, ml_o_fl,                             &
     &          ncomp_nod, i_velo, d_nod, ff)
!
      use m_machine_parameter
!
      integer (kind=kint), intent(in) :: numnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef_cor 
      real (kind=kreal), intent(in) :: angular(3)
      real (kind=kreal), intent(in) :: ml_o_fl(numnod)
!
      integer(kind = kint), intent(in) :: ncomp_nod, i_velo
      real (kind=kreal), intent(in) :: d_nod(numnod,ncomp_nod)
!
      real (kind=kreal), intent(inout) :: ff(numnod,3)
!
      integer (kind=kint) :: iproc, inod
      integer (kind=kint) :: ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!cdir nodep
!voption, indep, vec
        do inod = ist, ied
          ff(inod,1) = ff(inod,1) - ( angular(2)*d_nod(inod,i_velo+2)   &
     &                              - angular(3)*d_nod(inod,i_velo+1) ) &
     &                             * coef_cor * ml_o_fl(inod)
          ff(inod,2) = ff(inod,2) - ( angular(3)*d_nod(inod,i_velo  )   &
     &                              - angular(1)*d_nod(inod,i_velo+2) ) &
     &                             * coef_cor * ml_o_fl(inod)
          ff(inod,3) = ff(inod,3) - ( angular(1)*d_nod(inod,i_velo+1)   &
     &                              - angular(2)*d_nod(inod,i_velo  ) ) &
     &                             * coef_cor * ml_o_fl(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_coriolis_nod
!
! ----------------------------------------------------------------------
!
      end module cal_coriolis
