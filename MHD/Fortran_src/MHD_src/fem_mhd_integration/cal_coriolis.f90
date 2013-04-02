!
!     module cal_coriolis
!
!        programmed by H.Matsui on July 2000 (ver 1.1)
!        modified by H. Matsui on Aug., 2007
!
!      subroutine cal_coriolis_nod(angular, coef_cor, ml_o_fl, velo, ff)
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
      subroutine cal_coriolis_nod(angular, coef_cor, ml_o_fl, velo, ff)
!
      use m_geometry_parameter
      use m_machine_parameter
!
      real (kind=kreal), intent(in) :: coef_cor 
      real (kind=kreal), intent(in) :: angular(3)
      real (kind=kreal), intent(in) :: velo(numnod,3)
      real (kind=kreal), intent(in) :: ml_o_fl(numnod)
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
          ff(inod,1) = ff(inod,1) - ( angular(2)*velo(inod,3)         &
     &                              - angular(3)*velo(inod,2) )       &
     &                             * coef_cor * ml_o_fl(inod)
          ff(inod,2) = ff(inod,2) - ( angular(3)*velo(inod,1)         &
     &                              - angular(1)*velo(inod,3) )       &
     &                             * coef_cor * ml_o_fl(inod)
          ff(inod,3) = ff(inod,3) - ( angular(1)*velo(inod,2)         &
     &                              - angular(2)*velo(inod,1) ) &
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
