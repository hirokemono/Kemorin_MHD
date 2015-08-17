!
!      module int_vol_angular_momentum
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine int_all_angular_mom(iele_fsmp_stack, n_int,           &
!     &          jres, icomp)
!
      module int_vol_angular_momentum
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
      subroutine int_all_angular_mom(iele_fsmp_stack, n_int,            &
     &          jres, icomp)
!
      use m_machine_parameter
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_node_phys_address
      use m_bulk_values
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: jres, icomp
      integer (kind=kint), intent(in) :: n_int
!
      integer (kind=kint) :: k2
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
      real (kind=kreal) :: bulk_a_mom_smp(np_smp,3)
!
!
      if( (jres*icomp) .le. 0 ) return
!
      bulk_a_mom_smp = 0.0d0
!
      do k2 = 1, ele1%nnod_4_ele
!
       call vector_phys_2_each_element(k2, icomp, velo_1)
       call position_2_each_element(k2, xe, radius_e)
!
!$omp parallel do private(ii,ix,iele,istart,iend) 
       do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend   = iele_fsmp_stack(iproc)
!
        do ii= 1, n_int * n_int * n_int 
         ix = int_start3(n_int) + ii
!
!cdir nodep
!voption, indep, vec
         do iele = istart, iend
!
           bulk_a_mom_smp(iproc,1)=bulk_a_mom_smp(iproc,1)              &
     &      + dble(ele1%interior_ele(iele))                             &
     &      * (xe(iele,2)*velo_1(iele,3)                                &
     &       - xe(iele,3)*velo_1(iele,2))                               &
     &     * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
!
           bulk_a_mom_smp(iproc,2)=bulk_a_mom_smp(iproc,2)              &
     &       + dble(ele1%interior_ele(iele))                            &
     &      * (xe(iele,3)*velo_1(iele,1)                                &
     &       - xe(iele,1)*velo_1(iele,3))                               &
     &     * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
!
           bulk_a_mom_smp(iproc,3)=bulk_a_mom_smp(iproc,3)              &
     &       + dble(ele1%interior_ele(iele))                            &
     &      * (xe(iele,1)*velo_1(iele,2)                                &
     &       - xe(iele,2)*velo_1(iele,1))                               &
     &     * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)

!
          end do
         end do
        end do
!$omp end parallel do
       end do
!
      do iproc = 1, np_smp
       bulk_local(jres  ) = bulk_local(jres  )                          &
     &       + bulk_a_mom_smp(iproc,1)
       bulk_local(jres+1) = bulk_local(jres+1)                          &
     &       + bulk_a_mom_smp(iproc,2)
       bulk_local(jres+2) = bulk_local(jres+2)                          &
     &       + bulk_a_mom_smp(iproc,3)
      end do

      end subroutine int_all_angular_mom
!
! ----------------------------------------------------------------------
!
      end module int_vol_angular_momentum
