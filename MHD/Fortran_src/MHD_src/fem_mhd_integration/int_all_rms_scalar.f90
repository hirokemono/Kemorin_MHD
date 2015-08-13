!
!      module int_all_rms_scalar
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine int_all_4_scalar(iele_fsmp_stack, n_int,              &
!     &         ir_rms, ja_ave, i_comp)
!      subroutine int_vol_ave_rms_4_scalar(iele_fsmp_stack, n_int,      &
!     &         i_comp, rms_local, ave_local)
!      subroutine int_ave_rms_4_scalar(iele_fsmp_stack, n_int,          &
!     &         i_comp, rms_local, ave_local)
!
      module int_all_rms_scalar
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_all_4_scalar(iele_fsmp_stack, n_int,               &
     &          ir_rms, ja_ave, i_comp)
!
      use m_bulk_values
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind=kint), intent(in) :: i_comp
      integer (kind=kint), intent(in) :: ir_rms, ja_ave
      integer (kind=kint), intent(in) :: n_int
!
!
      if( (ir_rms*i_comp) .gt. 0) then
        call int_vol_ave_rms_4_scalar(iele_fsmp_stack, n_int,           &
     &      i_comp, rms_local(ir_rms), bulk_local(ja_ave) )
      end if
!
      end subroutine int_all_4_scalar
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_ave_rms_4_scalar(iele_fsmp_stack, n_int,       &
     &          i_comp, rms_local, ave_local)
!
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_node_phys_address
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: i_comp
!
      real (kind=kreal), intent(inout) :: rms_local, ave_local
!
      integer (kind=kint) :: k2
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
      real (kind=kreal) :: bulk_e_smp(np_smp)
      real (kind=kreal) :: average_smp(np_smp)
!
!
      bulk_e_smp =  zero
      average_smp = zero
      do k2 = 1, ele1%nnod_4_ele
!
        call scalar_phys_2_each_element(k2, i_comp, phi_e)
!
!$omp parallel do private(ii,ix,iele,ist,ied)
        do iproc = 1, np_smp
          ist = iele_fsmp_stack(iproc-1)+1
          ied = iele_fsmp_stack(iproc)
!
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir nodep
            do iele = ist, ied
!
              bulk_e_smp(iproc) = bulk_e_smp(iproc)  + e_multi(iele)    &
     &                             * ( phi_e(iele) * phi_e(iele) )      &
     &                             * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
!
              average_smp(iproc) = average_smp(iproc)                   &
     &                            + e_multi(iele) * phi_e(iele)         &
     &                             * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
!
           end do
          end do
        end do
!$omp end parallel do
      end do
!
      rms_local =   zero
      ave_local =   zero
!cdir noconcur
      do iproc = 1, np_smp
        rms_local = rms_local + bulk_e_smp(iproc)
        ave_local = ave_local + average_smp(iproc)
      end do
!
      end subroutine int_vol_ave_rms_4_scalar
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_ave_rms_4_scalar(iele_fsmp_stack, n_int,           &
     &          i_comp, rms_local, ave_local)
!
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_jacobians
      use m_node_phys_address
      use m_int_vol_data
!
      use nodal_fld_2_each_ele_1st
!
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer (kind=kint), intent(in) :: n_int
      integer (kind=kint), intent(in) :: i_comp
!
      real (kind=kreal), intent(inout) :: rms_local, ave_local
!
      integer (kind=kint) :: k2
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: ist, ied
!
      real (kind=kreal) :: bulk_e_smp(np_smp)
      real (kind=kreal) :: average_smp(np_smp)
!
!
      bulk_e_smp =  zero
      average_smp = zero
      do k2 = 1, ele1%nnod_4_ele
!
        call scalar_phys_2_each_element(k2, i_comp, phi_e)
!
!$omp parallel do private(ii,ix,iele,ist,ied)
        do iproc = 1, np_smp
          ist = iele_fsmp_stack(iproc-1)+1
          ied = iele_fsmp_stack(iproc)
!
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
!cdir nodep
            do iele = ist, ied
!
              bulk_e_smp(iproc) = bulk_e_smp(iproc)  + e_multi(iele)    &
     &                             * ( phi_e(iele) * phi_e(iele) )      &
     &                             * aw(k2,ix)*owe3d(ix)
!
              average_smp(iproc) = average_smp(iproc)                   &
     &                            + e_multi(iele) * phi_e(iele)         &
     &                             * aw(k2,ix)*owe3d(ix)
!
           end do
          end do
        end do
!$omp end parallel do
      end do
!
      rms_local =   zero
      ave_local =   zero
!cdir noconcur
      do iproc = 1, np_smp
        rms_local = rms_local + bulk_e_smp(iproc)
        ave_local = ave_local + average_smp(iproc)
      end do
!
      end subroutine int_ave_rms_4_scalar
!
! ----------------------------------------------------------------------
!
      end module int_all_rms_scalar
