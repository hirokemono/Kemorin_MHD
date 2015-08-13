!
!      module int_rms_ave_asym_tensor
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine int_ave_4_asym_tensor(iele_fsmp_stack, n_int,         &
!     &          ir_rms, ja_ave, i_vect)
!
      module int_rms_ave_asym_tensor
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
      subroutine int_ave_4_asym_tensor(iele_fsmp_stack, n_int,          &
     &          ir_rms, ja_ave, i_vect)
!
      use m_constants
      use m_geometry_data
      use m_machine_parameter
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
      integer (kind=kint), intent(in) :: i_vect
      integer (kind=kint), intent(in) :: ir_rms, ja_ave
      integer (kind=kint), intent(in) :: n_int
!
      integer (kind=kint) :: k2
      integer (kind=kint) :: iproc, iele, ii, ix
      integer (kind=kint) :: istart, iend
!
      real (kind=kreal) :: rms_smp(np_smp)
      real (kind=kreal) :: ave_smp(np_smp,3)
!
!
      if( (ir_rms*i_vect) .le. 0) return
!
      rms_smp = 0.0d0
      ave_smp = 0.0d0
!
      do k2 = 1, ele1%nnod_4_ele
!
       call vector_phys_2_each_element(k2, i_vect, vect_e)
!
!$omp parallel do private(ii,ix,iele,istart,iend)
       do iproc = 1, np_smp
        istart = iele_fsmp_stack(iproc-1)+1
        iend = iele_fsmp_stack(iproc)
!
         do ii= 1, n_int * n_int * n_int 
          ix = int_start3(n_int) + ii
!
!cdir nodep
!voption, indep, vec
          do iele = istart, iend
!
           rms_smp(iproc) = rms_smp(iproc)                              &
     &    + e_multi(iele)                                               &
     &     * (   vect_e(iele,1) * vect_e(iele,1)                        &
     &       + 2*vect_e(iele,2) * vect_e(iele,2)                        &
     &       + 2*vect_e(iele,3) * vect_e(iele,3) )                      &
     &     * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
!
           ave_smp(iproc,1) = ave_smp(iproc,1)                          &
     &    + e_multi(iele) * vect_e(iele,1)                              &
     &     * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
           ave_smp(iproc,2) = ave_smp(iproc,2)                          &
     &    + e_multi(iele) * vect_e(iele,2)                              &
     &     * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
           ave_smp(iproc,3) = ave_smp(iproc,3)                          &
     &    + e_multi(iele) * vect_e(iele,3)                              &
     &     * aw(k2,ix)*xjac(iele,ix)*owe3d(ix)
!
          end do
         end do
        end do
!$omp end parallel do
!
       end do
!
!poption noparallel
!cdir noconcur
      do iproc = 1, np_smp
        rms_local(ir_rms)     = rms_local(ir_rms) + two*rms_smp(iproc)
        bulk_local(ja_ave  ) = bulk_local(ja_ave  ) + ave_smp(iproc,1)
        bulk_local(ja_ave+1) = bulk_local(ja_ave+1) + ave_smp(iproc,2)
        bulk_local(ja_ave+2) = bulk_local(ja_ave+2) + ave_smp(iproc,3)
      end do
!
      rms_local(ir_rms) = 2.0d0*rms_local(ir_rms)
!
      end subroutine int_ave_4_asym_tensor
!
! ----------------------------------------------------------------------
!
      end module int_rms_ave_asym_tensor
