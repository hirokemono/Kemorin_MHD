!copy_field_4_sph_trans.f90
!     module copy_field_4_sph_trans
!
!!      subroutine copy_scalar_from_trans(i_trns, nnod, dnod_sph)
!!      subroutine copy_scalar_to_trans(i_trns, nnod, dnod_sph)
!!
!!      subroutine copy_vector_from_trans(i_trns, nnod, dnod_sph)
!!      subroutine copy_vector_to_trans(i_trns, nnod, dnod_sph)
!!
!!      subroutine copy_tensor_from_trans(i_trns, nnod, dnod_sph)
!!      subroutine copy_tensor_to_trans(i_trns, nnod, dnod_sph)
!!
!!      subroutine copy_vector_tmp_to_trans(i_trns)
!!      subroutine copy_tensor_tmp_to_trans(i_trns)
!!
!!      Written by H. Matsui on Feb., 2008
!
      module copy_field_4_sph_trans
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_4_sph_trans
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_scalar_from_trans(i_trns, nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: i_trns, nnod
      real(kind = kreal), intent(inout) :: dnod_sph(nnod)
!
      integer(kind = kint) :: ist, ied, ip
!
!
!$omp do private(ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        dnod_sph(ist:ied) = vr_rtp(ist:ied,i_trns)
      end do
!$omp end do nowait
!
      end subroutine copy_scalar_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_to_trans(i_trns, nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: i_trns, nnod
      real(kind = kreal), intent(in) :: dnod_sph(nnod)
!
      integer(kind = kint) :: ist, ied, ip
!
!
!$omp do private(ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        vr_rtp(ist:ied,i_trns) = dnod_sph(ist:ied)
      end do
!$omp end do nowait
!
      end subroutine copy_scalar_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_trans(i_trns, nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: i_trns, nnod
      real(kind = kreal), intent(inout) :: dnod_sph(nnod,3)
!
      integer(kind = kint) :: ist, ied, ip
!
!
!$omp do private(ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        dnod_sph(ist:ied,1) = vr_rtp(ist:ied,i_trns  )
        dnod_sph(ist:ied,2) = vr_rtp(ist:ied,i_trns+1)
        dnod_sph(ist:ied,3) = vr_rtp(ist:ied,i_trns+2)
      end do
!$omp end do nowait
!
      end subroutine copy_vector_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_to_trans(i_trns, nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: i_trns, nnod
      real(kind = kreal), intent(in) :: dnod_sph(nnod,3)
!
      integer(kind = kint) :: ist, ied, ip
!
!
!$omp do private(ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        vr_rtp(ist:ied,i_trns  ) = dnod_sph(ist:ied,1)
        vr_rtp(ist:ied,i_trns+1) = dnod_sph(ist:ied,2)
        vr_rtp(ist:ied,i_trns+2) = dnod_sph(ist:ied,3)
      end do
!$omp end do
!
      end subroutine copy_vector_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_from_trans(i_trns, nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: i_trns, nnod
      real(kind = kreal), intent(inout) :: dnod_sph(nnod,6)
!
      integer(kind = kint) :: ist, ied, ip
!
!
!$omp do private(ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        dnod_sph(ist:ied,1) = vr_rtp(ist:ied,i_trns  )
        dnod_sph(ist:ied,2) = vr_rtp(ist:ied,i_trns+1)
        dnod_sph(ist:ied,3) = vr_rtp(ist:ied,i_trns+2)
        dnod_sph(ist:ied,4) = vr_rtp(ist:ied,i_trns+3)
        dnod_sph(ist:ied,5) = vr_rtp(ist:ied,i_trns+4)
        dnod_sph(ist:ied,6) = vr_rtp(ist:ied,i_trns+5)
      end do
!$omp end do nowait
!
      end subroutine copy_tensor_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_to_trans(i_trns, nnod, dnod_sph)
!
      integer(kind = kint), intent(in) :: i_trns, nnod
      real(kind = kreal), intent(inout) :: dnod_sph(nnod,6)
!
      integer(kind = kint) :: inod, ist, ied, ip
!
!
!$omp do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        vr_rtp(ist:ied,i_trns  ) = dnod_sph(ist:ied,1)
        vr_rtp(ist:ied,i_trns+1) = dnod_sph(ist:ied,2)
        vr_rtp(ist:ied,i_trns+2) = dnod_sph(ist:ied,3)
        vr_rtp(ist:ied,i_trns+3) = dnod_sph(ist:ied,4)
        vr_rtp(ist:ied,i_trns+4) = dnod_sph(ist:ied,5)
        vr_rtp(ist:ied,i_trns+5) = dnod_sph(ist:ied,6)
      end do
!$omp end do nowait
!
      end subroutine copy_tensor_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_vector_tmp_to_trans(i_trns)
!
      integer(kind = kint), intent(in) :: i_trns
!
      integer(kind = kint) :: ist, ied, ip
!
!
!$omp do private(ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        vr_rtp(ist:ied,i_trns  ) = d_nod_rtp(ist:ied,1)
        vr_rtp(ist:ied,i_trns+1) = d_nod_rtp(ist:ied,2)
        vr_rtp(ist:ied,i_trns+2) = d_nod_rtp(ist:ied,3)
      end do
!$omp end do nowait
!
      end subroutine copy_vector_tmp_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_tmp_to_trans(i_trns)
!
      integer(kind = kint), intent(in) :: i_trns
!
      integer(kind = kint) :: ist, ied, ip
!
!
!$omp do private(ist,ied)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        vr_rtp(ist:ied,i_trns  ) = d_nod_rtp(ist:ied,1)
        vr_rtp(ist:ied,i_trns+1) = d_nod_rtp(ist:ied,2)
        vr_rtp(ist:ied,i_trns+2) = d_nod_rtp(ist:ied,3)
        vr_rtp(ist:ied,i_trns+3) = d_nod_rtp(ist:ied,4)
        vr_rtp(ist:ied,i_trns+4) = d_nod_rtp(ist:ied,5)
        vr_rtp(ist:ied,i_trns+5) = d_nod_rtp(ist:ied,6)
      end do
!$omp end do nowait
!
      end subroutine copy_tensor_tmp_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine swap_phi_scalar_from_trans(nnod, v_prt, d_sph)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in)                                    &
     &      :: v_prt(nidx_rtp(3),nidx_rtp(1)*nidx_rtp(2))
      real(kind = kreal), intent(inout) :: d_sph(nnod)
!
      integer(kind = kint) :: ist, ied, ip,  inod
      integer(kind = kint) :: kr_lt, mphi, nrl
!
!
      nrl = nidx_rtp(1)*nidx_rtp(2)
!$omp do private(ist,ied,inod,mphi,kr_lt)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          kr_lt = 1 + mod(inod-1,nrl)
          mphi =  1 + (inod - kr_lt) / nrl
          d_sph(inod) = v_prt(mphi,kr_lt)
        end do
      end do
!$omp end do nowait
!
      end subroutine swap_phi_scalar_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_vector_from_trans(nnod, v_prt, d_sph)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in)                                    &
     &      :: v_prt(nidx_rtp(3),nidx_rtp(1)*nidx_rtp(2),3)
      real(kind = kreal), intent(inout) :: d_sph(nnod,3)
!
      integer(kind = kint) :: ist, ied, ip,  inod
      integer(kind = kint) :: kr_lt, mphi, nrl
!
!
      nrl = nidx_rtp(1)*nidx_rtp(2)
!$omp do private(ist,ied,inod,mphi,kr_lt)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          kr_lt = 1 + mod(inod-1,nrl)
          mphi =  1 + (inod - kr_lt) / nrl
          d_sph(inod,1) = v_prt(mphi,kr_lt,1)
          d_sph(inod,2) = v_prt(mphi,kr_lt,2)
          d_sph(inod,3) = v_prt(mphi,kr_lt,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine swap_phi_vector_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine swap_phi_tensor_from_trans(nnod, v_prt, d_sph)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in)                                    &
     &      :: v_prt(nidx_rtp(3),nidx_rtp(1)*nidx_rtp(2),6)
      real(kind = kreal), intent(inout) :: d_sph(nnod,6)
!
      integer(kind = kint) :: ist, ied, ip,  inod
      integer(kind = kint) :: kr_lt, mphi, nrl
!
!
      nrl = nidx_rtp(1)*nidx_rtp(2)
!$omp do private(ist,ied,inod,mphi,kr_lt)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          kr_lt = 1 + mod(inod-1,nrl)
          mphi =  1 + (inod - kr_lt) / nrl
          d_sph(inod,1) = v_prt(mphi,kr_lt,1)
          d_sph(inod,2) = v_prt(mphi,kr_lt,2)
          d_sph(inod,3) = v_prt(mphi,kr_lt,3)
          d_sph(inod,4) = v_prt(mphi,kr_lt,4)
          d_sph(inod,5) = v_prt(mphi,kr_lt,5)
          d_sph(inod,6) = v_prt(mphi,kr_lt,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine swap_phi_tensor_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_field_4_sph_trans
