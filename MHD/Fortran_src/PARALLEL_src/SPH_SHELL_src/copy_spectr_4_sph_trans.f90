!
!      module copy_spectr_4_sph_trans
!
!        programmed by H.Matsui on Jan., 2008
!
!      subroutine copy_scalar_spec_to_trans(nscalar_trans,              &
!     &          is_spec, i_trns)
!      subroutine copy_scalar_spec_from_trans(nscalar_trans,            &
!     &          is_spec, i_trns)
!
!      subroutine copy_vec_spec_to_trans(nvector_trans, is_spec, i_trns)
!      subroutine copy_vec_spec_from_trans(nvector_trans,               &
!     &           is_spec, i_trns)
!
!      subroutine copy_grad_spec_to_trans(ngrad_trans,                  &
!     &          is_spec, igrad_spec, i_trns)
!      subroutine copy_grad_spec_from_trans(ngrad_trans,                &
!     &          is_spec, igrad_spec, i_trns)
!
!      subroutine copy_tensor_spec_to_trans(ntensor_trans,              &
!     &          is_spec, i_trns)
!      subroutine copy_tensor_spec_from_trans(ntensor_trans,            &
!     &          is_spec, i_trns)
!
      module copy_spectr_4_sph_trans
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_4_sph_trans
      use m_sph_spectr_data
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine copy_scalar_spec_to_trans(nscalar_trans,               &
     &          is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: nscalar_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*nscalar_trans
          sp_rj(jnod  ) = d_rj(inod,is_spec  )
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_scalar_spec_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_spec_from_trans(nscalar_trans,             &
     &          is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: nscalar_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*nscalar_trans
          d_rj(inod,is_spec  ) = sp_rj(jnod  )
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_scalar_spec_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_vec_spec_to_trans(nvector_trans, is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: nvector_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*nvector_trans
          sp_rj(3*jnod-2) = d_rj(inod,is_spec  )
          sp_rj(3*jnod-1) = d_rj(inod,is_spec+1)
          sp_rj(3*jnod  ) = d_rj(inod,is_spec+2)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_vec_spec_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vec_spec_from_trans(nvector_trans,                &
     &          is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: nvector_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*nvector_trans
          d_rj(inod,is_spec  ) = sp_rj(3*jnod-2)
          d_rj(inod,is_spec+1) = sp_rj(3*jnod-1)
          d_rj(inod,is_spec+2) = sp_rj(3*jnod  )
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_vec_spec_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_grad_spec_to_trans(ngrad_trans,                   &
     &          is_spec, igrad_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ngrad_trans
      integer(kind = kint), intent(in) :: is_spec, igrad_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*igrad_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ngrad_trans
          sp_rj(2*jnod  ) = d_rj(inod,is_spec  )
          sp_rj(2*jnod+1) = d_rj(inod,igrad_spec)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_grad_spec_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_grad_spec_from_trans(ngrad_trans,                 &
     &          is_spec, igrad_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ngrad_trans
      integer(kind = kint), intent(in) :: is_spec, igrad_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*igrad_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ngrad_trans
          d_rj(inod,is_spec   ) = sp_rj(2*jnod  )
          d_rj(inod,igrad_spec) = sp_rj(2*jnod+1)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_grad_spec_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_spec_to_trans(ntensor_trans,               &
     &          is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ntensor_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ntensor_trans
          sp_rj(6*jnod-5) = d_rj(inod,is_spec  )
          sp_rj(6*jnod-4) = d_rj(inod,is_spec+1)
          sp_rj(6*jnod-3) = d_rj(inod,is_spec+2)
          sp_rj(6*jnod-2) = d_rj(inod,is_spec+3)
          sp_rj(6*jnod-1) = d_rj(inod,is_spec+4)
          sp_rj(6*jnod  ) = d_rj(inod,is_spec+5)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_tensor_spec_to_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_spec_from_trans(ntensor_trans,             &
     &          is_spec, i_trns)
!
      integer(kind = kint), intent(in) :: ntensor_trans
      integer(kind = kint), intent(in) :: is_spec, i_trns
      integer(kind = kint) :: inod, jnod, ist, ied, ip
!
!
      if( (is_spec*i_trns) .le. 0) return
!
!$omp do private(ist,ied,inod,jnod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          jnod = i_trns + (inod-1)*ntensor_trans
          d_rj(inod,is_spec  ) = sp_rj(6*jnod-5)
          d_rj(inod,is_spec+1) = sp_rj(6*jnod-4)
          d_rj(inod,is_spec+2) = sp_rj(6*jnod-3)
          d_rj(inod,is_spec+3) = sp_rj(6*jnod-2)
          d_rj(inod,is_spec+4) = sp_rj(6*jnod-1)
          d_rj(inod,is_spec+5) = sp_rj(6*jnod  )
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_tensor_spec_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_spectr_4_sph_trans
