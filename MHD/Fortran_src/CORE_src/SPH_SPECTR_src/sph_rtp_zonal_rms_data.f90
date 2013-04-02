!
!     module sph_rtp_zonal_rms_data
!
!      Written by H. Matsui on Oct., 2007
!
!      subroutine cal_sph_zonal_rms_data
!      subroutine cal_sph_zonal_ave_data
!
      module sph_rtp_zonal_rms_data
!
      use m_precision
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine cal_sph_zonal_rms_data
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      use cvt_nod_data_to_sph_data
!
      integer(kind = kint) :: nd, kt, iphi, inod
!
!
!$omp parallel private(nd,iphi)
      do nd = 1, ntot_phys_rtp
!$omp do private(kt)
        do kt = 1, nidx_rtp(1)*nidx_rtp(2)
          d_rtp(kt,nd) = d_rtp(kt,nd)**2
        end do
!$omp end do nowait
!
        do iphi = 2, nidx_rtp(3)
!$omp do private(kt,inod)
          do kt = 1, nidx_rtp(1)*nidx_rtp(2)
            inod = kt + (iphi-1) * nidx_rtp(1)*nidx_rtp(2)
            d_rtp(kt,nd) = d_rtp(kt,nd) + d_rtp(inod,nd)**2
          end do
!$omp end do nowait
        end do
!
!$omp do private(kt)
        do kt = 1, nidx_rtp(1)*nidx_rtp(2)
          d_rtp(kt,nd) = sqrt( d_rtp(kt,nd) / dble(nidx_rtp(3)) )
        end do
!$omp end do nowait
!
        do iphi = 2, nidx_rtp(3)
!$omp do private(kt,inod)
          do kt = 1, nidx_rtp(1)*nidx_rtp(2)
            inod = kt + (iphi-1) * nidx_rtp(1)*nidx_rtp(2)
            d_rtp(inod,nd) = d_rtp(kt,nd)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
        call copy_nod_scalar_from_sph_data
        call cvt_xyz_from_sph_vec_sph_data
        call cvt_sph_to_xyz_tensor_data
!
      end subroutine cal_sph_zonal_rms_data
!
! -------------------------------------------------------------------
!
      subroutine cal_sph_zonal_ave_data
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use cvt_nod_data_to_sph_data
!
      integer(kind = kint) :: nd, kt, iphi, inod
!
!
!$omp parallel private(nd,iphi)
      do nd = 1, ntot_phys_rtp
        do iphi = 2, nidx_rtp(3)
!$omp do private(kt,inod)
          do kt = 1, nidx_rtp(1)*nidx_rtp(2)
            inod = kt + (iphi-1) * nidx_rtp(1)*nidx_rtp(2)
            d_rtp(kt,nd) = d_rtp(kt,nd) + d_rtp(inod,nd)
          end do
!$omp end do nowait
        end do
!
!$omp do private(kt)
        do kt = 1, nidx_rtp(1)*nidx_rtp(2)
          d_rtp(kt,nd) = d_rtp(kt,nd) / dble(nidx_rtp(3))
        end do
!$omp end do nowait
!
        do iphi = 2, nidx_rtp(3)
!$omp do private(kt,inod)
          do kt = 1, nidx_rtp(1)*nidx_rtp(2)
            inod = kt + (iphi-1) * nidx_rtp(1)*nidx_rtp(2)
            d_rtp(inod,nd) = d_rtp(kt,nd)
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
        call copy_nod_scalar_from_sph_data
        call cvt_xyz_from_sph_vec_sph_data
        call cvt_sph_to_xyz_tensor_data
!
      end subroutine cal_sph_zonal_ave_data
!
! -------------------------------------------------------------------
!
      end module sph_rtp_zonal_rms_data
