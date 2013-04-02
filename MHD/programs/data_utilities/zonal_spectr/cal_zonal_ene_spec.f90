!cal_zonal_ene_spec.f90
!      module cal_zonal_ene_spec
!
      module cal_zonal_ene_spec
!
!     Written by H. Matsui on Nov., 2007
!
      use m_precision
      use m_constants
      use m_spheric_parameter
      use m_node_id_spherical_IO
      use m_sph_spectr_data
      use m_sph_zonal_ene_spectr
!
      implicit none
!
!      subroutine set_range_4_zonal_ene_spec
!      subroutine set_idx_4_zonal_ene_spec
!
!      subroutine s_cal_zonal_ene_spec(icou_time)
!      subroutine cal_tave_zonal_ene_spec(icou_time)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_range_4_zonal_ene_spec
!
      integer(kind = kint) :: i
!
!
      max_abs_m = abs(idx_gl_1d_rtp_p(1,2))
      min_abs_m = abs(idx_gl_1d_rtp_p(1,2))
      do i = 2, nidx_rtp(3)
        max_abs_m = max(max_abs_m,abs(idx_gl_1d_rtp_p(i,2)))
        min_abs_m = min(min_abs_m,abs(idx_gl_1d_rtp_p(i,2)))
      end do
!
      num_ene_mphi = max_abs_m - min_abs_m + 1
      num_ene_spec = nidx_rtp(1)*nidx_rtp(2)*num_ene_mphi
!
      end subroutine set_range_4_zonal_ene_spec
!
! -------------------------------------------------------------------
!
      subroutine set_idx_4_zonal_ene_spec
!
      integer(kind = kint) :: i, j, k, icou, ii
!
!
      do i = 1, num_ene_mphi
        do j = 1, nidx_rtp(2)
          do k = 1, nidx_rtp(1)
            icou = k + (j-1)*nidx_rtp(1)                                &
     &               + (i-1)*nidx_rtp(1)*nidx_rtp(2)
            inod_ene_gl_rtp(icou,1) = idx_gl_1d_rtp_r(k)
            inod_ene_gl_rtp(icou,2) = idx_gl_1d_rtp_t(j)
            inod_ene_gl_rtp(icou,3) = i-1 + min_abs_m
          end do
        end do
      end do
!
      icou = 0
      do ii = 1, nidx_rtp(3)
        i = abs( idx_gl_1d_rtp_p(ii,2) ) - min_abs_m + 1
        do j = 1, nidx_rtp(2)
          do k = 1, nidx_rtp(1)
            icou = icou + 1
            itgt_zonal_ene_spec(icou) = k                               &
     &                               + (j-1)*nidx_rtp(1)                &
     &                               + (i-1)*nidx_rtp(1)*nidx_rtp(2)
          end do
        end do
      end do
!
      end subroutine set_idx_4_zonal_ene_spec
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine s_cal_zonal_ene_spec(icou_time)
!
      use m_phys_labels
!
      integer(kind = kint), intent(inout) :: icou_time
      integer(kind = kint) :: i, j, ist, ied, nd, i_field
!
!
      d_ene_spec = 0.0d0
!
      do i_field = 1, num_phys_rtp
        ist = istack_phys_comp_rtp(i_field-1) + 1
        ied = istack_phys_comp_rtp(i_field)
        do nd = ist, ied
          do i = 1, nnod_rtp
            j = itgt_zonal_ene_spec(i)
            d_ene_spec(nd,j) = d_ene_spec(nd,j)                         &
     &                        + d_rtp(i,nd)*d_rtp(i,nd)
          end do
        end do
      end do
!
      do i_field = 1, num_phys_rtp
        if (    phys_name_rtp(i_field) .eq. fhd_velo                    &
     &     .or. phys_name_rtp(i_field) .eq. fhd_magne) then
          ied = istack_phys_comp_rtp(i_field)
          do j = 1, num_ene_spec
            d_ene_spec(ied-2,j) = d_ene_spec(ied-2,j) * half
            d_ene_spec(ied-1,j) = d_ene_spec(ied-1,j) * half
            d_ene_spec(ied  ,j) = d_ene_spec(ied  ,j) * half
          end do
        end if
      end do
!
      icou_time = icou_time + 1
      do i_field = 1, num_phys_rtp
        ist = istack_phys_comp_rtp(i_field-1) + 1
        ied = istack_phys_comp_rtp(i_field)
        do nd = ist, ied
          do j = 1, num_ene_spec
            d_tave_ene_spec(nd,j) = d_tave_ene_spec(nd,j)               &
     &                             + d_ene_spec(nd,j)
          end do
        end do
      end do
!
      end subroutine s_cal_zonal_ene_spec
!
! -------------------------------------------------------------------
!
      subroutine cal_tave_zonal_ene_spec(icou_time)
!
      integer(kind = kint), intent(in) :: icou_time
      integer(kind = kint) :: j, ist, ied, nd, i_field
!
!
      do i_field = 1, num_phys_rtp
        ist = istack_phys_comp_rtp(i_field-1) + 1
        ied = istack_phys_comp_rtp(i_field)
        do nd = ist, ied
          do j = 1, num_ene_spec
            d_tave_ene_spec(nd,j) = d_tave_ene_spec(nd,j)               &
     &                             / dble(icou_time)
          end do
        end do
      end do
!
      end subroutine cal_tave_zonal_ene_spec
!
! -------------------------------------------------------------------
!
      end module cal_zonal_ene_spec
