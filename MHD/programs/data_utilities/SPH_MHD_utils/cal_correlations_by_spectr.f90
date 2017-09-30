!>@file   cal_correlations_by_spectr.f90
!!@brief  module cal_correlations_by_spectr
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine back_trans_4_correlate                               &
!!     &         (sph, comms_sph, ref_rj_fld, rj_fld, trans_p,          &
!!     &          trns_MHD, WK_sph)
!!      subroutine back_trans_4_rms_ratio                               &
!!     &         (sph, comms_sph, ref_rj_fld, rj_fld, trans_p,          &
!!     &          trns_MHD, WK_sph)
!!      subroutine cal_sph_correlations                                 &
!!     &         (sph, ipol, ref_rj_fld, rj_fld, trans_p, pwr, WK_pwr)
!!      subroutine cal_sph_rms_ratios                                   &
!!     &         (sph, ipol, ref_rj_fld, rj_fld, trans_p, pwr, WK_pwr)
!!        type(sph_mean_squares), intent(inout) :: pwr
!!@endverbatim
!
      module cal_correlations_by_spectr
!
      use m_precision
      use calypso_mpi
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_work_4_sph_trans
      use t_addresses_sph_transform
      use t_sph_transforms
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
!
      implicit none
!
      real(kind = kreal), allocatable :: fld1_rtp(:,:)
!
      real(kind = kreal), allocatable :: msq_s1(:,:)
      real(kind = kreal), allocatable :: msq_s2(:,:)
      real(kind = kreal), allocatable :: cor_s(:,:)
      real(kind = kreal), allocatable :: msq_v1(:,:)
      real(kind = kreal), allocatable :: msq_v2(:,:)
      real(kind = kreal), allocatable :: cor_v(:,:)
!
      private :: fld1_rtp
      private :: msq_s1, msq_s2, cor_s
      private :: msq_v1, msq_v2, cor_v
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine back_trans_4_correlate                                 &
     &         (sph, comms_sph, ref_rj_fld, rj_fld, trans_p,            &
     &          trns_MHD, WK_sph)
!
      use back_sph_trans_4_all_field
      use zonal_correlation_rtp
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(phys_data), intent(in) :: ref_rj_fld
      type(phys_data), intent(in)  :: rj_fld
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
!
      integer(kind = kint) :: nnod_rtp, ncomp_rtp
!
      nnod_rtp =  sph%sph_rtp%nnod_rtp
      ncomp_rtp = trns_MHD%ncomp_rj_2_rtp
      allocate(fld1_rtp(nnod_rtp,ncomp_rtp))
!
      call sph_back_transform_dual(sph, comms_sph, trans_p,             &
     &    ref_rj_fld, rj_fld, trns_MHD, WK_sph,                         &
     &    nnod_rtp, ncomp_rtp, fld1_rtp)
!
!       Evaluate correlation in zonal
!
      call ovrwrt_zonal_correlate_rtp(ncomp_rtp, nnod_rtp,              &
     &    sph%sph_rtp%nidx_rtp, fld1_rtp, trns_MHD%fld_rtp)
      trns_MHD%fld_pole = 0.0d0
!
      deallocate(fld1_rtp)
!
      end subroutine back_trans_4_correlate
!
! ----------------------------------------------------------------------
!
      subroutine back_trans_4_rms_ratio                                 &
     &         (sph, comms_sph, ref_rj_fld, rj_fld, trans_p,            &
     &          trns_MHD, WK_sph)
!
      use back_sph_trans_4_all_field
      use zonal_correlation_rtp
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(phys_data), intent(in) :: ref_rj_fld
      type(phys_data), intent(in)  :: rj_fld
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
!
      integer(kind = kint) :: nnod_rtp, ncomp_rtp
!
!       Transform first data
!
      nnod_rtp =  sph%sph_rtp%nnod_rtp
      ncomp_rtp = trns_MHD%ncomp_rj_2_rtp
      allocate(fld1_rtp(nnod_rtp,ncomp_rtp))
!
      call sph_back_transform_dual(sph, comms_sph, trans_p,             &
     &    ref_rj_fld, rj_fld, trns_MHD, WK_sph,                         &
     &    nnod_rtp, ncomp_rtp, fld1_rtp)
!
!       Evaluate correlation in zonal
!
      call ovrwrt_zonal_rms_ratio_rtp(ncomp_rtp, nnod_rtp,              &
     &    sph%sph_rtp%nidx_rtp, fld1_rtp, trns_MHD%fld_rtp)
      trns_MHD%fld_pole = 0.0d0
!
      deallocate(fld1_rtp)
!
      end subroutine back_trans_4_rms_ratio
!
! ----------------------------------------------------------------------
!
      subroutine cal_sph_correlations                                   &
     &         (sph, ipol, ref_rj_fld, rj_fld, trans_p, pwr, WK_pwr)
!
      use cal_rms_fields_by_sph
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: ref_rj_fld
      type(phys_data), intent(in)  :: rj_fld
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: k, nd
!
!       Evaluate correlation in sphere
!
      if(my_rank .eq. 0) then
        allocate(msq_s1(pwr%nri_rms,pwr%ntot_comp_sq))
        allocate(msq_s2(pwr%nri_rms,pwr%ntot_comp_sq))
        allocate(cor_s(pwr%nri_rms,pwr%ntot_comp_sq))
        allocate(msq_v1(pwr%num_vol_spectr,pwr%ntot_comp_sq))
        allocate(msq_v2(pwr%num_vol_spectr,pwr%ntot_comp_sq))
        allocate(cor_v(pwr%num_vol_spectr,pwr%ntot_comp_sq))
      end if
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph%sph_params%l_truncation, sph%sph_rj, ipol, ref_rj_fld,    &
     &    trans_p%leg%g_sph_rj, pwr, WK_pwr)
!
      if(my_rank .eq. 0) then
        do nd = 1, pwr%ntot_comp_sq
          msq_s1(1:pwr%nri_rms,nd) = pwr%shl_sq(1:pwr%nri_rms,nd)       &
     &                             - pwr%shl_l(1:pwr%nri_rms,0,nd)
        end do
        do nd = 1, pwr%ntot_comp_sq
          do k = 1, pwr%num_vol_spectr
            msq_v1(k,nd) = pwr%v_spectr(k)%v_sq(nd)                     &
     &                   - pwr%v_spectr(k)%v_l(0,nd)
          end do
        end do
      end if
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph%sph_params%l_truncation, sph%sph_rj, ipol, rj_fld,        &
     &    trans_p%leg%g_sph_rj, pwr, WK_pwr)
!
      if(my_rank .eq. 0) then
        do nd = 1, pwr%ntot_comp_sq
          msq_s2(1:pwr%nri_rms,nd) = pwr%shl_sq(1:pwr%nri_rms,nd)       &
     &                           - pwr%shl_l(1:pwr%nri_rms,0,nd)
        end do
        do nd = 1, pwr%ntot_comp_sq
          do k = 1, pwr%num_vol_spectr
            msq_v2(k,nd) = pwr%v_spectr(k)%v_sq(nd)                     &
     &                 - pwr%v_spectr(k)%v_l(0,nd)
          end do
        end do
      end if
!
      call cal_correlate_in_shell(sph%sph_params%l_truncation,          &
     &    sph%sph_rj, rj_fld, ref_rj_fld, trans_p%leg%g_sph_rj,         &
     &    pwr, WK_pwr)
!
!
      if(my_rank .eq. 0) then
        do nd = 1, pwr%ntot_comp_sq
          cor_s(1:pwr%nri_rms,nd) = pwr%shl_sq(1:pwr%nri_rms,nd)        &
     &                           - pwr%shl_l(1:pwr%nri_rms,0,nd)
        end do
        do nd = 1, pwr%ntot_comp_sq
          do k = 1, pwr%num_vol_spectr
            cor_v(k,nd) = pwr%v_spectr(k)%v_sq(nd)                      &
     &                   - pwr%v_spectr(k)%v_l(0,nd)
          end do
        end do
!
        do nd = 1, pwr%ntot_comp_sq
           do k = 1, pwr%nri_rms
             if(msq_s1(k,nd)*msq_s2(k,nd) .eq. zero) then
              pwr%shl_sq(k,nd) = zero
            else
              pwr%shl_sq(k,nd) = cor_s(k,nd)                            &
     &                        / sqrt(msq_s1(k,nd)*msq_s2(k,nd))
            end if
          end do
          do k = 1, pwr%num_vol_spectr
            if(msq_v1(k,nd)*msq_v2(k,nd) .eq. zero) then
              pwr%shl_sq(k,nd) = zero
            else
              pwr%v_spectr(k)%v_sq(nd) = cor_v(k,nd)                    &
     &                       / sqrt(msq_v1(k,nd)*msq_v2(k,nd))
            end if
          end do
        end do
!
        deallocate(msq_s1, msq_s2, cor_s)
        deallocate(msq_v1, msq_v2, cor_v)
      end if
!
      end subroutine cal_sph_correlations
!
! ----------------------------------------------------------------------
!
      subroutine cal_sph_rms_ratios                                     &
     &         (sph, ipol, ref_rj_fld, rj_fld, trans_p, pwr, WK_pwr)
!
      use cal_rms_fields_by_sph
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: ref_rj_fld
      type(phys_data), intent(in)  :: rj_fld
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(sph_mean_squares), intent(inout) :: pwr
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
      integer(kind = kint) :: k, nd
!
!
      if(my_rank .eq. 0) then
        allocate(msq_s1(pwr%nri_rms,pwr%ntot_comp_sq))
        allocate(msq_s2(pwr%nri_rms,pwr%ntot_comp_sq))
        allocate(msq_v1(pwr%num_vol_spectr,pwr%ntot_comp_sq))
        allocate(msq_v2(pwr%num_vol_spectr,pwr%ntot_comp_sq))
      end if
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph%sph_params%l_truncation, sph%sph_rj, ipol, ref_rj_fld,    &
     &    trans_p%leg%g_sph_rj, pwr, WK_pwr)
!
      if(my_rank .eq. 0) then
        do nd = 1, pwr%ntot_comp_sq
          msq_s1(1:pwr%nri_rms,nd) = pwr%shl_sq(1:pwr%nri_rms,nd)
        end do
        do nd = 1, pwr%ntot_comp_sq
          do k = 1, pwr%num_vol_spectr
            msq_v1(k,nd) = pwr%v_spectr(k)%v_sq(nd)
          end do
        end do
      end if
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph%sph_params%l_truncation, sph%sph_rj, ipol, rj_fld,        &
     &    trans_p%leg%g_sph_rj, pwr, WK_pwr)
!
      if(my_rank .eq. 0) then
        do nd = 1, pwr%ntot_comp_sq
          msq_s2(1:pwr%nri_rms,nd) = pwr%shl_sq(1:pwr%nri_rms,nd)
        end do
        do nd = 1, pwr%ntot_comp_sq
          do k = 1, pwr%num_vol_spectr
            msq_v2(k,nd) = pwr%v_spectr(k)%v_sq(nd)
          end do
        end do
      end if
!
!
      if(my_rank .eq. 0) then
        do nd = 1, pwr%ntot_comp_sq
           do k = 1, pwr%nri_rms
             if(msq_s1(k,nd) .eq. zero) then
              pwr%shl_sq(k,nd) = zero
            else
              pwr%shl_sq(k,nd) = sqrt(msq_s2(k,nd) / msq_s1(k,nd))
            end if
          end do
          do k = 1, pwr%num_vol_spectr
            if(msq_v1(k,nd) .eq. zero) then
              pwr%shl_sq(k,nd) = zero
            else
              pwr%v_spectr(k)%v_sq(nd)                                  &
     &                       = sqrt(msq_v2(k,nd) / msq_v1(k,nd))
            end if
          end do
        end do
!
        deallocate(msq_s1, msq_s2)
        deallocate(msq_v1, msq_v2)
      end if
!
      end subroutine cal_sph_rms_ratios
!
! ----------------------------------------------------------------------
!
      end module cal_correlations_by_spectr