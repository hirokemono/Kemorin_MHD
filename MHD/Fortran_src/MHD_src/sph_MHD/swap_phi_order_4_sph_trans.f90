!>@file   swap_phi_order_4_sph_trans.f90
!!@brief  module swap_phi_order_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2014
!
!>@brief  Swap array for phi componene
!!
!!@verbatim
!!      subroutine swap_phi_from_MHD_trans                              &
!!     &         (sph_rtp, trns_b_MHD, trns_f_MHD)
!!      subroutine swap_phi_to_MHD_trans                                &
!!     &         (sph_rtp, trns_b_MHD, trns_f_MHD)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(spherical_transform_data), intent(inout) :: trns_b_MHD
!!        type(spherical_transform_data), intent(inout) :: trns_f_MHD
!!@endverbatim
!
      module swap_phi_order_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_rtp_data
      use t_addresses_sph_transform
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine swap_phi_from_MHD_trans                                &
     &         (sph_rtp, trns_b_MHD, trns_f_MHD)
!
      use m_FFT_selector
      use swap_phi_4_sph_trans
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(spherical_transform_data), intent(inout) :: trns_b_MHD
      type(spherical_transform_data), intent(inout) :: trns_f_MHD
!
!
      if(iflag_FFT .ne. iflag_FFTW) return
      call swap_phi_order_from_trans(trns_b_MHD%ncomp,                  &
     &    sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, trns_b_MHD%fld_rtp)
      call swap_phi_order_from_trans(trns_f_MHD%ncomp,                  &
     &    sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, trns_f_MHD%fld_rtp)
!
      end subroutine swap_phi_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      end module swap_phi_order_4_sph_trans
