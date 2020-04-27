!>@file   t_sph_trans_arrays_SGS_MHD.f90
!!@brief  module t_sph_trans_arrays_SGS_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!!@verbatim
!!      subroutine alloc_SGS_sph_trans_address(sph_rtp, WK_LES)
!!      subroutine dealloc_SGS_sph_trans_address(WK_LES)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
!!@endverbatim
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
      module t_sph_trans_arrays_SGS_MHD
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
      use t_sph_multi_FFTW
      use t_sph_transforms
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
!
!
      implicit none
!
!>      strucutres for spherical transform for MHD dynamo with SGS
      type works_4_sph_trans_SGS_MHD
!>        strucutres for spherical transform for SGS model
        type(address_4_sph_trans) :: trns_SGS
!>        strucutres for spherical transform for dynamic SGS model
        type(address_4_sph_trans) :: trns_DYNS
!>        strucutres for spherical transform for model coefficients
        type(address_4_sph_trans) :: trns_Csim
!
!>        strucutres for spherical transform for intermediate
!!        nonlinear radient model
        type(address_4_sph_trans) :: trns_ngTMP
!>        strucutres for spherical transform for scale similarity
        type(address_4_sph_trans) :: trns_SIMI
!>        strucutres for spherical transform for dynamic SGS model
        type(address_4_sph_trans) :: trns_DYNG
!
!>        strucutres for spherical transform for snapshot output
        type(address_4_sph_trans) :: trns_SGS_snap
      end type works_4_sph_trans_SGS_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_SGS_sph_trans_address(sph_rtp, WK_LES)
!
      use t_spheric_rtp_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
!
!
      call alloc_nonlinear_w_SGS_data(sph_rtp, WK_LES%trns_SGS_snap)
      call alloc_nonlinear_pole_w_SGS(sph_rtp, WK_LES%trns_SGS_snap)
!
!
      call alloc_nonlinear_w_SGS_data(sph_rtp, WK_LES%trns_SGS)
      call alloc_nonlinear_w_SGS_data(sph_rtp, WK_LES%trns_DYNS)
      call alloc_nonlinear_w_SGS_data(sph_rtp, WK_LES%trns_Csim)
!
      call alloc_nonlinear_pole_w_SGS(sph_rtp, WK_LES%trns_SGS)
      call alloc_nonlinear_pole_w_SGS(sph_rtp, WK_LES%trns_DYNS)
!
!
      call alloc_nonlinear_w_SGS_data(sph_rtp, WK_LES%trns_ngTMP)
      call alloc_nonlinear_w_SGS_data(sph_rtp, WK_LES%trns_SIMI)
      call alloc_nonlinear_w_SGS_data(sph_rtp, WK_LES%trns_DYNG)
!
      call alloc_nonlinear_pole_w_SGS(sph_rtp, WK_LES%trns_ngTMP)
      call alloc_nonlinear_pole_w_SGS(sph_rtp, WK_LES%trns_SIMI)
      call alloc_nonlinear_pole_w_SGS(sph_rtp, WK_LES%trns_DYNG)
!
      end subroutine alloc_SGS_sph_trans_address
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_SGS_sph_trans_address(WK_LES)
!
      type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
!
!
      call dealloc_nonlinear_w_SGS_data(WK_LES%trns_Csim)
      call dealloc_nonlinear_w_SGS_data(WK_LES%trns_DYNS)
      call dealloc_nonlinear_w_SGS_data(WK_LES%trns_SGS)
!
      call dealloc_nonlinear_pole_w_SGS(WK_LES%trns_DYNS)
      call dealloc_nonlinear_pole_w_SGS(WK_LES%trns_SGS)
!
!
      call dealloc_nonlinear_pole_w_SGS(WK_LES%trns_ngTMP)
      call dealloc_nonlinear_pole_w_SGS(WK_LES%trns_SIMI)
      call dealloc_nonlinear_pole_w_SGS(WK_LES%trns_DYNG)
!
      call dealloc_nonlinear_w_SGS_data(WK_LES%trns_ngTMP)
      call dealloc_nonlinear_w_SGS_data(WK_LES%trns_SIMI)
      call dealloc_nonlinear_w_SGS_data(WK_LES%trns_DYNG)
!
      call dealloc_nonlinear_pole_w_SGS(WK_LES%trns_SGS_snap)
      call dealloc_nonlinear_w_SGS_data(WK_LES%trns_SGS_snap)
!
      end subroutine dealloc_SGS_sph_trans_address
!
!-----------------------------------------------------------------------
!
      end module t_sph_trans_arrays_SGS_MHD
