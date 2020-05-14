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
      use t_spheric_rtp_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_addresses_sph_transform
      use t_sph_multi_FFTW
!
      implicit none
!
!
!>      strucutre of spherical transform data addresses
      type SGS_address_sph_trans
!>        strucutre of backward spherical transform data addresses
        type(spherical_transform_data) :: backward
!>        strucutre of forward spherical transform data addresses
        type(spherical_transform_data) :: forward
!
!>        addresses of fields for backward transform
        type(phys_address) :: b_trns
!>        addresses of forces for forward transform
        type(phys_address) :: f_trns
!>        addresses of SGS models for backward transform
        type(SGS_model_addresses) :: b_trns_LES
!>        addresses of SGS models for forward transform
        type(SGS_model_addresses) :: f_trns_LES
!
!>        Work area of Fourier transform for MHD
        type(work_for_sgl_FFTW) :: mul_FFTW
      end type SGS_address_sph_trans
!
!>      strucutres for spherical transform for MHD dynamo with SGS
      type works_4_sph_trans_SGS_MHD
!>        strucutres for spherical transform for SGS model
        type(SGS_address_sph_trans) :: trns_SGS
!>        strucutres for spherical transform for dynamic SGS model
        type(SGS_address_sph_trans) :: trns_DYNS
!>        strucutres for spherical transform for model coefficients
        type(SGS_address_sph_trans) :: trns_Csim
!
!>        strucutres for spherical transform for intermediate
!!        nonlinear radient model
        type(SGS_address_sph_trans) :: trns_ngTMP
!>        strucutres for spherical transform for scale similarity
        type(SGS_address_sph_trans) :: trns_SIMI
!>        strucutres for spherical transform for dynamic SGS model
        type(SGS_address_sph_trans) :: trns_DYNG
!
!>        strucutres for spherical transform for snapshot output
        type(SGS_address_sph_trans) :: trns_SGS_snap
!
!>        strucutres for spherical transform for filtered forces
        type(SGS_address_sph_trans) :: trns_fil_MHD
      end type works_4_sph_trans_SGS_MHD
!
      private :: alloc_nonlinear_w_SGS_data
      private :: alloc_nonlinear_pole_w_SGS
      private :: dealloc_nonlinear_w_SGS_data
      private :: dealloc_nonlinear_pole_w_SGS
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_SGS_sph_trans_address(sph_rtp, WK_LES)
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
      call alloc_nonlinear_pole_w_SGS(sph_rtp, WK_LES%trns_Csim)
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
      call alloc_nonlinear_w_SGS_data(sph_rtp, WK_LES%trns_fil_MHD)
      call alloc_nonlinear_pole_w_SGS(sph_rtp, WK_LES%trns_fil_MHD)
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
      call dealloc_nonlinear_pole_w_SGS(WK_LES%trns_Csim)
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
      call dealloc_nonlinear_pole_w_SGS(WK_LES%trns_fil_MHD)
      call dealloc_nonlinear_w_SGS_data(WK_LES%trns_fil_MHD)
!
      end subroutine dealloc_SGS_sph_trans_address
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_w_SGS_data(sph_rtp, trns_SGS)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
!
!
      call alloc_sph_trns_field_data(sph_rtp, trns_SGS%backward)
      call alloc_sph_trns_field_data(sph_rtp, trns_SGS%forward)
!
      end subroutine alloc_nonlinear_w_SGS_data
!
!-----------------------------------------------------------------------
!
      subroutine alloc_nonlinear_pole_w_SGS(sph_rtp, trns_SGS)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
!
!
      call alloc_sph_trns_pole_data(sph_rtp, trns_SGS%backward)
      call alloc_sph_trns_pole_data(sph_rtp, trns_SGS%forward)
!
      end subroutine alloc_nonlinear_pole_w_SGS
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_nonlinear_w_SGS_data(trns_SGS)
!
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
!
!
      call dealloc_sph_trns_field_dats(trns_SGS%backward)
      call dealloc_sph_trns_field_dats(trns_SGS%forward)
!
      end subroutine dealloc_nonlinear_w_SGS_data
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_nonlinear_pole_w_SGS(trns_SGS)
!
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
!
!
      call dealloc_sph_trns_pole_data(trns_SGS%backward)
      call dealloc_sph_trns_pole_data(trns_SGS%forward)
!
      end subroutine dealloc_nonlinear_pole_w_SGS
!
!-----------------------------------------------------------------------
!
      end module t_sph_trans_arrays_SGS_MHD
