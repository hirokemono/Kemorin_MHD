!>@file   t_sph_SGS_transform_data.f90
!!@brief  module t_sph_SGS_transform_data
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine alloc_nonlinear_w_SGS_data(sph_rtp, trns_SGS)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!      subroutine alloc_nonlinear_pole_w_SGS(sph_rtp, trns_SGS)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!      subroutine dealloc_bwd_trns_SGS_fld_name(trns_SGS)
!!      subroutine dealloc_fwd_trns_SGS_fld_name(trns_SGS)
!!      subroutine dealloc_nonlinear_w_SGS_data(trns_SGS)
!!      subroutine dealloc_nonlinear_pole_w_SGS(trns_SGS)
!!        type(SGS_address_sph_trans), intent(inout) :: trns_SGS
!!@endverbatim
!
      module t_sph_SGS_transform_data
!
      use m_precision
!
      use t_phys_address
      use t_SGS_model_addresses
      use t_spheric_rtp_data
      use t_sph_multi_FFTW
      use t_addresses_sph_transform
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
!-----------------------------------------------------------------------
!
      contains
!
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
      subroutine dealloc_bwd_trns_SGS_fld_name(trns_SGS)
!
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
!
!
      call dealloc_sph_trns_field_name(trns_SGS%backward)
!
      end subroutine dealloc_bwd_trns_SGS_fld_name
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_fwd_trns_SGS_fld_name(trns_SGS)
!
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
!
!
      call dealloc_sph_trns_field_name(trns_SGS%forward)
!
      end subroutine dealloc_fwd_trns_SGS_fld_name
!
!-----------------------------------------------------------------------
!
      end module t_sph_SGS_transform_data
