!>@file   t_sph_trans_arrays_MHD.f90
!!@brief  module t_sph_trans_arrays_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!!@verbatim
!!      subroutine alloc_sph_trans_address(sph_rtp, WK)
!!      subroutine dealloc_sph_trans_address(WK)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!@endverbatim
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
      module t_sph_trans_arrays_MHD
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
      use t_sph_multi_FFTW
      use sph_filtering
!
      implicit none
!
!>        strucutres for spherical transform dor MHD dynamo
      type works_4_sph_trans_MHD
!>        strucutre for spherical transform data addresses
        type(address_4_sph_trans) :: trns_MHD
!>        strucutre for spherical transform data addresses
        type(address_4_sph_trans) :: trns_SGS
!>        strucutre for spherical transform data addresses
        type(address_4_sph_trans) :: trns_snap
!>        strucutre for spherical transform data addresses
        type(address_4_sph_trans) :: trns_tmp
!
        type(work_for_sgl_FFTW) :: MHD_mul_FFTW
        type(work_for_sgl_FFTW) :: SGS_mul_FFTW
!
        type(dynamic_SGS_data_4_sph) :: dynamic_SPH
      end type works_4_sph_trans_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_sph_trans_address(sph_rtp, WK)
!
      use t_spheric_rtp_data
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
      integer(kind = kint) :: ncomp
!
!
      call alloc_nonlinear_data(sph_rtp%nnod_rtp, wk%trns_MHD)
      call alloc_nonlinear_data(sph_rtp%nnod_rtp, wk%trns_SGS)
      call alloc_nonlinear_data(sph_rtp%nnod_rtp, WK%trns_snap)
      call alloc_nonlinear_data(sph_rtp%nnod_rtp, wk%trns_tmp)
!
!
      call alloc_nonlinear_pole(sph_rtp%nnod_pole, WK%trns_MHD)
      call alloc_nonlinear_pole(sph_rtp%nnod_pole, WK%trns_SGS)
      call alloc_nonlinear_pole(sph_rtp%nnod_pole, WK%trns_snap)
!
      end subroutine alloc_sph_trans_address
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_sph_trans_address(WK)
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      call dealloc_nonlinear_pole(WK%trns_snap)
      call dealloc_nonlinear_pole(WK%trns_SGS)
!
      call dealloc_nonlinear_data(WK%trns_tmp)
      call dealloc_nonlinear_data(WK%trns_snap)
      call dealloc_nonlinear_data(WK%trns_MHD)
!
      end subroutine dealloc_sph_trans_address
!
!-----------------------------------------------------------------------
!
      end module t_sph_trans_arrays_MHD
