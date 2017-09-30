!>@file   init_sph_trans_SGS_MHD.f90
!!@brief  module init_sph_trans_SGS_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine init_sph_transform_SGS_MHD                           &
!!     &         (SGS_param, SPH_model, iphys, trans_p, WK, SPH_MHD)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(phys_address), intent(in) :: iphys
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!@endverbatim
!!
      module init_sph_trans_SGS_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_SGS_control_parameter
      use t_addresses_sph_transform
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_multi_FFTW
      use t_legendre_trans_select
      use t_sph_transforms
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_boundary_data_sph_MHD
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_SGS_MHD                             &
     &         (SGS_param, SPH_model, iphys, trans_p, WK, SPH_MHD)
!
      use set_address_sph_trans_MHD
      use set_address_sph_trans_SGS
      use set_address_sph_trans_snap
      use set_address_sph_trans_tmp
      use init_sphrical_transform_MHD
      use pole_sph_transform
      use MHD_FFT_selector
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(phys_address), intent(in) :: iphys
      type(SPH_MHD_model_data), intent(in) :: SPH_model
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
!>      total number of vectors for spherical harmonics transform
      integer(kind = kint), save :: ncomp_max_trans = 0
!>      total number of vectors for spherical harmonics transform
      integer(kind = kint), save :: nvector_max_trans = 0
!>      total number of svalars for spherical harmonics transform
      integer(kind = kint), save :: nscalar_max_trans = 0
!
!
      call init_pole_transform(SPH_MHD%sph%sph_rtp)
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD                                  &
     &   (SPH_model%MHD_prop, SPH_MHD%ipol, WK%trns_MHD,                &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_trans_sph_SGS(SPH_MHD%ipol, WK%trns_SGS,       &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_trans_sph_Csim(SPH_MHD%ipol, WK%trns_Csim,     &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_snapshot_trans                                 &
     &   (SPH_MHD%ipol, iphys, WK%trns_snap,                            &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_temporal_trans                                 &
     &   (SPH_MHD%ipol, iphys, WK%trns_tmp,                             &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      if(iflag_debug .ge. iflag_routine_msg) then
        call check_address_trans_sph_MHD                                &
     &     (SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%itor, iphys,           &
     &      WK%trns_MHD, ncomp_max_trans)
        call check_address_trans_sph_SGS                                &
     &     (SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%itor, iphys,           &
     &      WK%trns_SGS)
        call check_address_trans_sph_Csim                               &
     &     (SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%itor, iphys,           &
     &      WK%trns_Csim)
        call check_address_trans_sph_snap                               &
     &     (SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%itor, iphys,           &
     &      WK%trns_snap)
        call check_address_trans_sph_tmp                                &
     &     (SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%itor, iphys,           &
     &      WK%trns_tmp)
      end if
!
      call alloc_sph_trans_address(SPH_MHD%sph%sph_rtp, WK)
!
      call sel_sph_transform_MHD(SGS_param%iflag_SGS, SPH_MHD%ipol,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                     &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_model%omega_sph,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    WK%trns_MHD, WK%trns_SGS, WK%WK_sph,                          &
     &    WK%MHD_mul_FFTW, WK%SGS_mul_FFTW, trans_p,                    &
     &    WK%gt_cor, WK%cor_rlm, SPH_MHD%fld)
!
      end subroutine init_sph_transform_SGS_MHD
!
!-----------------------------------------------------------------------
!
      end module init_sph_trans_SGS_MHD
