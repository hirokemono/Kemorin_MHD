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
      private :: init_fourier_transform_SGS_MHD
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
      use set_address_sph_trans_ngSGS
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
     &   (SPH_model%MHD_prop, SPH_MHD, iphys, WK%trns_MHD,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      if(SGS_param%iflag_SGS .eq. id_SGS_similarity) then
        if(iflag_debug .gt. 0) then
          write(*,*) 'Spherical transform field table ',                &
     &               'for similarity SGS (trns_SGS)'
        end if
        call init_sph_trns_fld_similarity(SPH_MHD, iphys, WK%trns_SGS,  &
     &      ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          call init_sph_trns_fld_dyn_simi                               &
     &       (SGS_param, SPH_MHD, iphys, WK%trns_DYNS,                  &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
          call set_addresses_trans_sph_Csim                             &
     &       (SPH_MHD, iphys, WK%trns_Csim,                             &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
         end if
!
      else if(SGS_param%iflag_SGS .eq. id_SGS_NL_grad) then
        call init_sph_trns_fld_ngrad_SGS(SPH_MHD, iphys, WK%trns_SGS,   &
     &      ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
        call init_sph_trns_fld_ngrad_pre(SPH_MHD, iphys, WK%trns_ngTMP, &
     &      ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          if(iflag_debug .gt. 0) then
            write(*,*) 'Spherical transform field table ',              &
     &                 'for similarity SGS (trns_SIMI)'
          end if
          call init_sph_trns_fld_similarity                             &
     &       (SPH_MHD, iphys, WK%trns_SIMI,                             &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
          call init_sph_trns_fld_dyn_ngrad                              &
     &       (SPH_MHD, iphys, WK%trns_DYNG,                             &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
          call set_addresses_trans_sph_ngCsim                           &
     &       (SPH_MHD, iphys, WK%trns_Csim,                             &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
         end if
      end if
!
      call set_addresses_snapshot_trans(SPH_MHD, iphys, WK%trns_snap,   &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_temporal_trans(SPH_MHD, iphys, WK%trns_tmp,    &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call alloc_sph_trans_address(SPH_MHD%sph%sph_rtp, WK)
!
      call init_leg_fourier_trans_SGS_MHD                               &
     &   (SGS_param, SPH_model%sph_MHD_bc, SPH_MHD%sph, SPH_MHD%comms,  &
     &    ncomp_max_trans, trans_p, WK)
!
      call sel_sph_transform_MHD                                        &
     &   (SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                     &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_model%omega_sph,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    WK%trns_MHD, WK%WK_sph, trans_p, WK%gt_cor, WK%cor_rlm,       &
     &    SPH_MHD%fld)
!
      end subroutine init_sph_transform_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine init_leg_fourier_trans_SGS_MHD                         &
     &         (SGS_param, sph_MHD_bc, sph, comms_sph,                  &
     &          ncomp_max_trans, trans_p, WK)
!
      use init_sph_trans
      use init_FFT_4_MHD
      use const_wz_coriolis_rtp
      use pole_sph_transform
      use skip_comment_f
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_legendre_trans'
      call initialize_legendre_trans(ncomp_max_trans,                   &
     &    sph, comms_sph, trans_p%leg, trans_p%idx_trns)
      call init_fourier_transform_SGS_MHD                               &
     &   (SGS_param, ncomp_max_trans, sph%sph_rtp, comms_sph%comm_rtp,  &
     &    WK%trns_MHD, WK%trns_SGS, WK%trns_DYNS, WK%trns_Csim,         &
     &    WK%trns_ngTMP, WK%trns_SIMI, WK%trns_DYNG, WK%WK_sph)
!
      if (iflag_debug.eq.1) write(*,*) 'alloc_sphere_ave_coriolis'
      call alloc_sphere_ave_coriolis(sph%sph_rj)
      if (iflag_debug.eq.1) write(*,*) 'init_sum_coriolis_rlm'
      call init_sum_coriolis_rlm                                        &
     &   (sph%sph_params%l_truncation, sph%sph_rlm,                     &
     &    sph_MHD_bc%sph_bc_U, trans_p%leg, WK%gt_cor, WK%cor_rlm)
!
      end subroutine init_leg_fourier_trans_SGS_MHD
!
!-----------------------------------------------------------------------
!
      subroutine init_fourier_transform_SGS_MHD                         &
     &       (SGS_param, ncomp_tot, sph_rtp, comm_rtp,                  &
     &        trns_MHD, trns_SGS, trns_DYNS, trns_Csim,                 &
     &        trns_ngTMP, trns_SIMI, trns_DYNG, WK_sph)
!
      use m_solver_SR
      use init_FFT_4_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_tot
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD,  trns_SGS
      type(address_4_sph_trans), intent(inout) :: trns_DYNS, trns_Csim
      type(address_4_sph_trans), intent(inout) :: trns_ngTMP
      type(address_4_sph_trans), intent(inout) :: trns_SIMI, trns_DYNG
      type(spherical_trns_works), intent(inout) :: WK_sph
!
!
      call init_fourier_transform_4_MHD(ncomp_tot,                      &
     &    sph_rtp, comm_rtp, trns_MHD, WK_sph, trns_MHD%mul_FFTW)
!
      if(SGS_param%iflag_SGS .eq. id_SGS_similarity) then
        call init_MHD_FFT_select(my_rank, sph_rtp, ncomp_tot,           &
     &      trns_SGS%forward%ncomp, trns_SGS%backward%ncomp,            &
     &      trns_SGS%mul_FFTW)
!
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          call init_MHD_FFT_select(my_rank, sph_rtp, ncomp_tot,         &
     &        trns_DYNS%forward%ncomp, trns_DYNS%backward%ncomp,        &
     &        trns_DYNS%mul_FFTW)
          call init_MHD_FFT_select(my_rank, sph_rtp, ncomp_tot,         &
     &        trns_Csim%forward%ncomp, trns_Csim%backward%ncomp,        &
     &        trns_Csim%mul_FFTW)
        end if
      else if(SGS_param%iflag_SGS .eq. id_SGS_NL_grad) then
        call init_MHD_FFT_select(my_rank, sph_rtp, ncomp_tot,           &
     &      trns_SGS%forward%ncomp, trns_SGS%backward%ncomp,            &
     &      trns_SGS%mul_FFTW)
        call init_MHD_FFT_select(my_rank, sph_rtp, ncomp_tot,           &
     &      trns_ngTMP%forward%ncomp, trns_ngTMP%backward%ncomp,        &
     &      trns_ngTMP%mul_FFTW)
!
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
        call init_MHD_FFT_select(my_rank, sph_rtp, ncomp_tot,           &
     &      trns_SIMI%forward%ncomp, trns_SIMI%backward%ncomp,          &
     &      trns_SIMI%mul_FFTW)
        call init_MHD_FFT_select(my_rank, sph_rtp, ncomp_tot,           &
     &      trns_DYNG%forward%ncomp, trns_DYNG%backward%ncomp,          &
     &      trns_DYNG%mul_FFTW)
        end if
      end if
!
      end subroutine init_fourier_transform_SGS_MHD
!
! -----------------------------------------------------------------------
!
      end module init_sph_trans_SGS_MHD
