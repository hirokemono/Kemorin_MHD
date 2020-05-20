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
!!     &         (SPH_model, SGS_par, ipol_LES, iphys_LES, iphys,       &
!!     &          trans_p, WK, WK_LES, SPH_MHD)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
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
      use t_SGS_control_parameter
      use t_SPH_mesh_field_data
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_multi_FFTW
      use t_legendre_trans_select
      use t_sph_transforms
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_boundary_data_sph_MHD
      use t_phys_address
      use t_SGS_model_addresses
!
      implicit  none
!
      private :: init_leg_fourier_trans_SGS_MHD
      private :: init_sph_transform_SGS_model
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_SGS_MHD                             &
     &         (SPH_model, SGS_par, ipol_LES, iphys_LES, iphys,         &
     &          trans_p, WK, WK_LES, SPH_MHD)
!
      use set_address_sph_trans_MHD
      use set_address_sph_trans_SGS
      use set_address_sph_trans_ngSGS
      use set_address_sph_trans_snap
      use address_sph_trans_SGS_snap
      use init_sphrical_transform_MHD
      use pole_sph_transform
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(SPH_MHD_model_data), intent(in) :: SPH_model
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
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
     &   (SPH_model%MHD_prop, SPH_MHD%ipol, iphys, WK%trns_MHD,         &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call init_sph_transform_SGS_model                                 &
     &   (SPH_model%MHD_prop, SGS_par%model_p,                          &
     &    SPH_MHD%ipol, ipol_LES, iphys, iphys_LES, WK_LES,             &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call set_addresses_snapshot_trans                                 &
     &   (SPH_MHD%ipol, iphys, WK%trns_snap,                            &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_ene_flux_trans                                 &
     &   (SPH_MHD%ipol, iphys, WK%trns_eflux,                           &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_temporal_trans                                 &
     &   (SPH_MHD%ipol, iphys, WK%trns_tmp,                             &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call set_addresses_SGS_snap_trans                                 &
     &   (ipol_LES, iphys_LES, WK_LES%trns_SGS_snap,                    &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call alloc_sph_trans_address(SPH_MHD%sph%sph_rtp, WK)
      call alloc_SGS_sph_trans_address(SPH_MHD%sph%sph_rtp, WK_LES)
!
!
      call init_leg_fourier_trans_SGS_MHD                               &
     &   (SPH_model%sph_MHD_bc, SPH_MHD%sph, SPH_MHD%comms,             &
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
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_SGS_model(MHD_prop, SGS_param,      &
     &          ipol, ipol_LES, iphys, iphys_LES, WK_LES,               &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      use set_address_sph_trans_SGS
      use set_address_sph_trans_ngSGS
      use set_address_sph_trans_snap
      use address_sph_trans_SGS_snap
      use address_sph_trans_fil_force
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(phys_address), intent(in) :: ipol, iphys
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!
      type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
      integer(kind = kint), intent(inout) :: ncomp_max_trans
      integer(kind = kint), intent(inout) :: nvector_max_trans
      integer(kind = kint), intent(inout) :: nscalar_max_trans
!
!
      call init_sph_trns_filter_MHD                                     &
     &   (MHD_prop, ipol_LES, iphys_LES, WK_LES%trns_fil_MHD,           &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      if(SGS_param%iflag_SGS .eq. id_SGS_similarity) then
        if(iflag_debug .gt. 0) then
          write(*,*) 'Spherical transform field table ',                &
     &               'for similarity SGS (trns_SGS)'
        end if
        call set_sph_trns_address_fld_simi                              &
     &     (ipol_LES, iphys_LES, WK_LES%trns_SGS,                       &
     &      ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          call set_sph_trns_address_dyn_simi                            &
     &       (ipol_LES, iphys_LES, WK_LES%trns_DYNS,                    &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
          call set_sph_trns_address_Csim                                &
     &      (SGS_param, ipol_LES, iphys_LES, WK_LES%trns_Csim,          &
     &       ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
         end if
!
      else if(SGS_param%iflag_SGS .eq. id_SGS_NL_grad) then
        call set_sph_trns_address_ngrad_SGS                             &
     &     (ipol_LES, iphys_LES, WK_LES%trns_SGS,                       &
     &      ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
        call set_sph_trns_address_ngrad_pre                             &
     &     (ipol, iphys, WK_LES%trns_ngTMP,                             &
     &      ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          if(iflag_debug .gt. 0) then
            write(*,*) 'Spherical transform field table ',              &
     &                 'for similarity SGS (trns_SIMI)'
          end if
          call set_sph_trns_address_fld_simi                            &
     &       (ipol_LES, iphys_LES, WK_LES%trns_SIMI,                    &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
          call set_sph_trns_address_dyn_ngrad                           &
     &       (ipol_LES, iphys_LES, WK_LES%trns_DYNG,                    &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
          call set_addresses_trans_sph_ngCsim                           &
     &       (SGS_param, ipol_LES, iphys_LES, WK_LES%trns_Csim,         &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
         end if
      end if
!
      end subroutine init_sph_transform_SGS_model
!
!-----------------------------------------------------------------------
!
      subroutine init_leg_fourier_trans_SGS_MHD(sph_MHD_bc,             &
     &          sph, comms_sph, ncomp_max_trans, trans_p, WK)
!
      use init_sph_trans
      use init_FFT_4_MHD
      use const_wz_coriolis_rtp
      use pole_sph_transform
      use skip_comment_f
!
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
      call init_fourier_transform_4_MHD(ncomp_max_trans,                &
     &    sph%sph_rtp, comms_sph%comm_rtp, WK%trns_MHD, WK%WK_sph)
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
      end module init_sph_trans_SGS_MHD
