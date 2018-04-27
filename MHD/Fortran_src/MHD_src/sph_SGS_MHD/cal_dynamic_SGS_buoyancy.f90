!>@file   cal_dynamic_SGS_buoyancy.f90
!!@brief  module cal_dynamic_SGS_buoyancy
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate SGS buoyancy terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine dynamic_buo_SGS_by_pseudo_sph                        &
!!     &         (SGS_param,  sph, comms_sph, MHD_prop, trans_p,        &
!!     &          trns_MHD, trns_SGS, trns_DYNS, WK_sph, DYNS_mul_FFTW, &
!!     &          dynamic_SPH, ipol, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS, trns_DYNS
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(work_for_sgl_FFTW), intent(inout) :: DYNS_mul_FFTW
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine product_buo_model_coefs_4_sph                        &
!!     &         (SGS_param, sph, comms_sph, trans_p, trns_SGS,         &
!!     &          WK_sph, SGS_mul_FFTW, dynamic_SPH, ipol, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
!
      module cal_dynamic_SGS_buoyancy
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_physical_property
      use t_SGS_control_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_sph_trans_arrays_MHD
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_filtering_data
      use t_sph_transforms
      use t_sph_filtering
!
      implicit none
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine dynamic_buo_SGS_by_pseudo_sph                          &
     &         (SGS_param,  sph, comms_sph, MHD_prop, trans_p,          &
     &          trns_MHD, trns_SGS, trns_DYNS, WK_sph, DYNS_mul_FFTW,   &
     &          dynamic_SPH, ipol, rj_fld)
!
      use t_SGS_buoyancy_sph
      use sph_transforms_4_SGS
      use dynamic_model_sph_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(address_4_sph_trans), intent(inout) :: trns_SGS, trns_DYNS
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: DYNS_mul_FFTW
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &       'const_dynamic_SGS_4_buo_sph', iflag_debug
      call const_dynamic_SGS_4_buo_sph                                  &
     &   (SGS_param%stab_weight, sph%sph_rtp, MHD_prop%fl_prop,         &
     &    trns_MHD%b_trns, trns_SGS%f_trns, trns_DYNS%f_trns,           &
     &    trns_MHD%backward, trns_SGS%forward, trns_DYNS%forward,       &
     &    dynamic_SPH)
!
      if(SGS_param%iflag_SGS_buo_usage .ne. id_use_zonal) then
        call start_elapsed_time(16)
        if (iflag_debug.eq.1) write(*,*)                                &
     &                     'sph_forward_trans_SGS_MHD dyns'
        call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,         &
     &      trns_DYNS%forward, WK_sph, DYNS_mul_FFTW, rj_fld)
        call end_elapsed_time(16)
!
        if(SGS_param%iflag_SGS_buo_usage .eq. id_use_sphere) then
          if (iflag_debug.eq.1) write(*,*)                              &
     &                      'sphere_averaged_SGS_buoyancy'
          call sphere_averaged_SGS_buoyancy(sph%sph_rj, sph%sph_rtp,    &
     &        ipol, rj_fld, dynamic_SPH%wk_sgs_buo)
        else if(SGS_param%iflag_SGS_buo_usage .eq. id_use_volume) then
          if (iflag_debug.eq.1) write(*,*)                              &
     &                     'volume_averaged_SGS_buoyancy'
          call volume_averaged_SGS_buoyancy(sph%sph_params,             &
     &        sph%sph_rj, ipol, rj_fld, dynamic_SPH%wk_sgs_buo)
        end if
      end if
!
      end subroutine dynamic_buo_SGS_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!
      subroutine product_buo_model_coefs_4_sph                          &
     &         (SGS_param, sph, comms_sph, trans_p, trns_SGS,           &
     &          WK_sph, SGS_mul_FFTW, dynamic_SPH, ipol, rj_fld)
!
      use t_SGS_buoyancy_sph
      use sph_transforms_4_SGS
      use SGS_buo_coefs_sph_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
!
!
        call start_elapsed_time(84)
        if(SGS_param%iflag_SGS_buo_usage .eq. id_use_zonal) then
          write(*,*) 'prod_SGS_buoyancy_to_Reynolds'
          call prod_SGS_buoyancy_to_Reynolds                            &
     &       (sph%sph_rtp, dynamic_SPH%sph_d_grp,                       &
     &        trns_SGS%f_trns, dynamic_SPH%ifld_sgs,                    &
     &        dynamic_SPH%wk_sgs, trns_SGS%forward)
!
          call start_elapsed_time(16)
          if (iflag_debug.eq.1) write(*,*)                              &
     &                        'sph_forward_trans_SGS_MHD SGS'
          call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,       &
     &        trns_SGS%forward, WK_sph, SGS_mul_FFTW, rj_fld)
          call end_elapsed_time(16)
        else if(SGS_param%iflag_SGS_buo_usage .eq. id_use_sphere) then
          call start_elapsed_time(16)
          if (iflag_debug.eq.1) write(*,*)                              &
     &                        'sph_forward_trans_SGS_MHD SGS'
          call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,       &
     &        trns_SGS%forward, WK_sph, SGS_mul_FFTW, rj_fld)
          call end_elapsed_time(16)
!
          if(iflag_debug.eq.1) write(*,*)                               &
     &                      'magnify_sph_ave_SGS_buoyancy'
          call magnify_sph_ave_SGS_buoyancy(sph%sph_rj, sph%sph_rtp,    &
     &        ipol, dynamic_SPH%ifld_sgs, dynamic_SPH%wk_sgs_buo,       &
     &        rj_fld, trns_SGS%f_trns, trns_SGS%forward)
        else if(SGS_param%iflag_SGS_buo_usage .ne. id_use_volume) then
          call start_elapsed_time(16)
          if (iflag_debug.eq.1) write(*,*)                              &
     &                        'sph_forward_trans_SGS_MHD SGS'
          call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,       &
     &        trns_SGS%forward, WK_sph, SGS_mul_FFTW, rj_fld)
          call end_elapsed_time(16)
!
            if(iflag_debug.eq.1) write(*,*)                             &
     &                      'magnify_vol_ave_SGS_buoyancy'
          call magnify_vol_ave_SGS_buoyancy(sph%sph_rtp, ipol,          &
     &        dynamic_SPH%ifld_sgs, dynamic_SPH%wk_sgs_buo,             &
     &        rj_fld, trns_SGS%f_trns, trns_SGS%forward)
        end if
        call end_elapsed_time(84)
!
      end subroutine product_buo_model_coefs_4_sph
!
!*   ------------------------------------------------------------------
!
      end module cal_dynamic_SGS_buoyancy
