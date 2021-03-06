!>@file   cal_dynamic_SGS_buoyancy.f90
!!@brief  module cal_dynamic_SGS_buoyancy
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate SGS buoyancy terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine product_buo_model_coefs_4_sph                        &
!!     &         (istep_dynamic, SGS_param, sph, ipol_LES,              &
!!     &          trns_SGS, dynamic_SPH, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_grids), intent(in) :: sph
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(SGS_address_sph_trans), intent(inout) :: trns_SGS
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
!
      use calypso_mpi
!
      use t_physical_property
      use t_SGS_control_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_filtering_data
      use t_sph_filtering
      use t_phys_data
      use t_SGS_model_addresses
!
      implicit none
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine product_buo_model_coefs_4_sph                          &
     &         (istep_dynamic, SGS_param, sph, ipol_LES,                &
     &          trns_SGS, dynamic_SPH, rj_fld)
!
      use t_SGS_buoyancy_sph
      use SGS_buo_coefs_sph_MHD
!
      integer(kind = kint), intent(in) :: istep_dynamic
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(SGS_param%iflag_SGS_buo_usage .eq. id_use_zonal) then
!        write(*,*) 'prod_SGS_buoyancy_to_Reynolds'
        call prod_SGS_buoyancy_to_Reynolds                              &
     &     (sph%sph_rtp, dynamic_SPH%sph_d_grp,                         &
     &      trns_SGS%f_trns_LES, dynamic_SPH%iak_sgs_term,              &
     &      dynamic_SPH%wk_sgs, trns_SGS%forward)
!
      else if(SGS_param%iflag_SGS_buo_usage .eq. id_use_sphere) then
        if(istep_dynamic .eq. 0) then
          if (iflag_debug.eq.1) write(*,*)                              &
     &                      'sphere_averaged_SGS_buoyancy'
          call sphere_averaged_SGS_buoyancy(sph%sph_rj, sph%sph_rtp,    &
     &        ipol_LES%Csim, rj_fld, dynamic_SPH%wk_sgs_buo)
        end if
!
!        if(iflag_debug.eq.1) write(*,*)                                &
!     &                    'magnify_sph_ave_SGS_buoyancy'
        call magnify_sph_ave_SGS_buoyancy(sph%sph_rtp,                  &
     &      dynamic_SPH%iak_sgs_term, dynamic_SPH%wk_sgs_buo,           &
     &      trns_SGS%f_trns_LES, trns_SGS%forward)
!
      else if(SGS_param%iflag_SGS_buo_usage .eq. id_use_volume) then
        if(istep_dynamic .eq. 0) then
          if (iflag_debug.eq.1) write(*,*)                              &
     &                     'volume_averaged_SGS_buoyancy'
          call volume_averaged_SGS_buoyancy(sph%sph_params, sph%sph_rj, &
     &        ipol_LES%Csim, rj_fld, dynamic_SPH%wk_sgs_buo)
        end if
!
!        if(iflag_debug.eq.1) write(*,*)                                &
!     &                    'magnify_vol_ave_SGS_buoyancy'
        call magnify_vol_ave_SGS_buoyancy(sph%sph_rtp,                  &
     &      dynamic_SPH%iak_sgs_term, dynamic_SPH%wk_sgs_buo,           &
     &      trns_SGS%f_trns_LES, trns_SGS%forward)
      end if
!
      end subroutine product_buo_model_coefs_4_sph
!
!*   ------------------------------------------------------------------
!
      end module cal_dynamic_SGS_buoyancy
