!>@file   scale_similarity_sph_SGS.f90
!!@brief  module scale_similarity_sph_SGS
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine cal_scale_similarity_sph_SGS                         &
!!     &         (sph, comms_sph, MHD_prop, trans_p, WK_sph,            &
!!     &          dynamic_SPH, ipol, rj_fld, trns_SIMI)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_SIMI
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
!
      module scale_similarity_sph_SGS
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
      subroutine cal_scale_similarity_sph_SGS                           &
     &         (sph, comms_sph, MHD_prop, trans_p, WK_sph,              &
     &          dynamic_SPH, ipol, rj_fld, trns_SIMI)
!
      use sph_transforms_4_SGS
      use cal_SGS_terms_sph_MHD
      use cal_filtered_sph_fields
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(inout) :: trns_SIMI
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
!
!
!   ----  Lead filtered forces for SGS terms
      if (iflag_debug.ge.1) write(*,*)                                  &
     &                    'cal_sph_base_filtering_fields'
      call start_elapsed_time(81)
      call cal_sph_base_filtering_fields                                &
     &   (sph%sph_rj, ipol, dynamic_SPH%sph_filters(1), rj_fld)
      if (iflag_debug.ge.1) write(*,*) 'cal_sph_base_filtering_forces'
      call cal_sph_base_filtering_forces                                &
     &   (sph%sph_rj, ipol, dynamic_SPH%sph_filters(1), rj_fld)
      call end_elapsed_time(81)
!
      call start_elapsed_time(14)
      if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_SGS_MHD SGS'
      call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_SIMI%backward, WK_sph, trns_SIMI%mul_FFTW)
      call end_elapsed_time(14)
!
      call start_elapsed_time(15)
      if (iflag_debug.eq.1) write(*,*) 'similarity_SGS_terms_rtp'
      call similarity_SGS_terms_rtp(sph%sph_rtp, MHD_prop,              &
     &    trns_SIMI%b_trns, trns_SIMI%f_trns,                           &
     &    trns_SIMI%backward, trns_SIMI%forward)
      call end_elapsed_time(15)
!
      end subroutine cal_scale_similarity_sph_SGS
!
!*   ------------------------------------------------------------------
!
      end module scale_similarity_sph_SGS
