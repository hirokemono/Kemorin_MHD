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
!!     &         (SGS_param, MHD_prop, sph_MHD_bc, ipol, idpdr, itor,   &
!!     &          iphys, sph, comms_sph, omega_sph, trans_p, WK, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol, idpdr, itor
!!        type(phys_address), intent(in) :: iphys
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(phys_data), intent(inout) :: rj_fld
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
      use t_control_parameter
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_addresses_sph_transform
      use t_poloidal_rotation
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
     &         (SGS_param, MHD_prop, sph_MHD_bc, ipol, idpdr, itor,     &
     &          iphys, sph, comms_sph, omega_sph, trans_p, WK, rj_fld)
!
      use set_address_sph_trans_MHD
      use set_address_sph_trans_SGS
      use set_address_sph_trans_snap
      use set_address_sph_trans_tmp
      use init_sphrical_transform_mhd
      use pole_sph_transform
      use MHD_FFT_selector
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: rj_fld
!
!>      total number of vectors for spherical harmonics transform
      integer(kind = kint), save :: ncomp_max_trans = 0
!>      total number of vectors for spherical harmonics transform
      integer(kind = kint), save :: nvector_max_trans = 0
!>      total number of svalars for spherical harmonics transform
      integer(kind = kint), save :: nscalar_max_trans = 0
!
!
      call init_pole_transform(sph%sph_rtp)
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD(MHD_prop, ipol, WK%trns_MHD,     &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_trans_sph_SGS(ipol, WK%trns_SGS,               &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_trans_sph_Csim(ipol, WK%trns_Csim,             &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_snapshot_trans(ipol, iphys, WK%trns_snap,      &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_temporal_trans(ipol, iphys, WK%trns_tmp,       &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      if(iflag_debug .ge. iflag_routine_msg) then
        call check_address_trans_sph_MHD(ipol, idpdr, itor, iphys,      &
     &      WK%trns_MHD, ncomp_max_trans)
        call check_address_trans_sph_SGS(ipol, idpdr, itor, iphys,      &
     &      WK%trns_SGS)
        call check_address_trans_sph_Csim(ipol, idpdr, itor, iphys,     &
     &      WK%trns_Csim)
        call check_address_trans_sph_snap(ipol, idpdr, itor, iphys,     &
     &      WK%trns_snap)
        call check_address_trans_sph_tmp(ipol, idpdr, itor, iphys,      &
     &      WK%trns_tmp)
      end if
!
      call alloc_sph_trans_address(sph%sph_rtp, WK)
!
      call sel_sph_transform_MHD                                        &
     &   (SGS_param%iflag_SGS, ipol,  MHD_prop%fl_prop,                 &
     &    sph_MHD_bc%sph_bc_U, sph, comms_sph, omega_sph,               &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    WK%trns_MHD, WK%trns_SGS, WK%WK_sph,                          &
     &    WK%MHD_mul_FFTW, WK%SGS_mul_FFTW, trans_p,                    &
     &    WK%gt_cor, WK%cor_rlm, rj_fld)
!
      end subroutine init_sph_transform_SGS_MHD
!
!-----------------------------------------------------------------------
!
      end module init_sph_trans_SGS_MHD
