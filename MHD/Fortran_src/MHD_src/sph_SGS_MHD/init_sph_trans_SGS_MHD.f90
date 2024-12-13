!>@file   init_sph_trans_SGS_MHD.f90
!!@brief  module init_sph_trans_SGS_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine init_4th_fdms_for_sph_MHD(id_check, sph,  MHD_prop,  &
!!     &          radial_variation, r_4th, sph_MHD_bc)
!!        integer(kind = kint), intent(in) :: id_check
!!        type(sph_grids), intent(in) :: sph
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        type(fdm_matrices), intent(inout) :: r_4th
!!        type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!!      subroutine init_sph_transform_SGS_MHD                           &
!!     &         (SPH_model, SGS_par, ipol_LES, iphys_LES, iphys,       &
!!     &          trans_p, WK, WK_LES, SPH_MHD, SR_sig, SR_r)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
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
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_4th_fdms_for_sph_MHD(id_check, sph,  MHD_prop,    &
     &          radial_variation, r_4th, sph_MHD_bc)
!
      use t_spheric_parameter
      use t_control_parameter
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_coef_sph_velocity_BCs
      use set_bc_sph_mhd
      use forth_fdm_node_coefs
!
      integer(kind = kint), intent(in) :: id_check
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_data), intent(in) :: radial_variation
!
      type(fdm_matrices), intent(inout) :: r_4th
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
      real(kind = kreal) :: h_rho_in, h_rho_out
!
!
      if (iflag_debug.gt.0) write(*,*) 'const_forth_fdm_coefs'
      call const_forth_fdm_coefs(sph%sph_rj, r_4th)
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call density_diff_at_boundaries                                 &
     &     (MHD_prop%fl_prop, radial_variation, sph_MHD_bc%sph_bc_U,    &
     &      h_rho_in, h_rho_out)
        call set_boundary_sph_4th_fdm                                   &
     &     (sph_MHD_bc%sph_bc_U%kr_in, sph_MHD_bc%sph_bc_U%kr_out,      &
     &      h_rho_in, h_rho_out, sph%sph_rj, r_4th,                     &
     &      sph_MHD_bc%bc_fdms_U)
        if(iflag_debug .gt. 0) then
          call check_sph_4th_fdm_boundaries(id_check,                   &
     &                                      sph_MHD_bc%bc_fdms_U)
        end if
      end if
!
      end subroutine init_4th_fdms_for_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_SGS_MHD                             &
     &         (SPH_model, SGS_par, ipol_LES, iphys_LES, iphys,         &
     &          trans_p, WK, WK_LES, SPH_MHD, SR_sig, SR_r)
!
      use t_SPH_MHD_model_data
      use t_SGS_control_parameter
      use t_phys_address
      use t_SGS_model_addresses
      use t_work_4_sph_trans
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
      use t_SPH_mesh_field_data
      use t_solver_SR
!
      use set_address_sph_trans_MHD
      use set_address_sph_trans_SGS
      use set_address_sph_trans_ngSGS
      use set_address_sph_trans_snap
      use address_sph_trans_SGS_snap
      use init_sphrical_transform_MHD
      use init_sph_trans_SGS_model
      use check_sph_mhd_openmp_size
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
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
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
      call s_check_sph_mhd_openmp_size(WK%WK_leg, SPH_MHD%sph)
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD                                  &
     &   (SPH_MHD%fld, SPH_MHD%ipol, iphys, WK%trns_MHD,                &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call init_sph_transform_SGS_model(SGS_par%model_p, SPH_MHD%fld,   &
     &    SPH_MHD%ipol, ipol_LES, iphys, iphys_LES, WK_LES,             &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call set_addresses_snapshot_trans                                 &
     &   (SPH_MHD%fld, SPH_MHD%ipol, iphys, WK%trns_snap,               &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_ene_flux_trans                                 &
     &   (SPH_MHD%fld, SPH_MHD%ipol, iphys, WK%trns_eflux,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_diff_vect_trans                                &
     &   (SPH_MHD%fld, SPH_MHD%ipol, iphys, WK%trns_difv,               &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call set_addresses_SGS_snap_trans                                 &
     &   (SPH_MHD%fld, ipol_LES, iphys_LES, WK_LES%trns_SGS_snap,       &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call alloc_sph_trans_address(SPH_MHD%sph, WK)
      call alloc_SGS_sph_trans_address(SPH_MHD%sph, WK_LES)
!
!
      call init_leg_fourier_trans_SGS_MHD                               &
     &   (SGS_par%model_p, SPH_MHD%sph, SPH_MHD%comms, ncomp_max_trans, &
     &    trans_p, WK, WK_LES, SR_sig, SR_r)
!
      if (iflag_debug.eq.1) write(*,*) 'init_work_4_coriolis'
      call init_work_4_coriolis                                         &
     &   (SPH_model%sph_MHD_bc, SPH_MHD%sph, trans_p, WK)
!
      call sel_sph_transform_MHD                                        &
     &   (SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                     &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_model%omega_sph,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    WK%trns_MHD, WK%WK_leg, WK%WK_FFTs_MHD, trans_p,              &
     &    WK%gt_cor, WK%cor_rlm, SPH_MHD%fld, SR_sig, SR_r)
!
      end subroutine init_sph_transform_SGS_MHD
!
!-----------------------------------------------------------------------
!
      end module init_sph_trans_SGS_MHD
