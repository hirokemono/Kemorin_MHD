!>@file   init_sph_trans_SGS_model.f90
!!@brief  module init_sph_trans_SGS_model
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine init_sph_transform_SGS_model(SGS_param, d_rj,        &
!!     &          ipol, ipol_LES, iphys, iphys_LES, WK_LES,             &
!!     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
!!        integer(kind = kint), intent(inout) :: ncomp_max_trans
!!        integer(kind = kint), intent(inout) :: nvector_max_trans
!!        integer(kind = kint), intent(inout) :: nscalar_max_trans
!!      subroutine init_leg_fourier_trans_SGS_MHD                       &
!!     &         (SGS_param, sph, comms_sph, ncomp_max_trans,           &
!!     &          trans_p, WK, WK_LES, SR_sig, SR_r)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        integer(kind = kint), intent(in) :: ncomp_max_trans
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!!
      module init_sph_trans_SGS_model
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_solver_SR
      use t_spheric_parameter
      use t_SGS_control_parameter
      use t_sph_trans_comm_tbl
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
!
      implicit  none
!
      private :: init_sph_FFTs_for_SGS_model
      private :: init_sph_FFTs_for_each_SGS
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_SGS_model(SGS_param, d_rj,          &
     &          ipol, ipol_LES, iphys, iphys_LES, WK_LES,               &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use set_address_sph_trans_SGS
      use set_address_sph_trans_ngSGS
      use set_address_sph_trans_snap
      use address_sph_trans_SGS_snap
      use address_sph_trans_fil_force
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(phys_data), intent(in) :: d_rj
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
     &   (d_rj, ipol_LES, iphys_LES, WK_LES%trns_fil_MHD,               &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call init_sph_trns_filter_snap                                    &
     &   (d_rj, ipol_LES, iphys_LES, WK_LES%trns_fil_snap,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call init_sph_trns_filter_diff_vect                               &
     &   (d_rj, ipol_LES, iphys_LES, WK_LES%trns_fil_difv,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      if(SGS_param%iflag_SGS .eq. id_SGS_similarity) then
        if(iflag_debug .gt. 0) then
          write(*,*) 'Spherical transform field table ',                &
     &               'for similarity SGS (trns_SGS)'
        end if
        call set_sph_trns_address_fld_simi                              &
     &     (d_rj, ipol_LES, iphys_LES, WK_LES%trns_SGS,                 &
     &      ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          call set_sph_trns_address_dyn_simi                            &
     &       (d_rj, ipol_LES, iphys_LES, WK_LES%trns_DYNS,              &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
          call set_sph_trns_address_Csim                                &
     &      (SGS_param, d_rj, ipol_LES, iphys_LES, WK_LES%trns_Csim,    &
     &       ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
         end if
!
      else if(SGS_param%iflag_SGS .eq. id_SGS_NL_grad) then
        call set_sph_trns_address_ngrad_SGS                             &
     &     (d_rj, ipol_LES, iphys_LES, WK_LES%trns_SGS,                 &
     &      ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
        call set_sph_trns_address_ngrad_pre                             &
     &     (d_rj, ipol, iphys, WK_LES%trns_ngTMP,                       &
     &      ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          if(iflag_debug .gt. 0) then
            write(*,*) 'Spherical transform field table ',              &
     &                 'for similarity SGS (trns_SIMI)'
          end if
          call set_sph_trns_address_fld_simi                            &
     &       (d_rj, ipol_LES, iphys_LES, WK_LES%trns_SIMI,              &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
          call set_sph_trns_address_dyn_ngrad                           &
     &       (d_rj, ipol_LES, iphys_LES, WK_LES%trns_DYNG,              &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
          call set_addresses_trans_sph_ngCsim                           &
     &       (SGS_param, d_rj, ipol_LES, iphys_LES, WK_LES%trns_Csim,   &
     &        ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
         end if
      end if
!
      end subroutine init_sph_transform_SGS_model
!
!-----------------------------------------------------------------------
!
      subroutine init_leg_fourier_trans_SGS_MHD                         &
     &         (SGS_param, sph, comms_sph, ncomp_max_trans,             &
     &          trans_p, WK, WK_LES, SR_sig, SR_r)
!
      use t_work_4_sph_trans
      use t_sph_trans_arrays_MHD
!
      use init_sph_trans
      use init_FFT_4_MHD
      use pole_sph_transform
      use skip_comment_f
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_legendre_trans'
      call initialize_legendre_trans                                    &
     &   (trans_p%nvector_legendre, ncomp_max_trans, sph, comms_sph,    &
     &    trans_p%leg, trans_p%idx_trns, SR_sig, SR_r,                  &
     &    trans_p%iflag_SPH_recv)
!
      WK%iflag_MHD_FFT = trans_p%iflag_FFT
      call init_fourier_transform_4_MHD                                 &
     &   (sph%sph_rtp, comms_sph%comm_rtp,                              &
     &    WK%trns_MHD, WK%WK_FFTs_MHD, SR_r, WK%iflag_MHD_FFT)
!
      call init_sph_FFTs_for_SGS_model                                  &
     &   (WK%iflag_MHD_FFT, SGS_param, sph, comms_sph, WK_LES)
!
      trans_p%iflag_FFT = set_FFT_mode_4_snapshot(WK%iflag_MHD_FFT)
      call init_sph_FFT_select(my_rank, trans_p%iflag_FFT,              &
     &    sph%sph_rtp, comms_sph%comm_rtp,                              &
     &    ncomp_max_trans, ncomp_max_trans, WK%WK_FFTs)
!
      if(my_rank .eq. 0)  call write_import_table_mode(trans_p)
!
      end subroutine init_leg_fourier_trans_SGS_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine init_sph_FFTs_for_SGS_model                            &
     &         (iflag_ref_FFT, SGS_param, sph, comms_sph, WK_LES)
!
      integer(kind = kint), intent(in) :: iflag_ref_FFT
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(SGS_model_control_params), intent(in) :: SGS_param
!
      type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
!
!
      call init_sph_FFTs_for_each_SGS                                   &
     &   (iflag_ref_FFT, sph, comms_sph, WK_LES%trns_fil_MHD)
!
      if(SGS_param%iflag_SGS .eq. id_SGS_similarity) then
        call init_sph_FFTs_for_each_SGS                                 &
     &     (iflag_ref_FFT, sph, comms_sph, WK_LES%trns_SGS)
!
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          call init_sph_FFTs_for_each_SGS                               &
     &       (iflag_ref_FFT, sph, comms_sph, WK_LES%trns_DYNS)
          call init_sph_FFTs_for_each_SGS                               &
     &       (iflag_ref_FFT, sph, comms_sph, WK_LES%trns_Csim)
         end if
!
      else if(SGS_param%iflag_SGS .eq. id_SGS_NL_grad) then
        call init_sph_FFTs_for_each_SGS                                 &
     &     (iflag_ref_FFT, sph, comms_sph, WK_LES%trns_SGS)
        call init_sph_FFTs_for_each_SGS                                 &
     &     (iflag_ref_FFT, sph, comms_sph, WK_LES%trns_ngTMP)
!
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          call init_sph_FFTs_for_each_SGS                               &
     &       (iflag_ref_FFT, sph, comms_sph, WK_LES%trns_SIMI)
          call init_sph_FFTs_for_each_SGS                               &
     &       (iflag_ref_FFT, sph, comms_sph, WK_LES%trns_DYNG)
          call init_sph_FFTs_for_each_SGS                               &
     &       (iflag_ref_FFT, sph, comms_sph, WK_LES%trns_Csim)
         end if
      end if
!
      end subroutine init_sph_FFTs_for_SGS_model
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_FFTs_for_each_SGS                             &
     &         (iflag_ref_FFT, sph, comms_sph, trns_SGS)
!
      use t_sph_trans_arrays_SGS_MHD
!
      integer(kind = kint), intent(in) :: iflag_ref_FFT
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
!
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
!
!
      call init_sph_FFT_select(my_rank, iflag_ref_FFT,                  &
     &    sph%sph_rtp, comms_sph%comm_rtp,                              &
     &    trns_SGS%backward%ncomp, trns_SGS%forward%ncomp,              &
     &    trns_SGS%WK_FFTs_SGS)
!
      end subroutine init_sph_FFTs_for_each_SGS
!
!-----------------------------------------------------------------------
!
      end module init_sph_trans_SGS_model
