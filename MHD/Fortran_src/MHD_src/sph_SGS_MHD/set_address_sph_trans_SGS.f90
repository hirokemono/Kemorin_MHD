!>@file   set_address_sph_trans_SGS.f90
!!@brief  module set_address_sph_trans_SGS
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine init_sph_trns_fld_similarity                         &
!!     &         (SPH_MHD, iphys, trns_SIMI,                            &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_SIMI
!!      subroutine init_sph_trns_fld_dyn_simi(SPH_MHD, iphys, trns_DYNS,&
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_DYNS
!!      subroutine set_addresses_trans_sph_Csim                         &
!!     &         (SGS_param, SPH_MHD, iphys, trns_Csim,                 &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(address_4_sph_trans), intent(inout) :: trns_Csim
!!@endverbatim
!
      module set_address_sph_trans_SGS
!
      use m_precision
!
      use t_phys_address
      use t_SPH_mesh_field_data
      use t_addresses_sph_transform
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_trns_fld_similarity                           &
     &         (SPH_MHD, iphys, trns_SIMI,                              &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use add_SGS_term_to_sph_trans
      use add_filter_fld_to_sph_trans
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_SIMI
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform (trns_SIMI): ',      &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_SIMI%backward%nfield = 0
      call alloc_sph_trns_field_name(trns_SIMI%backward)
!
!   filtered vector
      call add_fil_vector_sph_trns_by_pol                               &
     &   (SPH_MHD%ipol%filter_fld, iphys%filter_fld,                    &
     &    trns_SIMI%b_trns%filter_fld, trns_SIMI%backward)
!   filtered nonlinear terms
      call add_SGS_term_4_sph_trns_by_pol                               &
     &   (SPH_MHD%ipol%SGS_term, iphys%SGS_term,                        &
     &    trns_SIMI%b_trns%SGS_term, trns_SIMI%backward)
      trns_SIMI%backward%num_vector = trns_SIMI%backward%nfield
!
!   filtered scalar
      call add_fil_scalar_sph_trns_by_pol                               &
     &   (SPH_MHD%ipol%filter_fld, iphys%filter_fld,                    &
     &    trns_SIMI%b_trns%filter_fld, trns_SIMI%backward)
      trns_SIMI%backward%num_scalar = trns_SIMI%backward%nfield         &
     &                              - trns_SIMI%backward%num_vector
      trns_SIMI%backward%num_tensor = 0
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_SIMI%forward%nfield = 0
      call alloc_sph_trns_field_name(trns_SIMI%forward)
!
      call add_SGS_term_4_sph_trns_by_pol                               &
     &   (SPH_MHD%ipol%SGS_term, iphys%SGS_term,                        &
     &    trns_SIMI%f_trns%SGS_term, trns_SIMI%forward)
      trns_SIMI%forward%num_vector = trns_SIMI%forward%nfield
      trns_SIMI%forward%num_scalar = trns_SIMI%forward%nfield           &
     &                              - trns_SIMI%forward%num_vector
      trns_SIMI%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_SIMI%backward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_SIMI%forward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_SIMI%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_SIMI%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_SIMI%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_SIMI%forward%num_scalar
      end if
!
      end subroutine init_sph_trns_fld_similarity
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_trns_fld_dyn_simi(SPH_MHD, iphys, trns_DYNS,  &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use add_SGS_term_to_sph_trans
      use add_wide_f_fld_to_sph_trans
!
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_DYNS
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*)                                                      &
     &   'Spherical transform field table for dynamnic SGS (trns_DYNS)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_DYNS%backward%nfield = 0
      call alloc_sph_trns_field_name(trns_DYNS%backward)
!
      call add_wide_fil_vector_sph_trns                                 &
     &   (SPH_MHD%ipol%wide_filter_fld, iphys%wide_filter_fld,          &
     &    trns_DYNS%b_trns%wide_filter_fld, trns_DYNS%backward)
      call add_wide_SGS_term_4_sph_trns                                 &
     &   (SPH_MHD%ipol%wide_SGS, iphys%wide_SGS,                        &
     &    trns_DYNS%b_trns%wide_SGS, trns_DYNS%backward)
      call add_double_SGS_term_4_sph_trns                               &
     &   (SPH_MHD%ipol%dble_SGS, iphys%dble_SGS,                        &
     &    trns_DYNS%b_trns%dble_SGS, trns_DYNS%backward)
      trns_DYNS%backward%num_vector = trns_DYNS%backward%nfield
!
      call add_wide_fil_scalar_sph_trns                                 &
     &   (SPH_MHD%ipol%wide_filter_fld, iphys%wide_filter_fld,          &
     &    trns_DYNS%b_trns%wide_filter_fld, trns_DYNS%backward)
      trns_DYNS%backward%num_scalar = trns_DYNS%backward%nfield         &
     &                               - trns_DYNS%backward%num_vector
      trns_DYNS%backward%num_tensor = 0
!
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_DYNS%forward%nfield = 0
      call alloc_sph_trns_field_name(trns_DYNS%forward)
!
      trns_DYNS%forward%num_vector = 0
      trns_DYNS%forward%num_scalar = 0
      trns_DYNS%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_DYNS%backward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_DYNS%forward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_DYNS%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_DYNS%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_DYNS%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_DYNS%forward%num_scalar
      end if
!
      end subroutine init_sph_trns_fld_dyn_simi
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_Csim                           &
     &         (SGS_param, SPH_MHD, iphys, trns_Csim,                   &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use t_SGS_control_parameter
      use add_Csim_4_sph_trns
      use add_SGS_term_to_sph_trans
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_Csim
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &             'for model coefs (trns_Csim)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_Csim%backward%nfield = 0
      call alloc_sph_trns_field_name(trns_Csim%backward)
!
      trns_Csim%backward%num_vector = trns_Csim%backward%nfield
      trns_Csim%backward%num_scalar = trns_Csim%backward%nfield         &
     &                               - trns_Csim%backward%num_vector
      trns_Csim%backward%num_tensor = 0
!
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_Csim%forward%nfield = 0
      call alloc_sph_trns_field_name(trns_Csim%forward)
!
      trns_Csim%forward%num_vector = 0
      call add_Csim_4_sph_trns_by_pol                                   &
     &   (SPH_MHD%ipol%Csim, iphys%Csim, trns_Csim%f_trns%Csim,         &
     &    trns_Csim%forward)
      if(SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
        call add_SGS_eflux_sph_trns_by_pol                              &
     &     (SPH_MHD%ipol%SGS_ene_flux, iphys%SGS_ene_flux,              &
     &      trns_Csim%f_trns%SGS_ene_flux, trns_Csim%forward)
      end if
      trns_Csim%forward%num_scalar = trns_Csim%forward%nfield           &
     &                              - trns_Csim%forward%num_vector
      trns_Csim%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_Csim%backward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_Csim%forward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_Csim%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_Csim%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_Csim%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_Csim%forward%num_scalar
      end if
!
      end subroutine set_addresses_trans_sph_Csim
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_SGS
