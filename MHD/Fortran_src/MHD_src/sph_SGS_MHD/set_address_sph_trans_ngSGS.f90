!>@file   set_address_sph_trans_ngSGS.f90
!!@brief  module set_address_sph_trans_ngSGS
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine init_sph_trns_fld_ngrad_pre(ipol, iphys, trns_ngTMP, &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_ngTMP
!!
!!      subroutine init_sph_trns_fld_ngrad_SGS(ipol, iphys, trns_SGS,   &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!
!!      subroutine init_sph_trns_fld_dyn_ngrad(ipol, iphys, trns_DYNG,  &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_DYNG
!!
!!      subroutine set_addresses_trans_sph_ngCsim                       &
!!     &         (SGS_param, ipol, iphys, trns_Csim,                    &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_Csim
!!@endverbatim
!
      module set_address_sph_trans_ngSGS
!
      use m_precision
!
      use t_phys_address
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
      subroutine init_sph_trns_fld_ngrad_pre(ipol, iphys, trns_ngTMP,   &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use add_diff_vect_to_sph_trans
!
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_ngTMP
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &              'for nonlinear Gradient SGS (trns_ngTMP)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      trns_ngTMP%backward%nfield = 0
      call alloc_sph_trns_field_name(trns_ngTMP%backward)
!
      call add_diff_vect_sph_trns_by_pol                                &
     &   (ipol%diff_vector, iphys%diff_vector,                          &
     &    trns_ngTMP%b_trns%diff_vector, trns_ngTMP%backward)
      call add_grad_4_sph_trns_by_pol                                   &
     &   (ipol%grad_fld, iphys%grad_fld,                                &
     &    trns_ngTMP%b_trns%grad_fld, trns_ngTMP%backward)
      trns_ngTMP%backward%num_vector = trns_ngTMP%backward%nfield
      trns_ngTMP%backward%num_scalar = trns_ngTMP%backward%nfield       &
     &                              - trns_ngTMP%backward%num_vector
      trns_ngTMP%backward%num_tensor = 0
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
!
      trns_ngTMP%forward%nfield = 0
      call alloc_sph_trns_field_name(trns_ngTMP%forward)
!
      trns_ngTMP%forward%num_vector = trns_ngTMP%forward%nfield
      call add_diff_vect_scalar_trns_bpol                               &
     &   (ipol%diff_vector, iphys%diff_vector,                          &
     &    trns_ngTMP%f_trns%diff_vector, trns_ngTMP%forward)
      trns_ngTMP%forward%num_scalar = trns_ngTMP%forward%nfield         &
     &                               - trns_ngTMP%forward%num_vector
      trns_ngTMP%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_ngTMP%backward,             &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_ngTMP%forward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_ngTMP%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_ngTMP%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_ngTMP%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_ngTMP%forward%num_scalar
      end if
!
      end subroutine init_sph_trns_fld_ngrad_pre
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_trns_fld_ngrad_SGS(ipol, iphys, trns_SGS,     &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use add_SGS_term_to_sph_trans
!
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &             'for similarity SGS (trns_SGS)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_SGS%backward%nfield = 0
      call alloc_sph_trns_field_name(trns_SGS%backward)
!
      trns_SGS%backward%num_vector = trns_SGS%backward%nfield
      trns_SGS%backward%num_scalar = trns_SGS%backward%nfield           &
     &                              - trns_SGS%backward%num_vector
      trns_SGS%backward%num_tensor = 0
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_SGS%forward%nfield = 0
      call alloc_sph_trns_field_name(trns_SGS%forward)
!
      call add_SGS_term_4_sph_trns_by_pol                               &
     &   (ipol%SGS_term, iphys%SGS_term,                                &
     &    trns_SGS%f_trns%SGS_term, trns_SGS%forward)
      trns_SGS%forward%num_vector = trns_SGS%forward%nfield
      trns_SGS%forward%num_scalar = trns_SGS%forward%nfield             &
     &                              - trns_SGS%forward%num_vector
      trns_SGS%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_SGS%backward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_SGS%forward,                &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_SGS%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_SGS%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_SGS%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_SGS%forward%num_scalar
      end if
!
      end subroutine init_sph_trns_fld_ngrad_SGS
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_trns_fld_dyn_ngrad(ipol, iphys, trns_DYNG,    &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use add_diff_fil_vec_to_trans
      use add_SGS_term_to_sph_trans
!
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_DYNG
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &              'for dynamic nonlinear Gradient SGS (trns_DYNG)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      trns_DYNG%backward%nfield = 0
      call alloc_sph_trns_field_name(trns_DYNG%backward)
!
      call add_diff_fil_vec_sph_trns_pol                                &
     &   (ipol%diff_fil_vect, iphys%diff_fil_vect,                      &
     &    trns_DYNG%b_trns%diff_fil_vect, trns_DYNG%backward)
      call add_grad_filter_fld_4_sph_trns                               &
     &   (ipol%grad_fil_fld, iphys%grad_fil_fld,                        &
     &    trns_DYNG%b_trns%grad_fil_fld, trns_DYNG%backward)
      call add_double_SGS_term_4_sph_trns                               &
     &   (ipol%dble_SGS, iphys%dble_SGS,                                &
     &    trns_DYNG%b_trns%dble_SGS, trns_DYNG%backward)
      trns_DYNG%backward%num_vector = trns_DYNG%backward%nfield
      trns_DYNG%backward%num_scalar = trns_DYNG%backward%nfield         &
     &                              - trns_DYNG%backward%num_vector
      trns_DYNG%backward%num_tensor = 0
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
!
      trns_DYNG%forward%nfield = 0
      call alloc_sph_trns_field_name(trns_DYNG%forward)
!
      trns_DYNG%forward%num_vector = trns_DYNG%forward%nfield
      call add_diff_fil_vec_4_scalar_trns                               &
     &   (ipol%diff_fil_vect, iphys%diff_fil_vect,                      &
     &    trns_DYNG%f_trns%diff_fil_vect, trns_DYNG%forward)
      trns_DYNG%forward%num_scalar = trns_DYNG%forward%nfield           &
     &                               - trns_DYNG%forward%num_vector
      trns_DYNG%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_DYNG%backward,              &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_DYNG%forward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_DYNG%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_DYNG%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_DYNG%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_DYNG%forward%num_scalar
      end if
!
      end subroutine init_sph_trns_fld_dyn_ngrad
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_ngCsim                         &
     &         (SGS_param, ipol, iphys, trns_Csim,                      &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use t_SGS_control_parameter
      use add_Csim_4_sph_trns
      use add_SGS_term_to_sph_trans
      use add_SGS_eflux_to_sph_trans
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_Csim
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Field table for the second SGS terms ',             &
     &             'by n. gradient model (trns_Csim)'
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_Csim%backward%nfield = 0
      call alloc_sph_trns_field_name(trns_Csim%backward)
!
      call add_wide_SGS_term_4_sph_trns                                 &
     &   (ipol%wide_SGS, iphys%wide_SGS,                                &
     &    trns_Csim%b_trns%wide_SGS, trns_Csim%backward)
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
     &   (ipol%Csim, iphys%Csim, trns_Csim%f_trns%Csim,                 &
     &    trns_Csim%forward)
      if(SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
        call add_SGS_eflux_sph_trns_by_pol                              &
     &     (ipol%SGS_ene_flux, iphys%SGS_ene_flux,                      &
     &      trns_Csim%f_trns%SGS_ene_flux, trns_Csim%forward)
      end if
      trns_Csim%forward%num_scalar = trns_Csim%forward%nfield           &
     &                              - trns_Csim%forward%num_vector
      trns_Csim%forward%num_tensor = 0
!
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
      end subroutine set_addresses_trans_sph_ngCsim
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_ngSGS
