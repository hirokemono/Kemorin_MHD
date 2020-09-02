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
!!      subroutine set_sph_trns_address_fld_simi                        &
!!     &         (d_rj, ipol_LES, iphys_LES, trns_SIMI,                 &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_address_sph_trans), intent(inout) :: trns_SIMI
!!      subroutine set_sph_trns_address_dyn_simi                        &
!!     &         (d_rj, ipol_LES, iphys_LES, trns_DYNS,                 &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_address_sph_trans), intent(inout) :: trns_DYNS
!!      subroutine set_sph_trns_address_Csim                            &
!!     &         (SGS_param, d_rj, ipol_LES, iphys_LES, trns_Csim,      &
!!     &         ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_address_sph_trans), intent(inout) :: trns_Csim
!!@endverbatim
!
      module set_address_sph_trans_SGS
!
      use m_precision
!
      use t_phys_data
      use t_SGS_model_addresses
      use t_sph_trans_arrays_SGS_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_sph_trns_address_fld_simi                          &
     &         (d_rj, ipol_LES, iphys_LES, trns_SIMI,                   &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use t_addresses_sph_transform
      use address_sph_trans_dyn_simi
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_address_sph_trans), intent(inout) :: trns_SIMI
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*)                                                      &
     &   'Spherical transform field table for dynamnic SGS (trns_SIMI)'
      end if
!
      call bwd_sph_trns_fld_similarity(d_rj, ipol_LES, iphys_LES,       &
     &    trns_SIMI%b_trns_LES, trns_SIMI%backward)
      call fwd_sph_trns_fld_similarity(d_rj, ipol_LES, iphys_LES,       &
     &    trns_SIMI%f_trns_LES, trns_SIMI%forward)
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
      end subroutine set_sph_trns_address_fld_simi
!
!-----------------------------------------------------------------------
!
      subroutine set_sph_trns_address_dyn_simi                          &
     &         (d_rj, ipol_LES, iphys_LES, trns_DYNS,                   &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use t_addresses_sph_transform
      use address_sph_trans_dyn_simi
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_address_sph_trans), intent(inout) :: trns_DYNS
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*)                                                      &
     &   'Spherical transform field table for dynamnic SGS (trns_DYNS)'
      end if
!
      call bwd_trans_address_dyn_simi(d_rj, ipol_LES, iphys_LES,        &
     &    trns_DYNS%b_trns_LES, trns_DYNS%backward)
      call empty_trans_address(trns_DYNS%forward)
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
      end subroutine set_sph_trns_address_dyn_simi
!
!-----------------------------------------------------------------------
!
      subroutine set_sph_trns_address_Csim                              &
     &         (SGS_param, d_rj, ipol_LES, iphys_LES, trns_Csim,        &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use t_addresses_sph_transform
      use t_SGS_control_parameter
      use address_sph_trans_dyn_simi
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_address_sph_trans), intent(inout) :: trns_Csim
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &             'for model coefs (trns_Csim)'
      end if
!
      call empty_trans_address(trns_Csim%backward)
      call fwd_trans_address_Csim(SGS_param, d_rj, ipol_LES, iphys_LES, &
     &    trns_Csim%f_trns_LES, trns_Csim%forward)
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
      end subroutine set_sph_trns_address_Csim
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_SGS
