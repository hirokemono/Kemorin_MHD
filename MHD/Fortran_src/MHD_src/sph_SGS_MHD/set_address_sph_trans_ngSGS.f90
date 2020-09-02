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
!!      subroutine set_sph_trns_address_ngrad_pre                       &
!!     &         (d_rj, ipol, iphys, trns_ngTMP,                        &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_address_sph_trans), intent(inout) :: trns_ngTMP
!!
!!      subroutine set_sph_trns_address_ngrad_SGS                       &
!!     &         (d_rj, ipol_LES, iphys_LES, trns_SGS,                  &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_address_sph_trans), intent(inout) :: trns_SGS
!!
!!      subroutine set_sph_trns_address_dyn_ngrad                       &
!!     &         (d_rj, ipol_LES, iphys_LES, trns_DYNG,                 &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_address_sph_trans), intent(inout) :: trns_DYNG
!!
!!      subroutine set_addresses_trans_sph_ngCsim                       &
!!     &        (SGS_param, d_rj, ipol_LES, iphys_LES, trns_Csim,       &
!!     &         ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_address_sph_trans), intent(inout) :: trns_Csim
!!@endverbatim
!
      module set_address_sph_trans_ngSGS
!
      use m_precision
!
      use t_phys_data
      use t_phys_address
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
      subroutine set_sph_trns_address_ngrad_pre                         &
     &         (d_rj, ipol, iphys, trns_ngTMP,                          &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use t_addresses_sph_transform
      use address_sph_trans_ngSGS
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: iphys
      type(SGS_address_sph_trans), intent(inout) :: trns_ngTMP
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &              'for nonlinear Gradient SGS (trns_ngTMP)'
      end if
!
      call bwd_trns_address_fld_ngrad_pre                               &
     &   (d_rj, ipol, iphys, trns_ngTMP%b_trns, trns_ngTMP%backward)
      call fwd_trns_address_fld_ngrad_pre                               &
     &   (d_rj, ipol, iphys, trns_ngTMP%f_trns, trns_ngTMP%forward)
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
      end subroutine set_sph_trns_address_ngrad_pre
!
!-----------------------------------------------------------------------
!
      subroutine set_sph_trns_address_ngrad_SGS                         &
     &         (d_rj, ipol_LES, iphys_LES, trns_SGS,                    &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use t_addresses_sph_transform
      use address_sph_trans_ngSGS
      use address_sph_trans_dyn_simi
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &             'for similarity SGS (trns_SGS)'
      end if
!
      call empty_trans_address(trns_SGS%backward)
      call fwd_trns_address_fld_ngrad_SGS(d_rj, ipol_LES, iphys_LES,    &
     &    trns_SGS%f_trns_LES, trns_SGS%forward)
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
      end subroutine set_sph_trns_address_ngrad_SGS
!
!-----------------------------------------------------------------------
!
      subroutine set_sph_trns_address_dyn_ngrad                         &
     &         (d_rj, ipol_LES, iphys_LES, trns_DYNG,                   &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use t_addresses_sph_transform
      use address_sph_trans_ngSGS
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_address_sph_trans), intent(inout) :: trns_DYNG
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table ',                  &
     &              'for dynamic nonlinear Gradient SGS (trns_DYNG)'
      end if
!
      call bwd_trns_address_fld_dyn_ngrad(d_rj, ipol_LES, iphys_LES,    &
     &    trns_DYNG%b_trns_LES, trns_DYNG%backward)
      call fwd_trns_address_fld_dyn_ngrad(d_rj, ipol_LES, iphys_LES,    &
     &    trns_DYNG%f_trns_LES, trns_DYNG%forward)
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
      end subroutine set_sph_trns_address_dyn_ngrad
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_trans_sph_ngCsim                         &
     &        (SGS_param, d_rj, ipol_LES, iphys_LES, trns_Csim,         &
     &         ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use t_SGS_control_parameter
      use t_addresses_sph_transform
      use address_sph_trans_ngSGS
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
      type(SGS_address_sph_trans), intent(inout) :: trns_Csim
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Field table for the second SGS terms ',             &
     &             'by n. gradient model (trns_Csim)'
      end if
!
      call bwd_trans_address_sph_ngCsim(d_rj, ipol_LES, iphys_LES,      &
     &    trns_Csim%b_trns_LES, trns_Csim%backward)
      call fwd_trans_address_sph_ngCsim                                 &
     &   (SGS_param, d_rj, ipol_LES, iphys_LES,                         &
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
      end subroutine set_addresses_trans_sph_ngCsim
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_ngSGS
