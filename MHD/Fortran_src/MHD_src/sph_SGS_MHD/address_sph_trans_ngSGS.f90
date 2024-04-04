!>@file   address_sph_trans_ngSGS.f90
!!@brief  module address_sph_trans_ngSGS
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine bwd_trns_address_fld_ngrad_pre                       &
!!     &         (d_rj, ipol, iphys, b_trns, trns_bwd)
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(inout) :: b_trns
!!        type(spherical_transform_data), intent(inout) :: trns_bwd
!!      subroutine fwd_trns_address_fld_ngrad_pre                       &
!!     &         (d_rj, ipol, iphys, f_trns, trns_fwd)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(inout) :: f_trns
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!
!!      subroutine fwd_trns_address_fld_ngrad_SGS                       &
!!     &         (d_rj, ipol_LES, iphys_LES, f_trns_LES, trns_fwd)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_model_addresses), intent(inout) :: f_trns_LES
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!
!!      subroutine bwd_trns_address_fld_dyn_ngrad                       &
!!     &         (d_rj, ipol_LES, iphys_LES, b_trns_LES, trns_bwd)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_model_addresses), intent(inout) :: b_trns_LES
!!        type(spherical_transform_data), intent(inout) :: trns_bwd
!!      subroutine fwd_trns_address_fld_dyn_ngrad                       &
!!     &         (d_rj, ipol_LES, iphys_LES, f_trns_LES, trns_fwd)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_model_addresses), intent(inout) :: f_trns_LES
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!
!!      subroutine bwd_trans_address_sph_ngCsim                         &
!!     &         (d_rj, ipol_LES, iphys_LES, b_trns_LES, trns_bwd)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_model_addresses), intent(inout) :: b_trns_LES
!!        type(spherical_transform_data), intent(inout) :: trns_bwd
!!      subroutine fwd_trans_address_sph_ngCsim(SGS_param, d_rj,        &
!!     &          ipol_LES, iphys_LES, f_trns_LES, trns_fwd)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!!        type(SGS_model_addresses), intent(inout) :: f_trns_LES
!!        type(spherical_transform_data), intent(inout) :: trns_fwd
!!@endverbatim
!
      module address_sph_trans_ngSGS
!
      use m_precision
      use m_machine_parameter
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
      subroutine bwd_trns_address_fld_ngrad_pre                         &
     &         (d_rj, ipol, iphys, b_trns, trns_bwd)
!
      use add_diff_vect_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: iphys
!
      type(phys_address), intent(inout) :: b_trns
      type(spherical_transform_data), intent(inout) :: trns_bwd
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      trns_bwd%nfield = 0
      call alloc_sph_trns_field_name(trns_bwd)
!
      call add_diff_vect_sph_trns_by_pol                                &
     &   (d_rj, ipol%diff_vector, iphys%diff_vector,                    &
     &    b_trns%diff_vector, trns_bwd)
      call add_grad_4_sph_trns_by_pol                                   &
     &   (d_rj, ipol%grad_fld, iphys%grad_fld,                          &
     &    b_trns%grad_fld, trns_bwd)
      trns_bwd%num_vector = trns_bwd%nfield
      trns_bwd%num_scalar = trns_bwd%nfield - trns_bwd%num_vector
      trns_bwd%num_tensor = 0
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      end subroutine bwd_trns_address_fld_ngrad_pre
!
!-----------------------------------------------------------------------
!
      subroutine fwd_trns_address_fld_ngrad_pre                         &
     &         (d_rj, ipol, iphys, f_trns, trns_fwd)
!
      use add_diff_vect_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: iphys
!
      type(phys_address), intent(inout) :: f_trns
      type(spherical_transform_data), intent(inout) :: trns_fwd
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      trns_fwd%num_vector = trns_fwd%nfield
      call add_diff_vect_scalar_trns_bpol                               &
     &   (d_rj, ipol%diff_vector, iphys%diff_vector,                    &
     &    f_trns%diff_vector, trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trns_address_fld_ngrad_pre
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine fwd_trns_address_fld_ngrad_SGS                         &
     &         (d_rj, ipol_LES, iphys_LES, f_trns_LES, trns_fwd)
!
      use add_SGS_term_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!
      type(SGS_model_addresses), intent(inout) :: f_trns_LES
      type(spherical_transform_data), intent(inout) :: trns_fwd
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, grid data'
      end if
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      call add_SGS_term_4_sph_trns_by_pol                               &
     &   (d_rj, ipol_LES%SGS_term, iphys_LES%SGS_term,                  &
     &    f_trns_LES%SGS_term, trns_fwd)
      trns_fwd%num_vector = trns_fwd%nfield
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trns_address_fld_ngrad_SGS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine bwd_trns_address_fld_dyn_ngrad                         &
     &         (d_rj, ipol_LES, iphys_LES, b_trns_LES, trns_bwd)
!
      use add_diff_fil_vec_to_trans
      use add_SGS_term_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!
      type(SGS_model_addresses), intent(inout) :: b_trns_LES
      type(spherical_transform_data), intent(inout) :: trns_bwd
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      trns_bwd%nfield = 0
      call alloc_sph_trns_field_name(trns_bwd)
!
      call add_diff_fil_vec_sph_trns_pol                                &
     &   (d_rj, ipol_LES%diff_fil_vect, iphys_LES%diff_fil_vect,        &
     &    b_trns_LES%diff_fil_vect, trns_bwd)
      call add_grad_filter_fld_4_sph_trns                               &
     &   (d_rj, ipol_LES%grad_fil_fld, iphys_LES%grad_fil_fld,          &
     &    b_trns_LES%grad_fil_fld, trns_bwd)
      call add_double_SGS_term_4_sph_trns                               &
     &   (d_rj, ipol_LES%dble_SGS, iphys_LES%dble_SGS,                  &
     &    b_trns_LES%dble_SGS, trns_bwd)
      trns_bwd%num_vector = trns_bwd%nfield
      trns_bwd%num_scalar = trns_bwd%nfield - trns_bwd%num_vector
      trns_bwd%num_tensor = 0
!
      end subroutine bwd_trns_address_fld_dyn_ngrad
!
!-----------------------------------------------------------------------
!
      subroutine fwd_trns_address_fld_dyn_ngrad                         &
     &         (d_rj, ipol_LES, iphys_LES, f_trns_LES, trns_fwd)
!
      use add_diff_fil_vec_to_trans
      use add_SGS_term_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!
      type(SGS_model_addresses), intent(inout) :: f_trns_LES
      type(spherical_transform_data), intent(inout) :: trns_fwd
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      trns_fwd%num_vector = trns_fwd%nfield
      call add_diff_fil_vec_4_scalar_trns                               &
     &   (d_rj, ipol_LES%diff_fil_vect, iphys_LES%diff_fil_vect,        &
     &    f_trns_LES%diff_fil_vect, trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trns_address_fld_dyn_ngrad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine bwd_trans_address_sph_ngCsim                           &
     &         (d_rj, ipol_LES, iphys_LES, b_trns_LES, trns_bwd)
!
      use t_SGS_control_parameter
      use add_Csim_4_sph_trns
      use add_SGS_term_to_sph_trans
      use add_SGS_eflux_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!
      type(SGS_model_addresses), intent(inout) :: b_trns_LES
      type(spherical_transform_data), intent(inout) :: trns_bwd
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, grid data'
      end if
!
      trns_bwd%nfield = 0
      call alloc_sph_trns_field_name(trns_bwd)
!
      call add_wide_SGS_term_4_sph_trns                                 &
     &   (d_rj, ipol_LES%wide_SGS, iphys_LES%wide_SGS,                  &
     &    b_trns_LES%wide_SGS, trns_bwd)
      trns_bwd%num_vector = trns_bwd%nfield
      trns_bwd%num_scalar = trns_bwd%nfield - trns_bwd%num_vector
      trns_bwd%num_tensor = 0
!
      end subroutine bwd_trans_address_sph_ngCsim
!
!-----------------------------------------------------------------------
!
      subroutine fwd_trans_address_sph_ngCsim(SGS_param, d_rj,          &
     &          ipol_LES, iphys_LES, f_trns_LES, trns_fwd)
!
      use t_SGS_control_parameter
      use add_Csim_4_sph_trns
      use add_SGS_term_to_sph_trans
      use add_SGS_eflux_to_sph_trans
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_model_addresses), intent(in) :: ipol_LES, iphys_LES
!
      type(SGS_model_addresses), intent(inout) :: f_trns_LES
      type(spherical_transform_data), intent(inout) :: trns_fwd
!
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, grid data'
      end if
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      trns_fwd%num_vector = 0
      call add_Csim_4_sph_trns_by_pol                                   &
     &   (d_rj, ipol_LES%Csim, iphys_LES%Csim,                          &
     &    f_trns_LES%Csim, trns_fwd)
      if(SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
        call add_SGS_eflux_sph_trns_by_pol                              &
     &     (d_rj, ipol_LES%SGS_ene_flux, iphys_LES%SGS_ene_flux,        &
     &      f_trns_LES%SGS_ene_flux, trns_fwd)
      end if
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trans_address_sph_ngCsim
!
!-----------------------------------------------------------------------
!
      end module address_sph_trans_ngSGS
