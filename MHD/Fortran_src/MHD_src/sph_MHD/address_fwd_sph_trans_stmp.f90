!>@file   address_fwd_sph_trans_stmp.f90
!!@brief  module address_fwd_sph_trans_stmp
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine f_trans_address_vector_stmp(trns_snap)
!!      subroutine f_trans_address_scalar_stmp(fl_prop, trns_snap)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(inout) :: b_trns
!!
!!      subroutine set_f_trans_vector_field_stmp                        &
!!     &         (icou, ipol, itor, iphys, trns_snap)
!!      subroutine set_f_trans_scalar_field_stmp                        &
!!     &         (icou, ipol, itor, iphys, trns_snap)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!@endverbatim
!
      module address_fwd_sph_trans_stmp
!
      use m_precision
!
      use m_phys_labels
      use t_phys_address
      use t_addresses_sph_transform
      use t_control_parameter
      use t_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_vector_stmp(trns_tmp)
!
!      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_tmp
!
!
      trns_tmp%nvector_rtp_2_rj = 0
!      call add_vec_trans_flag_snap(ipol%i_coriolis, iphys%i_coriolis,  &
!     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%f_trns%i_coriolis)
!
      end subroutine f_trans_address_vector_stmp
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_stmp(ipol, iphys, trns_tmp)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_tmp
!
!
      trns_tmp%nscalar_rtp_2_rj = 0
      call add_scl_trans_flag_snap(ipol%i_grad_vx, iphys%i_grad_vx,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_vx)
      call add_scl_trans_flag_snap(ipol%i_grad_vy, iphys%i_grad_vy,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_vy)
      call add_scl_trans_flag_snap(ipol%i_grad_vz, iphys%i_grad_vz,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_vz)
!
      end subroutine f_trans_address_scalar_stmp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_f_trans_vector_field_stmp                          &
     &         (icou, ipol, itor, iphys, trns_tmp)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_tmp
      integer(kind = kint), intent(inout) :: icou
!
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Coriolis, trns_tmp%f_trns%i_coriolis,                     &
     &    ipol%i_coriolis, itor%i_coriolis, iphys%i_coriolis,           &
     &    icou, trns_tmp)
!
      end subroutine set_f_trans_vector_field_stmp
!
!-----------------------------------------------------------------------
!
      subroutine set_f_trans_scalar_field_stmp                          &
     &         (icou, ipol, itor, iphys, trns_tmp)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_tmp
      integer(kind = kint), intent(inout) :: icou
!
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_v_1, trns_tmp%f_trns%i_grad_vx,                      &
     &    ipol%i_grad_vx, itor%i_grad_vx, iphys%i_grad_vx,              &
     &    icou, trns_tmp)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_v_2, trns_tmp%f_trns%i_grad_vy,                      &
     &    ipol%i_grad_vy, itor%i_grad_vy, iphys%i_grad_vy,              &
     &    icou, trns_tmp)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_v_3, trns_tmp%f_trns%i_grad_vz,                      &
     &    ipol%i_grad_vz, itor%i_grad_vz, iphys%i_grad_vz,              &
     &    icou, trns_tmp)
!
      end subroutine set_f_trans_scalar_field_stmp
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_stmp
