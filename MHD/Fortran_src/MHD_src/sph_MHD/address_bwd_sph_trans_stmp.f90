!>@file   address_bwd_sph_trans_stmp.f90
!!@brief  module address_bwd_sph_trans_stmp
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine b_trans_address_vector_stmp(trns_tmp)
!!      subroutine b_trans_address_scalar_stmp(trns_tmp)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_tmp
!!
!!      subroutine set_b_trans_vector_field_stmp                        &
!!     &         (icou, ipol, itor, iphys, trns_tmp)
!!      subroutine set_b_trans_scalar_field_stmp                        &
!!     &         (icou, ipol, itor, iphys, trns_tmp)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_tmp
!!@endverbatim
!
      module address_bwd_sph_trans_stmp
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
      subroutine b_trans_address_vector_stmp(trns_tmp)
!
!      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_tmp
!
      trns_tmp%backward%num_vector = 0
!      call add_vec_trans_flag_snap(ipol%i_grad_vx, iphys%i_grad_vx,    &
!     &    trns_tmp%backward%num_vector, trns_tmp%b_trns%i_grad_vx)
!
      end subroutine b_trans_address_vector_stmp
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_stmp(trns_tmp)
!
!      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_tmp
!
!
      trns_tmp%backward%num_scalar = 0
!      call add_scl_trans_flag_snap(ipol%i_temp, iphys%i_temp,          &
!     &    trns_tmp%backward%num_vector, trns_tmp%backward%num_scalar,        &
!     &    trns_tmp%b_trns%i_temp)
!
      end subroutine b_trans_address_scalar_stmp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_vector_field_stmp                          &
     &         (icou, ipol, itor, iphys, trns_tmp)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_tmp
      integer(kind = kint), intent(inout) :: icou
!
      call set_field_name_4_bwd_trns(fhd_grad_v_1,                      &
     &    trns_tmp%b_trns%i_grad_vx, ipol%i_grad_vx, itor%i_grad_vx,    &
     &    iphys%i_grad_vx, icou, trns_tmp)
!
      end subroutine set_b_trans_vector_field_stmp
!
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_scalar_field_stmp                          &
     &         (icou, ipol, itor, iphys, trns_tmp)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_tmp
      integer(kind = kint), intent(inout) :: icou
!
!
      call set_field_name_4_bwd_trns(fhd_temp, trns_tmp%b_trns%i_temp,  &
     &    ipol%i_temp, itor%i_temp, iphys%i_temp, icou, trns_tmp)
!
      end subroutine set_b_trans_scalar_field_stmp
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_stmp
