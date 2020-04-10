!>@file   address_bwd_sph_trans_ngSGS.f90
!!@brief  module address_bwd_sph_trans_ngSGS
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine b_trans_vector_gradients                             &
!!     &         (ipol, iphys, b_trns, trns_back)
!!      subroutine b_trans_filter_vector_grads                          &
!!     &         (ipol, iphys, b_trns, trns_back)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!@endverbatim
!
      module address_bwd_sph_trans_ngSGS
!
      use m_precision
!
      use m_phys_labels
      use m_phys_constants
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
      subroutine b_trans_vector_gradients                               &
     &         (ipol, iphys, b_trns, trns_back)
!
      use add_diff_vect_to_sph_trans
      use add_field_to_sph_trans_list
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   Gradient of vector field
      call add_diff_vect_sph_trns_by_pol                                &
     &   (ipol%diff_vector, iphys%diff_vector, b_trns%diff_vector,      &
     &    trns_back)
!
!   Gradient of scalar
      call add_grad_4_sph_trns_by_pol                                   &
     &   (ipol%grad_fld, iphys%grad_fld, b_trns%grad_fld, trns_back)
!
      end subroutine b_trans_vector_gradients
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_filter_vector_grads                            &
     &         (ipol, iphys, b_trns, trns_back)
!
      use m_grad_filter_field_labels
      use m_diff_filter_vect_labels
      use add_field_to_sph_trans_list
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   Gradient of Radial velocity
      call add_field_4_sph_trns_by_pol(grad_filtered_v_1,               &
     &    ipol%diff_fil_vect%i_grad_vx, iphys%diff_fil_vect%i_grad_vx,  &
     &    b_trns%diff_fil_vect%i_grad_vx, trns_back)
!   Gradient of meridional velocity
      call add_field_4_sph_trns_by_pol(grad_filtered_v_2,               &
     &    ipol%diff_fil_vect%i_grad_vy, iphys%diff_fil_vect%i_grad_vy,  &
     &    b_trns%diff_fil_vect%i_grad_vy, trns_back)
!   Gradient of zonal velocity
      call add_field_4_sph_trns_by_pol(grad_filtered_v_3,               &
     &    ipol%diff_fil_vect%i_grad_vz, iphys%diff_fil_vect%i_grad_vz,  &
     &    b_trns%diff_fil_vect%i_grad_vz, trns_back)
!
!   Gradient of Radial vorticity
      call add_field_4_sph_trns_by_pol(grad_filtered_w_1,               &
     &    ipol%diff_fil_vect%i_grad_wx, iphys%diff_fil_vect%i_grad_wx,  &
     &    b_trns%diff_fil_vect%i_grad_wx, trns_back)
!   Gradient of meridional vorticity
      call add_field_4_sph_trns_by_pol(grad_filtered_w_2,               &
     &    ipol%diff_fil_vect%i_grad_wy, iphys%diff_fil_vect%i_grad_wy,  &
     &    b_trns%diff_fil_vect%i_grad_wy, trns_back)
!   Gradient of zonal vorticity
      call add_field_4_sph_trns_by_pol(grad_filtered_w_3,               &
     &    ipol%diff_fil_vect%i_grad_wz, iphys%diff_fil_vect%i_grad_wz,  &
     &    b_trns%diff_fil_vect%i_grad_wz, trns_back)
!
!   Gradient of Radial magnetic field
      call add_field_4_sph_trns_by_pol(grad_filtered_b_1,               &
     &    ipol%diff_fil_vect%i_grad_bx, iphys%diff_fil_vect%i_grad_bx,  &
     &    b_trns%diff_fil_vect%i_grad_bx, trns_back)
!   Gradient of meridional magnetic field
      call add_field_4_sph_trns_by_pol(grad_filtered_b_2,               &
     &    ipol%diff_fil_vect%i_grad_by, iphys%diff_fil_vect%i_grad_by,  &
     &    b_trns%diff_fil_vect%i_grad_by, trns_back)
!   Gradient of zonal magnetic field
      call add_field_4_sph_trns_by_pol(grad_filtered_b_3,               &
     &    ipol%diff_fil_vect%i_grad_bz, iphys%diff_fil_vect%i_grad_bz,  &
     &    b_trns%diff_fil_vect%i_grad_bz, trns_back)
!
!   Gradient of Radial current density
      call add_field_4_sph_trns_by_pol(grad_filtered_j_1,               &
     &    ipol%diff_fil_vect%i_grad_jx, iphys%diff_fil_vect%i_grad_jx,  &
     &    b_trns%diff_fil_vect%i_grad_jx, trns_back)
!   Gradient of meridional current density
      call add_field_4_sph_trns_by_pol(grad_filtered_j_2,               &
     &    ipol%diff_fil_vect%i_grad_jy, iphys%diff_fil_vect%i_grad_jy,  &
     &    b_trns%diff_fil_vect%i_grad_jy, trns_back)
!   Gradient of zonal current density
      call add_field_4_sph_trns_by_pol(grad_filtered_j_3,               &
     &    ipol%diff_fil_vect%i_grad_jz, iphys%diff_fil_vect%i_grad_jz,  &
     &    b_trns%diff_fil_vect%i_grad_jz, trns_back)
!
!   Gradient of temperature
      call add_field_4_sph_trns_by_pol(grad_filtered_temp,              &
     &    ipol%grad_fil_fld%i_grad_temp,                                &
     &    iphys%grad_fil_fld%i_grad_temp,                               &
     &    b_trns%grad_fil_fld%i_grad_temp, trns_back)
!
!   Gradient of composition
      call add_field_4_sph_trns_by_pol(grad_filtered_comp,              &
     &    ipol%grad_fil_fld%i_grad_composit,                            &
     &    iphys%grad_fil_fld%i_grad_composit,                           &
     &    b_trns%grad_fil_fld%i_grad_composit, trns_back)
!
      end subroutine b_trans_filter_vector_grads
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_ngSGS
