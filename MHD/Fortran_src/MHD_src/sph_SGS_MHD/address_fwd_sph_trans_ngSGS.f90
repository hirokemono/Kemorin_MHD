!>@file   address_fwd_sph_trans_ngSGS.f90
!!@brief  module address_fwd_sph_trans_ngSGS
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine f_trans_scalar_vector_grads                          &
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!      subroutine f_trans_scalar_filter_vec_grads                      &
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_fwd_sph_trans_ngSGS
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
      subroutine f_trans_scalar_vector_grads                            &
     &         (ipol, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   Radial velocity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_vx,        &
     &    grad_v_1%name, n_scalar, ipol%diff_vector%i_grad_vx,          &
     &    iphys%diff_vector%i_grad_vx, f_trns%diff_vector%i_grad_vx,    &
     &    trns_fwd)
!   meridional velocity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_vy,        &
     &    grad_v_2%name, n_scalar, ipol%diff_vector%i_grad_vy,          &
     &    iphys%diff_vector%i_grad_vy, f_trns%diff_vector%i_grad_vy,    &
     &    trns_fwd)
!   zonal velocity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_vz,        &
     &    grad_v_3%name, n_scalar, ipol%diff_vector%i_grad_vz,          &
     &    iphys%diff_vector%i_grad_vz, f_trns%diff_vector%i_grad_vz,    &
     &    trns_fwd)
!
!   Radial vorticity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_wx,        &
     &    grad_w_1%name, n_scalar, ipol%diff_vector%i_grad_wx,          &
     &    iphys%diff_vector%i_grad_wx, f_trns%diff_vector%i_grad_wx,    &
     &    trns_fwd)
!   meridional vorticity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_wy,        &
     &    grad_w_2%name, n_scalar, ipol%diff_vector%i_grad_wy,          &
     &    iphys%diff_vector%i_grad_wy, f_trns%diff_vector%i_grad_wy,    &
     &    trns_fwd)
!   zonal vorticity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_wz,        &
     &    grad_w_3%name, n_scalar, ipol%diff_vector%i_grad_wz,          &
     &    iphys%diff_vector%i_grad_wz, f_trns%diff_vector%i_grad_wz,    &
     &    trns_fwd)
!
!   Radial magnetic field
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_bx,        &
     &    grad_b_1%name, n_scalar, ipol%diff_vector%i_grad_bx,          &
     &    iphys%diff_vector%i_grad_bx, f_trns%diff_vector%i_grad_bx,    &
     &    trns_fwd)
!   meridional magnetic field
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_by,        &
     &    grad_b_2%name, n_scalar, ipol%diff_vector%i_grad_by,          &
     &    iphys%diff_vector%i_grad_by, f_trns%diff_vector%i_grad_by,    &
     &    trns_fwd)
!   zonal magnetic field
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_bz,        &
     &    grad_b_3%name, n_scalar, ipol%diff_vector%i_grad_bz,          &
     &    iphys%diff_vector%i_grad_bz, f_trns%diff_vector%i_grad_bz,    &
     &    trns_fwd)
!
!   Radial current density
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_jx,        &
     &    grad_j_1%name, n_scalar, ipol%diff_vector%i_grad_jx,          &
     &    iphys%diff_vector%i_grad_jx, f_trns%diff_vector%i_grad_jx,    &
     &    trns_fwd)
!   meridional current density
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_jy,        &
     &    grad_j_2%name, n_scalar, ipol%diff_vector%i_grad_jy,          &
     &    iphys%diff_vector%i_grad_jy, f_trns%diff_vector%i_grad_jy,    &
     &    trns_fwd)
!   zonal current density
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_jz,        &
     &    grad_j_3%name, n_scalar, ipol%diff_vector%i_grad_jz,          &
     &    iphys%diff_vector%i_grad_jz, f_trns%diff_vector%i_grad_jz,    &
     &    trns_fwd)
!
      end subroutine f_trans_scalar_vector_grads
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_scalar_filter_vec_grads                        &
     &         (ipol, iphys, f_trns, trns_fwd)
!
      use m_diff_filter_vect_labels
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   Radial velocity
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_vx,      &
     &    grad_filtered_v_1%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_vx, iphys%diff_fil_vect%i_grad_vx,  &
     &    f_trns%diff_fil_vect%i_grad_vx, trns_fwd)
!   meridional velocity
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_vy,      &
     &    grad_filtered_v_2%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_vy, iphys%diff_fil_vect%i_grad_vy,  &
     &    f_trns%diff_fil_vect%i_grad_vy, trns_fwd)
!   zonal velocity
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_vz,      &
     &    grad_filtered_v_3%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_vz, iphys%diff_fil_vect%i_grad_vz,  &
     &    f_trns%diff_fil_vect%i_grad_vz, trns_fwd)
!
!   Radial vorticity
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_wx,      &
     &    grad_filtered_w_1%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_wx, iphys%diff_fil_vect%i_grad_wx,  &
     &    f_trns%diff_fil_vect%i_grad_wx, trns_fwd)
!   meridional vorticity
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_wy,      &
     &    grad_filtered_w_2%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_wy, iphys%diff_fil_vect%i_grad_wy,  &
     &    f_trns%diff_fil_vect%i_grad_wy, trns_fwd)
!   zonal vorticity
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_wz,      &
     &    grad_filtered_w_3%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_wz, iphys%diff_fil_vect%i_grad_wz,  &
     &    f_trns%diff_fil_vect%i_grad_wz, trns_fwd)
!
!   Radial magnetic field
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_bx,      &
     &    grad_filtered_b_1%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_bx, iphys%diff_fil_vect%i_grad_bx,  &
     &    f_trns%diff_fil_vect%i_grad_bx, trns_fwd)
!   meridional magnetic field
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_by,      &
     &    grad_filtered_b_2%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_by, iphys%diff_fil_vect%i_grad_by,  &
     &    f_trns%diff_fil_vect%i_grad_by, trns_fwd)
!   zonal magnetic field
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_bz,      &
     &    grad_filtered_b_3%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_bz, iphys%diff_fil_vect%i_grad_bz,  &
     &    f_trns%diff_fil_vect%i_grad_bz, trns_fwd)
!
!   Radial current density
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_jx,      &
     &    grad_filtered_j_1%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_jx, iphys%diff_fil_vect%i_grad_jx,  &
     &    f_trns%diff_fil_vect%i_grad_jx, trns_fwd)
!   meridional current density
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_jy,      &
     &    grad_filtered_j_2%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_jy, iphys%diff_fil_vect%i_grad_jy,  &
     &    f_trns%diff_fil_vect%i_grad_jy, trns_fwd)
!   zonal current density
      call add_field_name_4_sph_trns(ipol%diff_fil_vect%i_grad_jz,      &
     &    grad_filtered_j_3%name, n_scalar,                             &
     &    ipol%diff_fil_vect%i_grad_jz, iphys%diff_fil_vect%i_grad_jz,  &
     &    f_trns%diff_fil_vect%i_grad_jz, trns_fwd)
!
      end subroutine f_trans_scalar_filter_vec_grads
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_ngSGS
