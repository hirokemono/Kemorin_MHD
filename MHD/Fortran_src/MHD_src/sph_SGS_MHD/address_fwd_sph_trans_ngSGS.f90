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
!!     &         (ipol, itor, iphys, f_trns, trns_fwd)
!!      subroutine f_trans_scalar_filter_vec_grads                      &
!!     &         (ipol, itor, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
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
     &         (ipol, itor, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   Radial velocity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_vx,        &
     &    grad_v_1%name, n_scalar, ipol%diff_vector%i_grad_vx,          &
     &    itor%diff_vector%i_grad_vx, iphys%diff_vector%i_grad_vx,      &
     &    f_trns%diff_vector%i_grad_vx, trns_fwd)
!   meridional velocity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_vy,        &
     &    grad_v_2%name, n_scalar, ipol%diff_vector%i_grad_vy,          &
     &    itor%diff_vector%i_grad_vy, iphys%diff_vector%i_grad_vy,      &
     &    f_trns%diff_vector%i_grad_vy, trns_fwd)
!   zonal velocity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_vz,        &
     &    grad_v_3%name, n_scalar, ipol%diff_vector%i_grad_vz,          &
     &    itor%diff_vector%i_grad_vz, iphys%diff_vector%i_grad_vz,      &
     &    f_trns%diff_vector%i_grad_vz, trns_fwd)
!
!   Radial vorticity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_wx,        &
     &    grad_w_1%name, n_scalar, ipol%diff_vector%i_grad_wx,          &
     &    itor%diff_vector%i_grad_wx, iphys%diff_vector%i_grad_wx,      &
     &    f_trns%diff_vector%i_grad_wx, trns_fwd)
!   meridional vorticity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_wy,        &
     &    grad_w_2%name, n_scalar, ipol%diff_vector%i_grad_wy,          &
     &    itor%diff_vector%i_grad_wy, iphys%diff_vector%i_grad_wy,      &
     &    f_trns%diff_vector%i_grad_wy, trns_fwd)
!   zonal vorticity
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_wz,        &
     &    grad_w_3%name, n_scalar, ipol%diff_vector%i_grad_wz,          &
     &    itor%diff_vector%i_grad_wz, iphys%diff_vector%i_grad_wz,      &
     &    f_trns%diff_vector%i_grad_wz, trns_fwd)
!
!   Radial magnetic field
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_bx,        &
     &    grad_b_1%name, n_scalar, ipol%diff_vector%i_grad_bx,          &
     &    itor%diff_vector%i_grad_bx, iphys%diff_vector%i_grad_bx,      &
     &    f_trns%diff_vector%i_grad_bx, trns_fwd)
!   meridional magnetic field
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_by,        &
     &    grad_b_2%name, n_scalar, ipol%diff_vector%i_grad_by,          &
     &    itor%diff_vector%i_grad_by, iphys%diff_vector%i_grad_by,      &
     &    f_trns%diff_vector%i_grad_by, trns_fwd)
!   zonal magnetic field
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_bz,        &
     &    grad_b_3%name, n_scalar, ipol%diff_vector%i_grad_bz,          &
     &    itor%diff_vector%i_grad_bz, iphys%diff_vector%i_grad_bz,      &
     &   f_trns%diff_vector%i_grad_bz, trns_fwd)
!
!   Radial current density
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_jx,        &
     &    grad_j_1%name, n_scalar, ipol%diff_vector%i_grad_jx,          &
     &    itor%diff_vector%i_grad_jx, iphys%diff_vector%i_grad_jx,      &
     &    f_trns%diff_vector%i_grad_jx, trns_fwd)
!   meridional current density
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_jy,        &
     &    grad_j_2%name, n_scalar, ipol%diff_vector%i_grad_jy,          &
     &    itor%diff_vector%i_grad_jy, iphys%diff_vector%i_grad_jy,      &
     &    f_trns%diff_vector%i_grad_jy, trns_fwd)
!   zonal current density
      call add_field_name_4_sph_trns(ipol%diff_vector%i_grad_jz,        &
     &    grad_j_3%name, n_scalar, ipol%diff_vector%i_grad_jz,          &
     &    itor%diff_vector%i_grad_jz, iphys%diff_vector%i_grad_jz,      &
     &    f_trns%diff_vector%i_grad_jz, trns_fwd)
!
      end subroutine f_trans_scalar_vector_grads
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_scalar_filter_vec_grads                        &
     &         (ipol, itor, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   Radial velocity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_vx, fhd_grad_filter_v_1, n_scalar,         &
     &    ipol%i_grad_filter_vx, itor%i_grad_filter_vx,                 &
     &    iphys%i_grad_filter_vx, f_trns%i_grad_filter_vx, trns_fwd)
!   meridional velocity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_vy, fhd_grad_filter_v_2, n_scalar,         &
     &    ipol%i_grad_filter_vy, itor%i_grad_filter_vy,                 &
     &    iphys%i_grad_filter_vy, f_trns%i_grad_filter_vy, trns_fwd)
!   zonal velocity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_vz, fhd_grad_filter_v_3, n_scalar,         &
     &    ipol%i_grad_filter_vz, itor%i_grad_filter_vz,                 &
     &    iphys%i_grad_filter_vz, f_trns%i_grad_filter_vz, trns_fwd)
!
!   Radial vorticity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_wx, fhd_grad_filter_w_1, n_scalar,         &
     &    ipol%i_grad_filter_wx, itor%i_grad_filter_wx,                 &
     &    iphys%i_grad_filter_wx, f_trns%i_grad_filter_wx, trns_fwd)
!   meridional vorticity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_wy, fhd_grad_filter_w_2, n_scalar,         &
     &    ipol%i_grad_filter_wy, itor%i_grad_filter_wy,                 &
     &    iphys%i_grad_filter_wy, f_trns%i_grad_filter_wy, trns_fwd)
!   zonal vorticity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_wz, fhd_grad_filter_w_3, n_scalar,         &
     &    ipol%i_grad_filter_wz, itor%i_grad_filter_wz,                 &
     &    iphys%i_grad_filter_wz, f_trns%i_grad_filter_wz, trns_fwd)
!
!   Radial magnetic field
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_bx, fhd_grad_filter_b_1, n_scalar,         &
     &    ipol%i_grad_filter_bx, itor%i_grad_filter_bx,                 &
     &    iphys%i_grad_filter_bx, f_trns%i_grad_filter_bx, trns_fwd)
!   meridional magnetic field
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_by, fhd_grad_filter_b_2, n_scalar,         &
     &    ipol%i_grad_filter_by, itor%i_grad_filter_by,                 &
     &    iphys%i_grad_filter_by, f_trns%i_grad_filter_by, trns_fwd)
!   zonal magnetic field
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_bz, fhd_grad_filter_b_3, n_scalar,         &
     &    ipol%i_grad_filter_bz, itor%i_grad_filter_bz,                 &
     &    iphys%i_grad_filter_bz, f_trns%i_grad_filter_bz, trns_fwd)
!
!   Radial current density
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_jx, fhd_grad_filter_j_1, n_scalar,         &
     &    ipol%i_grad_filter_jx, itor%i_grad_filter_jx,                 &
     &    iphys%i_grad_filter_jx, f_trns%i_grad_filter_jx, trns_fwd)
!   meridional current density
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_jy, fhd_grad_filter_j_2, n_scalar,         &
     &    ipol%i_grad_filter_jy, itor%i_grad_filter_jy,                 &
     &    iphys%i_grad_filter_jy, f_trns%i_grad_filter_jy, trns_fwd)
!   zonal current density
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_grad_filter_jz, fhd_grad_filter_j_3, n_scalar,         &
     &    ipol%i_grad_filter_jz, itor%i_grad_filter_jz,                 &
     &    iphys%i_grad_filter_jz, f_trns%i_grad_filter_jz, trns_fwd)
!
      end subroutine f_trans_scalar_filter_vec_grads
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_ngSGS
