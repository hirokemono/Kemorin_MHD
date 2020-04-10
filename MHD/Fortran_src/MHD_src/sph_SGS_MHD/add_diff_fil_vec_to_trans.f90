!>@file   add_diff_fil_vec_to_trans.f90
!!@brief  module add_diff_fil_vec_to_trans
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Add field addresses of difference of filtered fields 
!!       for spherical harmonics transform
!!
!!@verbatim
!!      subroutine add_grad_filter_fld_4_sph_trns                       &
!!     &         (ipol_gfl, iphys_gfl, b_trns_gfl, trns)
!!        type(gradient_field_address), intent(in) :: ipol_gfl
!!        type(gradient_field_address), intent(in) :: iphys_gfl
!!        type(gradient_field_address), intent(inout) :: b_trns_gfl
!!        type(address_each_sph_trans), intent(inout) :: trns
!!      subroutine add_diff_fil_vec_sph_trns_pol                        &
!!     &         (ipol_dfv, iphys_dfv, b_trns_dfv, trns)
!!      subroutine add_diff_fil_vec_4_scalar_trns                       &
!!     &         (ipol_dfv, iphys_dfv, f_trns_dfv, trns)
!!        type(diff_vector_address), intent(in) :: ipol_dfv, iphys_dfv
!!        type(diff_vector_address), intent(inout) :: f_trns_dfv
!!        type(address_each_sph_trans), intent(inout) :: trns
!!@endverbatim
!
      module add_diff_fil_vec_to_trans
!
      use m_precision
!
      use t_diff_vector_labels
      use t_grad_field_labels
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
      subroutine add_grad_filter_fld_4_sph_trns                         &
     &         (ipol_gfl, iphys_gfl, b_trns_gfl, trns)
!
      use m_grad_filter_field_labels
      use m_diff_filter_vect_labels
      use add_field_to_sph_trans_list
!
      type(gradient_field_address), intent(in) :: ipol_gfl
      type(gradient_field_address), intent(in) :: iphys_gfl
      type(gradient_field_address), intent(inout) :: b_trns_gfl
      type(address_each_sph_trans), intent(inout) :: trns
!
!
!   Gradient of temperature
      call add_field_4_sph_trns_by_pol(grad_filtered_temp,              &
     &    ipol_gfl%i_grad_temp, iphys_gfl%i_grad_temp,                  &
     &    b_trns_gfl%i_grad_temp, trns)
!
!   Gradient of composition
      call add_field_4_sph_trns_by_pol(grad_filtered_comp,              &
     &    ipol_gfl%i_grad_composit, iphys_gfl%i_grad_composit,          &
     &    b_trns_gfl%i_grad_composit, trns)
!
      end subroutine add_grad_filter_fld_4_sph_trns
!
!-----------------------------------------------------------------------
!
      subroutine add_diff_fil_vec_sph_trns_pol                          &
     &         (ipol_dfv, iphys_dfv, b_trns_dfv, trns)
!
      use m_grad_filter_field_labels
      use m_diff_filter_vect_labels
      use add_field_to_sph_trans_list
!
      type(diff_vector_address), intent(in) :: ipol_dfv, iphys_dfv
      type(diff_vector_address), intent(inout) :: b_trns_dfv
      type(address_each_sph_trans), intent(inout) :: trns
!
!
!   Gradient of Radial velocity
      call add_field_4_sph_trns_by_pol(grad_filtered_v_1,               &
     &    ipol_dfv%i_grad_vx, iphys_dfv%i_grad_vx,                      &
     &    b_trns_dfv%i_grad_vx, trns)
!   Gradient of meridional velocity
      call add_field_4_sph_trns_by_pol(grad_filtered_v_2,               &
     &    ipol_dfv%i_grad_vy, iphys_dfv%i_grad_vy,                      &
     &    b_trns_dfv%i_grad_vy, trns)
!   Gradient of zonal velocity
      call add_field_4_sph_trns_by_pol(grad_filtered_v_3,               &
     &    ipol_dfv%i_grad_vz, iphys_dfv%i_grad_vz,                      &
     &    b_trns_dfv%i_grad_vz, trns)
!
!   Gradient of Radial vorticity
      call add_field_4_sph_trns_by_pol(grad_filtered_w_1,               &
     &    ipol_dfv%i_grad_wx, iphys_dfv%i_grad_wx,                      &
     &    b_trns_dfv%i_grad_wx, trns)
!   Gradient of meridional vorticity
      call add_field_4_sph_trns_by_pol(grad_filtered_w_2,               &
     &    ipol_dfv%i_grad_wy, iphys_dfv%i_grad_wy,                      &
     &    b_trns_dfv%i_grad_wy, trns)
!   Gradient of zonal vorticity
      call add_field_4_sph_trns_by_pol(grad_filtered_w_3,               &
     &    ipol_dfv%i_grad_wz, iphys_dfv%i_grad_wz,                      &
     &    b_trns_dfv%i_grad_wz, trns)
!
!   Gradient of Radial magnetic field
      call add_field_4_sph_trns_by_pol(grad_filtered_b_1,               &
     &    ipol_dfv%i_grad_bx, iphys_dfv%i_grad_bx,                      &
     &    b_trns_dfv%i_grad_bx, trns)
!   Gradient of meridional magnetic field
      call add_field_4_sph_trns_by_pol(grad_filtered_b_2,               &
     &    ipol_dfv%i_grad_by, iphys_dfv%i_grad_by,                      &
     &    b_trns_dfv%i_grad_by, trns)
!   Gradient of zonal magnetic field
      call add_field_4_sph_trns_by_pol(grad_filtered_b_3,               &
     &    ipol_dfv%i_grad_bz, iphys_dfv%i_grad_bz,                      &
     &    b_trns_dfv%i_grad_bz, trns)
!
!   Gradient of Radial current density
      call add_field_4_sph_trns_by_pol(grad_filtered_j_1,               &
     &    ipol_dfv%i_grad_jx, iphys_dfv%i_grad_jx,                      &
     &    b_trns_dfv%i_grad_jx, trns)
!   Gradient of meridional current density
      call add_field_4_sph_trns_by_pol(grad_filtered_j_2,               &
     &    ipol_dfv%i_grad_jy, iphys_dfv%i_grad_jy,                      &
     &    b_trns_dfv%i_grad_jy, trns)
!   Gradient of zonal current density
      call add_field_4_sph_trns_by_pol(grad_filtered_j_3,               &
     &    ipol_dfv%i_grad_jz, iphys_dfv%i_grad_jz,                      &
     &    b_trns_dfv%i_grad_jz, trns)
!
      end subroutine add_diff_fil_vec_sph_trns_pol
!
!-----------------------------------------------------------------------
!
      subroutine add_diff_fil_vec_4_scalar_trns                         &
     &         (ipol_dfv, iphys_dfv, f_trns_dfv, trns)
!
      use m_diff_filter_vect_labels
      use add_field_to_sph_trans_list
!
      type(diff_vector_address), intent(in) :: ipol_dfv, iphys_dfv
      type(diff_vector_address), intent(inout) :: f_trns_dfv
      type(address_each_sph_trans), intent(inout) :: trns
!
!
!   Radial velocity
      call add_scalar_4_sph_trns_by_pol(grad_filtered_v_1,              &
     &    ipol_dfv%i_grad_vx, iphys_dfv%i_grad_vx,                      &
     &    f_trns_dfv%i_grad_vx, trns)
!   meridional velocity
      call add_scalar_4_sph_trns_by_pol(grad_filtered_v_2,              &
     &    ipol_dfv%i_grad_vy, iphys_dfv%i_grad_vy,                      &
     &    f_trns_dfv%i_grad_vy, trns)
!   zonal velocity
      call add_scalar_4_sph_trns_by_pol(grad_filtered_v_3,              &
     &    ipol_dfv%i_grad_vz, iphys_dfv%i_grad_vz,                      &
     &    f_trns_dfv%i_grad_vz, trns)
!
!   Radial vorticity
      call add_scalar_4_sph_trns_by_pol(grad_filtered_w_1,              &
     &    ipol_dfv%i_grad_wx, iphys_dfv%i_grad_wx,                      &
     &    f_trns_dfv%i_grad_wx, trns)
!   meridional vorticity
      call add_scalar_4_sph_trns_by_pol(grad_filtered_w_2,              &
     &    ipol_dfv%i_grad_wy, iphys_dfv%i_grad_wy,                      &
     &    f_trns_dfv%i_grad_wy, trns)
!   zonal vorticity
      call add_scalar_4_sph_trns_by_pol(grad_filtered_w_3,              &
     &    ipol_dfv%i_grad_wz, iphys_dfv%i_grad_wz,                      &
     &    f_trns_dfv%i_grad_wz, trns)
!
!   Radial magnetic field
      call add_scalar_4_sph_trns_by_pol(grad_filtered_b_1,              &
     &    ipol_dfv%i_grad_bx, iphys_dfv%i_grad_bx,                      &
     &    f_trns_dfv%i_grad_bx, trns)
!   meridional magnetic field
      call add_scalar_4_sph_trns_by_pol(grad_filtered_b_2,              &
     &    ipol_dfv%i_grad_by, iphys_dfv%i_grad_by,                      &
     &    f_trns_dfv%i_grad_by, trns)
!   zonal magnetic field
      call add_scalar_4_sph_trns_by_pol(grad_filtered_b_3,              &
     &    ipol_dfv%i_grad_bz, iphys_dfv%i_grad_bz,                      &
     &    f_trns_dfv%i_grad_bz, trns)
!
!   Radial current density
      call add_scalar_4_sph_trns_by_pol(grad_filtered_j_1,              &
     &    ipol_dfv%i_grad_jx, iphys_dfv%i_grad_jx,                      &
     &    f_trns_dfv%i_grad_jx, trns)
!   meridional current density
      call add_scalar_4_sph_trns_by_pol(grad_filtered_j_2,              &
     &    ipol_dfv%i_grad_jy, iphys_dfv%i_grad_jy,                      &
     &    f_trns_dfv%i_grad_jy, trns)
!   zonal current density
      call add_scalar_4_sph_trns_by_pol(grad_filtered_j_3,              &
     &    ipol_dfv%i_grad_jz, iphys_dfv%i_grad_jz,                      &
     &    f_trns_dfv%i_grad_jz, trns)
!
      end subroutine add_diff_fil_vec_4_scalar_trns
!
!-----------------------------------------------------------------------
!
      end module add_diff_fil_vec_to_trans
