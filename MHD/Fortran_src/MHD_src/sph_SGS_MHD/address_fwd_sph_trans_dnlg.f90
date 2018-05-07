!>@file   address_fwd_sph_trans_dnlg.f90
!!@brief  module address_fwd_sph_trans_dnlg
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine f_trans_address_vector_dnlg(trns_fwd)
!!      subroutine f_trans_address_scalar_dnlg                         &
!!     &         (ipol, itor, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_fwd_sph_trans_dnlg
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
      subroutine f_trans_address_vector_dnlg(trns_fwd)
!
!      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
!      type(phys_address), intent(inout) :: f_trns
!
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      trns_fwd%num_vector = trns_fwd%nfield
!
      end subroutine f_trans_address_vector_dnlg
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_dnlg                            &
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
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
!
      end subroutine f_trans_address_scalar_dnlg
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_dnlg
