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
!!      subroutine f_trans_address_scalar_stmp                          &
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_fwd_sph_trans_stmp
!
      use m_precision
!
      use m_phys_labels
      use m_phys_constants
      use t_phys_address
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
      subroutine f_trans_address_vector_stmp(trns_fwd)
!
!      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
!      type(phys_address), intent(inout) :: f_trns
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      trns_fwd%num_vector = trns_fwd%nfield
!
      end subroutine f_trans_address_vector_stmp
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_stmp                            &
     &         (ipol, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      call add_field_name_4_sph_trns_snap(grad_v_1%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_vx, iphys%diff_vector%i_grad_vx,      &
     &    f_trns%diff_vector%i_grad_vx, trns_fwd)
      call add_field_name_4_sph_trns_snap(grad_v_2%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_vy, iphys%diff_vector%i_grad_vy,      &
     &    f_trns%diff_vector%i_grad_vy, trns_fwd)
      call add_field_name_4_sph_trns_snap(grad_v_3%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_vz, iphys%diff_vector%i_grad_vz,      &
     &    f_trns%diff_vector%i_grad_vz, trns_fwd)
!
      call add_field_name_4_sph_trns_snap(grad_w_1%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_wx, iphys%diff_vector%i_grad_wx,      &
     &    f_trns%diff_vector%i_grad_wx, trns_fwd)
      call add_field_name_4_sph_trns_snap(grad_w_2%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_wy, iphys%diff_vector%i_grad_wy,      &
     &    f_trns%diff_vector%i_grad_wy, trns_fwd)
      call add_field_name_4_sph_trns_snap(grad_w_3%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_wz, iphys%diff_vector%i_grad_wz,      &
     &    f_trns%diff_vector%i_grad_wz, trns_fwd)
!
      call add_field_name_4_sph_trns_snap(grad_a_1%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_ax, iphys%diff_vector%i_grad_ax,      &
     &    f_trns%diff_vector%i_grad_ax, trns_fwd)
      call add_field_name_4_sph_trns_snap(grad_a_2%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_ay, iphys%diff_vector%i_grad_ay,      &
     &    f_trns%diff_vector%i_grad_ay, trns_fwd)
      call add_field_name_4_sph_trns_snap(grad_a_3%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_az, iphys%diff_vector%i_grad_az,      &
     &    f_trns%diff_vector%i_grad_az, trns_fwd)
!
      call add_field_name_4_sph_trns_snap(grad_b_1%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_bx, iphys%diff_vector%i_grad_bx,      &
     &    f_trns%diff_vector%i_grad_bx, trns_fwd)
      call add_field_name_4_sph_trns_snap(grad_b_2%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_by, iphys%diff_vector%i_grad_by,      &
     &    f_trns%diff_vector%i_grad_by, trns_fwd)
      call add_field_name_4_sph_trns_snap(grad_b_3%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_bz, iphys%diff_vector%i_grad_bz,      &
     &    f_trns%diff_vector%i_grad_bz, trns_fwd)
!
      call add_field_name_4_sph_trns_snap(grad_j_1%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_jx, iphys%diff_vector%i_grad_jx,      &
     &    f_trns%diff_vector%i_grad_jx, trns_fwd)
      call add_field_name_4_sph_trns_snap(grad_j_2%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_jy, iphys%diff_vector%i_grad_jy,      &
     &    f_trns%diff_vector%i_grad_jy, trns_fwd)
      call add_field_name_4_sph_trns_snap(grad_j_3%name, n_scalar,      &
     &    ipol%diff_vector%i_grad_jz, iphys%diff_vector%i_grad_jz,      &
     &    f_trns%diff_vector%i_grad_jz, trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
!
      end subroutine f_trans_address_scalar_stmp
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_stmp
