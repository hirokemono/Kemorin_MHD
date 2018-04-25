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
      call add_scl_trans_flag_snap(ipol%i_grad_wx, iphys%i_grad_wx,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_wx)
      call add_scl_trans_flag_snap(ipol%i_grad_wy, iphys%i_grad_wy,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_wy)
      call add_scl_trans_flag_snap(ipol%i_grad_wz, iphys%i_grad_wz,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_wz)
!
      call add_scl_trans_flag_snap(ipol%i_grad_ax, iphys%i_grad_ax,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_ax)
      call add_scl_trans_flag_snap(ipol%i_grad_ay, iphys%i_grad_ay,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_ay)
      call add_scl_trans_flag_snap(ipol%i_grad_az, iphys%i_grad_az,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_az)
!
      call add_scl_trans_flag_snap(ipol%i_grad_bx, iphys%i_grad_bx,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_bx)
      call add_scl_trans_flag_snap(ipol%i_grad_by, iphys%i_grad_by,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_by)
      call add_scl_trans_flag_snap(ipol%i_grad_bz, iphys%i_grad_bz,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_bz)
!
      call add_scl_trans_flag_snap(ipol%i_grad_jx, iphys%i_grad_jx,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_jx)
      call add_scl_trans_flag_snap(ipol%i_grad_jy, iphys%i_grad_jy,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_jy)
      call add_scl_trans_flag_snap(ipol%i_grad_jz, iphys%i_grad_jz,     &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj,         &
     &    trns_tmp%f_trns%i_grad_jz)
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
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_w_1, trns_tmp%f_trns%i_grad_wx,                      &
     &    ipol%i_grad_wx, itor%i_grad_wx, iphys%i_grad_wx,              &
     &    icou, trns_tmp)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_w_2, trns_tmp%f_trns%i_grad_wy,                      &
     &    ipol%i_grad_wy, itor%i_grad_wy, iphys%i_grad_wy,              &
     &    icou, trns_tmp)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_w_3, trns_tmp%f_trns%i_grad_wz,                      &
     &    ipol%i_grad_wz, itor%i_grad_wz, iphys%i_grad_wz,              &
     &    icou, trns_tmp)
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_a_1, trns_tmp%f_trns%i_grad_ax,                      &
     &    ipol%i_grad_ax, itor%i_grad_ax, iphys%i_grad_ax,              &
     &    icou, trns_tmp)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_a_2, trns_tmp%f_trns%i_grad_ay,                      &
     &    ipol%i_grad_ay, itor%i_grad_ay, iphys%i_grad_ay,              &
     &    icou, trns_tmp)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_a_3, trns_tmp%f_trns%i_grad_az,                      &
     &    ipol%i_grad_az, itor%i_grad_az, iphys%i_grad_az,              &
     &    icou, trns_tmp)
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_b_1, trns_tmp%f_trns%i_grad_bx,                      &
     &    ipol%i_grad_bx, itor%i_grad_bx, iphys%i_grad_bx,              &
     &    icou, trns_tmp)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_b_2, trns_tmp%f_trns%i_grad_by,                      &
     &    ipol%i_grad_by, itor%i_grad_by, iphys%i_grad_by,              &
     &    icou, trns_tmp)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_b_3, trns_tmp%f_trns%i_grad_bz,                      &
     &    ipol%i_grad_bz, itor%i_grad_bz, iphys%i_grad_bz,              &
     &    icou, trns_tmp)
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_j_1, trns_tmp%f_trns%i_grad_jx,                      &
     &    ipol%i_grad_jx, itor%i_grad_jx, iphys%i_grad_jx,              &
     &    icou, trns_tmp)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_j_2, trns_tmp%f_trns%i_grad_jy,                      &
     &    ipol%i_grad_jy, itor%i_grad_jy, iphys%i_grad_jy,              &
     &    icou, trns_tmp)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_grad_j_3, trns_tmp%f_trns%i_grad_jz,                      &
     &    ipol%i_grad_jz, itor%i_grad_jz, iphys%i_grad_jz,              &
     &    icou, trns_tmp)
!
      end subroutine set_f_trans_scalar_field_stmp
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_stmp
