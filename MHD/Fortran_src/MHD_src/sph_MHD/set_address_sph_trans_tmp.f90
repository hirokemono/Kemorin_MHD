!>@file   set_address_sph_trans_tmp.f90
!!@brief  module set_address_sph_trans_tmp
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_temporal_trans(ipol, trns_tmp,         &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_tmp
!!      subroutine check_address_trans_sph_tmp(ipol, trns_tmp)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(in) :: trns_tmp
!!@endverbatim
!
      module set_address_sph_trans_tmp
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
      private :: b_trans_address_vector_tmp
      private :: b_trans_address_scalar_tmp
      private :: f_trans_address_vector_tmp
      private :: f_trans_address_scalar_tmp
      private :: check_addresses_temporal_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_temporal_trans(ipol, trns_tmp,           &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use m_node_phys_data
      use t_addresses_sph_transform
!
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_tmp
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint) :: nscltsr_rtp_2_rj, nscltsr_rj_2_rtp
!
      trns_tmp%nvector_rj_2_rtp = 0
!      call b_trans_address_vector_tmp                                  &
!     &   (ipol, trns_tmp%nvector_rj_2_rtp, trns_tmp%b_trns)
      trns_tmp%nscalar_rj_2_rtp = 0
!      call b_trans_address_scalar_tmp(ipol, trns_tmp%nvector_rj_2_rtp, &
!     &    trns_tmp%nscalar_rj_2_rtp, trns_tmp%b_trns)
      trns_tmp%ntensor_rj_2_rtp = 0
!
      trns_tmp%nvector_rtp_2_rj = 0
!      call f_trans_address_vector_tmp                                  &
!     &   (ipol, trns_tmp%nvector_rtp_2_rj, trns_tmp%f_trns)
      call f_trans_address_scalar_tmp(ipol, trns_tmp%nvector_rtp_2_rj,  &
     &    trns_tmp%nscalar_rtp_2_rj, trns_tmp%f_trns)
      trns_tmp%ntensor_rtp_2_rj = 0
!
!
      nscltsr_rj_2_rtp                                                  &
     &      = trns_tmp%nscalar_rj_2_rtp + 6*trns_tmp%ntensor_rj_2_rtp
      trns_tmp%ncomp_rj_2_rtp                                           &
     &      = 3*trns_tmp%nvector_rj_2_rtp + nscltsr_rj_2_rtp
!
      nscltsr_rtp_2_rj                                                  &
     &      = trns_tmp%nscalar_rtp_2_rj + 6*trns_tmp%ntensor_rtp_2_rj
      trns_tmp%ncomp_rtp_2_rj                                           &
     &      = 3*trns_tmp%nvector_rtp_2_rj + nscltsr_rtp_2_rj
!
      ncomp_sph_trans = max(ncomp_sph_trans, trns_tmp%ncomp_rtp_2_rj)
      ncomp_sph_trans = max(ncomp_sph_trans, trns_tmp%ncomp_rj_2_rtp)
!
      nvector_sph_trans                                                 &
     &      = max(nvector_sph_trans, trns_tmp%nvector_rj_2_rtp)
      nvector_sph_trans                                                 &
     &      = max(nvector_sph_trans, trns_tmp%nvector_rtp_2_rj)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rj_2_rtp)
      nscalar_sph_trans = max(nscalar_sph_trans, nscltsr_rtp_2_rj)
!
      end subroutine set_addresses_temporal_trans
!
!-----------------------------------------------------------------------
!
      subroutine check_address_trans_sph_tmp(ipol, trns_tmp)
!
      use t_addresses_sph_transform
!
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(in) :: trns_tmp
!
!
      call check_addresses_temporal_trans                               &
     &   (ipol, trns_tmp%b_trns, trns_tmp%f_trns,                       &
     &    trns_tmp%ncomp_rj_2_rtp, trns_tmp%nvector_rj_2_rtp,           &
     &    trns_tmp%nscalar_rj_2_rtp, trns_tmp%ncomp_rtp_2_rj,           &
     &    trns_tmp%nvector_rtp_2_rj, trns_tmp%nscalar_rtp_2_rj)
!
      end subroutine check_address_trans_sph_tmp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_vector_tmp                             &
     &         (ipol, nvector_tmp_rj_2_rtp, bt_trns)
!
      use m_control_parameter
      use m_node_phys_data
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(inout) :: nvector_tmp_rj_2_rtp
      type(phys_address), intent(inout) :: bt_trns
!
!
      nvector_tmp_rj_2_rtp = 0
      call add_vec_trans_flag(ipol%i_grad_vx, iphys%i_grad_vx,          &
     &    nvector_tmp_rj_2_rtp, bt_trns%i_grad_vx)
!
      end subroutine b_trans_address_vector_tmp
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_tmp(ipol, nvector_tmp_rj_2_rtp, &
     &          nscalar_tmp_rj_2_rtp, bt_trns)
!
      use m_control_parameter
      use m_node_phys_data
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nvector_tmp_rj_2_rtp
      integer(kind = kint), intent(inout) :: nscalar_tmp_rj_2_rtp
      type(phys_address), intent(inout) :: bt_trns
!
!
      nscalar_tmp_rj_2_rtp = 0
      call add_scalar_trans_flag(ipol%i_temp, iphys%i_temp,             &
     &    nvector_tmp_rj_2_rtp, nscalar_tmp_rj_2_rtp, bt_trns%i_temp)
!
      end subroutine b_trans_address_scalar_tmp
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_vector_tmp                             &
     &         (ipol, nvector_tmp_rtp_2_rj, ft_trns)
!
      use m_control_parameter
      use m_node_phys_data
!
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(inout) :: ft_trns
      integer(kind = kint), intent(inout) :: nvector_tmp_rtp_2_rj
!
!
      nvector_tmp_rtp_2_rj = 0
      call add_vec_trans_flag(ipol%i_coriolis, iphys%i_coriolis,        &
     &    nvector_tmp_rtp_2_rj, ft_trns%i_coriolis)
!
      end subroutine f_trans_address_vector_tmp
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_tmp(ipol, nvector_tmp_rtp_2_rj, &
     &          nscalar_tmp_rtp_2_rj, ft_trns)
!
      use m_control_parameter
      use m_node_phys_data
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nvector_tmp_rtp_2_rj
      integer(kind = kint), intent(inout) :: nscalar_tmp_rtp_2_rj
      type(phys_address), intent(inout) :: ft_trns
!
!
      nscalar_tmp_rtp_2_rj = 0
      call add_scalar_trans_flag(ipol%i_grad_vx, iphys%i_grad_vx,       &
     &    nvector_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                   &
     &    ft_trns%i_grad_vx)
      call add_scalar_trans_flag(ipol%i_grad_vy, iphys%i_grad_vy,       &
     &    nvector_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                   &
     &    ft_trns%i_grad_vy)
      call add_scalar_trans_flag(ipol%i_grad_vz, iphys%i_grad_vz,       &
     &    nvector_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                   &
     &    ft_trns%i_grad_vz)
!
      end subroutine f_trans_address_scalar_tmp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_addresses_temporal_trans(ipol, bt_trns, ft_trns, &
     &          ncomp_tmp_rj_2_rtp, nvector_tmp_rj_2_rtp,               &
     &          nscalar_tmp_rj_2_rtp, ncomp_tmp_rtp_2_rj,               &
     &          nvector_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj)
!
      use m_node_phys_data
!
      type(phys_address), intent(in) :: ipol
      type(phys_address), intent(in) :: bt_trns, ft_trns
      integer(kind = kint), intent(in) :: ncomp_tmp_rj_2_rtp
      integer(kind = kint), intent(in) :: nvector_tmp_rj_2_rtp
      integer(kind = kint), intent(in) :: nscalar_tmp_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_tmp_rtp_2_rj
      integer(kind = kint), intent(in) :: nvector_tmp_rtp_2_rj
      integer(kind = kint), intent(in) :: nscalar_tmp_rtp_2_rj
!
!
      write(*,*) 'ncomp_tmp_rj_2_rtp', ncomp_tmp_rj_2_rtp
      write(*,*) 'ncomp_tmp_rtp_2_rj', ncomp_tmp_rtp_2_rj
!
      write(*,*) 'nvector_tmp_rj_2_rtp', nvector_tmp_rj_2_rtp
!      if(bt_trns%i_grad_vx .gt. 0) write(*,*)                          &
!     &            'bt_trns%i_grad_vx', bt_trns%i_grad_vx,              &
!     &            ipol%i_grad_vx, iphys%i_grad_vx
      write(*,*)
!
      write(*,*) 'nscalar_tmp_rj_2_rtp', nscalar_tmp_rj_2_rtp
!      if(bt_trns%i_temp .gt. 0) write(*,*)                             &
!     &            'bt_trns%i_temp', bt_trns%i_temp,                    &
!     &            ipol%i_temp, iphys%i_temp
      write(*,*)
!
!
      write(*,*) 'nvector_tmp_rtp_2_rj', nvector_tmp_rtp_2_rj
!      if(ft_trns%i_coriolis .gt. 0) write(*,*)                         &
!     &            'ft_trns%i_coriolis',  ft_trns%i_coriolis,           &
!     &            ipol%i_coriolis, iphys%i_coriolis
!
!
      write(*,*) 'nscalar_tmp_rtp_2_rj', nscalar_tmp_rtp_2_rj
      if(ft_trns%i_grad_vx .gt. 0) write(*,*)                           &
     &            'ft_trns%i_grad_vx', ft_trns%i_grad_vx,               &
     &            ipol%i_grad_vx, iphys%i_velo
      if(ft_trns%i_grad_vy .gt. 0) write(*,*)                           &
     &            'ft_trns%i_grad_vy', ft_trns%i_grad_vy,               &
     &            ipol%i_grad_vy, iphys%i_velo+1
      if(ft_trns%i_grad_vz .gt. 0) write(*,*)                           &
     &            'ft_trns%i_grad_vz', ft_trns%i_grad_vz,               &
     &            ipol%i_grad_vz, iphys%i_velo+2
        write(*,*)
!
      end subroutine check_addresses_temporal_trans
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_tmp
