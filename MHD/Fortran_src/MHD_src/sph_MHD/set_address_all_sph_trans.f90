!>@file   set_address_all_sph_trans.f90
!!@brief  module set_address_all_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_backward_trans(rj_fld, trns_MHD,       &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!      subroutine set_addresses_forward_trans(rj_fld, trns_MHD,        &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!@endverbatim
!
      module set_address_all_sph_trans
!
      use m_precision
      use m_phys_constants
!
      use t_phys_data
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
      subroutine set_addresses_backward_trans(rj_fld, trns_MHD,         &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
      integer(kind = kint) :: nscltsr_rtp_2_rj, nscltsr_rj_2_rtp
      integer(kind = kint) :: i_fld, icou
!
!
      trns_MHD%nscalar_rj_2_rtp = 0
      trns_MHD%nvector_rj_2_rtp = 0
      trns_MHD%ntensor_rj_2_rtp = 0
      trns_MHD%nscalar_rtp_2_rj = 0
      trns_MHD%nvector_rtp_2_rj = 1
      trns_MHD%ntensor_rtp_2_rj = 0
!
      do i_fld = 1, rj_fld%num_phys_viz
        if(rj_fld%num_component(i_fld) .eq. n_vector) then
          trns_MHD%nvector_rj_2_rtp = trns_MHD%nvector_rj_2_rtp + 1
        end if
        if(rj_fld%num_component(i_fld) .eq. n_scalar) then
          trns_MHD%nscalar_rj_2_rtp = trns_MHD%nscalar_rj_2_rtp + 1
        end if
        if(rj_fld%num_component(i_fld) .eq. n_sym_tensor) then
          trns_MHD%ntensor_rj_2_rtp = trns_MHD%ntensor_rj_2_rtp + 1
        end if
      end do
!
      trns_MHD%nfield_rj_2_rtp = rj_fld%num_phys_viz
      call alloc_bwd_trns_field_name(trns_MHD)
!
      icou = 0
      do i_fld = 1, rj_fld%num_phys_viz
        if(rj_fld%num_component(i_fld) .eq. n_vector) then
          icou = icou + 1
          trns_MHD%b_trns_name(icou) = rj_fld%phys_name(i_fld)
        end if
      end do
!
      do i_fld = 1, rj_fld%num_phys_viz
        if(rj_fld%num_component(i_fld) .eq. n_scalar) then
          icou = icou + 1
          trns_MHD%b_trns_name(icou) = rj_fld%phys_name(i_fld)
        end if
      end do
!
      do i_fld = 1, rj_fld%num_phys_viz
        if(rj_fld%num_component(i_fld) .eq. n_sym_tensor) then
          icou = icou + 1
          trns_MHD%b_trns_name(icou) = rj_fld%phys_name(i_fld)
        end if
      end do
!
      nscltsr_rtp_2_rj                                                  &
     &      = trns_MHD%nscalar_rj_2_rtp + 6*trns_MHD%ntensor_rj_2_rtp
      trns_MHD%ncomp_rj_2_rtp                                           &
     &      = 3*trns_MHD%nvector_rj_2_rtp + nscltsr_rtp_2_rj
!
      nscltsr_rj_2_rtp                                                  &
     &      = trns_MHD%nscalar_rtp_2_rj + 6*trns_MHD%ntensor_rtp_2_rj
      trns_MHD%ncomp_rtp_2_rj                                           &
     &      = 3*trns_MHD%nvector_rtp_2_rj + nscltsr_rj_2_rtp
!
      ncomp_sph_trans                                                   &
     &      = max(trns_MHD%ncomp_rj_2_rtp, trns_MHD%ncomp_rtp_2_rj)
      nvector_sph_trans                                                 &
     &      = max(trns_MHD%nvector_rj_2_rtp, trns_MHD%nvector_rtp_2_rj)
      nscalar_sph_trans = max(nscltsr_rtp_2_rj, nscltsr_rj_2_rtp)
!
      end subroutine set_addresses_backward_trans
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_forward_trans(rj_fld, trns_MHD,          &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
      integer(kind = kint) :: nscltsr_rtp_2_rj, nscltsr_rj_2_rtp
      integer(kind = kint) :: i_fld, icou
!
!
      trns_MHD%nscalar_rj_2_rtp = 0
      trns_MHD%nvector_rj_2_rtp = 1
      trns_MHD%ntensor_rj_2_rtp = 0
      trns_MHD%nscalar_rtp_2_rj = 0
      trns_MHD%nvector_rtp_2_rj = 0
      trns_MHD%ntensor_rtp_2_rj = 0
!
      do i_fld = 1, rj_fld%num_phys_viz
        if(rj_fld%num_component(i_fld) .eq. n_vector) then
          trns_MHD%nvector_rtp_2_rj = trns_MHD%nvector_rtp_2_rj + 1
        end if
        if(rj_fld%num_component(i_fld) .eq. n_scalar) then
          trns_MHD%nscalar_rtp_2_rj = trns_MHD%nscalar_rtp_2_rj + 1
        end if
        if(rj_fld%num_component(i_fld) .eq. n_sym_tensor) then
          trns_MHD%ntensor_rtp_2_rj = trns_MHD%ntensor_rtp_2_rj + 1
        end if
      end do
!
      trns_MHD%nfield_rtp_2_rj = rj_fld%num_phys_viz
      call alloc_fwd_trns_field_name(trns_MHD)
!
      icou = 0
      do i_fld = 1, rj_fld%num_phys_viz
        if(rj_fld%num_component(i_fld) .eq. n_vector) then
          icou = icou + 1
          trns_MHD%f_trns_name(icou) = rj_fld%phys_name(i_fld)
        end if
      end do
!
      do i_fld = 1, rj_fld%num_phys_viz
        if(rj_fld%num_component(i_fld) .eq. n_scalar) then
          icou = icou + 1
          trns_MHD%f_trns_name(icou) = rj_fld%phys_name(i_fld)
        end if
      end do
!
      do i_fld = 1, rj_fld%num_phys_viz
        if(rj_fld%num_component(i_fld) .eq. n_sym_tensor) then
          icou = icou + 1
          trns_MHD%f_trns_name(icou) = rj_fld%phys_name(i_fld)
        end if
      end do
!
!
      nscltsr_rtp_2_rj                                                  &
     &      = trns_MHD%nscalar_rj_2_rtp + 6*trns_MHD%ntensor_rj_2_rtp
      trns_MHD%ncomp_rj_2_rtp                                           &
     &      = 3*trns_MHD%nvector_rj_2_rtp + nscltsr_rtp_2_rj
!
      nscltsr_rj_2_rtp                                                  &
     &      = trns_MHD%nscalar_rtp_2_rj + 6*trns_MHD%ntensor_rtp_2_rj
      trns_MHD%ncomp_rtp_2_rj                                           &
     &      = 3*trns_MHD%nvector_rtp_2_rj + nscltsr_rj_2_rtp
!
      ncomp_sph_trans                                                   &
     &      = max(trns_MHD%ncomp_rj_2_rtp, trns_MHD%ncomp_rtp_2_rj)
      nvector_sph_trans                                                 &
     &      = max(trns_MHD%nvector_rj_2_rtp, trns_MHD%nvector_rtp_2_rj)
      nscalar_sph_trans = max(nscltsr_rtp_2_rj, nscltsr_rj_2_rtp)
!
      end subroutine set_addresses_forward_trans
!
!-----------------------------------------------------------------------
!
      end module set_address_all_sph_trans
