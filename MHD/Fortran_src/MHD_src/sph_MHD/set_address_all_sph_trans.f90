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
!!      subroutine set_all_spherical_transform(rj_fld, trns_MHD,        &
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
      private :: set_addresses_backward_trans
      private :: set_addresses_forward_trans
      private :: set_field_names_backward_trans
      private :: set_field_names_forward_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_all_spherical_transform(rj_fld, trns_MHD,          &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
!
      call set_addresses_backward_trans(rj_fld, trns_MHD)
      call set_addresses_forward_trans(rj_fld, trns_MHD)
!
      ncomp_sph_trans =   0
      nvector_sph_trans = 0
      nscalar_sph_trans = 0
      call count_num_fields_4_sph_trans(trns_MHD, ncomp_sph_trans,    &
     &    nvector_sph_trans, nscalar_sph_trans)
!
      call set_field_names_backward_trans(rj_fld, trns_MHD)
      call set_field_names_forward_trans(rj_fld, trns_MHD)
!
      end subroutine set_all_spherical_transform
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_addresses_backward_trans(rj_fld, trns_MHD)
!
      type(phys_data), intent(in) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
      integer(kind = kint) :: i_fld
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
      end subroutine set_addresses_backward_trans
!
!-----------------------------------------------------------------------
!
      subroutine set_addresses_forward_trans(rj_fld, trns_MHD)
!
      type(phys_data), intent(in) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
      integer(kind = kint) :: i_fld
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
      end subroutine set_addresses_forward_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_field_names_backward_trans(rj_fld, trns_MHD)
!
      type(phys_data), intent(in) :: rj_fld
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
      integer(kind = kint) :: i, i_fld, icou, icomp
!
!
      icou =  0
      icomp = 1
      do i = 1, rj_fld%num_phys_viz
        i_fld = rj_fld%istack_component(i-1) + 1
        if(rj_fld%num_component(i) .eq. n_vector) then
          call set_field_name_4_bwd_trns(rj_fld%phys_name(i),           &
     &        icomp, i_fld, (i_fld+2), i_fld, icou, trns_MHD)
          icomp = icomp + 3
        end if
      end do
!
      do i = 1, rj_fld%num_phys_viz
        i_fld = rj_fld%istack_component(i-1) + 1
        if(rj_fld%num_component(i) .eq. n_scalar) then
          call set_field_name_4_bwd_trns(rj_fld%phys_name(i),           &
     &        icomp, i_fld, izero, i_fld, icou, trns_MHD)
          icomp = icomp + 1
        end if
      end do
!
      do i = 1, rj_fld%num_phys_viz
        i_fld = rj_fld%istack_component(i-1) + 1
        if(rj_fld%num_component(i) .eq. n_sym_tensor) then
          call set_field_name_4_bwd_trns(rj_fld%phys_name(i),           &
     &        icomp, i_fld, izero, i_fld, icou, trns_MHD)
          icomp = icomp + 6
        end if
      end do
!
      end subroutine set_field_names_backward_trans
!
!-----------------------------------------------------------------------
!
      subroutine set_field_names_forward_trans(rj_fld, trns_MHD)
!
      type(phys_data), intent(in) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
      integer(kind = kint) :: i, i_fld, icou, icomp
!
!
      icou =  0
      icomp = 1
      do i = 1, rj_fld%num_phys_viz
        i_fld = rj_fld%istack_component(i-1) + 1
        if(rj_fld%num_component(i) .eq. n_vector) then
          call set_field_name_4_fwd_trns(rj_fld%phys_name(i),           &
     &        icomp, i_fld, (i_fld+2), i_fld, icou, trns_MHD)
          icomp = icomp + 3
        end if
      end do
!
      do i = 1, rj_fld%num_phys_viz
        i_fld = rj_fld%istack_component(i-1) + 1
        if(rj_fld%num_component(i) .eq. n_scalar) then
          call set_field_name_4_fwd_trns(rj_fld%phys_name(i),           &
     &        icomp, i_fld, izero, i_fld, icou, trns_MHD)
          icomp = icomp + 1
        end if
      end do
!
      do i = 1, rj_fld%num_phys_viz
        i_fld = rj_fld%istack_component(i-1) + 1
        if(rj_fld%num_component(i) .eq. n_sym_tensor) then
          call set_field_name_4_fwd_trns(rj_fld%phys_name(i),           &
     &        icomp, i_fld, izero, i_fld, icou, trns_MHD)
          icomp = icomp + 6
        end if
      end do
!
      end subroutine set_field_names_forward_trans
!
!-----------------------------------------------------------------------
!
      end module set_address_all_sph_trans
