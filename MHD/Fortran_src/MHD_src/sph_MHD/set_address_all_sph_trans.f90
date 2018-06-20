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
      private :: set_addresses_all_transform
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
      call set_addresses_all_transform(rj_fld, trns_MHD%backward)
      call set_addresses_all_transform(rj_fld, trns_MHD%forward)
!
      ncomp_sph_trans =   0
      nvector_sph_trans = 0
      nscalar_sph_trans = 0
      call count_num_fields_each_trans(trns_MHD%backward,               &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_MHD%forward,                &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      end subroutine set_all_spherical_transform
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_addresses_all_transform(rj_fld, each_trns)
!
      type(phys_data), intent(in) :: rj_fld
      type(address_each_sph_trans), intent(inout) :: each_trns
!
      integer(kind = kint) :: i, i_fld, i_trns
!
!
      each_trns%nfield = 0
      call alloc_sph_trns_field_name(each_trns)
!
      do i = 1, rj_fld%num_phys_viz
        i_fld = rj_fld%istack_component(i-1) + 1
        if(rj_fld%num_component(i) .eq. n_vector) then
          call add_field_name_4_sph_trns_snap                           &
     &       (rj_fld%phys_name(i), n_vector, i_fld, (i_fld+2),          &
     &        i_fld, i_trns, each_trns)
        end if
      end do
      each_trns%num_vector = each_trns%nfield
!
      do i = 1, rj_fld%num_phys_viz
        i_fld = rj_fld%istack_component(i-1) + 1
        if(rj_fld%num_component(i) .eq. n_scalar) then
          call add_field_name_4_sph_trns_snap                           &
     &       (rj_fld%phys_name(i), n_scalar, i_fld, (i_fld+2),          &
     &        i_fld, i_trns, each_trns)
        end if
      end do
      each_trns%num_scalar = each_trns%nfield - each_trns%num_vector
!
      do i = 1, rj_fld%num_phys_viz
        i_fld = rj_fld%istack_component(i-1) + 1
        if(rj_fld%num_component(i) .eq. n_sym_tensor) then
          call add_field_name_4_sph_trns_snap                           &
     &       (rj_fld%phys_name(i), n_sym_tensor, i_fld, (i_fld+2),      &
     &        i_fld, i_trns, each_trns)
        end if
      end do
      each_trns%num_tensor = each_trns%nfield                           &
     &                    - each_trns%num_vector - each_trns%num_scalar
!
      end subroutine set_addresses_all_transform
!
!-----------------------------------------------------------------------
!
      end module set_address_all_sph_trans
