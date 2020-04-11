!>@file   address_bwd_sph_trans_MHD.f90
!!@brief  module address_bwd_sph_trans_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine b_trans_address_vector_MHD                           &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop,                   &
!!     &          ipol, iphys, b_trns, trns_back)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!      subroutine b_trans_address_scalar_MHD                           &
!!     &         (ht_prop, cp_prop, ipol, iphys, b_trns, trns_back)
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!@endverbatim
!
      module address_bwd_sph_trans_MHD
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
      subroutine b_trans_address_vector_MHD                             &
     &         (fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          ipol, iphys, b_trns, trns_back)
!
      use add_base_field_4_sph_trns
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
      call add_base_vector_4_MHD_sph_trns                               &
     &   (fl_prop, cd_prop, ht_prop, cp_prop,                           &
     &    ipol%base, iphys%base, b_trns%base, trns_back)
      trns_back%num_vector = trns_back%nfield
!
      end subroutine b_trans_address_vector_MHD
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_MHD                             &
     &         (ht_prop, cp_prop, ipol, iphys, b_trns, trns_back)
!
      use add_base_field_4_sph_trns
!
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      call add_base_scalar_4_MHD_sph_trns(ht_prop, cp_prop,             &
     &    ipol%base, iphys%base, b_trns%base, trns_back)
!
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
!
      end subroutine b_trans_address_scalar_MHD
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_MHD
