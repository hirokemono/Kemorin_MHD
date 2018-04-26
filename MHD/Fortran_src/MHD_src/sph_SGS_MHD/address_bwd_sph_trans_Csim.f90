!>@file   address_bwd_sph_trans_Csim.f90
!!@brief  module address_bwd_sph_trans_Csim
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine b_trans_address_vector_Csim(itrns_back)
!!      subroutine b_trans_address_scalar_Csim(trns_back)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!@endverbatim
!
      module address_bwd_sph_trans_Csim
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
      subroutine b_trans_address_vector_Csim(trns_back)
!
!      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
!      type(phys_address), intent(inout) :: b_trns
!
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
      trns_back%num_vector = trns_back%nfield
!
      end subroutine b_trans_address_vector_Csim
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_Csim(trns_back)
!
!      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
!      type(phys_address), intent(inout) :: b_trns
!
!
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
!
      end subroutine b_trans_address_scalar_Csim
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_Csim
