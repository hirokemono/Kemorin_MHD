!>@file   address_fwd_sph_trans_MHD.f90
!!@brief  module address_fwd_sph_trans_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine f_trans_address_vector_MHD                           &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, ipol, iphys,      &
!!     &          f_trns, trns_fwd)
!!      subroutine f_trans_address_scalar_MHD                           &
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_fwd_sph_trans_MHD
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
      subroutine f_trans_address_vector_MHD                             &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, ipol, iphys,        &
     &          f_trns, trns_fwd)
!
      use add_base_force_4_sph_trns
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
!   rotation of Coriolis force
      call add_rot_coriolis_MHD_sph_trns                                &
     &   (fl_prop, ipol%rot_forces, iphys%rot_forces,                   &
     &    f_trns%rot_forces, trns_fwd)
!
!   forces
      call add_base_force_4_MHD_sph_trns                                &
     &   (fl_prop, cd_prop, ht_prop, cp_prop,                           &
     &    ipol%forces, iphys%forces, f_trns%forces, trns_fwd)
!
      trns_fwd%num_vector = trns_fwd%nfield
!
      end subroutine f_trans_address_vector_MHD
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_MHD                             &
     &         (ipol, iphys, f_trns, trns_fwd)
!
      use add_base_force_4_sph_trns
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      call add_div_coriolis_MHD_sph_trns                                &
     &   (ipol%div_forces, iphys%div_forces, f_trns%div_forces,         &
     &    trns_fwd)
!
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
!
      end subroutine f_trans_address_scalar_MHD
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_MHD
