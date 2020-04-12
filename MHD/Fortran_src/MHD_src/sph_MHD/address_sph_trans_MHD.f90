!>@file   address_sph_trans_MHD.f90
!!@brief  module address_sph_trans_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine bwd_trans_address_MHD                                &
!!     &         (MHD_prop, ipol, iphys, b_trns, trns_back)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!      subroutine fwd_trans_address_MHD                                &
!!     &         (MHD_prop, ipol, iphys, f_trns, trns_fwd)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_sph_trans_MHD
!
      use m_precision
      use m_machine_parameter
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
      subroutine bwd_trans_address_MHD                                  &
     &         (MHD_prop, ipol, iphys, b_trns, trns_back)
!
      use add_base_field_4_sph_trns
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
!      Vectors
      call add_base_vector_4_MHD_sph_trns(MHD_prop%fl_prop,             &
     &    MHD_prop%cd_prop, MHD_prop%ht_prop, MHD_prop%cp_prop,         &
     &    ipol%base, iphys%base, b_trns%base, trns_back)
      trns_back%num_vector = trns_back%nfield
!
!      Scalars
      call add_base_scalar_4_MHD_sph_trns                               &
     &   (MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    ipol%base, iphys%base, b_trns%base, trns_back)
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
      trns_back%num_tensor = 0
!
      end subroutine bwd_trans_address_MHD
!
!-----------------------------------------------------------------------
!
      subroutine fwd_trans_address_MHD                                  &
     &         (MHD_prop, ipol, iphys, f_trns, trns_fwd)
!
      use add_base_force_4_sph_trns
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: ipol, iphys
!
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for forward transform: ',                   &
     &             'transform, poloidal, toroidal, grid data'
      end if
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
!   rotation of Coriolis force
      call add_rot_coriolis_MHD_sph_trns(MHD_prop%fl_prop,              &
     &    ipol%rot_forces, iphys%rot_forces, f_trns%rot_forces,         &
     &    trns_fwd)
!   forces
      call add_base_force_4_MHD_sph_trns(MHD_prop%fl_prop,              &
     &    MHD_prop%cd_prop, MHD_prop%ht_prop, MHD_prop%cp_prop,         &
     &    ipol%forces, iphys%forces, f_trns%forces, trns_fwd)
      trns_fwd%num_vector = trns_fwd%nfield
!
!   divergence of Coriolis force
      call add_div_coriolis_MHD_sph_trns                                &
     &   (ipol%div_forces, iphys%div_forces, f_trns%div_forces,         &
     &    trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
      trns_fwd%num_tensor = 0
!
      end subroutine fwd_trans_address_MHD
!
!-----------------------------------------------------------------------
!
      end module address_sph_trans_MHD
