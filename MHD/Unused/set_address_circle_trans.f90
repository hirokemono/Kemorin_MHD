!>@file   set_address_circle_trans.f90
!!@brief  module set_address_circle_trans
!!
!!@author H. Matsui
!!@date Programmed in March, 2023
!
!>@brief Field addresses for spherical harmonics transform
!!       in dynamo benchmark
!!
!!@verbatim
!!      subroutine set_addresses_circle_trans                           &
!!     &         (d_rj, ipol, iphys, trns_dbench,                       &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_dbench
!!        integer(kind = kint), intent(inout) :: ncomp_sph_trans
!!        integer(kind = kint), intent(inout) :: nvector_sph_trans
!!        integer(kind = kint), intent(inout) :: nscalar_sph_trans
!!@endverbatim
      module set_address_circle_trans
!
      use m_precision
      use m_machine_parameter
!
      use t_phys_data
      use t_phys_address
      use t_sph_trans_arrays_MHD
      use t_mesh_data
      use t_spheric_parameter
!
      implicit none
!
      private :: bwd_trans_address_dbench
      private :: add_base_vec_circle_trns
      private :: add_base_scl_circle_trns
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
      subroutine init_address_dbench_trans(rj_fld, ipol, bench)
!
      use t_phys_data
      use t_phys_address
      use t_field_4_dynamobench
!
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(in) :: ipol
!
      type(dynamobench_monitor), intent(inout) :: bench
!
!
      bench%ncomp_sph_trans_meq = 0
      bench%nvec_sph_trans_meq =  0
      bench%nscl_sph_trans_meq =  0
      call set_addresses_circle_trans                                   &
     &   (rj_fld, ipol, bench%iphys_circle, bench%trns_dbench,          &
     &    bench%ncomp_sph_trans_meq, bench%nvec_sph_trans_meq,          &
     &    bench%nscl_sph_trans_meq)
!
!      if(my_rank .ne. 0) return
!      write(*,*) 'Velocity',     ipol%base%i_velo,                     &
!     &              bench%iphys_circle%base%i_velo,                    &
!     &        bench%trns_dbench%b_trns%base%i_velo
!      write(*,*) 'Magnetic',     ipol%base%i_magne,                    &
!     &              bench%iphys_circle%base%i_magne,                   &
!     &        bench%trns_dbench%b_trns%base%i_magne
!      write(*,*) 'Temperature',  ipol%base%i_temp,                     &
!     &              bench%iphys_circle%base%i_temp,                    &
!     &        bench%trns_dbench%b_trns%base%i_temp
!      write(*,*) 'Composition',  ipol%base%i_light,                    &
!     &              bench%iphys_circle%base%i_light,                   &
!     &        bench%trns_dbench%b_trns%base%i_light
!
      end subroutine init_address_dbench_trans
!
! ----------------------------------------------------------------------
!
      subroutine set_addresses_circle_trans                             &
     &         (d_rj, ipol, ibench, trns_dbench,                        &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_sph_trans_snap
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol, ibench
      type(address_4_sph_trans), intent(inout) :: trns_dbench
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
!
      if(iflag_debug .gt. 0) then
        write(*,*)  'Spherical transform field table ',                 &
     &              'for dynamo benchmark (trns_dbench)'
      end if
!
      call bwd_trans_address_dbench                                     &
     &   (d_rj, ipol, ibench, trns_dbench%b_trns, trns_dbench%backward)
!
      trns_dbench%forward%num_vector = 0
      trns_dbench%forward%num_scalar = 0
      trns_dbench%forward%num_tensor = 0
!
      call count_num_fields_each_trans(trns_dbench%backward,            &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call count_num_fields_each_trans(trns_dbench%forward,             &
     &   ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_dbench%backward%num_vector
        write(*,*) 'nscalar_rj_2_rtp ', trns_dbench%backward%num_scalar
!
        write(*,*) 'nvector_rtp_2_rj ', trns_dbench%forward%num_vector
        write(*,*) 'nscalar_rtp_2_rj ', trns_dbench%forward%num_scalar
      end if
!
      end subroutine set_addresses_circle_trans
!
!-----------------------------------------------------------------------
!
      subroutine bwd_trans_address_dbench                               &
     &         (d_rj, ipol, iphys, b_trns, trns_back)
!
      use add_base_field_4_sph_trns
!
      type(phys_data), intent(in) :: d_rj
      type(phys_address), intent(in) :: ipol, iphys
      type(phys_address), intent(inout) :: b_trns
      type(spherical_transform_data), intent(inout) :: trns_back
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, grid data'
      end if
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
!      Vectors
      call add_base_vec_circle_trns                                     &
     &   (d_rj, ipol%base, iphys%base, b_trns%base, trns_back)
      trns_back%num_vector = trns_back%nfield
!
!      Scalars
      call add_base_scl_circle_trns                                     &
     &   (d_rj, ipol%base, iphys%base, b_trns%base, trns_back)
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
!
      trns_back%num_tensor = 0
!
      end subroutine bwd_trans_address_dbench
!
!-----------------------------------------------------------------------
!
      subroutine add_base_vec_circle_trns                               &
     &         (d_rj, ipol_base, iphys_base, b_trns_base, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_base, iphys_base
      type(base_field_address), intent(inout) :: b_trns_base
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   velocity flag
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_base%i_velo, iphys_base%i_velo, b_trns_base%i_velo,    &
     &      trns)
!   magnetic field flag
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_base%i_magne, iphys_base%i_magne, b_trns_base%i_magne, &
     &      trns)
!
      end subroutine add_base_vec_circle_trns
!
!-----------------------------------------------------------------------
!
      subroutine add_base_scl_circle_trns                               &
     &         (d_rj, ipol_base, iphys_base, b_trns_base, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_base, iphys_base
      type(base_field_address), intent(inout) :: b_trns_base
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   temperature flag
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_base%i_temp, iphys_base%i_temp, b_trns_base%i_temp,      &
     &    trns)
!   composition flag
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_base%i_light, iphys_base%i_light, b_trns_base%i_light,   &
     &    trns)
!
      end subroutine add_base_scl_circle_trns
!
!-----------------------------------------------------------------------
!
      end module set_address_circle_trans
