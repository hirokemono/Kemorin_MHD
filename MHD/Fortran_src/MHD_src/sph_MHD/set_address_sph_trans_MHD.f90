!>@file   set_address_sph_trans_MHD.f90
!!@brief  module set_address_sph_trans_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_addresses_trans_sph_MHD                          &
!!     &         (MHD_prop, SPH_MHD, iphys, trns_MHD,                   &
!!     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(SPH_mesh_field_data), intent(in) :: SPH_MHD
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!
!!      subroutine mhd_spectr_to_sendbuf                                &
!!     &         (trns_MHD, comm_rj, rj_fld, n_WS, WS)
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_data), intent(in) :: rj_fld
!!      subroutine mhd_spectr_to_sendbuf_wpole(nnod_pole,               &
!!     &          sph_rj, comm_rj, rj_fld, n_WS, WS, trns_MHD)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!      subroutine mhd_spectr_from_recvbuf                              &
!!     &         (trns_MHD, comm_rj, n_WR, WR, rj_fld)
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module set_address_sph_trans_MHD
!
      use m_precision
!
      use t_sph_trans_comm_tbl
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_SPH_mesh_field_data
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
      subroutine set_addresses_trans_sph_MHD                            &
     &         (MHD_prop, SPH_MHD, iphys, trns_MHD,                     &
     &          ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
!
      use address_bwd_sph_trans_MHD
      use address_fwd_sph_trans_MHD
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SPH_mesh_field_data), intent(in) :: SPH_MHD
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      integer(kind = kint), intent(inout) :: ncomp_sph_trans
      integer(kind = kint), intent(inout) :: nvector_sph_trans
      integer(kind = kint), intent(inout) :: nscalar_sph_trans
!
      integer(kind = kint):: icou
!
!
      call b_trans_address_vector_MHD                                   &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, SPH_MHD%ipol, trns_MHD)
      call b_trans_address_scalar_MHD                                   &
     &   (MHD_prop%ht_prop, MHD_prop%cp_prop, SPH_MHD%ipol, trns_MHD)
      trns_MHD%ntensor_rj_2_rtp = 0
!
      call f_trans_address_vector_MHD                                   &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, SPH_MHD%ipol, trns_MHD)
      call f_trans_address_scalar_MHD(MHD_prop%fl_prop, trns_MHD)
      trns_MHD%ntensor_rtp_2_rj = 0
!
      ncomp_sph_trans =   0
      nvector_sph_trans = 0
      nscalar_sph_trans = 0
      call count_num_fields_4_sph_trans(trns_MHD, ncomp_sph_trans,      &
     &   nvector_sph_trans, nscalar_sph_trans)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'Spherical transform field table for MHD'
        write(*,*) 'ncomp_sph_trans ', ncomp_sph_trans
        write(*,*) 'nvector_rj_2_rtp ', trns_MHD%nvector_rj_2_rtp
        write(*,*) 'nscalar_rj_2_rtp ', trns_MHD%nscalar_rj_2_rtp
        write(*,*) 'Address for backward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_b_trans_vector_field_MHD                                 &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_MHD)
      call set_b_trans_scalar_field_MHD                                 &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_MHD)
!
     if(iflag_debug .gt. 0) then
        write(*,*) 'nvector_rtp_2_rj ', trns_MHD%nvector_rtp_2_rj
        write(*,*) 'nscalar_rtp_2_rj ', trns_MHD%nscalar_rtp_2_rj
        write(*,*) 'Address for forward transform: ',                  &
     &             'transform, poloidal, troidal, grid data'
      end if
!
      icou = 0
      call set_f_trans_vector_field_MHD                                 &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_MHD)
      call set_f_trans_scalar_field_MHD                                 &
     &   (icou, SPH_MHD%ipol, SPH_MHD%itor, iphys, trns_MHD)
!
      end subroutine set_addresses_trans_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mhd_spectr_to_sendbuf                                  &
     &         (trns_MHD, comm_rj, rj_fld, n_WS, WS)
!
      use copy_spectr_4_sph_trans
!
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
      integer(kind = kint) :: i, inum
!
!
      do i = 1, trns_MHD%nvector_rj_2_rtp
        call sel_sph_rj_vector_to_send(trns_MHD%backward%ncomp,         &
     &      trns_MHD%ifld_rj(i), trns_MHD%ifld_trns(i),                 &
     &      comm_rj, rj_fld, n_WS, WS)
      end do
      do inum = 1, trns_MHD%nscalar_rj_2_rtp
        i = inum + trns_MHD%nvector_rj_2_rtp
        call sel_sph_rj_scalar_to_send(trns_MHD%backward%ncomp,         &
     &      trns_MHD%ifld_rj(i), trns_MHD%ifld_trns(i),                 &
     &      comm_rj, rj_fld, n_WS, WS)
      end do
!
      end subroutine mhd_spectr_to_sendbuf
!
!-----------------------------------------------------------------------
!
      subroutine mhd_spectr_to_sendbuf_wpole(nnod_pole,                 &
     &          sph_rj, comm_rj, rj_fld, n_WS, WS, trns_MHD)
!
      use copy_spectr_4_sph_trans
!
      integer(kind = kint), intent(in) :: nnod_pole
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
      integer(kind = kint) :: i, inum
!
!
      do i = 1, trns_MHD%nvector_rj_2_rtp
        call sel_sph_rj_vector_to_send(trns_MHD%backward%ncomp,         &
     &      trns_MHD%ifld_rj(i), trns_MHD%ifld_trns(i),                 &
     &      comm_rj, rj_fld, n_WS, WS)
      end do
      do inum = 1, trns_MHD%nscalar_rj_2_rtp
        i = inum + trns_MHD%nvector_rj_2_rtp
        call sel_sph_rj_scalar_2_send_wpole(trns_MHD%backward%ncomp,    &
     &      trns_MHD%ifld_rj(i), trns_MHD%ifld_trns(i), nnod_pole,      &
     &      sph_rj, comm_rj, rj_fld, n_WS, WS, trns_MHD%flc_pole)
      end do
!
      end subroutine mhd_spectr_to_sendbuf_wpole
!
!-----------------------------------------------------------------------
!
      subroutine mhd_spectr_from_recvbuf                                &
     &         (trns_MHD, comm_rj, n_WR, WR, rj_fld)
!
      use copy_spectr_4_sph_trans
!
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(sph_comm_tbl), intent(in) :: comm_rj
      integer(kind = kint), intent(in) :: n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: i, inum
!
!
      do i = 1, trns_MHD%nvector_rtp_2_rj
        call sel_sph_rj_vector_from_recv(trns_MHD%forward%ncomp,        &
     &      trns_MHD%ifrc_rj(i), trns_MHD%ifrc_trns(i),                 &
     &      comm_rj, n_WR, WR, rj_fld)
      end do
      do inum = 1, trns_MHD%nscalar_rtp_2_rj
        i = inum + trns_MHD%nvector_rtp_2_rj
        call sel_sph_rj_scalar_from_recv(trns_MHD%forward%ncomp,        &
     &      trns_MHD%ifrc_rj(i), trns_MHD%ifrc_trns(i),                 &
     &      comm_rj, n_WR, WR, rj_fld)
      end do
!
      end  subroutine mhd_spectr_from_recvbuf
!
!-----------------------------------------------------------------------
!
      end module set_address_sph_trans_MHD
