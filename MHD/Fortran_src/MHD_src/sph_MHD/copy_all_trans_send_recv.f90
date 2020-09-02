!>@file   copy_all_trans_send_recv.f90
!!@brief  module copy_all_trans_send_recv
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Copy spectrum data and field data to spherical transform buffer
!!       for dynamo simulation
!!
!!@verbatim
!!      subroutine copy_all_spectr_to_send(nnod_pole, ncomp_send,       &
!!     &          sph_rj, comm_rj, rj_fld, backward,                    &
!!     &          n_WS, WS, v_pl_local)
!!        type(phys_address), intent(in) :: bs_trns
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(spherical_transform_data), intent(in) :: backward
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine copy_all_spectr_from_trns(iflag_recv,                &
!!     &         (ncomp_recv, comm_rj, forward, n_WR, WR, rj_fld)
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(spherical_transform_data), intent(in) :: forward
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module copy_all_trans_send_recv
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
      use copy_spectr_4_sph_trans
!
      use t_sph_trans_comm_tbl
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_addresses_sph_transform
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_all_spectr_to_send(nnod_pole, ncomp_send,         &
     &          sph_rj, comm_rj, rj_fld, backward,                      &
     &          n_WS, WS, v_pl_local)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(phys_data), intent(in) :: rj_fld
      type(spherical_transform_data), intent(in) :: backward
      integer(kind = kint), intent(in) :: nnod_pole
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pl_local(nnod_pole,ncomp_send)
!
      integer(kind = kint) :: j, j_fld
      integer(kind = kint) :: i_rj, jcomp
!
!
      do j_fld = 1, backward%num_vector
        jcomp = backward%ifld_trns(j_fld)
        i_rj = backward%ifld_rj(j_fld)
!
        if(iflag_debug .gt. 0) write(*,*) 'get vector to send ',        &
     &                                   i_rj, 'to ', jcomp
        call sel_sph_rj_vector_to_send(ncomp_send,                      &
     &      i_rj, jcomp, comm_rj, rj_fld, n_WS, WS)
      end do
!
!
      do j = 1, backward%num_scalar
        j_fld = j + backward%num_vector
        jcomp = backward%ifld_trns(j_fld)
        i_rj = backward%ifld_rj(j_fld)
!
        if(iflag_debug .gt. 0) write(*,*) 'get scalar to send ',        &
     &                                   i_rj, 'to ', jcomp
        call sel_sph_rj_scalar_2_send_wpole(ncomp_send,                 &
     &      i_rj, jcomp, nnod_pole, sph_rj, comm_rj, rj_fld,            &
     &      n_WS, WS, v_pl_local)
      end do
!
      do j = 1, backward%num_tensor
        j_fld = j + backward%num_vector + backward%num_scalar
        jcomp = backward%ifld_trns(j_fld)
        i_rj = backward%ifld_rj(j_fld)
        if(iflag_debug .gt. 0) write(*,*) 'get tensor to send ',        &
     &                                   i_rj, 'to ', jcomp
        call sel_sph_rj_vector_to_send(ncomp_send,                      &
     &      i_rj, jcomp, comm_rj, rj_fld, n_WS, WS)
        call sel_sph_rj_vector_to_send(ncomp_send,                      &
     &      (i_rj+3), (jcomp+3), comm_rj, rj_fld, n_WS, WS)
      end do
!
      end subroutine copy_all_spectr_to_send
!
!-----------------------------------------------------------------------
!
      subroutine copy_all_spectr_from_trns(iflag_recv,                  &
     &          ncomp_recv, comm_rj, forward, n_WR, WR, rj_fld)
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(spherical_transform_data), intent(in) :: forward
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: j, j_fld
      integer(kind = kint) :: i_rj, jcomp
!
!
      do j_fld = 1, forward%num_vector
        jcomp = forward%ifld_trns(j_fld)
        i_rj = forward%ifld_rj(j_fld)
!
        if(iflag_debug .gt. 0) write(*,*) 'get vector from recv ',      &
     &                                   i_rj, 'to ', jcomp
        call sel_sph_rj_vector_from_recv(iflag_recv, ncomp_recv,        &
     &      i_rj, jcomp, comm_rj, n_WR, WR, rj_fld)
      end do
!
      do j = 1, forward%num_scalar
        j_fld = j + forward%num_vector
        jcomp = forward%ifld_trns(j_fld)
        i_rj = forward%ifld_rj(j_fld)
!
        if(iflag_debug .gt. 0) write(*,*) 'get scalar from recv ',      &
     &                                   i_rj, 'to ', jcomp
        call sel_sph_rj_scalar_from_recv(iflag_recv, ncomp_recv,        &
     &      i_rj, jcomp, comm_rj, n_WR, WR, rj_fld)
      end do
!
      do j = 1, forward%num_tensor
        j_fld = j + forward%num_vector + forward%num_scalar
        jcomp = forward%ifld_trns(j_fld)
        i_rj = forward%ifld_rj(j_fld)
!
        if(iflag_debug .gt. 0) write(*,*) 'get tensor from recv ',      &
     &                                   i_rj, 'to ', jcomp
        call sel_sph_rj_vector_from_recv(iflag_recv, ncomp_recv,        &
     &      i_rj, jcomp, comm_rj, n_WR, WR, rj_fld)
        call sel_sph_rj_vector_from_recv(iflag_recv, ncomp_recv,        &
     &      (i_rj+3), (jcomp+3), comm_rj, n_WR, WR, rj_fld)
      end do
!
      end subroutine copy_all_spectr_from_trns
!
!-----------------------------------------------------------------------
!
       end module copy_all_trans_send_recv
