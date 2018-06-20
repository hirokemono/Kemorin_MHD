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
!!        type(address_each_sph_trans), intent(in) :: backward
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine copy_all_spectr_from_trns                            &
!!     &         (ncomp_recv, comm_rj, forward, n_WR, WR, rj_fld)
!!        type(sph_comm_tbl), intent(in) :: comm_rj
!!        type(address_each_sph_trans), intent(in) :: forward
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
      type(address_each_sph_trans), intent(in) :: backward
      integer(kind = kint), intent(in) :: nnod_pole
      integer(kind = kint), intent(in) :: ncomp_send, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
      real(kind = kreal), intent(inout)                                 &
     &                :: v_pl_local(nnod_pole,ncomp_send)
!
      integer(kind = kint) :: j, j_fld, i_fld
      integer(kind = kint) :: icomp, jcomp
!
!
      do j_fld = 1, backward%num_vector
        jcomp = n_vector*j_fld - 2
        do i_fld = 1, rj_fld%num_phys_viz
          if(rj_fld%phys_name(i_fld)                                    &
     &        .eq. backward%field_name(j_fld)) then
            icomp = rj_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'set to send buffer ',    &
     &               trim(rj_fld%phys_name(i_fld)), icomp, jcomp
            call sel_sph_rj_vector_to_send(ncomp_send,                  &
     &          icomp, jcomp, comm_rj, rj_fld, n_WS, WS)
          end if
        end do
      end do
!
!
      do j = 1, backward%num_scalar
        j_fld = j + backward%num_vector
        jcomp = j + n_vector*backward%num_vector
        do i_fld = 1, rj_fld%num_phys_viz
          if(rj_fld%phys_name(i_fld)                                    &
     &           .eq. backward%field_name(j_fld)) then
            icomp = rj_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'set to send buffer ',    &
     &               trim(rj_fld%phys_name(i_fld)), icomp, jcomp
            call sel_sph_rj_scalar_2_send_wpole(ncomp_send,             &
     &          icomp, jcomp, nnod_pole, sph_rj, comm_rj, rj_fld,       &
     &          n_WS, WS, v_pl_local)
          end if
        end do
      end do
!
      do j = 1, backward%num_tensor
        j_fld = j + backward%num_vector + backward%num_scalar
        jcomp = n_sym_tensor * j - 5 + backward%num_scalar              &
     &         + n_vector*backward%num_vector
        do i_fld = 1, rj_fld%num_phys_viz
          if(rj_fld%phys_name(i_fld)                                    &
     &           .eq. backward%field_name(j_fld)) then
            icomp = rj_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'set to send buffer ',    &
     &               trim(rj_fld%phys_name(i_fld)), icomp, jcomp
            call sel_sph_rj_vector_to_send(ncomp_send,                  &
     &          icomp, jcomp, comm_rj, rj_fld, n_WS, WS)
            call sel_sph_rj_vector_to_send(ncomp_send,                  &
     &          (icomp+3), (jcomp+3), comm_rj, rj_fld, n_WS, WS)
          end if
        end do
      end do
!
      end subroutine copy_all_spectr_to_send
!
!-----------------------------------------------------------------------
!
      subroutine copy_all_spectr_from_trns                              &
     &         (ncomp_recv, comm_rj, forward, n_WR, WR, rj_fld)
!
      type(sph_comm_tbl), intent(in) :: comm_rj
      type(address_each_sph_trans), intent(in) :: forward
      integer(kind = kint), intent(in) :: ncomp_recv, n_WR
      real(kind = kreal), intent(inout) :: WR(n_WR)
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: j, j_fld, i_fld
      integer(kind = kint) :: icomp, jcomp
!
!
      do j_fld = 1, forward%num_vector
        jcomp = n_vector*j_fld - 2
        do i_fld = 1, rj_fld%num_phys_viz
          if(rj_fld%phys_name(i_fld)                                    &
     &        .eq. forward%field_name(j_fld))  then
            icomp = rj_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'get from recv buffer ',  &
     &               trim(rj_fld%phys_name(i_fld)), icomp, jcomp
            call sel_sph_rj_vector_from_recv(ncomp_recv,                &
     &          icomp, jcomp, comm_rj, n_WR, WR, rj_fld)
          end if
        end do
      end do
!
      do j = 1, forward%num_scalar
        j_fld = j + forward%num_vector
        jcomp = j + n_vector*forward%num_vector
        do i_fld = 1, rj_fld%num_phys_viz
          if(rj_fld%phys_name(i_fld)                                    &
     &        .eq. forward%field_name(j_fld)) then
            icomp = rj_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'get from recv buffer ',  &
     &               trim(rj_fld%phys_name(i_fld)), icomp, jcomp
            call sel_sph_rj_scalar_from_recv(ncomp_recv,                &
     &          icomp, jcomp, comm_rj, n_WR, WR, rj_fld)
          end if
        end do
      end do
!
      do j = 1, forward%num_tensor
        j_fld = j + forward%num_vector + forward%num_scalar
        jcomp = n_sym_tensor * j - 5 + forward%num_scalar               &
     &         + n_vector*forward%num_vector
        do i_fld = 1, rj_fld%num_phys_viz
          if(rj_fld%phys_name(i_fld)                                    &
     &         .eq. forward%field_name(j_fld))then
            icomp = rj_fld%istack_component(i_fld-1) + 1
!
            if(iflag_debug .gt. 0) write(*,*) 'get from recv buffer ',  &
     &               trim(rj_fld%phys_name(i_fld)), icomp, jcomp
            call sel_sph_rj_vector_from_recv(ncomp_recv,                &
     &          icomp, jcomp, comm_rj, n_WR, WR, rj_fld)
            call sel_sph_rj_vector_from_recv(ncomp_recv,                &
     &          (icomp+3), (jcomp+3), comm_rj, n_WR, WR, rj_fld)
          end if
        end do
      end do
!
      end subroutine copy_all_spectr_from_trns
!
!-----------------------------------------------------------------------
!
       end module copy_all_trans_send_recv
