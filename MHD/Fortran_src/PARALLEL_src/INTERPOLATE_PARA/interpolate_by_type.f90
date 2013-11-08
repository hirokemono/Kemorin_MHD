!>@file   interpolate_by_type.f90
!!@brief  module interpolate_by_type
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2013
!
!>@brief  Interpolation by using insterpolation structure
!!
!!@verbatim
!!      subroutine init_interpolate_mat_type(ele_org, itp_table)
!!
!!      subroutine interpolate_type_1(NP_org, NP_dest,                  &
!!     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!!      subroutine interpolate_type_3(NP_org, NP_dest,                  &
!!     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!!      subroutine interpolate_type_6(NP_org, NP_dest,                  &
!!     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!!      subroutine interpolate_type_N(NP_org, NP_dest, NB,              &
!!     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!!        type(communication_table), intent(in) :: comm_dest
!!        type(interpolate_table), intent(inout) :: itp_table
!!        integer(kind = kint), intent(in) :: PEsmpTOT
!!        integer(kind = kint), intent(in) :: NP_org, NP_dest
!!        real(kind = kreal), intent(in) :: X_org(NP_org)
!!        real(kind = kreal), intent(inout) :: X_dest(NP_dest)
!!@endverbatim
!
      module interpolate_by_type
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_comm_table
      use t_interpolate_table
!
      use m_work_4_interpolation
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_interpolate_mat_type(ele_org, itp_table)
!
      type(element_data), intent(in) :: ele_org
      type(interpolate_table), intent(inout) :: itp_table
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_stack_tbl_org_smp_type'
      call set_stack_tbl_org_smp_type(itp_table%tbl_org)
!
      if (iflag_debug.eq.1) write(*,*) 'const_interporate_mat_type'
      call const_interporate_mat_type                                   &
     &   (ele_org, itp_table%tbl_org, itp_table%mat)
!
      call verifty_work_4_itp_field                                     &
     &   (isix, itp_table%tbl_org%ntot_table_org)
!
      end subroutine init_interpolate_mat_type
!
!  ---------------------------------------------------------------------
!
      subroutine interpolate_type_1(NP_org, NP_dest,                    &
     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!
      use interpolate_scalar_1pe
      use select_calypso_SR
      use solver_SR
!
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(NP_dest)
!
!     initialize
!
      call verifty_work_4_itp_field(ione,                               &
     &    itp_table%tbl_org%ntot_table_org)
!
!    interpolation
!
      if ( itp_table%tbl_org%num_dest_domain .gt. 0 ) then
        call itp_matvec_scalar(PEsmpTOT, NP_org, X_org,                 &
     &      itp_table%mat%NC, itp_table%mat%NCM, itp_table%mat%INM,     &
     &      itp_table%mat%IAM, itp_table%mat%AM,                        &
     &      itp_table%mat%NUM_SUM(4), itp_table%mat%IEND_SUM_smp,       &
     &      X_dest)
      end if
!
!     communication
!
      call sel_calypso_send_recv(iflag_import_item,                     &
     &           itp_table%tbl_org%ntot_table_org, NP_dest,             &
     &           itp_table%tbl_org%num_dest_domain,                     &
     &           itp_table%tbl_org%iflag_self_itp_send,                 &
     &           itp_table%tbl_org%id_dest_domain,                      &
     &           itp_table%tbl_org%istack_nod_tbl_org,                  &
     &           itp_table%tbl_org%inod_itp_send,                       &
     &           itp_table%tbl_dest%num_org_domain,                     &
     &           itp_table%tbl_dest%iflag_self_itp_recv,                &
     &           itp_table%tbl_dest%id_org_domain,                      &
     &           itp_table%tbl_dest%istack_nod_tbl_dest,                &
     &           itp_table%tbl_dest%inod_dest_4_dest,                   &
     &           itp_table%tbl_dest%irev_dest_4_dest,                   &
     &           x_inter_org(1), X_dest(1) )
!
!
      if (comm_dest%num_neib .gt. 0) then
        call SOLVER_SEND_RECV(NP_dest,                                  &
     &      comm_dest%num_neib, comm_dest%id_neib,                      &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export, X_dest(1) )
      end if
!
      end subroutine interpolate_type_1
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_type_3(NP_org, NP_dest,                    &
     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!
      use interpolate_vector_1pe
      use select_calypso_SR
      use solver_SR_3
!
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(ithree*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(ithree*NP_dest)
!
!     initialize
!
      call verifty_work_4_itp_field(ithree,                             &
     &    itp_table%tbl_org%ntot_table_org)
!
!    interpolation
!
      if ( itp_table%tbl_org%num_dest_domain .gt. 0 ) then
        call itp_matvec_vector(PEsmpTOT, NP_org, X_org,                 &
     &      itp_table%mat%NC, itp_table%mat%NCM, itp_table%mat%INM,     &
     &      itp_table%mat%IAM, itp_table%mat%AM,                        &
     &      itp_table%mat%NUM_SUM(4), itp_table%mat%IEND_SUM_smp,       &
     &      X_dest)
      end if
!
!     communication
!
      call sel_calypso_send_recv_3(iflag_import_item,                   &
     &           itp_table%tbl_org%ntot_table_org, NP_dest,             &
     &           itp_table%tbl_org%num_dest_domain,                     &
     &           itp_table%tbl_org%iflag_self_itp_send,                 &
     &           itp_table%tbl_org%id_dest_domain,                      &
     &           itp_table%tbl_org%istack_nod_tbl_org,                  &
     &           itp_table%tbl_org%inod_itp_send,                       &
     &           itp_table%tbl_dest%num_org_domain,                     &
     &           itp_table%tbl_dest%iflag_self_itp_recv,                &
     &           itp_table%tbl_dest%id_org_domain,                      &
     &           itp_table%tbl_dest%istack_nod_tbl_dest,                &
     &           itp_table%tbl_dest%inod_dest_4_dest,                   &
     &           itp_table%tbl_dest%irev_dest_4_dest,                   &
     &           x_inter_org(1), X_dest(1) )
!
!
      if (comm_dest%num_neib .gt. 0) then
        call SOLVER_SEND_RECV_3(NP_dest,                                &
     &      comm_dest%num_neib, comm_dest%id_neib,                      &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export, X_dest(1) )
      end if
!
      end subroutine interpolate_type_3
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_type_6(NP_org, NP_dest,                    &
     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!
      use interpolate_tensor_1pe
      use select_calypso_SR
      use solver_SR_6
!
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(isix*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(isix*NP_dest)
!
!     initialize
!
      call verifty_work_4_itp_field(isix,                               &
     &    itp_table%tbl_org%ntot_table_org)
!
!    interpolation
!
      if ( itp_table%tbl_org%num_dest_domain .gt. 0 ) then
        call itp_matvec_tensor(PEsmpTOT, NP_org, X_org,                 &
     &      itp_table%mat%NC, itp_table%mat%NCM, itp_table%mat%INM,     &
     &      itp_table%mat%IAM, itp_table%mat%AM,                        &
     &      itp_table%mat%NUM_SUM(4), itp_table%mat%IEND_SUM_smp,       &
     &      X_dest)
      end if
!
!     communication
!
      call sel_calypso_send_recv_6(iflag_import_item,                   &
     &           itp_table%tbl_org%ntot_table_org, NP_dest,             &
     &           itp_table%tbl_org%num_dest_domain,                     &
     &           itp_table%tbl_org%iflag_self_itp_send,                 &
     &           itp_table%tbl_org%id_dest_domain,                      &
     &           itp_table%tbl_org%istack_nod_tbl_org,                  &
     &           itp_table%tbl_org%inod_itp_send,                       &
     &           itp_table%tbl_dest%num_org_domain,                     &
     &           itp_table%tbl_dest%iflag_self_itp_recv,                &
     &           itp_table%tbl_dest%id_org_domain,                      &
     &           itp_table%tbl_dest%istack_nod_tbl_dest,                &
     &           itp_table%tbl_dest%inod_dest_4_dest,                   &
     &           itp_table%tbl_dest%irev_dest_4_dest,                   &
     &           x_inter_org(1), X_dest(1) )
!
!
      if (comm_dest%num_neib .gt. 0) then
        call SOLVER_SEND_RECV_6(NP_dest,                                &
     &      comm_dest%num_neib, comm_dest%id_neib,                      &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export, X_dest(1))
      end if
!
      end subroutine interpolate_type_6
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_type_N(NP_org, NP_dest, NB,                &
     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!
      use interpolate_fields_1pe
      use select_calypso_SR
      use solver_SR_N
!
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP_org, NP_dest, NB
      real(kind = kreal), intent(in) :: X_org(NB*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(NB*NP_dest)
!
!     initialize
!
      call verifty_work_4_itp_field(NB,                                 &
     &    itp_table%tbl_org%ntot_table_org)
!
!    interpolation
!
      if ( itp_table%tbl_org%num_dest_domain .gt. 0 ) then
        call itp_matvec_fields(PEsmpTOT, NP_org, NB, X_org,             &
     &      itp_table%mat%NC, itp_table%mat%NCM, itp_table%mat%INM,     &
     &      itp_table%mat%IAM, itp_table%mat%AM,                        &
     &      itp_table%mat%NUM_SUM(4), itp_table%mat%IEND_SUM_smp,       &
     &      X_dest)
      end if
!
!     communication
!
      call sel_calypso_send_recv_N(iflag_import_item, NB,               &
     &           itp_table%tbl_org%ntot_table_org, NP_dest,             &
     &           itp_table%tbl_org%num_dest_domain,                     &
     &           itp_table%tbl_org%iflag_self_itp_send,                 &
     &           itp_table%tbl_org%id_dest_domain,                      &
     &           itp_table%tbl_org%istack_nod_tbl_org,                  &
     &           itp_table%tbl_org%inod_itp_send,                       &
     &           itp_table%tbl_dest%num_org_domain,                     &
     &           itp_table%tbl_dest%iflag_self_itp_recv,                &
     &           itp_table%tbl_dest%id_org_domain,                      &
     &           itp_table%tbl_dest%istack_nod_tbl_dest,                &
     &           itp_table%tbl_dest%inod_dest_4_dest,                   &
     &           itp_table%tbl_dest%irev_dest_4_dest,                   &
     &           x_inter_org(1), X_dest(1) )
!
!
      if (comm_dest%num_neib .gt. 0) then
        call SOLVER_SEND_RECV_N(NP_dest, NB,                            &
     &      comm_dest%num_neib, comm_dest%id_neib,                      &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export, X_dest(1))
      end if
!
      end subroutine interpolate_type_N
!
! ----------------------------------------------------------------------
!
      end module interpolate_by_type
