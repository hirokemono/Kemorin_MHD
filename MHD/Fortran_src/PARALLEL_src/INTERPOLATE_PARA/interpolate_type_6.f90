!
!     module interpolate_type_6
!
!     Written by H. Matsui on Jan., 2009
!
!      subroutine s_interpolate_type_6(NP_org, NP_dest,                 &
!     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!        type(communication_table), intent(in) :: comm_dest
!        type(interpolate_table), intent(in) :: itp_table
!        integer(kind = kint), intent(in) :: PEsmpTOT
!        integer(kind = kint), intent(in) :: NP_org, NP_dest
!        real(kind = kreal), intent(in) :: X_org(isix*NP_org)
!        real(kind = kreal), intent(inout) :: X_dest(isix*NP_dest)
!
      module interpolate_type_6
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_interpolate_type_6(NP_org, NP_dest,                  &
     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!
      use m_constants
      use t_comm_table
      use t_interpolate_table
      use m_work_4_interpolation
!
      use interpolate_1pe
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
        call interporate_tensor_para(PEsmpTOT, NP_org,                  &
     &      itp_table%ele_org%numele, itp_table%ele_org%nnod_4_ele,     &
     &      itp_table%ele_org%ie, X_org(1),                             &
     &      itp_table%tbl_org%istack_tbl_type_org_smp,                  &
     &      itp_table%tbl_org%ntot_table_org,                           &
     &      itp_table%tbl_org%iele_org_4_org,                           &
     &      itp_table%tbl_org%itype_inter_org,                          &
     &      itp_table%tbl_org%coef_inter_org, x_inter_org(1) )
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
      end subroutine s_interpolate_type_6
!
! ----------------------------------------------------------------------
!
      end module interpolate_type_6
