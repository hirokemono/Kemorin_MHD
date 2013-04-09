!     module interpolate_type_3
!
!     Written by H. Matsui on Jan., 2009
!
!      subroutine s_interpolate_type_3(NP_org, NP_dest,                 &
!     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT,         &
!     &          my_rank, SOLVER_COMM)
!        type(communication_table), intent(in) :: comm_dest
!        type(interpolate_table), intent(in) :: itp_table
!        integer(kind = kint), intent(in) :: PEsmpTOT
!        integer(kind = kint), intent(in) :: NP_org, NP_dest
!        integer(kind = kint), intent(in) :: SOLVER_COMM, my_rank
!        real(kind = kreal), intent(in) :: X_org(ithree*NP_org)
!        real(kind = kreal), intent(inout) :: X_dest(ithree*NP_dest)
!
      module interpolate_type_3
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
      subroutine s_interpolate_type_3(NP_org, NP_dest,                  &
     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT,          &
     &          my_rank, SOLVER_COMM)
!
      use m_constants
      use t_comm_table
      use t_interpolate_table
      use m_work_4_interpolation
!
      use interpolate_parallel
      use interpolate_SR_3
      use solver_SR_3
!
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      integer(kind = kint), intent(in) :: SOLVER_COMM, my_rank
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
        call s_interporate_vector_para(PEsmpTOT, NP_org,                &
     &      itp_table%ele_org%numele, itp_table%ele_org%nnod_4_ele,     &
     &      itp_table%ele_org%ie, X_org(1),                             &
     &      itp_table%tbl_org%num_dest_domain,                          &
     &      itp_table%tbl_org%istack_tbl_wtype_org_smp,                 &
     &      itp_table%tbl_org%ntot_table_org,                           &
     &      itp_table%tbl_org%iele_org_4_org,                           &
     &      itp_table%tbl_org%itype_inter_org,                          &
     &      itp_table%tbl_org%coef_inter_org, x_inter_org(1) )
      end if
!
!     communication
!
      call interpolate_send_recv_3                                      &
     &          (itp_table%tbl_org%num_dest_domain,                     &
     &           itp_table%tbl_org%iflag_self_itp_send,                 &
     &           itp_table%tbl_org%ntot_table_org,                      &
     &           itp_table%tbl_org%id_dest_domain,                      &
     &           itp_table%tbl_org%istack_nod_tbl_org,                  &
     &           itp_table%tbl_dest%num_org_domain,                     &
     &           itp_table%tbl_dest%iflag_self_itp_recv,                &
     &           itp_table%tbl_dest%ntot_table_dest,                    &
     &           itp_table%tbl_dest%id_org_domain,                      &
     &           itp_table%tbl_dest%istack_nod_tbl_dest,                &
     &           itp_table%tbl_dest%inod_dest_4_dest,                   &
     &           itp_table%tbl_org%ntot_table_org,                      &
     &           x_inter_org(1), NP_dest, X_dest(1), SOLVER_COMM)
!
!
      if (comm_dest%num_neib .gt. 0) then
        call SOLVER_SEND_RECV_3(NP_dest,                                &
     &      comm_dest%num_neib, comm_dest%id_neib,                      &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export,             &
     &      X_dest(1), SOLVER_COMM, my_rank)
      end if
!
      end subroutine s_interpolate_type_3
!
! ----------------------------------------------------------------------
!
      end module interpolate_type_3
