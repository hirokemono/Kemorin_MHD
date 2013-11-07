!>@file   interpolate_by_module.f90
!!@brief  module interpolate_by_module
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!!@n    modified by H. Matsui on Nov., 2013
!
!>@brief  Interpolation by using module data
!!
!!@verbatim
!!      subroutine interpolate_mod_1(NP_org, NP_dest, X_org, X_dest)
!!      subroutine interpolate_mod_3(NP_org, NP_dest, X_org, X_dest)
!!      subroutine interpolate_mod_6(NP_org, NP_dest, X_org, X_dest)
!!      subroutine interpolate_mod_N(NP_org, NP_dest, NB, X_org, X_dest)
!!@endverbatim
!
      module interpolate_by_module
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_nod_comm_table
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
!
      use m_work_4_interpolation
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_mod_1(NP_org, NP_dest, X_org, X_dest)
!
      use m_interpolate_matrix
      use interpolate_scalar_1pe
      use select_calypso_SR
      use solver_SR
!
!
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(3*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(3*NP_dest)
!
!
!  initialize
      call verifty_work_4_itp_field(ione, ntot_table_org)
!
!  interpolation
      if (num_dest_domain .gt. 0) then
        call itp_matvec_scalar(np_smp, numnod, X_org(1),                &
     &      NC_itp, NCM_itp, INM_itp, IAM_itp, AM_itp,                  &
     &      NUM_SUM_itp(4), IEND_SUM_itp_smp, x_inter_org(1))
      end if
!
!   communication
      call sel_calypso_send_recv                                        &
     &          (iflag_import_item, ntot_table_org, nnod_2nd,           &
     &           num_dest_domain, iflag_self_itp_send,                  &
     &           id_dest_domain, istack_nod_tbl_org, inod_itp_send,     &
     &           num_org_domain, iflag_self_itp_recv,                   &
     &           id_org_domain, istack_nod_tbl_dest,                    &
     &           inod_dest_4_dest, irev_dest_4_dest,                    &
     &           x_inter_org(1), X_dest(1) )
!
      if (num_neib_2 .gt. 0) then
        call SOLVER_SEND_RECV                                           &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2, X_dest(1) )
      end if
!
      end subroutine interpolate_mod_1
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_mod_3(NP_org, NP_dest, X_org, X_dest)
!
      use m_interpolate_matrix
      use interpolate_vector_1pe
      use select_calypso_SR
      use solver_SR_3
!
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(3*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(3*NP_dest)
!
!
      call verifty_work_4_itp_field(ithree, ntot_table_org)
!
!    interpolation
!
      if (num_dest_domain.gt.0) then
        call itp_matvec_vector(np_smp, numnod, X_org(1),                &
     &      NC_itp, NCM_itp, INM_itp, IAM_itp, AM_itp,                  &
     &      NUM_SUM_itp(4), IEND_SUM_itp_smp, x_inter_org(1))
      end if
!
!     communication
!
      call sel_calypso_send_recv_3                                      &
     &          (iflag_import_item, ntot_table_org, nnod_2nd,           &
     &           num_dest_domain, iflag_self_itp_send,                  &
     &           id_dest_domain, istack_nod_tbl_org, inod_itp_send,     &
     &           num_org_domain, iflag_self_itp_recv,                   &
     &           id_org_domain, istack_nod_tbl_dest,                    &
     &           inod_dest_4_dest, irev_dest_4_dest,                    &
     &           x_inter_org(1), X_dest(1) )
!
!
      if (num_neib_2.gt.0) then
        call SOLVER_SEND_RECV_3                                         &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2, X_dest(1))
      end if
!
      end subroutine interpolate_mod_3
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_mod_6(NP_org, NP_dest, X_org, X_dest)
!
      use m_interpolate_matrix
      use interpolate_tensor_1pe
      use select_calypso_SR
      use solver_SR_6
!
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(6*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(6*NP_dest)
!
!
      call verifty_work_4_itp_field(isix, ntot_table_org)
!
!
      if (num_dest_domain.gt.0) then
        call itp_matvec_tensor(np_smp, numnod, X_org(1),                &
     &      NC_itp, NCM_itp, INM_itp, IAM_itp, AM_itp,                  &
     &      NUM_SUM_itp(4), IEND_SUM_itp_smp, x_inter_org(1))
      end if
!
      call sel_calypso_send_recv_6                                      &
     &          (iflag_import_item, ntot_table_org, nnod_2nd,           &
     &           num_dest_domain, iflag_self_itp_send,                  &
     &           id_dest_domain, istack_nod_tbl_org, inod_itp_send,     &
     &           num_org_domain, iflag_self_itp_recv,                   &
     &           id_org_domain, istack_nod_tbl_dest,                    &
     &           inod_dest_4_dest, irev_dest_4_dest,                    &
     &           x_inter_org(1), X_dest(1) )
!
      if (num_neib_2.gt.0) then
        call SOLVER_SEND_RECV_6                                         &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2, X_dest(1) )
      end if
!
      end subroutine interpolate_mod_6
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_mod_N(NP_org, NP_dest, NB, X_org, X_dest)
!
      use m_interpolate_matrix
      use interpolate_fields_1pe
      use select_calypso_SR
      use solver_SR_N
!
      integer(kind = kint), intent(in) :: NB, NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(NB*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(NB*NP_dest)
!
!
      call verifty_work_4_itp_field(NB, ntot_table_org)
!
      if (num_dest_domain.gt.0) then
        call itp_matvec_fields(np_smp, numnod, NB,                      &
     &      X_org(1), NC_itp, NCM_itp, INM_itp, IAM_itp, AM_itp,        &
     &      NUM_SUM_itp(4), IEND_SUM_itp_smp, x_inter_org(1))
      end if
!
      call sel_calypso_send_recv_N                                      &
     &          (iflag_import_item, NB, ntot_table_org, nnod_2nd,       &
     &           num_dest_domain, iflag_self_itp_send,                  &
     &           id_dest_domain, istack_nod_tbl_org, inod_itp_send,     &
     &           num_org_domain, iflag_self_itp_recv,                   &
     &           id_org_domain, istack_nod_tbl_dest,                    &
     &           inod_dest_4_dest, irev_dest_4_dest,                    &
     &           x_inter_org(1), X_dest(1) )
!
      if (num_neib_2.gt.0) then
        call SOLVER_SEND_RECV_N                                         &
     &                (nnod_2nd, NB, num_neib_2, id_neib_2,             &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2, X_dest(1) )
      end if
!
      end subroutine interpolate_mod_N
!
! ----------------------------------------------------------------------
!
      end module interpolate_by_module
