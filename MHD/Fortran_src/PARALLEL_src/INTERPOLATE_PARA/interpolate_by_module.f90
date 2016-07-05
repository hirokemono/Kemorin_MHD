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
!!      subroutine interpolate_mod_1(comm_dest, NP_org, NP_dest,        &
!!     &          X_org, X_dest)
!!      subroutine interpolate_mod_3(comm_dest, NP_org, NP_dest,        &
!!     &          X_org, X_dest)
!!      subroutine interpolate_mod_6(comm_dest, NP_org, NP_dest,        &
!!     &          X_org, X_dest)
!!      subroutine interpolate_mod_N(comm_dest, NP_org, NP_dest, NB,    &
!!     &          X_org, X_dest)
!!      subroutine s_interpolate_integer(ele_org, comm_dest,            &
!!    &           NP_org, NP_dest, i_vector_dest, i_vector_org)
!!        type(element_data), intent(in) :: ele_org
!!        type(communication_table), intent(in) :: comm_dest
!!@endverbatim
!
      module interpolate_by_module
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
!
      use select_copy_from_recv
!
      use t_work_4_interpolation
      use t_comm_table
!
      implicit none
!
      type(work_4_interoplation), private, save ::  itp1_WK
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_mod_1(comm_dest, NP_org, NP_dest,          &
     &          X_org, X_dest)
!
      use m_solver_SR
      use m_interpolate_table
      use interpolate_scalar_1pe
      use calypso_SR
      use solver_SR
!
!
      type(communication_table), intent(in) :: comm_dest
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(3*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(3*NP_dest)
!
!
!  initialize
      call verifty_work_4_itp_field(ione, itp1_org%ntot_table_org, itp1_org, itp1_WK)
!
!  interpolation
      if (itp1_org%num_dest_domain .gt. 0) then
        call itp_matvec_scalar(np_smp, NP_org, X_org(1),                &
     &      itp1_mat%NC, itp1_mat%NCM, itp1_mat%INM, itp1_mat%IAM, itp1_mat%AM,               &
     &      itp1_mat%NUM_SUM(4), itp1_mat%IEND_SUM_smp, itp1_WK%x_inter_org)
      end if
!
!   communication
      call calypso_send_recv                                            &
     &          (iflag_import_item, itp1_org%ntot_table_org, NP_dest,   &
     &           itp1_org%num_dest_domain, itp1_org%iflag_self_itp_send,                  &
     &           itp1_org%id_dest_domain, itp1_org%istack_nod_tbl_org, itp1_org%inod_itp_send,     &
     &           itp1_dest%num_org_domain, itp1_dest%iflag_self_itp_recv,                   &
     &           itp1_dest%id_org_domain, itp1_dest%istack_nod_tbl_dest, &
     &           itp1_dest%inod_dest_4_dest, itp1_dest%irev_dest_4_dest,                    &
     &           itp1_WK%x_inter_org, X_dest(1) )
!
      if (comm_dest%num_neib .gt. 0) then
        call SOLVER_SEND_RECV                                           &
     &      (NP_dest, comm_dest%num_neib, comm_dest%id_neib,            &
     &       comm_dest%istack_import, comm_dest%item_import,            &
     &       comm_dest%istack_export, comm_dest%item_export, X_dest(1))
      end if
!
      end subroutine interpolate_mod_1
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_mod_3(comm_dest, NP_org, NP_dest,          &
     &          X_org, X_dest)
!
      use m_solver_SR
      use m_interpolate_table
      use interpolate_vector_1pe
      use calypso_SR_3
      use solver_SR_3
!
      type(communication_table), intent(in) :: comm_dest
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(3*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(3*NP_dest)
!
!
      call verifty_work_4_itp_field(ithree, itp1_org%ntot_table_org, itp1_org, itp1_WK)
!
!    interpolation
!
      if (itp1_org%num_dest_domain.gt.0) then
        call itp_matvec_vector(np_smp, NP_org, X_org(1),                &
     &      itp1_mat%NC, itp1_mat%NCM, itp1_mat%INM, itp1_mat%IAM, itp1_mat%AM,                  &
     &      itp1_mat%NUM_SUM(4), itp1_mat%IEND_SUM_smp, itp1_WK%x_inter_org)
      end if
!
!     communication
!
      call calypso_send_recv_3                                          &
     &          (iflag_import_item, itp1_org%ntot_table_org, NP_dest,            &
     &           itp1_org%num_dest_domain, itp1_org%iflag_self_itp_send,                  &
     &           itp1_org%id_dest_domain, itp1_org%istack_nod_tbl_org, itp1_org%inod_itp_send,     &
     &           itp1_dest%num_org_domain, itp1_dest%iflag_self_itp_recv,                   &
     &           itp1_dest%id_org_domain, itp1_dest%istack_nod_tbl_dest, &
     &           itp1_dest%inod_dest_4_dest, itp1_dest%irev_dest_4_dest,                    &
     &           itp1_WK%x_inter_org, X_dest(1) )
!
!
      if (comm_dest%num_neib.gt.0) then
        call SOLVER_SEND_RECV_3                                         &
     &     (NP_dest, comm_dest%num_neib, comm_dest%id_neib,             &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export, X_dest(1))
      end if
!
      end subroutine interpolate_mod_3
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_mod_6(comm_dest, NP_org, NP_dest,          &
     &          X_org, X_dest)
!
      use m_solver_SR
      use m_interpolate_table
      use interpolate_tensor_1pe
      use calypso_SR_6
      use solver_SR_6
!
      type(communication_table), intent(in) :: comm_dest
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(6*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(6*NP_dest)
!
!
      call verifty_work_4_itp_field(isix, itp1_org%ntot_table_org, itp1_org, itp1_WK)
!
!
      if (itp1_org%num_dest_domain.gt.0) then
        call itp_matvec_tensor(np_smp, NP_org, X_org(1),                &
     &      itp1_mat%NC, itp1_mat%NCM, itp1_mat%INM, itp1_mat%IAM, itp1_mat%AM,                  &
     &      itp1_mat%NUM_SUM(4), itp1_mat%IEND_SUM_smp, itp1_WK%x_inter_org)
      end if
!
      call calypso_send_recv_6                                          &
     &          (iflag_import_item, itp1_org%ntot_table_org, NP_dest,            &
     &           itp1_org%num_dest_domain, itp1_org%iflag_self_itp_send,                  &
     &           itp1_org%id_dest_domain, itp1_org%istack_nod_tbl_org, itp1_org%inod_itp_send,     &
     &           itp1_dest%num_org_domain, itp1_dest%iflag_self_itp_recv,                   &
     &           itp1_dest%id_org_domain, itp1_dest%istack_nod_tbl_dest, &
     &           itp1_dest%inod_dest_4_dest, itp1_dest%irev_dest_4_dest,                    &
     &           itp1_WK%x_inter_org, X_dest(1) )
!
      if (comm_dest%num_neib.gt.0) then
        call SOLVER_SEND_RECV_6                                         &
     &     (NP_dest, comm_dest%num_neib, comm_dest%id_neib,             &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export, X_dest(1))
      end if
!
      end subroutine interpolate_mod_6
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_mod_N(comm_dest, NP_org, NP_dest, NB,      &
     &          X_org, X_dest)
!
      use m_solver_SR
      use m_interpolate_table
      use interpolate_fields_1pe
      use select_calypso_SR
      use solver_SR_N
!
      type(communication_table), intent(in) :: comm_dest
      integer(kind = kint), intent(in) :: NB, NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(NB*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(NB*NP_dest)
!
!
      call verifty_work_4_itp_field(NB, itp1_org%ntot_table_org, itp1_org, itp1_WK)
!
      if (itp1_org%num_dest_domain.gt.0) then
        call itp_matvec_fields(np_smp, NP_org, NB,                      &
     &      X_org(1), itp1_mat%NC, itp1_mat%NCM, itp1_mat%INM, itp1_mat%IAM, itp1_mat%AM,        &
     &      itp1_mat%NUM_SUM(4), itp1_mat%IEND_SUM_smp, itp1_WK%x_inter_org)
      end if
!
      call sel_calypso_send_recv_N                                      &
     &          (iflag_import_item, NB, itp1_org%ntot_table_org, NP_dest,        &
     &           itp1_org%num_dest_domain, itp1_org%iflag_self_itp_send,                  &
     &           itp1_org%id_dest_domain, itp1_org%istack_nod_tbl_org, itp1_org%inod_itp_send,     &
     &           itp1_dest%num_org_domain, itp1_dest%iflag_self_itp_recv,                   &
     &           itp1_dest%id_org_domain, itp1_dest%istack_nod_tbl_dest, &
     &           itp1_dest%inod_dest_4_dest, itp1_dest%irev_dest_4_dest,                    &
     &           itp1_WK%x_inter_org, X_dest(1) )
!
      call finish_calypso_send_recv                                     &
     &   (itp1_org%num_dest_domain, itp1_org%iflag_self_itp_send)
!
      if (comm_dest%num_neib.gt.0) then
        call SOLVER_SEND_RECV_N                                         &
     &     (NP_dest, NB, comm_dest%num_neib, comm_dest%id_neib,         &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export, X_dest(1))
      end if
!
      end subroutine interpolate_mod_N
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_integer(ele_org, comm_dest,              &
     &          NP_org, NP_dest, i_vector_dest, i_vector_org)
!
      use t_geometry_data
      use m_solver_SR
      use m_2nd_pallalel_vector
      use m_array_for_send_recv
!
      use interpolate_imark_1pe
      use calypso_SR_int
      use solver_SR_int
!
      type(element_data), intent(in) :: ele_org
      type(communication_table), intent(in) :: comm_dest
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      integer(kind = kint), intent(in) :: i_vector_org(NP_org)
      integer(kind = kint), intent(inout) :: i_vector_dest(NP_dest)
!
!     initialize
!
      call verify_2nd_iccg_int_mat(NP_dest)
!
      call verifty_work_4_itp_int                                       &
     &   (itp1_org%ntot_table_org, itp1_org, itp1_WK)
!
!
      ix_vec(1:NP_org) = i_vector_org(1:NP_org)
!
!    interpolation
!
      if (itp1_org%num_dest_domain.gt.0) then
        call s_interporate_imark_para(np_smp, NP_org,                   &
     &      ele_org%numele, ele_org%nnod_4_ele, ele_org%ie,             &
     &      ix_vec(1), itp1_org%istack_tbl_type_org_smp,                &
     &      itp1_org%ntot_table_org, itp1_org%iele_org_4_org,           &
     &      itp1_org%itype_inter_org, itp1_WK%i_inter_org)
      end if
!
!
!     communication
!
      call calypso_send_recv_int                                        &
     &   (iflag_import_item, itp1_org%ntot_table_org, NP_dest,          &
     &    itp1_org%num_dest_domain, itp1_org%iflag_self_itp_send,       &
     &    itp1_org%id_dest_domain, itp1_org%istack_nod_tbl_org,         &
     &    itp1_org%inod_itp_send,                                       &
     &    itp1_dest%num_org_domain, itp1_dest%iflag_self_itp_recv,      &
     &    itp1_dest%id_org_domain, itp1_dest%istack_nod_tbl_dest,       &
     &    itp1_dest%inod_dest_4_dest, itp1_dest%irev_dest_4_dest,       &
     &    itp1_WK%i_inter_org, ivec_2nd(1) )
!
!
      if (comm_dest%num_neib.gt.0) then
        call solver_send_recv_i                                         &
     &                (NP_dest, comm_dest%num_neib, comm_dest%id_neib,  &
     &                 comm_dest%istack_import, comm_dest%item_import,  &
     &                 comm_dest%istack_export, comm_dest%item_export,  &
     &                 ivec_2nd(1) )
      end if
!
      i_vector_dest(1:NP_dest) = ivec_2nd(1:NP_dest)
!
      end subroutine s_interpolate_integer
!
! ----------------------------------------------------------------------
!
      end module interpolate_by_module
