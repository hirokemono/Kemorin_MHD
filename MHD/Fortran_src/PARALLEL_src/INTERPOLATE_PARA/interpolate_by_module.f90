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
!!      subroutine init_interpolate_mat_type(ele_org, tbl_org, itp_mat)
!!
!!      subroutine interpolate_mod_1                                    &
!!     &         (iflag_recv, comm_dest, tbl_org, tbl_dest, itp_mat,    &
!!     &          PEsmpTOT, NP_org, NP_dest, X_org,                     &
!!     &          SR_sig, SR_r, X_dest)
!!      subroutine interpolate_mod_3                                    &
!!     &         (iflag_recv, comm_dest, tbl_org, tbl_dest, itp_mat,    &
!!     &          PEsmpTOT, NP_org, NP_dest, X_org,                     &
!!     &          SR_sig, SR_r, X_dest)
!!      subroutine interpolate_mod_6                                    &
!!     &         (iflag_recv, comm_dest, tbl_org, tbl_dest, itp_mat,    &
!!     &          PEsmpTOT, NP_org, NP_dest, X_org,                     &
!!     &          SR_sig, SR_r, X_dest)
!!      subroutine interpolate_mod_N                                    &
!!     &         (iflag_recv, comm_dest, tbl_org, tbl_dest, itp_mat,    &
!!     &          PEsmpTOT, NP_org, NP_dest, NB, X_org,                 &
!!     &          SR_sig, SR_r, X_dest)
!!      subroutine s_interpolate_integer                                &
!!     &        (iflag_recv, ele_org, comm_dest, tbl_org, tbl_dest,     &
!!     &         PEsmpTOT, NP_org, NP_dest, i_vector_org,               &
!!     &         SR_sig, SR_i, i_vector_dest)
!!        type(element_data), intent(in) :: ele_org
!!        type(communication_table), intent(in) :: comm_dest
!!        type(interpolate_table_org), intent(in) ::  tbl_org
!!        type(interpolate_table_dest), intent(in) :: tbl_dest
!!        type(CRS_SMP_CONNECT_MATRIX), intent(in) :: itp_mat
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!@endverbatim
!
      module interpolate_by_module
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use select_copy_from_recv
!
      use t_work_4_interpolation
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_solver_ordered_crs
      use t_comm_table
      use t_vector_for_solver
      use t_solver_SR
!
      implicit none
!
      type(work_4_interoplation), private, save ::  itp_WORK
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_interpolate_mat_type(ele_org, tbl_org, itp_mat)
!
      use m_machine_parameter
      use t_geometry_data
      use t_interpolate_table
!
      type(element_data), intent(in) :: ele_org
      type(interpolate_table_org), intent(inout) ::  tbl_org
      type(CRS_SMP_CONNECT_MATRIX), intent(inout) :: itp_mat
!
!
      if (iflag_debug.eq.1) write(*,*) 'const_interporate_matrix'
      call const_interporate_matrix(ele_org, tbl_org, itp_mat)
!
      call verifty_work_4_itp_field                                     &
     &   (isix, tbl_org%ntot_table_org, tbl_org, itp_WORK)
!
      end subroutine init_interpolate_mat_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine interpolate_mod_1                                      &
     &         (iflag_recv, comm_dest, tbl_org, tbl_dest, itp_mat,      &
     &          PEsmpTOT, NP_org, NP_dest, X_org,                       &
     &          SR_sig, SR_r, X_dest)
!
      use interpolate_scalar_1pe
      use calypso_SR
      use solver_SR
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table_org), intent(in) ::  tbl_org
      type(interpolate_table_dest), intent(in) :: tbl_dest
      type(CRS_SMP_CONNECT_MATRIX), intent(in) :: itp_mat
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(3*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(3*NP_dest)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
!  initialize
      call verifty_work_4_itp_field                                     &
     &   (ione, tbl_org%ntot_table_org, tbl_org, itp_WORK)
!
!  interpolation
      if (tbl_org%num_dest_domain .gt. 0) then
        call itp_matvec_scalar(PEsmpTOT, NP_org, X_org(1),              &
     &      itp_mat%NC, itp_mat%NCM, itp_mat%INM, itp_mat%IAM,          &
     &      itp_mat%AM, itp_mat%NUM_SUM(4), itp_mat%IEND_SUM_smp,       &
     &      itp_WORK%x_inter_org)
      end if
!
!   communication
      call calypso_send_recv(iflag_recv,                                &
     &    tbl_org%ntot_table_org, NP_dest, tbl_org%num_dest_domain,     &
     &    tbl_org%iflag_self_itp_send, tbl_org%id_dest_domain,          &
     &    tbl_org%istack_nod_tbl_org, tbl_org%inod_itp_send,            &
     &    tbl_dest%num_org_domain, tbl_dest%iflag_self_itp_recv,        &
     &    tbl_dest%id_org_domain, tbl_dest%istack_nod_tbl_dest,         &
     &    tbl_dest%inod_dest_4_dest, tbl_dest%irev_dest_4_dest,         &
     &    SR_sig, SR_r, itp_WORK%x_inter_org, X_dest(1))
!
      if (comm_dest%num_neib .gt. 0) then
        call SOLVER_SEND_RECV                                           &
     &     (NP_dest, comm_dest%num_neib, comm_dest%id_neib,             &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export,             &
     &      SR_sig, SR_r, X_dest(1))
      end if
!
      end subroutine interpolate_mod_1
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_mod_3                                      &
     &         (iflag_recv, comm_dest, tbl_org, tbl_dest, itp_mat,      &
     &          PEsmpTOT, NP_org, NP_dest, X_org,                       &
     &          SR_sig, SR_r, X_dest)
!
      use interpolate_vector_1pe
      use calypso_SR_3
      use solver_SR_3
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table_org), intent(in) ::  tbl_org
      type(interpolate_table_dest), intent(in) :: tbl_dest
      type(CRS_SMP_CONNECT_MATRIX), intent(in) :: itp_mat
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(3*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(3*NP_dest)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call verifty_work_4_itp_field                                     &
     &   (ithree, tbl_org%ntot_table_org, tbl_org, itp_WORK)
!
!    interpolation
!
      if (tbl_org%num_dest_domain.gt.0) then
        call itp_matvec_vector(PEsmpTOT, NP_org, X_org(1),              &
     &      itp_mat%NC, itp_mat%NCM, itp_mat%INM, itp_mat%IAM,          &
     &      itp_mat%AM, itp_mat%NUM_SUM(4), itp_mat%IEND_SUM_smp,       &
     &      itp_WORK%x_inter_org)
      end if
!
!     communication
!
      call calypso_send_recv_3(iflag_recv,                              &
     &    tbl_org%ntot_table_org, NP_dest, tbl_org%num_dest_domain,     &
     &    tbl_org%iflag_self_itp_send, tbl_org%id_dest_domain,          &
     &    tbl_org%istack_nod_tbl_org, tbl_org%inod_itp_send,            &
     &    tbl_dest%num_org_domain, tbl_dest%iflag_self_itp_recv,        &
     &    tbl_dest%id_org_domain, tbl_dest%istack_nod_tbl_dest,         &
     &    tbl_dest%inod_dest_4_dest, tbl_dest%irev_dest_4_dest,         &
     &    SR_sig, SR_r, itp_WORK%x_inter_org, X_dest(1) )
!
!
      if (comm_dest%num_neib.gt.0) then
        call SOLVER_SEND_RECV_3                                         &
     &     (NP_dest, comm_dest%num_neib, comm_dest%id_neib,             &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export,             &
     &      SR_sig, SR_r, X_dest(1))
      end if
!
      end subroutine interpolate_mod_3
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_mod_6                                      &
     &         (iflag_recv, comm_dest, tbl_org, tbl_dest, itp_mat,      &
     &          PEsmpTOT, NP_org, NP_dest, X_org,                       &
     &          SR_sig, SR_r, X_dest)
!
      use interpolate_tensor_1pe
      use calypso_SR_6
      use solver_SR_6
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table_org), intent(in) ::  tbl_org
      type(interpolate_table_dest), intent(in) :: tbl_dest
      type(CRS_SMP_CONNECT_MATRIX), intent(in) :: itp_mat
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(6*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(6*NP_dest)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call verifty_work_4_itp_field                                     &
     &   (isix, tbl_org%ntot_table_org, tbl_org, itp_WORK)
!
!
      if (tbl_org%num_dest_domain.gt.0) then
        call itp_matvec_tensor(PEsmpTOT, NP_org, X_org(1),              &
     &      itp_mat%NC, itp_mat%NCM, itp_mat%INM, itp_mat%IAM,          &
     &      itp_mat%AM, itp_mat%NUM_SUM(4), itp_mat%IEND_SUM_smp,       &
     &      itp_WORK%x_inter_org)
      end if
!
      call calypso_send_recv_6(iflag_recv,                              &
     &    tbl_org%ntot_table_org, NP_dest, tbl_org%num_dest_domain,     &
     &    tbl_org%iflag_self_itp_send, tbl_org%id_dest_domain,          &
     &    tbl_org%istack_nod_tbl_org, tbl_org%inod_itp_send,            &
     &    tbl_dest%num_org_domain, tbl_dest%iflag_self_itp_recv,        &
     &    tbl_dest%id_org_domain, tbl_dest%istack_nod_tbl_dest,         &
     &    tbl_dest%inod_dest_4_dest, tbl_dest%irev_dest_4_dest,         &
     &    SR_sig, SR_r, itp_WORK%x_inter_org, X_dest(1) )
!
      if (comm_dest%num_neib.gt.0) then
        call SOLVER_SEND_RECV_6                                         &
     &     (NP_dest, comm_dest%num_neib, comm_dest%id_neib,             &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export,             &
     &      SR_sig, SR_r, X_dest(1))
      end if
!
      end subroutine interpolate_mod_6
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_mod_N                                      &
     &         (iflag_recv, comm_dest, tbl_org, tbl_dest, itp_mat,      &
     &          PEsmpTOT, NP_org, NP_dest, NB, X_org,                   &
     &          SR_sig, SR_r, X_dest)
!
      use solver_SR_N
      use interpolate_fields_1pe
      use calypso_SR_N
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table_org), intent(in) ::  tbl_org
      type(interpolate_table_dest), intent(in) :: tbl_dest
      type(CRS_SMP_CONNECT_MATRIX), intent(in) :: itp_mat
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NB, NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(NB*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(NB*NP_dest)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 8-byte real
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call verifty_work_4_itp_field                                     &
     &   (NB, tbl_org%ntot_table_org, tbl_org, itp_WORK)
!
      if (tbl_org%num_dest_domain.gt.0) then
        call itp_matvec_fields(PEsmpTOT, NP_org, NB, X_org(1),          &
     &      itp_mat%NC, itp_mat%NCM, itp_mat%INM, itp_mat%IAM,          &
     &      itp_mat%AM, itp_mat%NUM_SUM(4), itp_mat%IEND_SUM_smp,       &
     &      itp_WORK%x_inter_org)
      end if
!
      call calypso_send_recv_N(iflag_recv, NB,                          &
     &    tbl_org%ntot_table_org, NP_dest, tbl_org%num_dest_domain,     &
     &    tbl_org%iflag_self_itp_send, tbl_org%id_dest_domain,          &
     &    tbl_org%istack_nod_tbl_org, tbl_org%inod_itp_send,            &
     &    tbl_dest%num_org_domain, tbl_dest%iflag_self_itp_recv,        &
     &    tbl_dest%id_org_domain, tbl_dest%istack_nod_tbl_dest,         &
     &    tbl_dest%inod_dest_4_dest, tbl_dest%irev_dest_4_dest,         &
     &    SR_sig, SR_r, itp_WORK%x_inter_org, X_dest(1))
!
      if (comm_dest%num_neib.gt.0) then
        call SOLVER_SEND_RECV_N                                         &
     &     (NP_dest, NB, comm_dest%num_neib, comm_dest%id_neib,         &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export,             &
     &      SR_sig, SR_r, X_dest(1))
      end if
!
      end subroutine interpolate_mod_N
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_integer                                  &
     &        (iflag_recv, ele_org, comm_dest, tbl_org, tbl_dest,       &
     &         PEsmpTOT, NP_org, NP_dest, i_vector_org,                 &
     &         SR_sig, SR_i, i_vector_dest)
!
      use t_solver_SR_int
      use t_geometry_data
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
!
      use interpolate_imark_1pe
      use calypso_SR_int
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(element_data), intent(in) :: ele_org
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table_org), intent(in) ::  tbl_org
      type(interpolate_table_dest), intent(in) :: tbl_dest
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      integer(kind = kint), intent(in) :: i_vector_org(NP_org)
!
      integer(kind = kint), intent(inout) :: i_vector_dest(NP_dest)
!
!>      Structure of communication flags
      type(send_recv_status), intent(inout) :: SR_sig
!>      Structure of communication buffer for 4-byte integer
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
!>      Structure for vectors for solver
      type(vectors_4_solver) :: v_1st_sol, v_2nd_sol
!
!     initialize
!
      call alloc_iccg_int_vector(NP_org,  v_1st_sol)
      call alloc_iccg_int_vector(NP_dest, v_2nd_sol)
!
      call verifty_work_4_itp_int                                       &
     &   (tbl_org%ntot_table_org, tbl_org, itp_WORK)
!
!
!$omp parallel workshare
      v_1st_sol%ix_vec(1:NP_org) = i_vector_org(1:NP_org)
!$omp end parallel workshare
!
!    interpolation
!
      if (tbl_org%num_dest_domain.gt.0) then
        call s_interporate_imark_para(PEsmpTOT, NP_org,                 &
     &      ele_org%numele, ele_org%nnod_4_ele, ele_org%ie,             &
     &      v_1st_sol%ix_vec(1), tbl_org%istack_tbl_type_org_smp,       &
     &      tbl_org%ntot_table_org, tbl_org%iele_org_4_org,             &
     &      tbl_org%itype_inter_org, itp_WORK%i_inter_org)
      end if
!
!     communication
!
      call calypso_send_recv_int(iflag_recv,                            &
     &    tbl_org%ntot_table_org, NP_dest, tbl_org%num_dest_domain,     &
     &    tbl_org%iflag_self_itp_send, tbl_org%id_dest_domain,          &
     &    tbl_org%istack_nod_tbl_org, tbl_org%inod_itp_send,            &
     &    tbl_dest%num_org_domain, tbl_dest%iflag_self_itp_recv,        &
     &    tbl_dest%id_org_domain, tbl_dest%istack_nod_tbl_dest,         &
     &    tbl_dest%inod_dest_4_dest, tbl_dest%irev_dest_4_dest,         &
     &    SR_sig, SR_i, itp_WORK%i_inter_org, v_2nd_sol%ix_vec(1))
!
!
      if (comm_dest%num_neib.gt.0) then
        call SOLVER_SEND_RECV_int_type(NP_dest, comm_dest,              &
     &                                 v_2nd_sol%ix_vec(1))
      end if
!
!$omp parallel workshare
      i_vector_dest(1:NP_dest) = v_2nd_sol%ix_vec(1:NP_dest)
!$omp end parallel workshare
!
      call dealloc_iccg_int_vector(v_1st_sol)
      call dealloc_iccg_int_vector(v_2nd_sol)
!
      end subroutine s_interpolate_integer
!
! ----------------------------------------------------------------------
!
      end module interpolate_by_module
