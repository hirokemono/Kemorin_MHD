!>@file   interpolate_scalar
!!@brief  module interpolate_scalar
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  interpolation for scalar data
!!
!!@verbatim
!!      subroutine s_interpolate_scalar(i_dest, i_origin)
!!      subroutine interpolate_by_matrix_1(i_dest, i_origin)
!!@endverbatim
!
      module interpolate_scalar
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_scalar(i_dest, i_origin)
!
      use calypso_mpi
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_data
      use m_2nd_pallalel_vector
      use m_2nd_nod_comm_table
      use m_2nd_geometry_param
      use m_2nd_phys_data
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
!
      use m_array_for_send_recv
      use m_work_4_interpolation
!
      use interpolate_1pe
      use select_calypso_SR
      use solver_SR
!
!
      integer(kind = kint), intent(in) :: i_dest, i_origin
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(n_scalar, numnod)
      call verify_2nd_iccg_matrix(n_scalar, nnod_2nd)
!
      call verifty_work_4_itp_field(n_scalar, ntot_table_org)
!
!
!$omp parallel do
      do inod = 1, numnod
        x_vec(inod) = d_nod(inod, i_origin)
      end do
!$omp end parallel do
!
!    interpolation
!
      if (num_dest_domain.gt.0) then
        call interporate_scalar_para(np_smp, numnod, numele,            &
     &    nnod_4_ele, ie, x_vec(1), istack_table_wtype_org_smp,         &
     &    ntot_table_org, iele_org_4_org, itype_inter_org,              &
     &    coef_inter_org, x_inter_org(1) )
      end if
!
!
!     communication
!
      call sel_calypso_send_recv                                        &
     &          (iflag_import_item, ntot_table_org, nnod_2nd,           &
     &           num_dest_domain, iflag_self_itp_send,                  &
     &           id_dest_domain, istack_nod_tbl_org, inod_itp_send,     &
     &           num_org_domain, iflag_self_itp_recv,                   &
     &           id_org_domain, istack_nod_tbl_dest,                    &
     &           inod_dest_4_dest, irev_dest_4_dest,                    &
     &           x_inter_org(1), xvec_2nd(1) )
!
!
      if (num_neib_2.gt.0) then
        call SOLVER_SEND_RECV                                           &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2, xvec_2nd(1) )
      end if
!
!$omp parallel do
      do inod = 1, nnod_2nd
        d_nod_2nd(inod,i_dest) = xvec_2nd(inod)
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_scalar
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_by_matrix_1(i_dest, i_origin)
!
      use calypso_mpi
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_phys_constants
      use m_node_phys_data
      use m_2nd_pallalel_vector
      use m_2nd_nod_comm_table
      use m_2nd_geometry_param
      use m_2nd_phys_data
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
      use m_interpolate_matrix
!
      use m_array_for_send_recv
      use m_work_4_interpolation
!
      use matvec_by_djo
      use select_calypso_SR
      use solver_SR
!
!
      integer(kind = kint), intent(in) :: i_dest, i_origin
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(n_scalar, numnod)
      call verify_2nd_iccg_matrix(n_scalar, nnod_2nd)
!
      call verifty_work_4_itp_field(n_scalar, ntot_table_org)
!
!
!$omp parallel do
      do inod = 1, numnod
        x_vec(inod) = d_nod(inod, i_origin)
      end do
!$omp end parallel do
!
!    interpolation
!
      if (num_dest_domain.gt.0) then
        call matvec_djo_11(numnod, NC_itp, NCM_itp, INM_itp,            &
     &      IAM_itp, AM_itp, x_vec(1), x_inter_org(1),                  &
     &      NUM_NCOMP_itp, INOD_itp_mat, NUM_SUM_itp,                   &
     &      np_smp, IEND_SUM_itp_smp)
      end if
!
!
!     communication
!
      call sel_calypso_send_recv                                        &
     &          (iflag_import_item, ntot_table_org, nnod_2nd,           &
     &           num_dest_domain, iflag_self_itp_send,                  &
     &           id_dest_domain, istack_nod_tbl_org, inod_itp_send,     &
     &           num_org_domain, iflag_self_itp_recv,                   &
     &           id_org_domain, istack_nod_tbl_dest,                    &
     &           inod_dest_4_dest, irev_dest_4_dest,                    &
     &           x_inter_org(1), xvec_2nd(1) )
!
!
      if (num_neib_2.gt.0) then
        call SOLVER_SEND_RECV                                           &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2, xvec_2nd(1) )
      end if
!
!$omp parallel do
      do inod = 1, nnod_2nd
        d_nod_2nd(inod,i_dest) = xvec_2nd(inod)
      end do
!$omp end parallel do
!
      end subroutine interpolate_by_matrix_1
!
! ----------------------------------------------------------------------
!
      end module interpolate_scalar
