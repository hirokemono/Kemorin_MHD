!
!     module interpolate_fields
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interpolate_fields(i_dest, i_origin)
!
      module interpolate_fields
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
      subroutine s_interpolate_fields(numdir, i_dest, i_origin)
!
      use calypso_mpi
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
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
      use interpolate_parallel
      use select_calypso_SR
      use solver_SR_N
!
!
      integer(kind = kint), intent(in) :: numdir, i_dest, i_origin
!
      integer(kind = kint) :: inod, nd
!
!     initialize
!
      call verify_vector_for_solver(numdir, numnod)
      call verify_2nd_iccg_matrix(numdir, nnod_2nd)
!
      call verifty_work_4_itp_field(numdir,ntot_table_org)
!
!
!$omp parallel private(inod)
      do nd = 1, numdir
!$omp do
        do inod = 1, numnod
          x_vec(numdir*(inod-1)+nd) = d_nod(inod,i_origin+nd-1)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!    interpolation
!
      if (num_dest_domain.gt.0) then
        call s_interporate_fields_para(np_smp, numnod, numele,          &
     &    nnod_4_ele, ie, numdir, x_vec(1), num_dest_domain,            &
     &    istack_table_wtype_org_smp, ntot_table_org, iele_org_4_org,   &
     &    itype_inter_org, coef_inter_org, x_inter_org(1) )
      end if
!
!     communication
!
      call sel_calypso_send_recv_N                                      &
     &          (iflag_import_item, numdir, ntot_table_org, nnod_2nd,   &
     &           num_dest_domain, iflag_self_itp_send,                  &
     &           id_dest_domain, istack_nod_tbl_org, inod_itp_send,     &
     &           num_org_domain, iflag_self_itp_recv,                   &
     &           id_org_domain, istack_nod_tbl_dest,                    &
     &           inod_dest_4_dest, irev_dest_4_dest,                    &
     &           x_inter_org(1), xvec_2nd(1) )
!
!
      if (num_neib_2.gt.0) then
        call SOLVER_SEND_RECV_N                                         &
     &                (nnod_2nd, numdir, num_neib_2, id_neib_2,         &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2, xvec_2nd(1))
      end if
!
!$omp parallel private(inod)
      do nd = 1, numdir
!$omp do
        do inod = 1, nnod_2nd
          d_nod_2nd(inod,i_dest+nd-1) = xvec_2nd(numdir*(inod-1)+nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine s_interpolate_fields
!
! ----------------------------------------------------------------------
!
      end module interpolate_fields
