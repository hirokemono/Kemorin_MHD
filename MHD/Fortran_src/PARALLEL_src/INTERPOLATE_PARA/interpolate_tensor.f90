!
!     module interpolate_tensor
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interpolate_tensor(i_dest, i_origin)
!
      module interpolate_tensor
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
      subroutine s_interpolate_tensor(i_dest, i_origin)
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
      use interpolate_parallel
      use select_calypso_SR
      use solver_SR_6
!
!
      integer(kind = kint), intent(in) :: i_dest, i_origin
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(n_sym_tensor, numnod)
      call verify_2nd_iccg_matrix(n_sym_tensor, nnod_2nd)
!
      call verifty_work_4_itp_field(n_sym_tensor, ntot_table_org)
!
!
!      write(*,*) 'x_vec', numnod, size(x_vec)
!$omp parallel do
      do inod = 1, numnod
        x_vec(6*inod-5) = d_nod(inod,i_origin  )
        x_vec(6*inod-4) = d_nod(inod,i_origin+1)
        x_vec(6*inod-3) = d_nod(inod,i_origin+2)
        x_vec(6*inod-2) = d_nod(inod,i_origin+3)
        x_vec(6*inod-1) = d_nod(inod,i_origin+4)
        x_vec(6*inod  ) = d_nod(inod,i_origin+5)
      end do
!$omp end parallel do
!
!    interpolation
!
      if (num_dest_domain.gt.0) then
        call s_interporate_tensor_para(np_smp, numnod, numele,          &
     &    nnod_4_ele, ie, x_vec(1), num_dest_domain,                    &
     &    istack_table_wtype_org_smp, ntot_table_org, iele_org_4_org,   &
     &    itype_inter_org, coef_inter_org, x_inter_org(1) )
      end if
!
!     communication
!
      call sel_calypso_send_recv_6                                      &
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
        call SOLVER_SEND_RECV_6                                         &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2, xvec_2nd(1) )
      end if
!
!$omp parallel do
      do inod = 1, nnod_2nd
        d_nod_2nd(inod,i_dest  ) = xvec_2nd(6*inod-5)
        d_nod_2nd(inod,i_dest+1) = xvec_2nd(6*inod-4)
        d_nod_2nd(inod,i_dest+2) = xvec_2nd(6*inod-3)
        d_nod_2nd(inod,i_dest+3) = xvec_2nd(6*inod-2)
        d_nod_2nd(inod,i_dest+4) = xvec_2nd(6*inod-1)
        d_nod_2nd(inod,i_dest+5) = xvec_2nd(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_tensor
!
! ----------------------------------------------------------------------
!
      end module interpolate_tensor
