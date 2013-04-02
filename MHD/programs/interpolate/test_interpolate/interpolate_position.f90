!
!     module interpolate_position
!
      module interpolate_position
!
!     Written by H. Matsui on Sep., 2006
!
      use m_precision
!
      use m_parallel_var_dof
      use m_machine_parameter
!
      implicit none
!
!      subroutine s_interpolate_position
!      subroutine s_interpolate_global_node
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_position
!
      use m_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_pallalel_vector
      use m_2nd_nod_comm_table
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_interpolated_geometry
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
!
      use m_work_4_interpolation
!
      use interpolate_parallel
      use interpolate_SR_3
      use solver_SR_3
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_iccgN_matrix(ithree, numnod)
      call verify_2nd_iccg_matrix(ithree, nnod_2nd)
!
      call verifty_work_4_itp_field(ithree,ntot_table_org)
!
!
      do inod = 1, numnod
        x_vec(3*inod-2) = xx(inod,1)
        x_vec(3*inod-1) = xx(inod,2)
        x_vec(3*inod  ) = xx(inod,3)
      end do
!
!    interpolation
!
      if (num_dest_domain.gt.0) then
        call s_interporate_vector_para(np_smp, numnod, numele,          &
     &    nnod_4_ele, ie, x_vec(1), num_dest_domain,                    &
     &    istack_table_wtype_org_smp, ntot_table_org, iele_org_4_org,   &
     &    itype_inter_org, coef_inter_org, x_inter_org(1) )
      end if
!
!     communication
!
      call interpolate_send_recv_3                                      &
     &          (num_dest_domain, iflag_self_itp_send, ntot_table_org,  &
     &           id_dest_domain, istack_nod_table_org,                  &
     &           num_org_domain, iflag_self_itp_recv, ntot_table_dest,  &
     &           id_org_domain, istack_nod_table_dest,                  &
     &           inod_dest_4_dest, ntot_table_org, x_inter_org(1),      &
     &           nnod_2nd, xvec_2nd(1), SOLVER_COMM, my_rank)
!
!
      if (num_neib_2.gt.0) then
        call SOLVER_SEND_RECV_3                                         &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2,                  &
     &                 xvec_2nd(1), SOLVER_COMM, my_rank)
      end if
!
      do inod = 1, nnod_2nd
        xx_interpolate(inod,1) = xvec_2nd(3*inod-2) - xx_2nd(inod,1)
        xx_interpolate(inod,2) = xvec_2nd(3*inod-1) - xx_2nd(inod,2)
        xx_interpolate(inod,3) = xvec_2nd(3*inod  ) - xx_2nd(inod,3)
      end do
!
      end subroutine s_interpolate_position
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_global_node
!
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
!
      use m_interpolated_geometry
      use m_2nd_pallalel_vector
      use m_2nd_nod_comm_table
      use m_2nd_geometry_param
!
      use interpolate_SR_int
      use solver_SR_int
!
!     initialize
!
!
      call verify_2nd_iccg_int_mat(nnod_2nd)
!
!     communication
!
!
      if (iflag_debug.eq.1) write(*,*) 'interpolate_send_recv_int'
      call interpolate_send_recv_int                                    &
     &          (num_dest_domain, iflag_self_itp_send, ntot_table_org,  &
     &           id_dest_domain, istack_nod_table_org,                  &
     &           num_org_domain, iflag_self_itp_recv, ntot_table_dest,  &
     &           id_org_domain, istack_nod_table_dest,                  &
     &           inod_dest_4_dest, ntot_table_org, inod_gl_dest_4_org,  &
     &           nnod_2nd, ivec_2nd(1), SOLVER_COMM, my_rank)
!
!
      if (iflag_debug.eq.1)  write(*,*) 'solver_send_recv_i'
      if (num_neib_2.gt.0) then
        call solver_send_recv_i                                         &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2,                  &
     &                 ivec_2nd(1), SOLVER_COMM, my_rank)
      end if
!
      inod_global_itp(1:nnod_2nd) = ivec_2nd(1:nnod_2nd)
!
      end subroutine s_interpolate_global_node
!
! ----------------------------------------------------------------------
!
      end module interpolate_position
