!
!     module interpolate_position
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interpolate_position
!      subroutine s_interpolate_position_by_N
!      subroutine s_interpolate_position_by_s
!      subroutine s_interpolate_global_node
!
      module interpolate_position
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      implicit none
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
      use m_interpolated_geometry
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
!
      use m_array_for_send_recv
      use m_work_4_interpolation
!
      use interpolate_1pe
      use select_calypso_SR
      use solver_SR_3
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(ithree, numnod)
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
        call interporate_vector_para(np_smp, numnod, numele,            &
     &      nnod_4_ele, ie, x_vec(1), istack_table_type_org_smp,        &
     &      ntot_table_org, iele_org_4_org, itype_inter_org,            &
     &      coef_inter_org, x_inter_org(1) )
      end if
!
!     communication
!
      if (iflag_debug.eq.1)  write(*,*) 'sel_calypso_send_recv_3'
      call sel_calypso_send_recv_3                                      &
     &          (iflag_import_item, ntot_table_org, nnod_2nd,           &
     &           num_dest_domain, iflag_self_itp_send,                  &
     &           id_dest_domain, istack_nod_tbl_org, inod_itp_send,     &
     &           num_org_domain, iflag_self_itp_recv,                   &
     &           id_org_domain, istack_nod_tbl_dest,                    &
     &           inod_dest_4_dest, irev_dest_4_dest,                    &
     &           x_inter_org(1), xvec_2nd(1) )
!
!
!
      if (num_neib_2.gt.0) then
        call SOLVER_SEND_RECV_3                                         &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2, xvec_2nd(1) )
      end if
!
      do inod = 1, nnod_2nd
        xx_interpolate(inod,1) = xvec_2nd(3*inod-2)
        xx_interpolate(inod,2) = xvec_2nd(3*inod-1)
        xx_interpolate(inod,3) = xvec_2nd(3*inod  )
      end do
!
      end subroutine s_interpolate_position
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_position_by_N
!
      use m_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_pallalel_vector
      use m_2nd_nod_comm_table
      use m_2nd_geometry_param
      use m_interpolated_geometry
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
!
      use m_array_for_send_recv
      use m_work_4_interpolation
!
      use interpolate_1pe
      use select_calypso_SR
      use solver_SR_3
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(ithree, numnod)
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
        call interporate_vector_para(np_smp, numnod, numele,            &
     &      nnod_4_ele, ie, x_vec(1), istack_table_type_org_smp,        &
     &      ntot_table_org, iele_org_4_org, itype_inter_org,            &
     &      coef_inter_org, x_inter_org(1) )
      end if
!
!     communication
!
      if (iflag_debug.eq.1)  write(*,*) 'sel_calypso_send_recv_N'
      call sel_calypso_send_recv_N                                      &
     &          (iflag_import_item, ithree, ntot_table_org, nnod_2nd,   &
     &           num_dest_domain, iflag_self_itp_send,                  &
     &           id_dest_domain, istack_nod_tbl_org, inod_itp_send,     &
     &           num_org_domain, iflag_self_itp_recv,                   &
     &           id_org_domain, istack_nod_tbl_dest,                    &
     &           inod_dest_4_dest, irev_dest_4_dest,                    &
     &           x_inter_org(1), xvec_2nd(1) )
!
!
!
      if (num_neib_2.gt.0) then
        call SOLVER_SEND_RECV_3                                         &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2, xvec_2nd(1) )
      end if
!
      do inod = 1, nnod_2nd
        xx_interpolate(inod,1) = xvec_2nd(3*inod-2)
        xx_interpolate(inod,2) = xvec_2nd(3*inod-1)
        xx_interpolate(inod,3) = xvec_2nd(3*inod  )
      end do
!
      end subroutine s_interpolate_position_by_N
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_position_by_s
!
      use m_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_pallalel_vector
      use m_2nd_nod_comm_table
      use m_2nd_geometry_param
      use m_interpolated_geometry
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
      use m_interpolate_matrix
!
      use m_array_for_send_recv
      use m_work_4_interpolation
!
      use interpolate_1pe
      use matvec_by_djo
      use select_calypso_SR
      use solver_SR
!
      integer(kind = kint) :: inod, nd
!
!     initialize
!
      call verify_vector_for_solver(ione, numnod)
      call verify_2nd_iccg_matrix(ione, nnod_2nd)
!
      call verifty_work_4_itp_field(ione,ntot_table_org)
!
!
      do nd = 1, 3
        do inod = 1, numnod
          x_vec(inod  ) = xx(inod,nd)
        end do
!
!    interpolation
!
        if (num_dest_domain.gt.0) then
          if (iflag_debug.eq.1)  write(*,*) 'interporate_scalar_para'
          call interporate_scalar_para(np_smp, numnod, numele,         &
     &      nnod_4_ele, ie, x_vec(1), istack_table_type_org_smp,       &
     &      ntot_table_org, iele_org_4_org, itype_inter_org,           &
     &      coef_inter_org, x_inter_org(1) )
!          if (iflag_debug.eq.1)  write(*,*) 'matvec_djo_11'
!          call matvec_djo_11(numnod, NC_itp, NCM_itp, INM_itp,         &
!     &      IAM_itp, AM_itp, x_vec(1), x_inter_org(1),                 &
!     &      NUM_NCOMP_itp, INOD_itp_mat, NUM_SUM_itp,                  &
!     &      np_smp, IEND_SUM_itp_smp)
        end if
!
!     communication
!
        if (iflag_debug.eq.1)  write(*,*) 'sel_calypso_send_recv'
        call sel_calypso_send_recv                                      &
     &          (iflag_import_item, ntot_table_org, nnod_2nd,           &
     &           num_dest_domain, iflag_self_itp_send,                  &
     &           id_dest_domain, istack_nod_tbl_org, inod_itp_send,     &
     &           num_org_domain, iflag_self_itp_recv,                   &
     &           id_org_domain, istack_nod_tbl_dest,                    &
     &           inod_dest_4_dest, irev_dest_4_dest,                    &
     &           x_inter_org(1), xvec_2nd(1) )
!
!
!
        if (num_neib_2.gt.0) then
          call SOLVER_SEND_RECV                                         &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2, xvec_2nd(1) )
        end if
!
        do inod = 1, nnod_2nd
          xx_interpolate(inod,nd) = xvec_2nd(inod  )
        end do
      end do
!
      end subroutine s_interpolate_position_by_s
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
      use select_calypso_SR
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
      if (iflag_debug.eq.1) write(*,*) 'sel_calypso_send_recv_int'
      call sel_calypso_send_recv_int                                    &
     &          (iflag_import_item, ntot_table_org, nnod_2nd,           &
     &           num_dest_domain, iflag_self_itp_send,                  &
     &           id_dest_domain, istack_nod_tbl_org, inod_itp_send,     &
     &           num_org_domain, iflag_self_itp_recv,                   &
     &           id_org_domain, istack_nod_tbl_dest,                    &
     &           inod_dest_4_dest, irev_dest_4_dest,                    &
     &           inod_gl_dest_4_org, ivec_2nd(1) )
!
!
      if (iflag_debug.eq.1)  write(*,*) 'solver_send_recv_i'
      if (num_neib_2.gt.0) then
        call solver_send_recv_i                                         &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2,                  &
     &                 ivec_2nd(1))
      end if
!
      inod_global_itp(1:nnod_2nd) = ivec_2nd(1:nnod_2nd)
!
      end subroutine s_interpolate_global_node
!
! ----------------------------------------------------------------------
!
      end module interpolate_position
