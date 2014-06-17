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
      use m_interpolate_matrix
!
      use m_array_for_send_recv
      use m_work_4_interpolation
!
      use interpolate_by_module
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(ithree, numnod)
      call verify_2nd_iccg_matrix(ithree, nnod_2nd)
!
!
      do inod = 1, numnod
        x_vec(3*inod-2) = xx(inod,1)
        x_vec(3*inod-1) = xx(inod,2)
        x_vec(3*inod  ) = xx(inod,3)
      end do
!
      call interpolate_mod_3(numnod, nnod_2nd, x_vec, xvec_2nd)
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
      use m_interpolate_matrix
!
      use interpolate_by_module
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(ithree, numnod)
      call verify_2nd_iccg_matrix(ithree, nnod_2nd)
!
!
      do inod = 1, numnod
        x_vec(3*inod-2) = xx(inod,1)
        x_vec(3*inod-1) = xx(inod,2)
        x_vec(3*inod  ) = xx(inod,3)
      end do
!
      call interpolate_mod_N(numnod, nnod_2nd, ithree, x_vec, xvec_2nd)
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
      use m_interpolate_matrix
!
      use interpolate_by_module
      use matvec_by_djo
!
      integer(kind = kint) :: inod, nd
!
!     initialize
!
      call verify_vector_for_solver(ione, numnod)
      call verify_2nd_iccg_matrix(ione, nnod_2nd)
!
!
      do nd = 1, 3
        do inod = 1, numnod
          x_vec(inod  ) = xx(inod,nd)
        end do
!
        call interpolate_mod_1(numnod, nnod_2nd, x_vec, xvec_2nd)
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
      subroutine s_interpolate_global_node(NP_dest)
!
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
!
      use m_interpolated_geometry
      use m_2nd_pallalel_vector
      use m_2nd_nod_comm_table
!
      use select_calypso_SR
      use solver_SR_int
!
      integer(kind = kint), intent(in) :: NP_dest
!
      call verify_2nd_iccg_int_mat(NP_dest)
!
!     communication
!
!
      if (iflag_debug.eq.1) write(*,*) 'sel_calypso_send_recv_int'
      call sel_calypso_send_recv_int                                    &
     &          (iflag_import_item, ntot_table_org, NP_dest,            &
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
     &                (NP_dest, num_neib_2, id_neib_2,                  &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2,                  &
     &                 ivec_2nd(1))
      end if
!
      inod_global_itp(1:NP_dest) = ivec_2nd(1:NP_dest)
!
      end subroutine s_interpolate_global_node
!
! ----------------------------------------------------------------------
!
      end module interpolate_position
