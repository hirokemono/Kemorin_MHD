!
!     module interpolate_position
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interpolate_position(node, NP_dest, comm_dest)
!      subroutine s_interpolate_position_by_N(node, NP_dest, comm_dest)
!      subroutine s_interpolate_position_by_s(node, NP_dest, comm_dest)
!!      subroutine s_interpolate_global_node                            &
!!     &         (NP_dest, comm_dest, itp_org, itp_dest)
!
      module interpolate_position
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_comm_table
      use t_geometry_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_position(node, NP_dest, comm_dest)
!
      use m_constants
      use m_2nd_pallalel_vector
      use m_interpolated_geometry
!
      use m_array_for_send_recv
!
      use interpolate_by_module
!
      type(node_data), intent(inout) :: node
      integer(kind = kint), intent(in) :: NP_dest
      type(communication_table), intent(in) :: comm_dest
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(ithree, node%numnod)
      call verify_2nd_iccg_matrix(ithree, NP_dest)
!
!
      do inod = 1, node%numnod
        x_vec(3*inod-2) = node%xx(inod,1)
        x_vec(3*inod-1) = node%xx(inod,2)
        x_vec(3*inod  ) = node%xx(inod,3)
      end do
!
      call interpolate_mod_3(comm_dest, node%numnod, NP_dest,           &
     &    x_vec(1), xvec_2nd(1))
!
      do inod = 1, NP_dest
        xx_interpolate(inod,1) = xvec_2nd(3*inod-2)
        xx_interpolate(inod,2) = xvec_2nd(3*inod-1)
        xx_interpolate(inod,3) = xvec_2nd(3*inod  )
      end do
!
      end subroutine s_interpolate_position
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_position_by_N(node, NP_dest, comm_dest)
!
      use m_constants
      use m_2nd_pallalel_vector
      use m_interpolated_geometry
!
      use m_array_for_send_recv
!
      use interpolate_by_module
!
      type(node_data), intent(inout) :: node
      integer(kind = kint), intent(in) :: NP_dest
      type(communication_table), intent(in) :: comm_dest
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(ithree, node%numnod)
      call verify_2nd_iccg_matrix(ithree, NP_dest)
!
!
      do inod = 1, node%numnod
        x_vec(3*inod-2) = node%xx(inod,1)
        x_vec(3*inod-1) = node%xx(inod,2)
        x_vec(3*inod  ) = node%xx(inod,3)
      end do
!
      call interpolate_mod_N(comm_dest, node%numnod, NP_dest, ithree,   &
     &    x_vec(1), xvec_2nd(1))
!
      do inod = 1, NP_dest
        xx_interpolate(inod,1) = xvec_2nd(3*inod-2)
        xx_interpolate(inod,2) = xvec_2nd(3*inod-1)
        xx_interpolate(inod,3) = xvec_2nd(3*inod  )
      end do
!
      end subroutine s_interpolate_position_by_N
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_position_by_s(node, NP_dest, comm_dest)
!
      use m_constants
      use m_2nd_pallalel_vector
      use m_interpolated_geometry
      use m_interpolate_table
!
      use m_array_for_send_recv
!
      use interpolate_by_module
      use matvec_by_djo
!
      type(node_data), intent(inout) :: node
      integer(kind = kint), intent(in) :: NP_dest
      type(communication_table), intent(in) :: comm_dest
!
      integer(kind = kint) :: inod, nd
!
!     initialize
!
      call verify_vector_for_solver(ione, node%numnod)
      call verify_2nd_iccg_matrix(ione, NP_dest)
!
!
      do nd = 1, 3
        do inod = 1, node%numnod
          x_vec(inod  ) = node%xx(inod,nd)
        end do
!
        call interpolate_mod_1(comm_dest, node%numnod, NP_dest,         &
     &      x_vec(1), xvec_2nd(1))
!
        do inod = 1, NP_dest
          xx_interpolate(inod,nd) = xvec_2nd(inod  )
        end do
      end do
!
      end subroutine s_interpolate_position_by_s
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_global_node                              &
     &         (NP_dest, comm_dest, itp_org, itp_dest)
!
      use m_solver_SR
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
!
      use m_interpolated_geometry
      use m_2nd_pallalel_vector
!
      use calypso_SR_int
      use solver_SR_int
      use select_copy_from_recv
!
      integer(kind = kint), intent(in) :: NP_dest
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table_org), intent(in) :: itp_org
      type(interpolate_table_dest), intent(in) :: itp_dest
!
      call verify_2nd_iccg_int_mat(NP_dest)
!
!     communication
!
!
      if (iflag_debug.eq.1) write(*,*) 'calypso_send_recv_int'
      call calypso_send_recv_int                                        &
     &   (iflag_import_item, itp_org%ntot_table_org, NP_dest,           &
     &    itp_org%num_dest_domain, itp_org%iflag_self_itp_send,         &
     &    itp_org%id_dest_domain, itp_org%istack_nod_tbl_org,           &
     &    itp_org%inod_itp_send,                                        &
     &    itp_dest%num_org_domain, itp_dest%iflag_self_itp_recv,        &
     &    itp_dest%id_org_domain, itp_dest%istack_nod_tbl_dest,         &
     &    itp_dest%inod_dest_4_dest, itp_dest%irev_dest_4_dest,         &
     &    itp_org%inod_gl_dest_4_org, ivec_2nd(1) )
!
!
      if (iflag_debug.eq.1)  write(*,*) 'solver_send_recv_i'
      if (comm_dest%num_neib.gt.0) then
        call solver_send_recv_i                                         &
     &                (NP_dest, comm_dest%num_neib, comm_dest%id_neib,  &
     &                 comm_dest%istack_import, comm_dest%item_import,  &
     &                 comm_dest%istack_export, comm_dest%item_export,  &
     &                 ivec_2nd(1) )
      end if
!
      inod_global_itp(1:NP_dest) = ivec_2nd(1:NP_dest)
!
      end subroutine s_interpolate_global_node
!
! ----------------------------------------------------------------------
!
      end module interpolate_position
