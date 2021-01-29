!
!     module interpolate_position
!
!     Written by H. Matsui on Sep., 2006
!
!!      subroutine s_interpolate_position(node, NP_dest, comm_dest,     &
!!     &          itp_table, xx_interpolate, vect)
!!      subroutine s_interpolate_position_by_N(node, NP_dest, comm_dest,&
!!     &          itp_table, xx_interpolate, vect)
!!      subroutine s_interpolate_position_by_S(node, NP_dest, comm_dest,&
!!     &          itp_table, xx_interpolate, vect)
!!      subroutine s_interpolate_global_node(iflag_recv,                &
!!     &        (NP_dest, comm_dest, itp_org, itp_dest, inod_global_itp)
!!        type(node_data), intent(inout) :: node
!!        type(communication_table), intent(in) :: comm_dest
!!        type(interpolate_table), intent(in) :: itp_table
!!        type(vectors_4_solver), intent(inout) :: vect
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
      use t_interpolate_table
      use t_vector_for_solver
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_position(node, NP_dest, comm_dest,       &
     &          itp_table, xx_interpolate, vect)
!
      use m_solver_SR
      use m_constants
      use m_2nd_pallalel_vector
!
      use interpolate_by_module
!
      type(node_data), intent(inout) :: node
      integer(kind = kint), intent(in) :: NP_dest
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
!
      type(vectors_4_solver), intent(inout) :: vect
      real(kind = kreal), intent(inout) :: xx_interpolate(NP_dest,3)
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_iccgN_vec_type(ithree, node%numnod, vect)
      call verify_2nd_iccg_matrix(ithree, NP_dest)
!
!
      do inod = 1, node%numnod
        vect%x_vec(3*inod-2) = node%xx(inod,1)
        vect%x_vec(3*inod-1) = node%xx(inod,2)
        vect%x_vec(3*inod  ) = node%xx(inod,3)
      end do
!
      call interpolate_mod_3(itp_table%iflag_itp_recv, comm_dest,       &
     &    itp_table%tbl_org, itp_table%tbl_dest, itp_table%mat,         &
     &    np_smp, node%numnod, NP_dest, vect%x_vec(1),                  &
     &    SR_sig1, SR_r1, xvec_2nd(1))
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
      subroutine s_interpolate_position_by_N(node, NP_dest, comm_dest,  &
     &          itp_table, xx_interpolate, vect)
!
      use m_constants
      use m_solver_SR
      use m_2nd_pallalel_vector
!
      use interpolate_by_module
!
      type(node_data), intent(inout) :: node
      integer(kind = kint), intent(in) :: NP_dest
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
!
      type(vectors_4_solver), intent(inout) :: vect
      real(kind = kreal), intent(inout) :: xx_interpolate(NP_dest,3)
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_iccgN_vec_type(ithree, node%numnod, vect)
      call verify_2nd_iccg_matrix(ithree, NP_dest)
!
!
      do inod = 1, node%numnod
        vect%x_vec(3*inod-2) = node%xx(inod,1)
        vect%x_vec(3*inod-1) = node%xx(inod,2)
        vect%x_vec(3*inod  ) = node%xx(inod,3)
      end do
!
      call interpolate_mod_N(iflag_import_mod, comm_dest,               &
     &    itp_table%tbl_org, itp_table%tbl_dest, itp_table%mat,         &
     &    np_smp, node%numnod, NP_dest, ithree, vect%x_vec(1),          &
     &    SR_sig1, SR_r1, xvec_2nd(1))
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
      subroutine s_interpolate_position_by_S(node, NP_dest, comm_dest,  &
     &          itp_table, xx_interpolate, vect)
!
      use m_constants
      use m_solver_SR
      use m_2nd_pallalel_vector
!
      use interpolate_by_module
      use matvec_by_djo
!
      type(node_data), intent(inout) :: node
      integer(kind = kint), intent(in) :: NP_dest
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
!
      type(vectors_4_solver), intent(inout) :: vect
      real(kind = kreal), intent(inout) :: xx_interpolate(NP_dest,3)
!
      integer(kind = kint) :: inod, nd
!
!     initialize
!
      call verify_iccgN_vec_type(ione, node%numnod, vect)
      call verify_2nd_iccg_matrix(ione, NP_dest)
!
!
      do nd = 1, 3
        do inod = 1, node%numnod
          vect%x_vec(inod  ) = node%xx(inod,nd)
        end do
!
        call interpolate_mod_1(itp_table%iflag_itp_recv, comm_dest,     &
     &      itp_table%tbl_org, itp_table%tbl_dest, itp_table%mat,       &
     &      np_smp, node%numnod, NP_dest, vect%x_vec(1),                &
     &      SR_sig1, SR_r1, xvec_2nd(1))
!
        do inod = 1, NP_dest
          xx_interpolate(inod,nd) = xvec_2nd(inod  )
        end do
      end do
!
      end subroutine s_interpolate_position_by_S
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_global_node(iflag_recv,                  &
     &         NP_dest, comm_dest, itp_org, itp_dest, inod_global_itp)
!
      use m_solver_SR
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
!
      use m_2nd_pallalel_vector
!
      use calypso_SR_int
      use solver_SR_type
      use select_copy_from_recv
!
      integer(kind = kint), intent(in) :: iflag_recv
      integer(kind = kint), intent(in) :: NP_dest
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table_org), intent(in) :: itp_org
      type(interpolate_table_dest), intent(in) :: itp_dest
!
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: inod_global_itp(NP_dest)
!
      call verify_2nd_iccg_int8_mat(NP_dest)
!
!     communication
!
!
      if (iflag_debug.eq.1) write(*,*) 'calypso_send_recv_int8'
      call calypso_send_recv_int8                                       &
     &   (iflag_recv, itp_org%ntot_table_org, NP_dest,                  &
     &    itp_org%num_dest_domain, itp_org%iflag_self_itp_send,         &
     &    itp_org%id_dest_domain, itp_org%istack_nod_tbl_org,           &
     &    itp_org%inod_itp_send,                                        &
     &    itp_dest%num_org_domain, itp_dest%iflag_self_itp_recv,        &
     &    itp_dest%id_org_domain, itp_dest%istack_nod_tbl_dest,         &
     &    itp_dest%inod_dest_4_dest, itp_dest%irev_dest_4_dest,         &
     &    SR_sig1, SR_il1, itp_org%inod_gl_dest_4_org, ivec8_2nd(1))
!
!
      if (iflag_debug.eq.1)  write(*,*) 'SOLVER_SEND_RECV_int8_type '
      if (comm_dest%num_neib.gt.0) then
        call SOLVER_SEND_RECV_int8_type                                 &
     &                (NP_dest, comm_dest, ivec8_2nd(1) )
      end if
!
      inod_global_itp(1:NP_dest) = ivec8_2nd(1:NP_dest)
!
      end subroutine s_interpolate_global_node
!
! ----------------------------------------------------------------------
!
      end module interpolate_position
