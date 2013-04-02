!
!     module interpolate_integer
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine s_interpolate_integer(i_dest, i_origin)
!
      module interpolate_integer
!
      use m_precision
      use m_constants

      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_integer(i_vector_dest, i_vector_org)
!
      use m_parallel_var_dof
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_2nd_pallalel_vector
      use m_2nd_nod_comm_table
      use m_2nd_geometry_param
      use m_interpolate_table_orgin
      use m_interpolate_table_dest
!
      use m_work_4_interpolation
!
      use interpolate_imark_para
      use interpolate_SR_int
      use solver_SR_int
!
!
      real(kind = kreal), intent(in) :: i_vector_org(numnod)
      real(kind = kreal), intent(inout) :: i_vector_dest(nnod_2nd)
!
!     initialize
!
      call verify_2nd_iccg_int_mat(nnod_2nd)
!
      call verifty_work_4_itp_int(ntot_table_org)
!
!
      ix_vec(1:numnod) = i_vector_org(1:numnod)
!
!    interpolation
!
      if (num_dest_domain.gt.0) then
        call s_interporate_imark_para(np_smp, numnod, numele,           &
     &    nnod_4_ele, ie, ix_vec(1), num_dest_domain,                   &
     &    istack_table_wtype_org_smp, ntot_table_org, iele_org_4_org,   &
     &    itype_inter_org, i_inter_org(1) )
      end if
!
!
!     communication
!
      call interpolate_send_recv_int                                    &
     &          (num_dest_domain, iflag_self_itp_send, ntot_table_org,  &
     &           id_dest_domain, istack_nod_table_org,                  &
     &           num_org_domain, iflag_self_itp_recv, ntot_table_dest,  &
     &           id_org_domain, istack_nod_table_dest,                  &
     &           inod_dest_4_dest, ntot_table_org, i_inter_org(1),      &
     &           nnod_2nd, ivec_2nd(1), SOLVER_COMM, my_rank)
!
!
      if (num_neib_2.gt.0) then
        call SOLVER_SEND_RECV_int                                       &
     &                (nnod_2nd, num_neib_2, id_neib_2,                 &
     &                 istack_import_2, item_import_2,                  &
     &                 istack_export_2, item_export_2,                  &
     &                 ivec_2nd(1), SOLVER_COMM, my_rank)
      end if
!
      i_vector_dest(1:nnod_2nd) = ivec_2nd(1:nnod_2nd)
!
      end subroutine s_interpolate_integer
!
! ----------------------------------------------------------------------
!
      end module interpolate_integer
