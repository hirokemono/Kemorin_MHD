!     module interpolate_type_3
!
!     Written by H. Matsui on Jan., 2009
!
!!      subroutine s_interpolate_type_3(NP_org, NP_dest,                &
!!     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!!        type(communication_table), intent(in) :: comm_dest
!!        type(interpolate_table), intent(in) :: itp_table
!!        integer(kind = kint), intent(in) :: PEsmpTOT
!!        integer(kind = kint), intent(in) :: NP_org, NP_dest
!!        real(kind = kreal), intent(in) :: X_org(ithree*NP_org)
!!        real(kind = kreal), intent(inout) :: X_dest(ithree*NP_dest)
!
      module interpolate_type_3
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_interpolate_type_3(NP_org, NP_dest,                  &
     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!
      use m_constants
      use t_comm_table
      use t_interpolate_table
      use m_work_4_interpolation
!
      use interpolate_1pe
      use select_calypso_SR
      use solver_SR_3
!
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(ithree*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(ithree*NP_dest)
!
!     initialize
!
      call verifty_work_4_itp_field(ithree,                             &
     &    itp_table%tbl_org%ntot_table_org)
!
!    interpolation
!
      if ( itp_table%tbl_org%num_dest_domain .gt. 0 ) then
        call interporate_vector_para(PEsmpTOT, NP_org,                  &
     &      itp_table%ele_org%numele, itp_table%ele_org%nnod_4_ele,     &
     &      itp_table%ele_org%ie, X_org(1),                             &
     &      itp_table%tbl_org%istack_tbl_wtype_org_smp,                 &
     &      itp_table%tbl_org%ntot_table_org,                           &
     &      itp_table%tbl_org%iele_org_4_org,                           &
     &      itp_table%tbl_org%itype_inter_org,                          &
     &      itp_table%tbl_org%coef_inter_org, x_inter_org(1) )
      end if
!
!     communication
!
      call sel_calypso_send_recv_3(iflag_import_item,                   &
     &           itp_table%tbl_org%ntot_table_org, NP_dest,             &
     &           itp_table%tbl_org%num_dest_domain,                     &
     &           itp_table%tbl_org%iflag_self_itp_send,                 &
     &           itp_table%tbl_org%id_dest_domain,                      &
     &           itp_table%tbl_org%istack_nod_tbl_org,                  &
     &           itp_table%tbl_org%inod_itp_send,                       &
     &           itp_table%tbl_dest%num_org_domain,                     &
     &           itp_table%tbl_dest%iflag_self_itp_recv,                &
     &           itp_table%tbl_dest%id_org_domain,                      &
     &           itp_table%tbl_dest%istack_nod_tbl_dest,                &
     &           itp_table%tbl_dest%inod_dest_4_dest,                   &
     &           itp_table%tbl_dest%irev_dest_4_dest,                   &
     &           x_inter_org(1), X_dest(1) )
!
!
      if (comm_dest%num_neib .gt. 0) then
        call SOLVER_SEND_RECV_3(NP_dest,                                &
     &      comm_dest%num_neib, comm_dest%id_neib,                      &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export, X_dest(1) )
      end if
!
      end subroutine s_interpolate_type_3
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_type_3x(NP_org, NP_dest,                 &
     &          comm_dest, itp_table, X_org, X_dest, PEsmpTOT)
!
      use calypso_mpi
      use m_constants
      use t_comm_table
      use t_interpolate_table
      use m_work_4_interpolation
!
      use interpolate_1pe
      use select_calypso_SR
      use solver_SR_3
!
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
      integer(kind = kint), intent(in) :: PEsmpTOT
      integer(kind = kint), intent(in) :: NP_org, NP_dest
      real(kind = kreal), intent(in) :: X_org(ithree*NP_org)
!
      real(kind = kreal), intent(inout) :: X_dest(ithree*NP_dest)
!
!     initialize
!
      call verifty_work_4_itp_field(ithree,                             &
     &    itp_table%tbl_org%ntot_table_org)
!
!    interpolation
!
      if ( itp_table%tbl_org%num_dest_domain .gt. 0 ) then
        call s_interporate_vector_para_8x(PEsmpTOT, NP_org,             &
     &      itp_table%ele_org%numele, itp_table%ele_org%nnod_4_ele,     &
     &      itp_table%ele_org%ie, X_org(1),                             &
     &      itp_table%tbl_org%num_dest_domain,                          &
     &      itp_table%tbl_org%istack_tbl_wtype_org_smp,                 &
     &      itp_table%tbl_org%ntot_table_org,                           &
     &      itp_table%tbl_org%iele_org_4_org,                           &
     &      itp_table%tbl_org%itype_inter_org,                          &
     &      itp_table%tbl_org%coef_inter_org, x_inter_org(1) )
      end if
      call calypso_MPI_barrier
      call calypso_MPI_abort(101, 'ika')
!
!     communication
!
      call sel_calypso_send_recv_3(iflag_import_item,                   &
     &           itp_table%tbl_org%ntot_table_org, NP_dest,             &
     &           itp_table%tbl_org%num_dest_domain,                     &
     &           itp_table%tbl_org%iflag_self_itp_send,                 &
     &           itp_table%tbl_org%id_dest_domain,                      &
     &           itp_table%tbl_org%istack_nod_tbl_org,                  &
     &           itp_table%tbl_org%inod_itp_send,                       &
     &           itp_table%tbl_dest%num_org_domain,                     &
     &           itp_table%tbl_dest%iflag_self_itp_recv,                &
     &           itp_table%tbl_dest%id_org_domain,                      &
     &           itp_table%tbl_dest%istack_nod_tbl_dest,                &
     &           itp_table%tbl_dest%inod_dest_4_dest,                   &
     &           itp_table%tbl_dest%irev_dest_4_dest,                   &
     &           x_inter_org(1), X_dest(1) )
!
!
      if (comm_dest%num_neib .gt. 0) then
        call SOLVER_SEND_RECV_3(NP_dest,                                &
     &      comm_dest%num_neib, comm_dest%id_neib,                      &
     &      comm_dest%istack_import, comm_dest%item_import,             &
     &      comm_dest%istack_export, comm_dest%item_export, X_dest(1) )
      end if
!
      end subroutine s_interpolate_type_3x
!
! ----------------------------------------------------------------------
!
      subroutine s_interporate_vector_para_8x(np_smp, numnod, numele,   &
     & nnod_4_ele, ie, v_org, num_dest_domain, istack_tbl_wtype_smp,       &
     &          num_points, iele_gauss, itype_gauss, xi_gauss, vect)
!
      use calypso_mpi
      use m_geometry_constants
      use interpolate_1pe_linear
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind = kint), intent(in) :: ie(numele,num_t_linear)
      integer (kind = kint), intent(in) :: num_dest_domain
      integer (kind = kint), intent(in)                                 &
     &       :: istack_tbl_wtype_smp(0:4*np_smp*num_dest_domain)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(3*numnod)
!
      real (kind=kreal), intent(inout) :: vect(3*num_points)
!
      integer(kind = kint) :: ip, ist
!
!
      do ip = 1, num_dest_domain
        ist = 4*np_smp*(ip-1)
        call s_interpolate_vector_8x(np_smp, numnod, numele,            &
     &      ie, v_org, istack_tbl_wtype_smp(ist), num_points,           &
     &      iele_gauss, itype_gauss, xi_gauss, vect)
      end do
!
      end subroutine s_interporate_vector_para_8x
!
!  ---------------------------------------------------------------------
!
      subroutine s_interpolate_vector_8x(np_smp, numnod, numele,        &
     &          ie, v_org, istack_wtype_smp, num_points,                &
     &          iele_gauss, itype_gauss, xi_gauss, vect)
!
      use interpolate_on_node
      use interpolate_vector_edge2
      use interpolate_vector_surf4
      use interpolate_vector_ele8
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: numnod, numele
      integer (kind = kint), intent(in) :: ie(numele,8)
      integer (kind = kint), intent(in) :: istack_wtype_smp(0:4*np_smp)
      integer (kind = kint), intent(in) :: num_points
      integer (kind = kint), intent(in) :: iele_gauss(num_points)
      integer (kind = kint), intent(in) :: itype_gauss(num_points)
      real (kind=kreal), intent(in) :: xi_gauss(num_points,3)
      real (kind=kreal), intent(in) :: v_org(3*numnod)
!
      real (kind=kreal), intent(inout) :: vect(3*num_points)
!
      integer(kind = kint) :: ist
!
!
      ist = 0
      write(*,*) 's_interpolate_vector_node'
      call s_interpolate_vector_node(np_smp, numnod, numele,            &
     &    8, ie, v_org, istack_wtype_smp(ist), num_points,     &
     &    iele_gauss, itype_gauss, vect)
!
      ist = np_smp
      write(*,*) 's_interpolate_vector_edge2'
      call s_interpolate_vector_edge2(np_smp, numnod, numele, ie,       &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    itype_gauss, xi_gauss, vect)
!
      ist = 2*np_smp
      write(*,*) 's_interpolate_vector_surf4'
      call s_interpolate_vector_surf4(np_smp, numnod, numele, ie,       &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    itype_gauss, xi_gauss, vect)
      return
!
      ist = 3*np_smp
      write(*,*) 's_interpolate_vector_ele8'
      call s_interpolate_vector_ele8(np_smp, numnod, numele, ie,        &
     &    v_org, istack_wtype_smp(ist), num_points, iele_gauss,         &
     &    xi_gauss, vect)
!
      end subroutine s_interpolate_vector_8x
!
! ----------------------------------------------------------------------
!
      end module interpolate_type_3
