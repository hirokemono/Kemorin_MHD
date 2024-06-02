!>@file   t_trace_data_send_recv.f90
!!@brief  module t_trace_data_send_recv
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine alloc_broadcast_trace_data(num_each_field_line,      &
!!     &                                      viz_fields, fln_SR)
!!      subroutine dealloc_broadcast_trace_data(fln_SR)
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        type(trace_data_send_recv), intent(inout) :: fln_SR
!!
!!      subroutine s_broadcast_trace_data(fln_tce, fln_SR,           &
!!     &                                  nline_global)
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(trace_data_send_recv), intent(inout) :: fln_SR
!!        integer(kind = kint), intent(inout) :: nline_global
!!      subroutine set_fline_start_2_bcast(iflag_comm, i, ele, surf,    &
!!     &          isf_4_ele_dbl, iele_4_surf_dbl, fln_tce, fln_SR)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        integer(kind = kint), intent(in)                              &
!!     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
!!        integer(kind = kint), intent(in)                              &
!!     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
!!        type(each_fieldline_trace), intent(in) :: fln_tce
!!        type(trace_data_send_recv), intent(inout) :: fln_SR
!!@endverbatim
!
      module t_trace_data_send_recv
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_comm_table
      use t_para_double_numbering
!
      implicit  none
!
      integer(kind= kint), parameter, private :: nitem_export = 6
!
      type trace_data_send_recv
        integer(kind = kint) :: npe_send
        integer(kind = kint) :: npe_recv
        integer(kind = kint), allocatable :: num_send(:)
        integer(kind = kint), allocatable :: num_recv(:)
!
        integer(kind = kint), allocatable :: id_pe_send(:)
        integer(kind = kint), allocatable :: ineib_send(:)
        integer(kind = kint), allocatable :: istack_send(:)
        integer(kind = kint), allocatable :: istack_isend(:)
        integer(kind = kint), allocatable :: icou_send(:)
!
        integer(kind = kint), allocatable :: id_pe_recv(:)
        integer(kind = kint), allocatable :: ineib_recv(:)
        integer(kind = kint), allocatable :: istack_recv(:)
        integer(kind = kint), allocatable :: istack_irecv(:)
!
        integer(kind = kint) :: ncomp_export
        integer(kind = kint) :: ntot_send
        integer(kind = kint) :: ntot_sendbuf
        integer(kind = kint), allocatable :: item_send(:)
        integer(kind = kint), allocatable :: iSend(:,:)
        real(kind = kreal),   allocatable :: rSend(:,:)
        integer(kind = kint) :: ntot_recv
        integer(kind = kint) :: ntot_recvbuf
        integer(kind = kint), allocatable :: item_recv(:)
        integer(kind = kint), allocatable :: iRecv(:,:)
        real(kind = kreal),   allocatable :: rRecv(:,:)
      end type trace_data_send_recv
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_trace_data_SR_num(viz_fields, fln_SR)
!
      use t_ctl_params_viz_fields
!
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      fln_SR%ntot_sendbuf = 0
      fln_SR%ntot_recvbuf = 0
      fln_SR%ncomp_export = 9 + viz_fields%ntot_color_comp

      allocate(fln_SR%num_send(nprocs))
      allocate(fln_SR%num_recv(nprocs))
!
      allocate(fln_SR%id_pe_send(nprocs))
      allocate(fln_SR%ineib_send(nprocs))
      allocate(fln_SR%istack_send(0:nprocs))
      allocate(fln_SR%istack_isend(0:nprocs))
      allocate(fln_SR%icou_send(nprocs))

      allocate(fln_SR%id_pe_recv(nprocs))
      allocate(fln_SR%ineib_recv(nprocs))
      allocate(fln_SR%istack_recv(0:nprocs))
      allocate(fln_SR%istack_irecv(0:nprocs))
!
!$omp parallel workshare
      fln_SR%num_send(1:nprocs) =   0
      fln_SR%num_recv(1:nprocs) =   0
      fln_SR%ineib_send(1:nprocs) = 0
      fln_SR%ineib_recv(1:nprocs) = 0
      fln_SR%id_pe_send(1:nprocs) = -1
      fln_SR%id_pe_recv(1:nprocs) = -1
      fln_SR%icou_send(1:nprocs) =  0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%istack_send(0:nprocs) =   0
      fln_SR%istack_recv(0:nprocs) =   0
      fln_SR%istack_isend(0:nprocs) =  0
      fln_SR%istack_irecv(0:nprocs) =  0
!$omp end parallel workshare
!
      call alloc_trace_SR_export(ione, fln_SR)
      call alloc_trace_SR_import(ione, fln_SR)
!
      end subroutine alloc_trace_data_SR_num
!
!  ---------------------------------------------------------------------
!
      subroutine s_trace_data_send_recv(ele, surf,                      &
     &          isf_4_ele_dbl, iele_4_surf_dbl,                         &
     &          fln_tce, fln_SR, m_SR, nline_global)
!
      use calypso_SR
      use calypso_SR_int
      use calypso_SR_N
      use solver_SR_int
      use set_to_send_buffer
      use t_mesh_SR
      use t_source_of_filed_line
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in)                                  &
     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
!
      integer(kind = kint), intent(inout) :: nline_global
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(trace_data_send_recv), intent(inout) :: fln_SR
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i
!
      call count_trace_data_SR_npe(ele, isf_4_ele_dbl, fln_tce, fln_SR)
      call count_trace_data_SR_num(fln_SR)
      call raise_trace_SR_export(fln_tce%num_current_fline,             &
     &                           fln_SR)
      call raise_trace_SR_import(fln_SR)

      call set_trace_data_to_SR(ele, surf,                              &
     &    isf_4_ele_dbl, iele_4_surf_dbl, fln_tce, fln_SR)
!
      call calypso_send_recv_N                                          &
     &   (0, fln_SR%ncomp_export, fln_tce%num_current_fline,            &
     &    fln_SR%ntot_recv, fln_SR%npe_send, fln_SR%id_pe_send,         &
     &    fln_SR%istack_send, fln_SR%item_send,                         &
     &    fln_SR%npe_recv, fln_SR%id_pe_recv,                           &
     &    fln_SR%istack_recv, fln_SR%item_recv, fln_SR%item_recv, 0,    &
     &    m_SR%SR_sig, m_SR%SR_r, fln_SR%rSend, fln_SR%rRecv)
      do i = 1, nitem_export
        call resize_iwork_SR_t(fln_SR%npe_send, fln_SR%npe_recv,        &
     &                         fln_SR%istack_send(fln_SR%npe_send),     &
     &                         fln_SR%istack_recv(fln_SR%npe_recv),     &
     &                         m_SR%SR_sig, m_SR%SR_i)
        call set_to_send_buf_int(fln_tce%num_current_fline,             &
     &      fln_SR%istack_send(fln_SR%npe_send), fln_SR%item_send, fln_SR%iSend(1,i), &
     &      m_SR%SR_i%iWS)
        call calypso_send_recv_intcore                                  &
     &   (fln_SR%npe_send, fln_SR%id_pe_send,                           &
     &    fln_SR%istack_send, m_SR%SR_i%iWS(1), 0,                     &
     &    fln_SR%npe_recv, fln_SR%id_pe_recv,                           &
     &    fln_SR%istack_recv, fln_SR%iRecv(1,i), m_SR%SR_sig)
        call calypso_send_recv_fin(fln_SR%npe_send, 0, m_SR%SR_sig)
      end do
!
      call set_trace_data_from_SR(fln_SR, fln_tce)
      nline_global = fln_tce%istack_current_fline(nprocs)             &
     &              - fln_tce%istack_current_fline(0)
!
      end subroutine s_trace_data_send_recv
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_trace_data_SR_num(fln_SR)
!
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      deallocate(fln_SR%istack_isend, fln_SR%istack_irecv)
      deallocate(fln_SR%istack_send,  fln_SR%istack_recv)
      deallocate(fln_SR%ineib_send,   fln_SR%ineib_recv)
      deallocate(fln_SR%id_pe_send,   fln_SR%id_pe_recv)
      deallocate(fln_SR%num_send,     fln_SR%num_recv)
      deallocate(fln_SR%icou_send)
!
      end subroutine dealloc_trace_data_SR_num
!
!  ---------------------------------------------------------------------
!
      subroutine raise_trace_SR_export(nnod_org, fln_SR)
!
      integer(kind = kint), intent(in) :: nnod_org
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      if(fln_SR%ntot_sendbuf .gt. nnod_org) return
      call dealloc_trace_SR_export(fln_SR)
      call alloc_trace_SR_export(nnod_org, fln_SR)
!
      end subroutine raise_trace_SR_export
!
!  ---------------------------------------------------------------------
!
      subroutine raise_trace_SR_import(fln_SR)
!
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      if(fln_SR%ntot_recvbuf .gt. fln_SR%ntot_recv) return
      call dealloc_trace_SR_import(fln_SR)
      call alloc_trace_SR_import(fln_SR%ntot_recv, fln_SR)
!
      end subroutine raise_trace_SR_import
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_trace_SR_export(ntot, fln_SR)
!
      integer(kind = kint), intent(in) :: ntot
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      fln_SR%ntot_sendbuf = ntot
      allocate(fln_SR%item_send(ntot))
      allocate(fln_SR%iSend(ntot,nitem_export))
      allocate(fln_SR%rSend(fln_SR%ncomp_export,ntot))
      return
!
      if(ntot .le. 0) return
!$omp parallel workshare
      fln_SR%item_send(1:ntot) = 0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%iSend(1:ntot,1:nitem_export) = 0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%rSend(1:fln_SR%ncomp_export,1:ntot) = 0
!$omp end parallel workshare
!
      end subroutine alloc_trace_SR_export
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_trace_SR_import(ntot, fln_SR)
!
      integer(kind = kint), intent(in) :: ntot
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      fln_SR%ntot_recvbuf = ntot
      allocate(fln_SR%item_recv(ntot))
      allocate(fln_SR%iRecv(ntot,nitem_export))
      allocate(fln_SR%rRecv(fln_SR%ncomp_export,ntot))
      return
!
      if(ntot .le. 0) return
!$omp parallel workshare
      fln_SR%item_recv(1:fln_SR%ntot_recvbuf) =          0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%iRecv(1:fln_SR%ntot_recvbuf,nitem_export) = 0
!$omp end parallel workshare
!$omp parallel workshare
      fln_SR%rRecv(1:fln_SR%ncomp_export,1:fln_SR%ntot_recvbuf) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_trace_SR_import
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_trace_SR_export(fln_SR)
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      if(allocated(fln_SR%item_send) .eqv. .FALSE.) return
      deallocate(fln_SR%item_send)
      deallocate(fln_SR%iSend)
      deallocate(fln_SR%rSend)
!
      end subroutine dealloc_trace_SR_export
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_trace_SR_import(fln_SR)
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      if(allocated(fln_SR%item_recv) .eqv. .FALSE.) return
      deallocate(fln_SR%item_recv)
      deallocate(fln_SR%rRecv)
      deallocate(fln_SR%iRecv)
!
      end subroutine dealloc_trace_SR_import
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_trace_data_SR_npe(ele,                           &
     &          isf_4_ele_dbl, fln_tce, fln_SR)
!
      use calypso_mpi_int
      use t_source_of_filed_line
!
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      type(each_fieldline_trace), intent(in) :: fln_tce
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      integer(kind = kint) :: num, i, ip
      integer(kind = kint) :: iele, isf
!
!$omp parallel workshare
      fln_SR%num_send(1:nprocs) = 0
!$omp end parallel workshare

      do i =  1, fln_tce%num_current_fline
        iele = fln_tce%isf_fline_start(1,i)
        isf =  fln_tce%isf_fline_start(2,i)
        if(isf_4_ele_dbl(iele,isf,1) .eq. my_rank                       &
     &             .and. fln_tce%iflag_comm_start(i) .ne. ione) cycle
!
        ip = isf_4_ele_dbl(iele,isf,1) + 1
            fln_SR%num_send(ip) = fln_SR%num_send(ip) + 1
      end do
      call calypso_mpi_alltoall_one_int(fln_SR%num_send,                &
     &                                  fln_SR%num_recv)
!
      fln_SR%npe_send = 0
      do ip = 1, nprocs
        if(fln_SR%num_send(ip) .gt. 0) then
          fln_SR%npe_send = fln_SR%npe_send + 1
        end if
      end do
      fln_SR%npe_recv = 0
      do ip = 1, nprocs
        if(fln_SR%num_recv(ip) .gt. 0) then
          fln_SR%npe_recv = fln_SR%npe_recv + 1
        end if
      end do
!
      end subroutine count_trace_data_SR_npe
!
!  ---------------------------------------------------------------------
!
      subroutine count_trace_data_SR_num(fln_SR)
!
      use t_source_of_filed_line
!
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      integer(kind = kint) :: ip, icou
!
      icou = 0
      fln_SR%istack_send(0) = 0
      do ip = 1, nprocs
        if(fln_SR%num_send(ip) .gt. 0) then
          icou = icou + 1
          fln_SR%ineib_send(ip) = icou
          fln_SR%id_pe_send(icou) = ip-1
          fln_SR%istack_send(icou) = fln_SR%istack_send(icou-1)         &
     &                            + fln_SR%num_send(ip)
        end if
      end do
      fln_SR%ntot_send = fln_SR%istack_send(fln_SR%npe_send)

      icou = 0
      fln_SR%istack_recv(0) = 0
      do ip = 1, nprocs
        if(fln_SR%num_recv(ip) .gt. 0) then
          icou = icou + 1
          fln_SR%ineib_recv(ip) = icou
          fln_SR%id_pe_recv(icou) = ip-1
          fln_SR%istack_recv(icou) = fln_SR%istack_recv(icou-1)         &
     &                            + fln_SR%num_recv(ip)
        end if
      end do
      fln_SR%ntot_recv = fln_SR%istack_recv(fln_SR%npe_recv)

      fln_SR%istack_isend(0:fln_SR%npe_send)                            &
     &      = nitem_export * fln_SR%istack_send(0:fln_SR%npe_send)
      fln_SR%istack_irecv(0:fln_SR%npe_recv)                            &
     &      = nitem_export * fln_SR%istack_recv(0:fln_SR%npe_recv)
!
      end subroutine count_trace_data_SR_num
!
!  ---------------------------------------------------------------------
!
      subroutine set_trace_data_to_SR(ele, surf,                        &
     &          isf_4_ele_dbl, iele_4_surf_dbl, fln_tce, fln_SR)
!
      use t_source_of_filed_line
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in)                                  &
     &               :: iele_4_surf_dbl(surf%numsurf,2,3)
      type(each_fieldline_trace), intent(in) :: fln_tce
      type(trace_data_send_recv), intent(inout) :: fln_SR
!
      integer(kind = kint) :: inum, isurf
      integer(kind = kint) :: iele, isf, icou
      integer(kind = kint) :: irank_send, ineib_tmp
!
      do inum = 1, fln_SR%ntot_recv
        fln_SR%item_recv(inum) = inum
      end do

      fln_SR%icou_send(1:nprocs) = fln_SR%istack_send(0:nprocs-1)
      do inum = 1, fln_tce%num_current_fline
        if(fln_tce%iflag_comm_start(inum) .ne. ione) cycle
        
        iele = fln_tce%isf_fline_start(1,inum)
        isf =  fln_tce%isf_fline_start(2,inum)
        irank_send = isf_4_ele_dbl(iele,isf,1)
        if(irank_send .eq. my_rank) cycle
        
!
        ineib_tmp = fln_SR%ineib_send(irank_send+1)
        fln_SR%icou_send(ineib_tmp) = fln_SR%icou_send(ineib_tmp) + 1
        icou = fln_SR%icou_send(ineib_tmp)
        fln_SR%item_send(icou) = inum
      end do
!
!$omp parallel do private(inum,iele,isf,isurf)
      do inum = 1, fln_tce%num_current_fline
        iele = fln_tce%isf_fline_start(1,inum)
        isf =  fln_tce%isf_fline_start(2,inum)
        isurf = abs(surf%isf_4_ele(iele,isf))
!
        fln_SR%iSend(inum,1) = fln_tce%iline_original(inum)
        fln_SR%iSend(inum,2) = fln_tce%iflag_direction(inum)
        fln_SR%iSend(inum,3) = fln_tce%icount_fline(inum)
!
       if(isf_4_ele_dbl(iele,isf,2) .lt. 0) then
          fln_SR%iSend(inum,4:5) = iele_4_surf_dbl(isurf, 1, 2:3)
        else
          fln_SR%iSend(inum,4:5) = iele_4_surf_dbl(isurf, 2, 2:3)
        end if
        fln_SR%iSend(inum,6) = isf_4_ele_dbl(iele,isf,1)
!
        fln_SR%rSend(1:4,inum) = fln_tce%xx_fline_start(1:4,inum)
        fln_SR%rSend(5:8,inum) = fln_tce%v_fline_start(1:4,inum)
        fln_SR%rSend(9,inum) =   fln_tce%trace_length(inum)
        fln_SR%rSend(9+1:fln_SR%ncomp_export,inum)                      &
    &         = fln_tce%c_fline_start(1:fln_SR%ncomp_export-9,inum)
      end do
!$omp end parallel do
!
      end subroutine set_trace_data_to_SR
!
!  ---------------------------------------------------------------------
!
      subroutine set_trace_data_from_SR(fln_SR, fln_tce)
!
      use calypso_mpi_int
      use t_source_of_filed_line
!
      type(trace_data_send_recv), intent(in) :: fln_SR
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: ip, i
!
      fln_tce%num_current_fline = fln_SR%ntot_recv
      fln_tce%istack_current_fline(0) = 0
      call calypso_mpi_allgather_one_int(fln_tce%num_current_fline,     &
     &                                 fln_tce%istack_current_fline(1))
!
      do ip = 1, nprocs
        fln_tce%istack_current_fline(ip)                                &
     &                   = fln_tce%istack_current_fline(ip-1)           &
     &                    + fln_tce%istack_current_fline(ip)
      end do
      do i = 1, fln_SR%ntot_recv
          fln_tce%iline_original(i) =      fln_SR%iRecv(i,1)
          fln_tce%iflag_direction(i) =     fln_SR%iRecv(i,2)
          fln_tce%icount_fline(i) =        fln_SR%iRecv(i,3)
          fln_tce%isf_fline_start(1:2,i) = fln_SR%iRecv(i,4:5)
!
          fln_tce%xx_fline_start(1:4,i) = fln_SR%rRecv(1:4,i)
          fln_tce%v_fline_start(1:4,i) =  fln_SR%rRecv(5:8,i)
          fln_tce%trace_length(i) =       fln_SR%rRecv(9,i)
          fln_tce%c_fline_start(1:fln_SR%ncomp_export-9,i)              &
     &            = fln_SR%rRecv(9+1:fln_SR%ncomp_export,i)
      end do
!
      end subroutine set_trace_data_from_SR
!
!  ---------------------------------------------------------------------
!
      end module t_trace_data_send_recv

