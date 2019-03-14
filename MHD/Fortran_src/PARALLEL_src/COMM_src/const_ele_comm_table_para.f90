!> @file  const_ele_comm_table_para.f90
!!      module const_ele_comm_table_para
!!
!! @author  H. Matsui
!! @date Programmed in Nov., 2008
!
!> @brief Routines to make ele,ment communication table
!!
!!@verbatim
!!      subroutine const_ele_type_comm_tbl_para(numnod, internal_node,  &
!!     &          numele, nnod_4_ele, inod_global, ie, id_org_domain,   &
!!     &          comm_tbl)
!!        type(communication_table), intent(inout) :: comm_tbl
!!@endverbatim
!
      module const_ele_comm_table_para
!
      use m_precision
!
      use calypso_mpi
      use m_constants
!
      use t_comm_table
      use t_const_export_table
!
      implicit  none
!
      type work_4_const_ele_comm_table
        integer(kind = kint) :: np
        type(communication_table), allocatable :: ele_comm_tmp(:)
        type(work_4_const_export), allocatable :: ele_comm_gl(:)
      end type work_4_const_ele_comm_table
!
      private :: alloc_const_ele_comm_tbl, dealloc_const_ele_comm_tbl
      private :: bcast_ele_import_table
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine const_ele_type_comm_tbl_para(numnod, internal_node,    &
     &          numele, nnod_4_ele, inod_global, ie, id_org_domain,     &
     &          comm_tbl)
!
      use const_nod_ele_comm_table
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: id_org_domain(numele)
!
      type(communication_table), intent(inout) :: comm_tbl
!
      integer(kind = kint) :: ip
      type(work_4_const_ele_comm_table) :: ecomm_wk
!
!
      call alloc_const_ele_comm_tbl(ecomm_wk)
!
      ip = my_rank + 1
      call const_ele_import_table(my_rank, nprocs,                      &
     &    numnod, numele, nnod_4_ele, inod_global, ie, id_org_domain,   &
     &    ecomm_wk%ele_comm_tmp(ip), ecomm_wk%ele_comm_gl(ip))
!
      call bcast_ele_import_table                                       &
     &   (nnod_4_ele, ecomm_wk%ele_comm_tmp, ecomm_wk%ele_comm_gl)
!
      call const_ele_export_table(my_rank, nprocs, numnod,              &
     &    internal_node, numele, nnod_4_ele, inod_global, ie,           &
     &    ecomm_wk%ele_comm_tmp, ecomm_wk%ele_comm_gl, comm_tbl)
!
      call dealloc_const_ele_comm_tbl(ecomm_wk)
!
      end subroutine const_ele_type_comm_tbl_para
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_const_ele_comm_tbl(ecomm_wk)
!
      type(work_4_const_ele_comm_table), intent(inout) :: ecomm_wk
!
!
      ecomm_wk%np = nprocs
      allocate(ecomm_wk%ele_comm_gl(nprocs))
      allocate(ecomm_wk%ele_comm_tmp(nprocs))
!
      end subroutine alloc_const_ele_comm_tbl
!
!------------------------------------------------------------------
!
      subroutine dealloc_const_ele_comm_tbl(ecomm_wk)
!
      type(work_4_const_ele_comm_table), intent(inout) :: ecomm_wk
      integer(kind = kint) :: ip
!
!
      do ip = 1, nprocs
        call dealloc_ie_gl_import( ecomm_wk%ele_comm_gl(ip) )
        call dealloc_import_table( ecomm_wk%ele_comm_tmp(ip) )
      end do
      deallocate(ecomm_wk%ele_comm_gl, ecomm_wk%ele_comm_tmp)
!
      end subroutine dealloc_const_ele_comm_tbl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine bcast_ele_import_table                                 &
     &         (nnod_4_ele, ele_comm_tmp, ele_comm_gl)
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      type(communication_table), intent(inout) :: ele_comm_tmp(nprocs)
      type(work_4_const_export), intent(inout) :: ele_comm_gl(nprocs)
!
      integer(kind = kint) :: max_import
      integer(kind = kint) :: num_send
      integer(kind = kint), allocatable :: num_recv(:)
!
      integer(kind = kint) :: ip, num, j, jj, k1
      integer(kind = kint_gl) :: num64
!
!
      allocate(num_recv(nprocs))
!
        num_send = ele_comm_tmp(my_rank)%num_neib
        call MPI_Allgather(num_send, 1, CALYPSO_INTEGER, num_recv,      &
     &      1, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
      ele_comm_tmp(1:nprocs)%num_neib = num_recv(1:nprocs)
!
        num_send = ele_comm_tmp(my_rank)%ntot_import
        call MPI_Allgather(num_send, 1, CALYPSO_INTEGER, num_recv,      &
     &      1, CALYPSO_INTEGER, CALYPSO_COMM, ierr_MPI)
      ele_comm_tmp(1:nprocs)%ntot_import = num_recv(1:nprocs)
!
!
      max_import = 0
      do ip = 1, nprocs
        if(my_rank .ne. (ip-1)) then
          call alloc_neighbouring_id( ele_comm_tmp(ip) )
          call alloc_import_num(ele_comm_tmp(ip))
          call alloc_import_item(ele_comm_tmp(ip))
        end if
        max_import = max(max_import,ele_comm_tmp(ip)%ntot_import)
      end do
!
      deallocate(num_recv)
      allocate(num_recv(0:nnod_4_ele*max_import))
!
      do ip = 1, nprocs
        num64 = ele_comm_tmp(ip)%num_neib
        if(my_rank .eq. (ip-1)) then
          num_recv(1:num64) = ele_comm_tmp(ip)%id_neib(1:num64)
        end if
        call calypso_mpi_bcast_int(num_recv(1), num64, my_rank)
!
        ele_comm_tmp(ip)%id_neib(1:num64) = num_recv(1:num64)
      end do
!
      do ip = 1, nprocs
        num64 = ele_comm_tmp(ip)%num_neib
        if(my_rank .eq. (ip-1)) then
          num_recv(1:num64) = ele_comm_tmp(ip)%istack_import(1:num64)
        end if
        call calypso_mpi_bcast_int(num_recv(1), num64, my_rank)
!
        ele_comm_tmp(ip)%istack_import(0) =     0
        ele_comm_tmp(ip)%istack_import(1:num64) = num_recv(1:num64)
      end do
!
      do ip = 1, nprocs
        num64 = ele_comm_tmp(ip)%ntot_import
        if(my_rank .eq. (ip-1)) then
          num_recv(1:num64) = ele_comm_tmp(ip)%item_import(1:num64)
        end if
        call calypso_mpi_bcast_int(num_recv(1), num64, my_rank)
!
        ele_comm_tmp(ip)%item_import(1:num64) = num_recv(1:num64)
      end do
!
!
      do ip = 1, nprocs
        num64 = ele_comm_tmp(ip)%ntot_import
        if(my_rank .eq. (ip-1)) then
          num_recv(1:num64) = ele_comm_tmp(ip)%item_import(1:num64)
        end if
        call calypso_mpi_bcast_int(num_recv(1), num64, my_rank)
!
        ele_comm_tmp(ip)%item_import(1:num64) = num_recv(1:num64)
      end do
!
!
      do ip = 1, nprocs
        if(my_rank .ne. (ip-1)) then
          call alloc_ie_gl_import(nnod_4_ele,                           &
     &        ele_comm_tmp(ip)%ntot_import, ele_comm_gl(ip))
        end if
      end do
!
      do ip = 1, nprocs
        num64 = nnod_4_ele * ele_comm_tmp(ip)%ntot_import
        if(my_rank .eq. (ip-1)) then
          do j = 1, num64
            do k1 = 1, nnod_4_ele
              jj = k1 + (j-1)*nnod_4_ele
              num_recv(jj) = ele_comm_gl(ip)%ie_gl_import(k1,j)
            end do
          end do
        end if
!
        call calypso_mpi_bcast_int(num_recv(1), num64, my_rank)
!
        do j = 1, num64
          do k1 = 1, nnod_4_ele
            jj = k1 + (j-1)*nnod_4_ele
            ele_comm_gl(ip)%ie_gl_import(k1,j) = num_recv(jj)
          end do
        end do
      end do
!
      deallocate(num_recv)
!
      end subroutine bcast_ele_import_table
!
!------------------------------------------------------------------
!
      end module const_ele_comm_table_para
