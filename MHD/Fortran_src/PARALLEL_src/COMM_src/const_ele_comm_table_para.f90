!const_ele_comm_table_para.f90
!      module const_ele_comm_table_para
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine const_ele_type_comm_tbl_para(numnod, internal_node,   &
!     &          numele, nnod_4_ele, inod_global, ie, id_org_domain,    &
!     &          comm_tbl)
!        type(communication_table), intent(inout) :: comm_tbl
!
      module const_ele_comm_table_para
!
      use m_precision
!
      use m_constants
      use m_const_ele_comm_tbl
      use m_parallel_var_dof
      use calypso_mpi
      use t_comm_table
!
      implicit  none
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
      use const_nod_ele_import_table
      use const_ele_comm_table_type
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
!
!
      call alloc_const_ele_comm_tbl(nprocs)
!
      ip = my_rank + 1
      call const_ele_import_table(my_rank, nprocs,                      &
     &    numnod, numele, nnod_4_ele, inod_global, ie, id_org_domain,   &
     &    ele_comm_tmp(ip), ele_comm_work(ip))
!
      call bcast_ele_import_table(nnod_4_ele)
!
      call s_const_ele_comm_table_type(my_rank, nprocs, numnod,         &
     &    internal_node, numele, nnod_4_ele, inod_global, ie, comm_tbl)
!
      do ip = 1, nprocs
        call dealloc_ie_gl_import( ele_comm_work(ip) )
!
        call deallocate_type_neib_id( ele_comm_tmp(ip) )
        call deallocate_type_import_item( ele_comm_tmp(ip) )
      end do
      call dealloc_const_ele_comm_tbl
!
      end subroutine const_ele_type_comm_tbl_para
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine bcast_ele_import_table(nnod_4_ele)
!
      integer(kind = kint), intent(in) :: nnod_4_ele
!
      integer(kind = kint) :: max_import
      integer(kind = kint) :: num_send
      integer(kind = kint), allocatable :: num_recv(:)
!
      integer(kind = kint) :: ip, num, j, jj, k1
!
!
      allocate(num_recv(nprocs))
!
      num_send = ele_comm_tmp(ip)%num_neib
      call MPI_Allgather(num_send, ione, MPI_INTEGER, num_recv, ione,  &
     &    MPI_INTEGER, SOLVER_COMM, ierr)
      ele_comm_tmp(1:nprocs)%num_neib = num_recv(1:nprocs)
!
      num_send = ele_comm_tmp(ip)%ntot_import
      call MPI_Allgather(num_send, ione, MPI_INTEGER, num_recv, ione,  &
     &    MPI_INTEGER, SOLVER_COMM, ierr)
      ele_comm_tmp(1:nprocs)%ntot_import = num_recv(1:nprocs)
!
!
      max_import = 0
      do ip = 1, nprocs
        if(my_rank .ne. (ip-1)) then
          call allocate_type_neib_id( ele_comm_tmp(ip) )
          call allocate_type_import_num( ele_comm_tmp(ip) )
          call allocate_type_import_item(ele_comm_tmp(ip))
!
          call alloc_ie_gl_import(ele_comm_work(ip), nnod_4_ele,        &
     &        ele_comm_tmp(ip)%ntot_import)
        end if
        max_import = max(max_import,ele_comm_tmp(ip)%ntot_import)
      end do
!
      deallocate(num_recv)
      allocate(num_recv(0:nnod_4_ele*max_import))
!
      do ip = 1, nprocs
        num = ele_comm_tmp(ip)%num_neib
        if(my_rank .eq. (ip-1)) then
          num_recv(1:num) = ele_comm_tmp(ip)%id_neib(1:num)
        end if
        call MPI_Bcast(num_recv(1), num, MPI_INTEGER, my_rank,          &
     &      SOLVER_COMM, ierr)
!
        ele_comm_tmp(ip)%id_neib(1:num) = num_recv(1:num)
      end do
!
      do ip = 1, nprocs
        num = ele_comm_tmp(ip)%num_neib
        if(my_rank .eq. (ip-1)) then
          num_recv(1:num) = ele_comm_tmp(ip)%istack_import(1:num)
        end if
        call MPI_Bcast(num_recv(1), num, MPI_INTEGER, my_rank,          &
     &      SOLVER_COMM, ierr)
!
        ele_comm_tmp(ip)%istack_import(0) =     0
        ele_comm_tmp(ip)%istack_import(1:num) = num_recv(1:num)
      end do
!
      do ip = 1, nprocs
        num = ele_comm_tmp(ip)%ntot_import
        if(my_rank .eq. (ip-1)) then
          num_recv(1:num) = ele_comm_tmp(ip)%item_import(1:num)
        end if
        call MPI_Bcast(num_recv(1), num, MPI_INTEGER, my_rank,          &
     &      SOLVER_COMM, ierr)
!
        ele_comm_tmp(ip)%item_import(1:num) = num_recv(1:num)
      end do
!
!
      do ip = 1, nprocs
        num = ele_comm_tmp(ip)%ntot_import
        if(my_rank .eq. (ip-1)) then
          num_recv(1:num) = ele_comm_tmp(ip)%item_import(1:num)
        end if
        call MPI_Bcast(num_recv(1), num, MPI_INTEGER, my_rank,          &
     &      SOLVER_COMM, ierr)
!
        ele_comm_tmp(ip)%item_import(1:num) = num_recv(1:num)
      end do
!
      do ip = 1, nprocs
        num = nnod_4_ele * ele_comm_tmp(ip)%ntot_import
        if(my_rank .eq. (ip-1)) then
          do j = 1, num
            do k1 = 1, nnod_4_ele
              jj = k1 + (j-1)*nnod_4_ele
              num_recv(jj) = ele_comm_work(ip)%ie_gl_import(k1,j)
            end do
          end do
        end if
!
        call MPI_Bcast(num_recv(1), num, MPI_INTEGER, my_rank,          &
     &      SOLVER_COMM, ierr)
!
        do j = 1, num
          do k1 = 1, nnod_4_ele
            jj = k1 + (j-1)*nnod_4_ele
            ele_comm_work(ip)%ie_gl_import(k1,j) = num_recv(jj)
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
