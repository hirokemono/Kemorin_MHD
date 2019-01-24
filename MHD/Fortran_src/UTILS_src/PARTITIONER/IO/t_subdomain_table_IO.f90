!> @file  t_subdomain_table_IO.f90
!!      module t_subdomain_table_IO
!!
!! @author  H. Matsui
!! @date Written in June, 2009
!
!> @brief Arrays for subdomain list for partitioner
!!
!!@verbatim
!!      subroutine input_domain_group_by_file                           &
!!     &         (file_name, node, nod_d_grp, num_domain)
!!      subroutine input_domain_group_by_metis                          &
!!     &         (file_name, node, nod_d_grp, num_domain)
!!        type(node_data), intent(in) :: node
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!      subroutine finer_domain_list_from_file                          &
!!     &         (file_name, new_node, nod_f_grp, num_domain)
!!        type(node_data), intent(inout) :: new_node
!!        type(finer_domain_group), intent(inout) :: nod_f_grp
!!      subroutine output_domain_group_4_part                           &
!!     &         (file_name, num_domain, node, nod_d_grp)
!!        type(node_data), intent(in) :: node
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!@endverbatim
!
      module t_subdomain_table_IO
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_domain_group_4_partition
!
      implicit none
!
      integer(kind=kint ), parameter, private :: id_subdomain = 21
!
      type subdomain_table_IO
        integer(kind = kint) :: nproc_group_IO
        integer(kind = kint) :: nnod_group_IO
        integer(kind = kint) :: internod_group_IO
        integer(kind = kint),  allocatable :: IGROUP_IO(:)
      end type subdomain_table_IO
!
      private :: alloc_domain_group_IO, dealloc_domain_group_IO
      private :: read_group_4_partition, read_group_by_metis
      private :: write_group_4_partition, copy_domain_list_from_IO
      private :: copy_domain_list_to_IO, copy_finer_domain_list_from_IO
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine input_domain_group_by_file                             &
     &         (file_name, node, nod_d_grp, num_domain)
!
      character(len=kchara), intent(in) :: file_name
      type(node_data), intent(in) :: node
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
      integer(kind = kint), intent(inout) :: num_domain
!
      type(subdomain_table_IO) :: sub_IO
!
      call read_group_4_partition(file_name, sub_IO)
      call copy_domain_list_from_IO(node%numnod, node%internal_node,    &
     &    sub_IO, nod_d_grp, num_domain)
      call dealloc_domain_group_IO(sub_IO)
!
      end subroutine input_domain_group_by_file
!
!   --------------------------------------------------------------------
!
      subroutine input_domain_group_by_metis                            &
     &         (file_name, node, nod_d_grp, num_domain)
!
      use m_error_IDs
      use error_exit_4_part
!
      character(len=kchara), intent(in) :: file_name
      type(node_data), intent(in) :: node
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
      integer(kind = kint), intent(inout) :: num_domain
!
      integer(kind = kint) :: ierr
      type(subdomain_table_IO) :: sub_IO
!
      call read_group_by_metis                                          &
     &   (file_name, node%numnod, node%internal_node, sub_IO, ierr)
      if (ierr .eq. ierr_P_MPI) call ERROR_EXIT(ierr_P_MPI, izero)
      call copy_domain_list_from_IO(node%numnod, node%internal_node,    &
     &    sub_IO, nod_d_grp, num_domain)
      call dealloc_domain_group_IO(sub_IO)
!
      end subroutine input_domain_group_by_metis
!
!   --------------------------------------------------------------------
!
      subroutine finer_domain_list_from_file                            &
     &         (file_name, new_node, nod_f_grp, num_domain)
!
      character(len=kchara), intent(in) :: file_name
      type(node_data), intent(inout) :: new_node
      type(finer_domain_group), intent(inout) :: nod_f_grp
      integer(kind = kint), intent(inout) :: num_domain
!
      type(subdomain_table_IO) :: sub_IO
!
!
      call read_group_4_partition(file_name, sub_IO)
      call copy_finer_domain_list_from_IO                               &
     &   (sub_IO, new_node, nod_f_grp, num_domain)
      call dealloc_domain_group_IO(sub_IO)
!
      end subroutine finer_domain_list_from_file
!
!   --------------------------------------------------------------------
!
      subroutine output_domain_group_4_part                             &
     &         (file_name, num_domain, node, nod_d_grp)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: num_domain
      type(node_data), intent(in) :: node
      type(domain_group_4_partition), intent(in) :: nod_d_grp
!
      type(subdomain_table_IO) :: sub_IO
!
!
      call copy_domain_list_to_IO(num_domain,                           &
     &    node%numnod, node%internal_node, nod_d_grp, sub_IO)
      call write_group_4_partition(file_name, sub_IO)
      call dealloc_domain_group_IO(sub_IO)
!
      end subroutine output_domain_group_4_part
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_group_4_partition(file_name, sub_IO)
!
      character(len=kchara), intent(in) :: file_name
      type(subdomain_table_IO), intent(in) :: sub_IO
!
!
      open(id_subdomain,file = file_name)
!
      write(id_subdomain,*) '! number of subdomains'
      write(id_subdomain,'(i16)') sub_IO%nproc_group_IO
!
      write(id_subdomain,*) '! number of total node'
      write(id_subdomain,'(2i16)')                                      &
     &      sub_IO%nnod_group_IO, sub_IO%internod_group_IO
!
      write(id_subdomain,'(10i16)')                                     &
     &      sub_IO%IGROUP_IO(1:sub_IO%nnod_group_IO)
!
      close(id_subdomain)
!
      end subroutine write_group_4_partition
!
!   --------------------------------------------------------------------
!
      subroutine read_group_4_partition(file_name, sub_IO)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: file_name
      type(subdomain_table_IO), intent(inout) :: sub_IO
!
      character(len=255) :: character_4_read
!
!
      open(id_subdomain,file = file_name)
!
      call skip_comment(character_4_read, id_subdomain)
      read(character_4_read,*) sub_IO%nproc_group_IO
!
      call skip_comment(character_4_read, id_subdomain)
      read(character_4_read,*)                                          &
     &      sub_IO%nnod_group_IO, sub_IO%internod_group_IO
!
      call alloc_domain_group_IO(sub_IO)
      read(id_subdomain,*) sub_IO%IGROUP_IO(1:sub_IO%nnod_group_IO)
!
      close(id_subdomain)
!
      end subroutine read_group_4_partition
!
!   --------------------------------------------------------------------
!
      subroutine read_group_by_metis                                    &
     &         (file_name, numnod, internal_node, sub_IO, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: numnod, internal_node
      type(subdomain_table_IO), intent(inout) :: sub_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: inod, NPARTMAX
!
!
      ierr = 0
      open(id_subdomain,file = file_name)
!
      NPARTMAX=-100
!
      sub_IO%nnod_group_IO =     numnod
      sub_IO%internod_group_IO = internal_node
      call alloc_domain_group_IO(sub_IO)
!
      do inod = 1, internal_node
        read(id_subdomain,*) sub_IO%IGROUP_IO(inod)
      end do
      close(id_subdomain)
!
      do inod = 1, internal_node
        sub_IO%IGROUP_IO(inod) = sub_IO%IGROUP_IO(inod) + 1
        NPARTMAX = max(NPARTMAX,sub_IO%IGROUP_IO(inod))
      end do
!
      sub_IO%nproc_group_IO = NPARTMAX
      if (sub_IO%nproc_group_IO.lt.1) ierr = 32
!
      end subroutine read_group_by_metis
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_domain_group_IO(sub_IO)
!
      type(subdomain_table_IO), intent(inout) :: sub_IO
!
!
      allocate(sub_IO%IGROUP_IO(sub_IO%nnod_group_IO))
      sub_IO%IGROUP_IO = 0
!
      end subroutine alloc_domain_group_IO
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_domain_group_IO(sub_IO)
!
      type(subdomain_table_IO), intent(inout) :: sub_IO
!
      deallocate(sub_IO%IGROUP_IO)
!
      end subroutine dealloc_domain_group_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine copy_domain_list_from_IO                               &
     &         (numnod, internal_node, sub_IO, nod_d_grp, num_domain)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      type(subdomain_table_IO), intent(in) :: sub_IO
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
      integer(kind = kint), intent(inout) :: num_domain
!
!
      if(sub_IO%nnod_group_IO .ne. numnod) stop 'check number of node'
!
      if(sub_IO%internod_group_IO .ne. internal_node) then
        stop 'check number of internal node'
      end if
!
      num_domain = sub_IO%nproc_group_IO
      nod_d_grp%IGROUP(1:internal_node)                                 &
     &      = sub_IO%IGROUP_IO(1:internal_node)
!
      end subroutine copy_domain_list_from_IO
!
!   --------------------------------------------------------------------
!
      subroutine copy_domain_list_to_IO                                 &
     &         (num_domain, numnod, internal_node, nod_d_grp, sub_IO)
!
      integer(kind = kint), intent(in) :: num_domain
      integer(kind = kint), intent(in) :: numnod, internal_node
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      type(subdomain_table_IO), intent(inout) :: sub_IO
!
!
      sub_IO%nnod_group_IO =     numnod
      sub_IO%internod_group_IO = internal_node
      sub_IO%nproc_group_IO =    num_domain
!
      call alloc_domain_group_IO(sub_IO)
!
      sub_IO%IGROUP_IO(1:internal_node)                                 &
     &      = nod_d_grp%IGROUP(1:internal_node)
!
      end subroutine copy_domain_list_to_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine copy_finer_domain_list_from_IO                         &
     &         (sub_IO, new_node, nod_f_grp, num_domain)
!
      type(subdomain_table_IO), intent(in) :: sub_IO
      type(node_data), intent(inout) :: new_node
      type(finer_domain_group), intent(inout) :: nod_f_grp
      integer(kind = kint), intent(inout) :: num_domain
!
!
      if(sub_IO%nnod_group_IO .ne. new_node%numnod)                     &
     &      stop 'check number of node'
      if(sub_IO%internod_group_IO .ne. new_node%internal_node)          &
     &      stop 'check number of internal node'
!
      num_domain = sub_IO%nproc_group_IO
      nod_f_grp%nnod_group_finer = new_node%numnod
      call alloc_finer_domain_group(nod_f_grp)
!
!$omp parallel workshare
      nod_f_grp%IGROUP_FINER(1:new_node%internal_node)                 &
     &      = sub_IO%IGROUP_IO(1:new_node%internal_node)
!$omp end parallel workshare
!
      end subroutine copy_finer_domain_list_from_IO
!
!   --------------------------------------------------------------------
!
      end module t_subdomain_table_IO
