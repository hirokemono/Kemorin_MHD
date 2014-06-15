!local_mesh_by_part.f90
!      module local_mesh_by_part
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine local_fem_mesh(my_rank, nprocs,                       &
!     &          num_domain, work_f_head)
!
      module local_mesh_by_part
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine local_fem_mesh(my_rank, nprocs, work_f_head)
!
      use m_constants
      use m_2nd_geometry_data
      use m_partitioner_comm_table
      use m_ctl_param_partitioner
      use m_read_mesh_data
      use set_parallel_file_name
!
      use m_precision
!
      use const_local_mesh_id
      use set_local_connectivities
      use const_local_groups
      use sel_part_nod_comm_input
      use delete_data_files
      use load_2nd_mesh_data
!
      implicit none
!
      integer(kind = kint), intent(in) :: my_rank, nprocs
      character(len=kchara), intent(in) :: work_f_head
!
      integer(kind=kint) :: ip, i
      integer(kind=kint) :: irank_subdomain

!C
!C-- init.
!
      do ip= 1, num_domain
        irank_subdomain = ip-1
!
        if(mod(irank_subdomain,nprocs) .ne. my_rank) cycle
!C
!C +--------------------------+
!C | load communication table |
!C +--------------------------+
!C===
        call load_node_comm_tbl_4_part(ip, work_f_head)
!C
!C +-----------------+
!C | LOCAL NUMBERING |
!C +-----------------+
!C===
        do i = 1, comm_2nd%num_neib
          comm_2nd%id_neib(i) = comm_2nd%id_neib(i) - 1
        end do

        call s_const_local_meshes(ip)
        call set_local_connectivity_4_ele(ele_2nd)
        call s_const_local_groups
!C
!C +-------------------------+
!C | write FINAL LOCAL files |
!C +-------------------------+
!C===
        call output_local_mesh(irank_subdomain)
      end do
!
!C===
      call deallocate_local_ne_id_tbl
!
      if(iflag_memory_conserve .ne. 0) then
        call delete_parallel_files(ione, num_domain, work_f_head)
      end if
!
      end subroutine local_fem_mesh
!
!   --------------------------------------------------------------------
!
      end module local_mesh_by_part
