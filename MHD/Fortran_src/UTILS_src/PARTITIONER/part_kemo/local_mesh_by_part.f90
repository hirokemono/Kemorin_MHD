!local_mesh_by_part.f90
!      module local_mesh_by_part
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine local_fem_mesh                                        &
!     &         (my_rank, nprocs, work_f_head, new_fem)
!
      module local_mesh_by_part
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine local_fem_mesh                                         &
     &         (my_rank, nprocs, work_f_head, new_fem)
!
      use t_mesh_data
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
      type(mesh_data), intent(inout) :: new_fem
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
        call load_node_comm_tbl_4_part                                  &
     &     (ip, work_f_head, new_fem%mesh%nod_comm)
!C
!C +-----------------+
!C | LOCAL NUMBERING |
!C +-----------------+
!C===
        do i = 1, new_fem%mesh%nod_comm%num_neib
          new_fem%mesh%nod_comm%id_neib(i)                              &
     &        = new_fem%mesh%nod_comm%id_neib(i) - 1
        end do

        call s_const_local_meshes(ip, new_fem%mesh)
        call set_local_connectivity_4_ele(new_fem%mesh%ele)
        call s_const_local_groups(new_fem%group)
!C
!C +-------------------------+
!C | write FINAL LOCAL files |
!C +-------------------------+
!C===
        call output_local_mesh(irank_subdomain, new_fem)
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
