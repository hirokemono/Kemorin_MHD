!local_mesh_by_part.f90
!      module local_mesh_by_part
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine local_fem_mesh                                       &
!!     &         (id_rank, nprocs, part_p, node_org, ele_org, group_org,&
!!     &          internals_part, nod_d_grp, ele_d_grp, comm_part)
!!        type(ctl_param_partitioner), intent(in) :: part_p
!!        type(node_data), intent(in) :: node_org
!!        type(element_data), intent(in) :: ele_org
!!        type(mesh_groups), intent(in) :: group_org
!!        type(internals_4_part), intent(in) :: internals_part
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!        type(domain_group_4_partition), intent(inout) :: ele_d_grp
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
     &         (id_rank, nprocs, part_p, node_org, ele_org, group_org,  &
     &          internals_part, nod_d_grp, ele_d_grp, comm_part)
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_domain_group_4_partition
      use t_internal_4_partitioner
      use t_partitioner_comm_table
      use t_ctl_param_partitioner
      use set_parallel_file_name
!
      use m_precision
!
      use const_local_mesh_id
      use set_local_connectivities
      use const_local_groups
      use sel_part_nod_comm_input
      use delete_data_files
      use load_mesh_data
!
      implicit none
!
      integer, intent(in) :: id_rank, nprocs
      type(ctl_param_partitioner), intent(in) :: part_p
      type(node_data), intent(in) :: node_org
      type(element_data), intent(in) :: ele_org
      type(mesh_groups), intent(in) :: group_org
      type(internals_4_part), intent(in) :: internals_part
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
      type(domain_group_4_partition), intent(inout) :: ele_d_grp
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      type(mesh_data), allocatable :: para_fem(:)
!
      integer :: irank_subdomain
      integer(kind=kint) :: i, ip

!C
!C-- init.
!
      allocate(para_fem(part_p%num_domain))
!
      do ip = 1, part_p%num_domain
        irank_subdomain = int(ip - 1)
!
        if(mod(irank_subdomain,nprocs) .ne. id_rank) cycle
!C
!C +--------------------------+
!C | load communication table |
!C +--------------------------+
!C===
        call load_node_comm_tbl_4_part                                  &
     &     (ip, comm_part, para_fem(ip)%mesh%nod_comm)
!C
!C +-----------------+
!C | LOCAL NUMBERING |
!C +-----------------+
!C===
        do i = 1, para_fem(ip)%mesh%nod_comm%num_neib
          para_fem(ip)%mesh%nod_comm%id_neib(i)                         &
     &        = para_fem(ip)%mesh%nod_comm%id_neib(i) - 1
        end do

        call s_const_local_meshes(ip, node_org, ele_org,                &
     &      internals_part, nod_d_grp, ele_d_grp, para_fem(ip)%mesh)
        call set_local_connectivity_4_ele                               &
     &     (ele_org, nod_d_grp, para_fem(ip)%mesh%ele)
        call s_const_local_groups                                       &
     &     (group_org, nod_d_grp, ele_d_grp, para_fem(ip)%group)
      end do
!C
!C +-------------------------+
!C | write FINAL LOCAL files |
!C +-------------------------+
!C===
      do ip = 1, part_p%num_domain
        irank_subdomain = int(ip - 1)
        if(mod(irank_subdomain,nprocs) .ne. id_rank) cycle
!
        call output_mesh(part_p%distribute_mesh_file, irank_subdomain,  &
     &      para_fem(ip)%mesh, para_fem(ip)%group)
        call dealloc_mesh_infos(para_fem(ip)%mesh, para_fem(ip)%group)
      end do
!
!C===
      deallocate(para_fem)
!
      end subroutine local_fem_mesh
!
!   --------------------------------------------------------------------
!
      end module local_mesh_by_part
