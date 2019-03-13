!init_partitioner.f90
!      module  init_partitioner
!
!!      subroutine initialize_partitioner                               &
!!     &         (num_domain, org_mesh, org_group, domain_grp)
!!        type(mesh_geometry), intent(inout) :: org_mesh
!!        type(mesh_groups), intent(inout) :: org_group
!!        type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!
!     modified by H. Matsui
!
      module  init_partitioner
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine initialize_partitioner                                 &
     &         (num_domain, org_mesh, org_group, domain_grp)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_domain_group_4_partition
!
      use m_error_IDs
      use set_domain_and_org_id
      use quick_mesh_check_for_part
!
      use error_exit_4_part
!
      integer(kind = kint), intent(in) :: num_domain
!
      type(mesh_geometry), intent(inout) :: org_mesh
      type(mesh_groups), intent(inout) :: org_group
!
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
!
!
      write(*,*) 'quick_mesh_chk_4_part'
      call quick_mesh_chk_4_part(org_mesh%node, org_mesh%ele,           &
     &    org_group%nod_grp, org_group%ele_grp, org_group%surf_grp)
!
!   set numbers of global mesh
!
      domain_grp%nod_d_grp%num_s_domin =   org_mesh%node%numnod
      domain_grp%ele_d_grp%num_s_domin =   org_mesh%ele%numele
      domain_grp%intnod_s_domin = org_mesh%node%internal_node
!
!      write(*,*) 'alloc_domain_nod_group'
      call alloc_domain_group(domain_grp%nod_d_grp)
      call alloc_org_gl_id(domain_grp%nod_d_grp)
      call alloc_local_id_tbl(domain_grp%nod_d_grp)
      call alloc_domain_group(domain_grp%ele_d_grp)
      call alloc_local_id_tbl(domain_grp%ele_d_grp)
!
!      const periodic table
!
      if (iflag_debug.gt.0) write(*,*) 'set_origin_global_node'
      call set_origin_global_node                                       &
     &   (org_mesh%nod_comm, domain_grp%nod_d_grp)
!
      write (*,'(//,"*** ",i5," SUBDOMAINS",//)') num_domain
      if (num_domain .gt. org_mesh%node%numnod) then
        call ERROR_EXIT(ierr_P_MPI,org_mesh%node%numnod)
      end if
!
      end subroutine  initialize_partitioner
!
!   --------------------------------------------------------------------
!
      end module init_partitioner
