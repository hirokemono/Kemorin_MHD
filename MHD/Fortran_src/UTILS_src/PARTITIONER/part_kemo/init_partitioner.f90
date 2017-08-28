!init_partitioner.f90
!      module  init_partitioner
!
!!      subroutine initialize_partitioner(org_mesh, org_group)
!!        type(mesh_geometry), intent(inout) :: org_mesh
!!        type(mesh_groups), intent(inout) :: org_group
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
      integer(kind = kint), parameter, private :: my_rank = 0
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine initialize_partitioner(org_mesh, org_group)
!
      use t_mesh_data
      use t_next_node_ele_4_node
!
      use m_error_IDs
      use m_ctl_param_partitioner
      use m_domain_group_4_partition
      use const_mesh_information
      use set_domain_and_org_id
      use quick_mesh_check_for_part
!
      use error_exit_4_part
!
      type(mesh_geometry), intent(inout) :: org_mesh
      type(mesh_groups), intent(inout) :: org_group
!
!
      call quick_mesh_chk_4_part(org_mesh%node, org_mesh%ele,           &
     &    org_group%nod_grp, org_group%ele_grp, org_group%surf_grp)
!
!    construct element and surface data
!
      if (iflag_debug.gt.0) write(*,*) 'const_nod_ele_infos'
      call const_nod_ele_infos                                          &
     &   (my_rank, org_mesh%node, org_mesh%ele,                         &
     &    org_group%nod_grp, org_group%ele_grp, org_group%surf_grp)
!
!   set numbers of global mesh
!
      nnod_s_domin =   org_mesh%node%numnod
      nele_s_domin =   org_mesh%ele%numele
      intnod_s_domin = org_mesh%node%internal_node
!
      call allocate_domain_nod_group
      call allocate_org_gl_nod_id
      call allocate_local_ne_id_tbl
!
!      const periodic table
!
      if (iflag_debug.gt.0) write(*,*) 'set_origin_global_node'
      call set_origin_global_node(org_mesh%nod_comm)
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
