!init_partitioner.f90
!      module  init_partitioner
!
!!      subroutine initialize_partitioner(comm_org, node_org, ele_org,  &
!!     &          nod_grp, ele_grp, sf_grp)
!!        type(node_data), intent(in) :: node_org
!!        type(element_data), intent(inout) :: ele_org
!!
!!        type(group_data), intent(in) :: nod_grp
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: sf_grp
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
      subroutine initialize_partitioner(comm_org, node_org, ele_org,    &
     &          nod_grp, ele_grp, sf_grp)
!
      use t_comm_table
      use t_geometry_data
      use t_group_data
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
      type(communication_table), intent(in) :: comm_org
      type(node_data), intent(inout) :: node_org
      type(element_data), intent(inout) :: ele_org
!
      type(group_data), intent(inout) :: nod_grp
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: sf_grp
!
!
      call quick_mesh_chk_4_part                                        &
     &   (node_org, ele_org, nod_grp, ele_grp, sf_grp)
!
!    construct element and surface data
!
      if (iflag_debug.gt.0) write(*,*) 'const_nod_ele_infos'
      call const_nod_ele_infos(my_rank,                                 &
     &    node_org, ele_org, nod_grp, ele_grp, sf_grp)
!
!   set numbers of global mesh
!
      nnod_s_domin =   node_org%numnod
      nele_s_domin =   ele_org%numele
      intnod_s_domin = node_org%internal_node
!
      call allocate_domain_nod_group
      call allocate_org_gl_nod_id
      call allocate_local_ne_id_tbl
!
!      const periodic table
!
      if (iflag_debug.gt.0) write(*,*) 'set_origin_global_node'
      call set_origin_global_node(comm_org)
!
      write (*,'(//,"*** ",i5," SUBDOMAINS",//)') num_domain
      if (num_domain .gt. node_org%numnod) then
        call ERROR_EXIT(ierr_P_MPI,node_org%numnod)
      end if
!
      end subroutine  initialize_partitioner
!
!   --------------------------------------------------------------------
!
      end module init_partitioner
