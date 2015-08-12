!init_partitioner.f90
!      module  init_partitioner
!
!      subroutine initialize_partitioner(nod_grp, ele_grp, sf_grp)
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
      subroutine initialize_partitioner(nod_grp, ele_grp, sf_grp)
!
      use m_error_IDs
      use m_geometry_data
      use m_element_id_4_node
      use m_ctl_param_partitioner
      use m_domain_group_4_partition
      use t_group_data
      use const_mesh_info
      use set_domain_and_org_id
      use quick_mesh_check_for_part
!
      use error_exit_4_part
!
      type(group_data), intent(in) :: nod_grp
      type(group_data), intent(in) :: ele_grp
      type(surface_group_data), intent(in) :: sf_grp
!!    check single grid data
!
      call quick_mesh_chk_4_part(nod_grp, ele_grp, sf_grp)
!
!    construct element and surface data
!
      if (iflag_debug.gt.0) write(*,*) 'const_nod_ele_infos'
      call const_nod_ele_infos
!
      if (iflag_debug.gt.0) write(*,*) 'set_ele_id_4_node'
      call set_ele_id_4_node
!
!   set numbers of global mesh
!
      nnod_s_domin =  node1%numnod
      nele_s_domin =  ele1%numele
      intnod_s_domin = internal_node
!
      call allocate_domain_nod_group
      call allocate_org_gl_nod_id
      call allocate_local_ne_id_tbl
!
!      const periodic table
!
      if (iflag_debug.gt.0) write(*,*) 'set_origin_global_node'
      call set_origin_global_node
!
      write (*,'(//,"*** ",i5," SUBDOMAINS",//)') num_domain
      if (num_domain .gt. node1%numnod) then
        call ERROR_EXIT(ierr_P_MPI,node1%numnod)
      end if
!
      end subroutine  initialize_partitioner
!
!   --------------------------------------------------------------------
!
      end module init_partitioner
