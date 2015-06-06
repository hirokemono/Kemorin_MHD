!init_partitioner.f90
!      module  init_partitioner
!
!      subroutine initialize_partitioner
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
      subroutine initialize_partitioner
!
      use m_error_IDs
      use m_geometry_parameter
      use m_ctl_param_partitioner
      use m_domain_group_4_partition
      use m_read_mesh_data
      use const_mesh_info
      use set_element_id_4_node
      use set_domain_and_org_id
      use quick_mesh_check_for_part
!
      use error_exit_4_part
!
!    check single grid data
!
      call quick_mesh_chk_4_part
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
      nnod_s_domin =  numnod
      nele_s_domin =  numele
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
      if (num_domain .gt. numnod) call ERROR_EXIT(ierr_P_MPI,numnod)
!
      end subroutine  initialize_partitioner
!
!   --------------------------------------------------------------------
!
      end module init_partitioner
