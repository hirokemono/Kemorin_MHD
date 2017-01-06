!analyzer_filter_newdomains.f90
!      module analyzer_filter_newdomains
!..................................................
!
!      modified by H. Matsui on Nov., 2009
!
!      subroutine filter_to_newdomain_init
!      subroutine filter_to_newdomain_analyze
!
      module analyzer_filter_newdomains
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use m_2nd_pallalel_vector
      use filters_for_newdomains
      use t_mesh_data
      use t_filtering_data
!
      implicit none
!
      type(mesh_geometry), save ::    orgmesh
      type(element_geometry), save :: org_ele_mesh
!
      type(mesh_geometry), save ::    newmesh
      type(element_geometry), save :: new_ele_mesh
!
      type(filtering_data_type), save :: filtering_nd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine filter_to_newdomain_init
!
      use m_ctl_data_filter_files
      use m_ctl_data_newdomain_filter
      use m_ctl_param_newdom_filter
      use const_domain_tbl_by_file
!
      use bcast_nodes_for_trans
!
      integer(kind = kint) :: ierr
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Transfer filtering table to another decomposition'
        write(*,*) 'Required file: '
        write(*,*) 'mesh data for filter function'
        write(*,*) 'mesh data for new gridds'
        write(*,*) 'filter coefficients data on original decomposition'
        write(*,*) 'filter moments data on original decomposition'
        write(*,*) 'element size data on new decomposition'
      end if
!
!     --------------------- 
!
      call copy_num_processes_to_2nd
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_filter_newdomain'
      call read_control_filter_newdomain
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_filter_newdomain'
      call set_control_filter_newdomain(ffile_ctl1, ierr)
!
!
      if (iflag_debug.eq.1) write(*,*) 'bcast_parallel_domain_tbl'
      call bcast_parallel_domain_tbl(tgt_mesh_file)
!
      end subroutine filter_to_newdomain_init
!
! ----------------------------------------------------------------------
!
      subroutine filter_to_newdomain_analyze
!
      use m_ctl_data_newdomain_filter
      use local_newdomain_filter
      use trans_filter_moms_newdomain
!
!
      if (iflag_debug.eq.1) write(*,*) 'local_newdomain_filter_para'
      call local_newdomain_filter_para                                  &
     &   (org_mesh_file, orgmesh%node, orgmesh%ele, newmesh)
!
      if (iflag_debug.eq.1) write(*,*) 'trans_filter_moms_newmesh_para'
      if (iflag_set_filter_elen .gt. 0                                  &
     &  .or. iflag_set_filter_moms.gt.0) then
        call trans_filter_moms_newmesh_para                             &
     &     (orgmesh, org_ele_mesh, newmesh, new_ele_mesh)
      end if
!
      if (iflag_set_filter_coef .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'filters_4_newdomains_para'
        call filters_4_newdomains_para(org_mesh_file,                   &
     &      filtering_nd, orgmesh%node, orgmesh%ele, newmesh)
      end if
!
      end subroutine filter_to_newdomain_analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_filter_newdomains

