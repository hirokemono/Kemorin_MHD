!analyzer_filter_newdom_sgl.f90
!      module analyzer_filter_newdom_sgl
!..................................................
!
!      modified by H. Matsui on Apr., 2008
!
!      subroutine newdomain_filter_init
!      subroutine newdomain_filter_analyze
!
      module analyzer_filter_newdom_sgl
!
      use m_precision
      use m_machine_parameter
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
      subroutine newdomain_filter_init
!
      use calypso_mpi
      use m_ctl_data_newdomain_filter
      use m_ctl_param_newdom_filter
      use const_domain_tbl_by_file
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
      if (iflag_debug.eq.1) write(*,*) 'read_control_filter_newdomain'
      call read_control_filter_newdomain
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_filter_newdomain'
      nprocs_2nd = 0
      call set_control_filter_newdomain(ffile_ndom_ctl, ierr)
      if(ierr .gt. 0) stop
!
!
      if (iflag_debug.eq.1) write(*,*) 's_const_domain_tbl_by_file'
      call s_const_domain_tbl_by_file(tgt_mesh_file)
!
      end subroutine newdomain_filter_init
!
! ----------------------------------------------------------------------
!
      subroutine newdomain_filter_analyze
!
      use m_ctl_data_newdomain_filter
      use local_newdomain_filter
      use filters_for_newdomains
      use trans_filter_moms_newdomain
!
!
      if (iflag_debug.eq.1) write(*,*) 'local_newdomain_filter_sngl'
      call local_newdomain_filter_sngl                                  &
     &   (org_mesh_file, orgmesh%node, orgmesh%ele, newmesh)
!
      if (iflag_debug.eq.1) write(*,*) 'trans_filter_moms_newmesh_sgl'
      if (iflag_set_filter_elen .gt. 0                                  &
     &  .or. iflag_set_filter_moms.gt.0) then
        call trans_filter_moms_newmesh_sgl                              &
     &     (orgmesh, org_ele_mesh, newmesh, new_ele_mesh)
      end if
!
      if (iflag_set_filter_coef .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'filters_4_newdomains_single'
        call filters_4_newdomains_single(org_mesh_file,                 &
     &      filtering_nd, orgmesh%node, orgmesh%ele, newmesh)
      end if
!
!
      end subroutine newdomain_filter_analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_filter_newdom_sgl
