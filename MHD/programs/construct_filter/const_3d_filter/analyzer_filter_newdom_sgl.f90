!analyzer_filter_newdom_sgl.f90
!      module analyzer_filter_newdom_sgl
!..................................................
!
!      modified by H. Matsui on Apr., 2008
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_filter_newdom_sgl
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use calypso_mpi
      use m_read_mesh_data
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
      call set_control_filter_newdomain(ierr)
      if(ierr .gt. 0) stop
!
!
      if (iflag_debug.eq.1) write(*,*) 's_const_domain_tbl_by_file'
      call s_const_domain_tbl_by_file(target_mesh_head)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_ctl_data_newdomain_filter
      use local_newdomain_filter
      use filters_for_newdomains
      use trans_filter_moms_newdomain
!
!
      if (iflag_debug.eq.1) write(*,*) 'local_newdomain_filter_sngl'
      call local_newdomain_filter_sngl
!
      if (iflag_debug.eq.1) write(*,*) 'trans_filter_moms_newmesh_sgl'
      if (iflag_set_filter_elen .gt. 0                                  &
     &  .or. iflag_set_filter_moms.gt.0) then
        call trans_filter_moms_newmesh_sgl
      end if
!
      if (iflag_set_filter_coef .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'filters_4_newdomains_single'
        call filters_4_newdomains_single
      end if
!
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_filter_newdom_sgl
