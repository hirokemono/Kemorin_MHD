!analyzer_filter_newdomains.f90
!      module analyzer_filter_newdomains
!..................................................
!
!      modified by H. Matsui on Nov., 2009
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_filter_newdomains
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use m_2nd_pallalel_vector
      use filters_for_newdomains
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
      call set_control_filter_newdomain(ierr)
!
!
      if (iflag_debug.eq.1) write(*,*) 'bcast_parallel_domain_tbl'
      call bcast_parallel_domain_tbl(target_mesh_head)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_ctl_data_newdomain_filter
      use local_newdomain_filter
      use trans_filter_moms_newdomain
!
!
      if (iflag_debug.eq.1) write(*,*) 'local_newdomain_filter_para'
      call local_newdomain_filter_para
!
      if (iflag_debug.eq.1) write(*,*) 'trans_filter_moms_newmesh_para'
      if (iflag_set_filter_elen .gt. 0                                  &
     &  .or. iflag_set_filter_moms.gt.0) then
        call trans_filter_moms_newmesh_para
      end if
!
      if (iflag_set_filter_coef .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'filters_4_newdomains_para'
        call filters_4_newdomains_para
      end if
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_filter_newdomains
