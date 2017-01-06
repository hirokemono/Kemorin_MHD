!analyzer_trans_filter_coef.f90
!      module analyzer_trans_filter_coef
!..................................................
!
!      Written by H. Matsui on Nov., 2010
!
!      subroutine init_analyzer
!
      module analyzer_trans_filter_coef
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
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
      use m_2nd_pallalel_vector
      use m_ctl_data_filter_files
      use m_ctl_data_newdomain_filter
      use m_ctl_param_newdom_filter
      use bcast_nodes_for_trans
!
      integer(kind = kint) :: ierr
!
!
      call copy_num_processes_to_2nd
!
      if (my_rank.eq.0)   write(*,*) 'Transfer filter coefficiens'
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_filter_newdomain'
      call read_control_filter_newdomain
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_filter_newdomain'
      call set_control_filter_newdomain(ffile_ctl1, ierr)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'bcast_parallel_domain_tbl'
      call bcast_parallel_domain_tbl(tgt_mesh_file)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      end module analyzer_trans_filter_coef
