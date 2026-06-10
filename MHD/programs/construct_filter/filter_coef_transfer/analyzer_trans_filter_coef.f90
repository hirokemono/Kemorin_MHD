!analyzer_trans_filter_coef.f90
!      module analyzer_trans_filter_coef
!..................................................
!
!      Written by H. Matsui on Nov., 2010
!
!      subroutine init_trans_filter_coef
!
      module analyzer_trans_filter_coef
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
      use m_work_time
      use t_ctl_param_newdom_filter
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &             :: fname_trans_flt_ctl = "ctl_new_domain_filter"
!
      integer, save :: nprocs_2nd
      type(ctl_param_newdom_filter), save :: newfil_p1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_trans_filter_coef
!
      use t_ctl_data_newdomain_filter
      use t_domain_group_4_partition
      use t_ctl_param_newdom_filter
      use input_ctl_filter_newdomain
      use bcast_nodes_for_trans
!
      type(ctl_data_newdomain_filter) :: newd_fil_ctl1
      type(domain_groups_4_partitioner) :: domain_grp1
      integer(kind = kint) :: ierr
!
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_3dfilter
!
      nprocs_2nd = nprocs
!
      if (my_rank.eq.0)   write(*,*) 'Transfer filter coefficiens'
!
!     ---------------------
      if (iflag_debug.eq.1) write(*,*) 's_input_ctl_filter_newdomain'
      call s_input_ctl_filter_newdomain(fname_trans_flt_ctl,            &
     &                                  nprocs_2nd, newfil_p1)
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'bcast_parallel_domain_tbl'
      call bcast_parallel_domain_tbl                                    &
     &   (nprocs_2nd, newfil_p1%tgt_mesh_file, domain_grp1%nod_d_grp)
!
      call output_elapsed_times
!
      end subroutine init_trans_filter_coef
!
! ----------------------------------------------------------------------
!
      end module analyzer_trans_filter_coef
