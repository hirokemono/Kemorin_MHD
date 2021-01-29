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
      use m_work_time
      use calypso_mpi
      use m_2nd_pallalel_vector
      use filters_for_newdomains
      use t_mesh_data
      use t_filtering_data
      use t_domain_group_4_partition
      use t_internal_4_partitioner
      use t_partitioner_comm_table
      use t_filter_coefs
      use t_filter_func_4_sorting
      use t_ctl_param_newdom_filter
!
      implicit none
!
      type(mesh_geometry), save ::    orgmesh
      type(mesh_geometry), save ::    newmesh
!
      type(ctl_param_newdom_filter), save :: newfil_p1
!
      type(filtering_data_type), save :: filtering_nd
      type(domain_groups_4_partitioner), save :: domain_grp1
      type(internal_4_partitioner), save :: itl_nod_part1
      type(partitioner_comm_tables), save :: comm_part1
      type(each_filter_coef), save :: fil_coef1
      type(filters_4_sorting), save :: fils_sort1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine filter_to_newdomain_init
!
      use t_ctl_data_newdomain_filter
      use const_domain_tbl_by_file
!
      use bcast_nodes_for_trans
!
      type(ctl_data_newdomain_filter) :: newd_fil_ctl1
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
      call init_elapse_time_by_TOTAL
      call elpsed_label_3dfilter
!
!     --------------------- 
!
      call copy_num_processes_to_2nd
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_filter_newdomain'
      call read_control_filter_newdomain(newd_fil_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_filter_newdomain'
      call set_control_filter_newdomain                                 &
     &   (nprocs_2nd, newd_fil_ctl1%org_filter_plt,                     &
     &    newd_fil_ctl1%new_filter_plt, newd_fil_ctl1%ffile_ndom_ctl,   &
     &    newd_fil_ctl1%org_filter_file_ctls, newfil_p1, ierr)
!
!
      if (iflag_debug.eq.1) write(*,*) 'bcast_parallel_domain_tbl'
      call bcast_parallel_domain_tbl                                    &
     &   (nprocs_2nd, newfil_p1%tgt_mesh_file, domain_grp1%nod_d_grp)
!
      domain_grp1%ele_d_grp%num_s_domin = 0
      call alloc_domain_group(domain_grp1%ele_d_grp)
      call alloc_local_id_tbl(domain_grp1%ele_d_grp)
!
      end subroutine filter_to_newdomain_init
!
! ----------------------------------------------------------------------
!
      subroutine filter_to_newdomain_analyze
!
      use local_newdomain_filter
      use trans_filter_moms_newdomain
!
!
      if (iflag_debug.eq.1) write(*,*) 'local_newdomain_filter_para'
      call local_newdomain_filter_para                                  &
     &   (nprocs_2nd, newfil_p1, itl_nod_part1, domain_grp1%nod_d_grp,  &
     &    comm_part1, orgmesh%node, orgmesh%ele, newmesh, fil_coef1,    &
     &    fils_sort1%whole_fil_sort, fils_sort1%fluid_fil_sort)
!
      if (iflag_debug.eq.1) write(*,*) 'trans_filter_moms_newmesh_para'
      if (newfil_p1%iflag_set_filter_elen .gt. 0                        &
     &  .or. newfil_p1%iflag_set_filter_moms.gt.0) then
        call trans_filter_moms_newmesh_para                             &
     &     (newfil_p1, orgmesh, newmesh)
      end if
!
      if (newfil_p1%iflag_set_filter_coef .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'filters_4_newdomains_para'
        call filters_4_newdomains_para                                  &
     &     (newfil_p1, filtering_nd, orgmesh%node, orgmesh%ele,         &
     &      domain_grp1%nod_d_grp, newmesh, fil_coef1, fils_sort1)
!
        call dealloc_local_ne_id_tbl(domain_grp1)
      end if
!
      call output_elapsed_times
!
      end subroutine filter_to_newdomain_analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_filter_newdomains

