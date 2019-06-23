!analyzer_sorting_3dfilter.f90
!      module analyzer_sorting_3dfilter
!
!      modified by H. Matsui on Mar., 2008
!
!
      module analyzer_sorting_3dfilter
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_mesh_data
      use t_filtering_data
      use t_filter_coefficients
      use t_file_IO_parameter
      use t_ctl_data_gen_3d_filter
      use t_filter_func_4_sorting
      use t_ctl_params_4_gen_filter
!
      implicit none
!
      type(ctl_data_gen_3d_filter), save :: filter3d_ctl1
      type(ctl_params_4_gen_filter), save :: gfil_p1
      type(field_IO_params), save ::  mesh_filter_file
      type(mesh_geometry), save :: mesh_filter
      type(filtering_data_type), save :: filtering_gen
      type(filter_func_4_sorting), save :: whole_fil_sort1
      type(filter_func_4_sorting), save :: fluid_fil_sort1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sort_3dfilter_init
!
      use set_ctl_gen_filter
      use set_control_platform_data
!
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_sort_filter'
      call read_control_4_sort_filter(filter3d_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_controls_sort_3dfilter'
      call set_controls_sort_3dfilter                                   &
     &   (filter3d_ctl1, mesh_filter_file, gfil_p1, nprocs)
      call dealloc_const_filter_ctl_data(filter3d_ctl1)
!
      end subroutine sort_3dfilter_init
!
! ----------------------------------------------------------------------
!
      subroutine sort_3dfilter_analyze
!
      use m_filter_file_names
      use m_nod_filter_comm_table
!
      use t_filter_file_data
      use t_filter_coefficients
      use t_filter_coefs
!
      use load_mesh_data
      use sorting_by_filtering_area
      use filter_moment_IO_select
      use read_filter_file_4_sorting
      use set_filter_geometry_4_IO
!
      integer :: ip
      integer(kind = kint) :: ierr
      type (filter_file_data) :: filter_IO
      type(const_filter_coefs) :: fil_gen1
!
!
!  ---------------------------------------------------
!       read original filter file
!  ---------------------------------------------------
!
      do ip = 1, nprocs
        my_rank = ip-1
!
!  --  read geometry
!
        if (iflag_debug.eq.1) write(*,*) 'input_mesh_geometry'
        call input_mesh_geometry                                        &
     &     (mesh_filter_file, my_rank, mesh_filter, ierr)
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Error in mesh data')
        end if
!
        call dealloc_node_geometry_w_sph(mesh_filter%node)
        call dealloc_comm_table(mesh_filter%nod_comm)
!
!     read filtering information
!
        call s_read_filter_file_4_sorting                               &
     &     (ifmt_3d_filter, my_rank, filtering_gen, fil_gen1,           &
     &      whole_fil_sort1, fluid_fil_sort1)
!
        call dealloc_filter_func_4_sort(whole_fil_sort1)
        call dealloc_filter_num_4_sort(whole_fil_sort1)
        call dealloc_filter_func_4_sort(fluid_fil_sort1)
        call dealloc_filter_num_4_sort(fluid_fil_sort1)
!
!  ---------------------------------------------------
!     construct filter function
!  ---------------------------------------------------
!
         if(iflag_debug.eq.1)  write(*,*) 's_sorting_by_filtering_area'
        call s_sorting_by_filtering_area                                &
     &     (fil_gen1%nmin_nod_near_all_w, fil_gen1%nmax_nod_near_all_w, &
     &      fil_gen1%fil_sorted, filtering_gen%filter)
!
        call dealloc_3d_filter_func(fil_gen1%fil_sorted)
        call dealloc_3d_filter_weight(fil_gen1%fil_sorted)
        call dealloc_inod_filter_weights(fil_gen1%fil_sorted)
        call deallocate_ele_connect_type(mesh_filter%ele)
!
!  ---------------------------------------------------
!       output filter moment
!  ---------------------------------------------------
!
        call copy_comm_tbl_type(filtering_gen%comm, filter_IO%nod_comm)
        call copy_filtering_geometry_to_IO(filter_IO%node)
!
        call copy_3d_filter_stacks                                      &
     &     (filtering_gen%filter, filter_IO%filters)
        call copy_3d_filter_weight_func                                 &
     &     (filtering_gen%filter, filter_IO%filters)
!
        call deallocate_globalnod_filter
        call dealloc_comm_table(filtering_gen%comm)
!
        ifmt_filter_file = ifmt_3d_filter
        filter_file_head = filter_3d_head
        call sel_write_sort_filter_coef_file(my_rank, filter_IO)
!
!  ---------------------------------------------------
!       output filter moment
!  ---------------------------------------------------
!
        call dealloc_filter_num_sort(fil_gen1%whole_area)
        call dealloc_filter_num_sort(fil_gen1%fluid_area)
      end do
!
!      if (iflag_debug.eq.1) write(*,*) 'exit sort_3dfilter_analyze'
!
        end subroutine sort_3dfilter_analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_sorting_3dfilter
