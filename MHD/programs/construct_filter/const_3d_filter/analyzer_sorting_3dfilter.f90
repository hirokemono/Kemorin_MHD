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
      use m_ctl_params_4_gen_filter
!
      use m_ctl_data_gen_3d_filter
      use set_ctl_gen_filter
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
      call read_control_4_sort_filter
!
      if (iflag_debug.eq.1) write(*,*) 'set_file_heads_3d_comm_filter'
      call set_file_heads_3d_comm_filter
      call set_numdomain_3d_comm_filter(nprocs)
!
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_nod_comm_table
      use m_geometry_data
      use m_node_group
      use m_element_group
      use m_surface_group
!
      use m_ctl_params_4_gen_filter
      use m_filter_file_names
      use m_filter_coefs
      use m_filter_coef_combained
      use m_nod_filter_comm_table
      use m_filter_func_4_sorting
!
      use load_mesh_data
      use sorting_by_filtering_area
      use filter_IO_select_4_zlib
      use filter_moments_file_IO
      use read_filter_file_4_sorting
      use set_filter_geometry_4_IO
      use set_filter_comm_tbl_4_IO
      use copy_3d_filters_4_IO
!
      integer(kind=kint ) :: ip
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
        if (iflag_debug.eq.1) write(*,*) 'input_mesh'
        call input_mesh(my_rank)
!
        call deallocate_surface_data
        call deallocate_material_data
        call deallocate_boundary_data
        call deallocate_element_geometry
        call deallocate_node_geometry
        call deallocate_neib_id
        call deallocate_nod_import_item
        call deallocate_nod_export_item
!
!     read filtering information
!
        call s_read_filter_file_4_sorting(ifmt_3d_filter, my_rank)
!
!
        call deallocate_whole_filter_coefs
        call deallocate_fluid_filter_coefs
!
!  ---------------------------------------------------
!     construct filter function
!  ---------------------------------------------------
!
         if(iflag_debug.eq.1)  write(*,*) 's_sorting_by_filtering_area'
        call s_sorting_by_filtering_area
!
        call deallocate_num_near_all_w
        call deallocate_element_connection
!
!  ---------------------------------------------------
!       output filter moment
!  ---------------------------------------------------
!
        call copy_filter_comm_tbl_to_IO(my_rank)
        call copy_filtering_geometry_to_IO
!
        call copy_3d_filter_stacks_to_IO
        call copy_3d_filter_weights_to_IO
!
        call deallocate_globalnod_filter
        call deallocate_filter_import_item
        call deallocate_filter_export_item
        call deallocate_neib_filter_id
!
        ifmt_filter_file = ifmt_3d_filter
        filter_file_head = filter_3d_head
        call sel_write_sort_filter_coef_file(my_rank)
!
!  ---------------------------------------------------
!       output filter moment
!  ---------------------------------------------------
!
        call deallocate_filter_num_sort_IO
      end do
!
!      if (iflag_debug.eq.1) write(*,*) 'exit analyze'
!
        end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_sorting_3dfilter
