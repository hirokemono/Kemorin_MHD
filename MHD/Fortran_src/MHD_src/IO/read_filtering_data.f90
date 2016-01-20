!
!     module read_filtering_data
!
!
!     programmed by H. Matsui
!     modified by H. Matsui on Aug., 2007
!     modified by H. Matsui on May, 2008
!
!      subroutine s_read_filtering_data(node, ele)
!
      module read_filtering_data
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
!
      implicit none
!
      private :: read_3d_filtering_data
      private :: read_w_filtering_data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_read_filtering_data(node, ele)
!
      use m_control_parameter
      use m_filter_elength
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
!
      if (iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
        call read_line_filtering_data(node%numnod, ele%numele)
      else
        call read_3d_filter_moments(node%numnod, ele%numele)
        call read_3d_filtering_data
!
        if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                   &
     &      .and. iflag_SGS_model.eq.id_SGS_similarity) then
          call read_w_filtering_data
        end if
      end if
!
      end subroutine s_read_filtering_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_3d_filtering_data
!
      use m_control_parameter
      use m_filter_file_names
      use m_filter_coef_combained
      use m_nod_filter_comm_table
      use filter_moment_IO_select
      use set_filter_comm_tbl_4_IO
      use set_filter_geometry_4_IO
      use copy_3d_filters_4_IO
      use set_parallel_file_name
!
!
      if (iflag_SGS_filter .gt. 0) then
        ifmt_filter_file = ifmt_3d_filter
        filter_file_head = filter_3d_head
        call sel_read_sort_filter_coef_file(my_rank)
        call copy_filter_comm_tbl_from_IO
        call copy_filtering_geometry_from_IO
!
        call allocate_nod_data_4_filter
!
        call copy_3d_filter_stacks_from_IO
        call copy_3d_filter_weights_from_IO
!
        call deallocate_globalnod_filter
        call deallocate_3d_filter_func_comb
      end if
!
      end subroutine read_3d_filtering_data
!
!-----------------------------------------------------------------------
!
      subroutine read_w_filtering_data
!
      use m_control_parameter
      use m_nod_w_filter_comm_table
      use m_filter_file_names
      use filter_moment_IO_select
      use copy_w_filters_4_IO
!
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                     &
     &      .or. iflag_SGS_model.eq.id_SGS_similarity) then
!
        ifmt_filter_file = ifmt_wide_filter
        filter_file_head = filter_wide_head
        call sel_read_sort_filter_coef_file(my_rank)
        call copy_w_filter_comm_tbl_from_IO
        call copy_w_filter_geometry_from_IO
!
        call allocate_nod_data_w_fil
!
        call copy_w_filter_stacks_from_IO
        call copy_w_filter_weights_from_IO
!
        call deallocate_globalnod_w_fil
      end if
!
      end subroutine read_w_filtering_data
!
!-----------------------------------------------------------------------
!
      end module read_filtering_data
