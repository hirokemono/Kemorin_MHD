!
!     module read_filtering_data
!
!
!     programmed by H. Matsui
!     modified by H. Matsui on Aug., 2007
!     modified by H. Matsui on May, 2008
!
!!      subroutine s_read_filtering_data(SGS_param, filter_param,       &
!!     &          node, ele, FEM_filters, wk_filter)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(filters_on_FEM), intent(inout) :: FEM_filters
!!        type(filtering_work_type), intent(inout) :: wk_filter
!
      module read_filtering_data
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_comm_table
      use t_geometry_data
      use t_FEM_MHD_filter_data
!
      implicit none
!
      private :: read_3d_filtering_data
      private :: read_3d_filter_moments, read_line_filtering_data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_read_filtering_data(SGS_param, filter_param,         &
     &          node, ele, FEM_filters, wk_filter)
!
      use m_nod_filter_comm_table
      use m_filter_file_names
      use t_geometry_data
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(SGS_filtering_params), intent(in) :: filter_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(filters_on_FEM), intent(inout) :: FEM_filters
      type(filtering_work_type), intent(inout) :: wk_filter
!
!
      if(filter_param%iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
        call read_line_filtering_data                                   &
     &     (node%numnod, ele%numele, SGS_param,                         &
     &      FEM_filters%FEM_elens, FEM_filters%filtering%fil_l)
      else
        call read_3d_filter_moments                                     &
     &     (node%numnod, ele%numele, SGS_param, FEM_filters%FEM_elens)
        if(filter_param%iflag_SGS_filter .gt. id_turn_OFF) then
          call read_3d_filtering_data                                   &
     &       (filter_3d_head, ifmt_3d_filter, FEM_filters%filtering)
          call alloc_nod_data_4_filter(nnod_filtering, wk_filter)
        end if
!
        if       (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF       &
     &      .and. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
          call read_3d_filtering_data                                   &
     &       (filter_wide_head, ifmt_wide_filter,                       &
     &        FEM_filters%wide_filtering)
        end if
      end if
!
      end subroutine s_read_filtering_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_3d_filtering_data                                 &
     &         (filter_head, ifmt_filter, filtering)
!
      use t_filter_file_data
!
      use filter_moment_IO_select
      use set_filter_geometry_4_IO
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: filter_head
      integer(kind = kint) , intent(in) :: ifmt_filter
      type(filtering_data_type), intent(inout) :: filtering
!
      type(filter_file_data) :: filter_IO_t
      integer(kind = kint) :: ierr
!
!
      ifmt_filter_file = ifmt_filter
      filter_file_head = filter_head
      call sel_read_sort_filter_coef_file(my_rank, filter_IO_t, ierr)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr, 'Mesh data is wrong!!')
      end if
!
!
      call copy_comm_tbl_type(filter_IO_t%nod_comm, filtering%comm)
      call copy_filtering_geometry_from_IO(filter_IO_t%node)
!
      call copy_3d_filter_stacks(filter_IO_t%filters, filtering%filter)
      call copy_3d_filter_weights(filter_IO_t%filters, filtering%filter)
!
      call dealloc_filter_geometry_data(filter_IO_t)
      call dealloc_3d_filter_func(filter_IO_t%filters)
!
      call deallocate_globalnod_filter
!
      end subroutine read_3d_filtering_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_3d_filter_moments                                 &
     &         (numnod, numele, SGS_param, FEM_elens)
!
      use calypso_mpi
      use m_machine_parameter
      use m_error_IDs
      use m_filter_file_names
!
      use t_filter_elength
!
      use filter_moment_IO_select
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(SGS_model_control_params), intent(in) :: SGS_param
!
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
      integer(kind = kint) :: ierr
!
!
      if(SGS_param%iflag_SGS .ne. id_SGS_NL_grad) return
      ifmt_filter_file = ifmt_filter_elen
      filter_file_head = filter_elen_head
      call sel_read_filter_elen_file                                    &
     &      (my_rank, numnod, numele, FEM_elens, ierr)
!
      if (ierr.eq.500) then
        write(e_message,*)                                              &
     &        'Check num. of node in mesh and filter file for', my_rank
        call calypso_MPI_abort(ierr_file, e_message)
      else if (ierr.eq.501) then
        write(e_message,*)                                              &
     &        'Check num. of element in mesh and filter for', my_rank
        call calypso_MPI_abort(ierr_file, e_message)
      end if
!
      end subroutine read_3d_filter_moments
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_line_filtering_data                               &
     &         (numnod, numele, SGS_param, FEM_elens, fil_l)
!
      use m_machine_parameter
      use m_error_IDs
      use m_filter_file_names
      use m_field_file_format
!
      use t_l_filtering_data
      use t_filter_elength
!
      use set_parallel_file_name
      use filter_mom_type_data_IO
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(gradient_model_data_type), intent(inout) :: FEM_elens
      type(line_filtering_type), intent(inout) :: fil_l
!
      integer(kind = kint) :: ierr
      character(len=kchara) :: file_name
!
!
      file_name = add_process_id(my_rank, filter_line_head)
      open(filter_file_code, file=file_name,                            &
     &        form='formatted', status= 'old')
      call read_filter_elen_data_type(filter_file_code,                 &
     &    numnod, numele, FEM_elens, ierr)
!
      if (ierr.eq.500) then
        write(e_message,*)                                              &
     &        'Check num. of node in mesh and filter file for', my_rank
        call calypso_MPI_abort(ierr_file, e_message)
      else if (ierr.eq.501) then
        write(e_message,*)                                              &
     &        'Check num. of element in mesh and filter for', my_rank
        call calypso_MPI_abort(ierr_file, e_message)
      end if
!
!
      if        (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF        &
     &      .or. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
        call read_line_filter_data_a(filter_file_code, numnod, fil_l)
      end if
      close(filter_file_code)
!
!
      end subroutine read_line_filtering_data
!
!-----------------------------------------------------------------------
!
      end module read_filtering_data
