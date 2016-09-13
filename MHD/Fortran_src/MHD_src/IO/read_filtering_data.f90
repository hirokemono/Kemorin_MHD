!
!     module read_filtering_data
!
!
!     programmed by H. Matsui
!     modified by H. Matsui on Aug., 2007
!     modified by H. Matsui on May, 2008
!
!!      subroutine s_read_filtering_data                                &
!!     &         (node, ele, filtering, wide_filtering, wk_filter)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(filtering_data_type), intent(inout) :: filtering
!!        type(filtering_data_type), intent(inout) :: wide_filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!
      module read_filtering_data
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
      use t_comm_table
      use t_geometry_data
      use t_filtering_data
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
      subroutine s_read_filtering_data                                  &
     &         (node, ele, filtering, wide_filtering, wk_filter)
!
      use m_control_parameter
      use m_nod_filter_comm_table
      use m_filter_elength
      use m_filter_file_names
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(filtering_data_type), intent(inout) :: filtering
      type(filtering_data_type), intent(inout) :: wide_filtering
      type(filtering_work_type), intent(inout) :: wk_filter
!
!
      if (iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
        call read_line_filtering_data                                   &
     &     (node%numnod, ele%numele, FEM1_elen)
      else
        call read_3d_filter_moments                                     &
     &     (node%numnod, ele%numele, FEM1_elen)
        if(iflag_SGS_filter .gt. 0) then
          call read_3d_filtering_data                                   &
     &       (filter_3d_head, ifmt_3d_filter, filtering)
          call alloc_nod_data_4_filter(nnod_filtering, wk_filter)
        end if
!
        if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                   &
     &      .and. iflag_SGS_model.eq.id_SGS_similarity) then
          call read_3d_filtering_data                                   &
     &       (filter_wide_head, ifmt_wide_filter, wide_filtering)
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
      use m_control_parameter
      use t_filter_file_data
!
      use filter_moment_IO_select
      use set_filter_geometry_4_IO
      use set_parallel_file_name
      use set_comm_table_4_IO
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
      call dealloc_node_geometry_base(filter_IO_t%node)
      call deallocate_type_comm_tbl(filter_IO_t%nod_comm)
      call dealloc_3d_filter_function(filter_IO_t%filters)
!
      call deallocate_globalnod_filter
!
      end subroutine read_3d_filtering_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_3d_filter_moments(numnod, numele, FEM_elens)
!
      use calypso_mpi
      use m_machine_parameter
      use m_error_IDs
      use m_control_parameter
      use m_filter_file_names
!
      use t_filter_elength
!
      use filter_moment_IO_select
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
      integer(kind = kint) :: ierr
!
!
      if(iflag_SGS_model .ne. id_SGS_NL_grad) return
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
      subroutine read_line_filtering_data(numnod, numele, FEM_elens)
!
      use m_machine_parameter
      use m_error_IDs
      use m_control_parameter
      use m_filter_file_names
      use m_field_file_format
!
      use t_filter_elength
!
      use read_line_filter_data
      use set_parallel_file_name
      use filter_moment_IO_select
!
      integer(kind = kint), intent(in) :: numnod, numele
      type(gradient_model_data_type), intent(inout) :: FEM_elens
!
      integer(kind = kint) :: ierr
!
!
      ifmt_filter_file = ifmt_filter_elen
      filter_file_head = filter_line_head
      call sel_read_filter_elen_file                                    &
     &   (my_rank, numnod, numele, FEM_elens, ierr)
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
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                     &
     &      .or. iflag_SGS_model.eq.id_SGS_similarity) then
        if (ifmt_line_filter .eq. iflag_bin) then
          call read_line_filter_data_b(filter_file_code, numnod)
        else
          call read_line_filter_data_a(filter_file_code, numnod)
        end if
      end if
!
!
      end subroutine read_line_filtering_data
!
!-----------------------------------------------------------------------
!
      end module read_filtering_data
