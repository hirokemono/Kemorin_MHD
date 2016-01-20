!
!      module m_filter_elength
!
!     Written by H. Matsui
!
!      subroutine copy_filter_elen_ele_from_type(elen_e)
!      subroutine copy_filter_elen_ele_to_type(elen_e)
!
      module m_filter_elength
!
      use m_precision
      use t_filter_elength
!
      implicit none
!
      type(gradient_model_data_type), save :: FEM1_elen
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_elen_ele_from_type(elen_e)
!
      type(elen_ele_diffs_type), intent(in)  :: elen_e
!
!
      call copy_elength_type(FEM1_elen%nele_filter_mom,                 &
     &    elen_e%moms,  FEM1_elen%elen_ele%moms)
      call copy_elen_diffs_type(FEM1_elen%nele_filter_mom,              &
     &    elen_e%diff, FEM1_elen%elen_ele%diff)
      call copy_elen_diffs_type(FEM1_elen%nele_filter_mom,              &
     &    elen_e%diff2, FEM1_elen%elen_ele%diff2)
!
      end subroutine copy_filter_elen_ele_from_type
!
!  ---------------------------------------------------------------------
!
      subroutine copy_filter_elen_ele_to_type(elen_e)
!
      type(elen_ele_diffs_type), intent(inout) :: elen_e
!
!
      call copy_elength_type(FEM1_elen%nele_filter_mom,                 &
     &    FEM1_elen%elen_ele%moms, elen_e%moms)
      call copy_elen_diffs_type(FEM1_elen%nele_filter_mom,              &
     &    FEM1_elen%elen_ele%diff, elen_e%diff)
      call copy_elen_diffs_type(FEM1_elen%nele_filter_mom,              &
     &    FEM1_elen%elen_ele%diff2, elen_e%diff2)
!
      end subroutine copy_filter_elen_ele_to_type
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_3d_filter_moments(numnod, numele)
!
      use calypso_mpi
      use m_machine_parameter
      use m_error_IDs
      use m_control_parameter
      use m_filter_file_names
      use filter_moment_IO_select
!
      integer(kind = kint), intent(in) :: numnod, numele
!
      integer(kind = kint) :: ierr
!
!
      if(iflag_SGS_model .eq. id_SGS_NL_grad) then
        ifmt_filter_file = ifmt_filter_elen
        filter_file_head = filter_elen_head
        call sel_read_filter_elen_file                                  &
     &      (my_rank, numnod, numele, FEM1_elen, ierr)
!
        if (ierr.eq.500) then
          write(e_message,*)                                            &
     &        'Check num. of node in mesh and filter file for', my_rank
          call calypso_MPI_abort(ierr_file, e_message)
        else if (ierr.eq.501) then
          write(e_message,*)                                            &
     &        'Check num. of element in mesh and filter for', my_rank
          call calypso_MPI_abort(ierr_file, e_message)
        end if
      end if
!
      end subroutine read_3d_filter_moments
!
!-----------------------------------------------------------------------
!
      subroutine read_line_filtering_data(numnod, numele)
!
      use m_machine_parameter
      use m_error_IDs
      use m_control_parameter
      use m_filter_file_names
      use m_filter_coef_combained
      use m_field_file_format
      use read_line_filter_data
      use set_parallel_file_name
      use filter_moment_IO_select
!
      integer(kind = kint), intent(in) :: numnod, numele
!
      integer(kind = kint) :: ierr
!
!
      ifmt_filter_file = ifmt_filter_elen
      filter_file_head = filter_line_head
      call sel_read_filter_elen_file                                    &
     &   (my_rank, numnod, numele, FEM1_elen, ierr)
!
        if (ierr.eq.500) then
          write(e_message,*)                                            &
     &        'Check num. of node in mesh and filter file for', my_rank
          call calypso_MPI_abort(ierr_file, e_message)
        else if (ierr.eq.501) then
          write(e_message,*)                                            &
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
      end module m_filter_elength
