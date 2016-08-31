!write_filters_4_each_node.f90
!      module write_filters_4_each_node
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine write_each_filter_stack_coef(file_name, inod)
!      subroutine write_each_no_filter_coef(file_name, inod)
!      subroutine write_each_same_filter_coef(file_name, inod)
!
!      subroutine read_each_filter_stack_coef(id_file)
!
      module write_filters_4_each_node
!
      use m_precision
!
      use m_constants
      use calypso_mpi
      use m_filter_coefs
      use m_filter_file_names
      use m_field_file_format
      use binary_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_each_filter_stack_coef(file_name, inod)
!
      use filter_IO_for_sorting
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: inod
!
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name,                      &
     &       form='formatted', position='append')
        write(filter_coef_code,'(4i12)')  inod, nnod_near_1nod_weight,  &
     &                  i_exp_level_1nod_weight
        close(org_filter_coef_code)
        call write_filter_coef_4_each(filter_coef_code)
!
      else if (ifmt_3d_filter .eq. iflag_bin) then
        call open_append_binary_file(file_name)
        call write_filter_coef_4_each_b
        call close_binary_file
      end if
!
      end subroutine write_each_filter_stack_coef
!
! -----------------------------------------------------------------------
!
      subroutine write_each_no_filter_coef(file_name, inod)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: inod
!
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name,                      &
     &       form='formatted', position='append')
        write(org_filter_coef_code,'(4i12)')                            &
     &           inod, nnod_near_1nod_weight, izero
        close(org_filter_coef_code)
      else if (ifmt_3d_filter .eq. iflag_bin) then
        call open_append_binary_file(file_name)
        call write_one_integer_b(nnod_near_1nod_weight)
        call write_one_integer_b(izero)
        call close_binary_file
      end if
!
      end subroutine write_each_no_filter_coef
!
! -----------------------------------------------------------------------
!
      subroutine write_each_same_filter_coef(file_name, inod)
!
      use filter_coefs_file_IO
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: inod
!
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name,                      &
     &       form='formatted', position='append')
        call read_filter_geometry(org_filter_coef_code)
        write(org_filter_coef_code,'(4i12)') inod,                      &
     &          (-nnod_near_1nod_weight), i_exp_level_1nod_weight
        close(org_filter_coef_code)
      else if (ifmt_3d_filter .eq. iflag_bin) then
        call open_append_binary_file(file_name)
        call write_one_integer_b(-nnod_near_1nod_weight)
        call write_one_integer_b(i_exp_level_1nod_weight)
        call close_binary_file
      end if
!
      end subroutine write_each_same_filter_coef
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_each_filter_stack_coef(id_file)
!
      use skip_comment_f
      use filter_IO_for_sorting
!
      integer(kind = kint), intent(in) :: id_file
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        call read_filter_coef_4_each(id_file)
      else if (ifmt_3d_filter .eq. iflag_bin) then
        call read_filter_coef_4_each_b
      end if
!
      end subroutine read_each_filter_stack_coef
!
! -----------------------------------------------------------------------
!
      end module write_filters_4_each_node
