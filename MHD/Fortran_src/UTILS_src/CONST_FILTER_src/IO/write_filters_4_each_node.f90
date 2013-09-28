!write_filters_4_each_node.f90
!      module write_filters_4_each_node
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine write_each_filter_stack_coef(inod)
!      subroutine write_each_no_filter_coef(inod)
!      subroutine write_each_same_filter_coef(inod)
!
!      subroutine read_each_filter_stack_coef(id_file)
!
      module write_filters_4_each_node
!
      use m_precision
!
      use m_constants
      use m_parallel_var_dof
      use m_filter_coefs
      use m_filter_file_names
      use m_field_file_format
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_each_filter_stack_coef(inod)
!
      use filter_IO_for_sorting
!
      integer(kind = kint), intent(in) :: inod
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        write(filter_coef_code,'(4i12)')  inod, nnod_near_1nod_weight,  &
     &                  i_exp_level_1nod_weight
        call write_filter_coef_4_each(filter_coef_code)
!
      else if (ifmt_3d_filter .eq. iflag_bin) then
!
        write(filter_coef_code) nnod_near_1nod_weight,                  &
     &           i_exp_level_1nod_weight
        call write_filter_coef_4_each_b(filter_coef_code)
!
      end if
!
      end subroutine write_each_filter_stack_coef
!
! -----------------------------------------------------------------------
!
      subroutine write_each_no_filter_coef(inod)
!
      integer(kind = kint), intent(in) :: inod
!
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        write(filter_coef_code,'(4i12)') inod,                          &
     &            nnod_near_1nod_weight, izero
      else if (ifmt_3d_filter .eq. iflag_bin) then
        write(filter_coef_code) nnod_near_1nod_weight
      end if
!
      end subroutine write_each_no_filter_coef
!
! -----------------------------------------------------------------------
!
      subroutine write_each_same_filter_coef(inod)
!
      integer(kind = kint), intent(in) :: inod
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        write(filter_coef_code,'(4i12)') inod,                          &
     &          (-nnod_near_1nod_weight), i_exp_level_1nod_weight
      else if (ifmt_3d_filter .eq. iflag_bin) then
        write(filter_coef_code) (-nnod_near_1nod_weight)
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
      integer(kind = kint) :: inod
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
!
        call skip_comment(character_4_read, id_file)
        read(character_4_read,*)  inod, nnod_near_1nod_weight,          &
     &                                  i_exp_level_1nod_weight
!
        call read_filter_coef_4_each(id_file)
!
      else if (ifmt_3d_filter .eq. iflag_bin) then
!
        read(id_file) nnod_near_1nod_weight, i_exp_level_1nod_weight
        if (nnod_near_1nod_weight.gt. 0) then
          call read_filter_coef_4_each_b(id_file)
        end if
!
      end if
!
      end subroutine read_each_filter_stack_coef
!
! -----------------------------------------------------------------------
!
      end module write_filters_4_each_node
