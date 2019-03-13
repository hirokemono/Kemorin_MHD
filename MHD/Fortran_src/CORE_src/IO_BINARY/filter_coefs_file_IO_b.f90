!>@file   filter_coefs_file_IO_b.f90
!!@brief  module filter_coefs_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in 2004
!
!> @brief ASCII filter data file IO
!!
!!@verbatim
!!      subroutine read_sorted_filter_coef_file_b                       &
!!     &         (file_name, id_rank, filter_IO, ierr)
!!      subroutine write_sorted_filter_coef_file_b                      &
!!     &         (file_name, id_rank, filter_IO)
!!        type(filter_file_data), intent(inout) :: filter_IO
!!
!!      subroutine read_filter_geometry_file_b                          &
!!     &         (file_name, id_rank, filter_IO, ierr)
!!      subroutine write_filter_geometry_file_b                         &
!!     &         (file_name, id_rank, filter_IO)
!!        type(filter_file_data), intent(inout) :: filter_IO
!!@endverbatim
!
      module filter_coefs_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_filter_file_data
      use t_filter_coefficients
      use binary_IO
!
      implicit none
!
      type(file_IO_flags), private :: bin_fcflags
!
      private :: read_3d_filter_stack_b, write_3d_filter_stack_b
      private :: read_3d_filter_weights_coef_b
      private :: write_3d_filter_weights_coef_b
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_sorted_filter_coef_file_b                         &
     &         (file_name, id_rank, filter_IO, ierr)
!
      use mesh_data_IO_b
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter file: ', trim(file_name)
      end if
!
      call open_read_binary_file                                        &
     &   (file_name, id_rank, bin_fcflags%iflag_bin_swap)
      call read_filter_geometry_b                                       &
     &   (id_rank, bin_fcflags, filter_IO%nod_comm, filter_IO%node)
      if(bin_fcflags%ierr_IO .gt. 0) goto 99
!
      call read_3d_filter_stack_b(bin_fcflags, filter_IO%filters)
      if(bin_fcflags%ierr_IO .gt. 0) goto 99
!
      call read_3d_filter_weights_coef_b                                &
     &   (bin_fcflags, filter_IO%filters)
!
  99  continue
      call close_binary_file
      ierr = bin_fcflags%ierr_IO
!
      end subroutine read_sorted_filter_coef_file_b
!
!------------------------------------------------------------------
!
      subroutine write_sorted_filter_coef_file_b                        &
     &         (file_name, id_rank, filter_IO)
!
      use mesh_data_IO_b
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
      type(filter_file_data), intent(inout) :: filter_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter file: ', trim(file_name)
      end if
!
      call open_write_binary_file(file_name)
      call write_filter_geometry_b                                      &
     &   (id_rank, filter_IO%nod_comm, filter_IO%node)
      call write_3d_filter_stack_b(filter_IO%filters)
      call write_3d_filter_weights_coef_b(filter_IO%filters)
!
      call close_binary_file
!
      call dealloc_filter_geometry_data(filter_IO)
!
      end subroutine write_sorted_filter_coef_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_filter_geometry_file_b                            &
     &         (file_name, id_rank, filter_IO, ierr)
!
      use mesh_data_IO_b
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter file: ', trim(file_name)
      end if
!
      call open_read_binary_file                                        &
     &   (file_name, id_rank, bin_fcflags%iflag_bin_swap)
      call read_filter_geometry_b                                       &
     &   (id_rank, bin_fcflags, filter_IO%nod_comm, filter_IO%node)
      call close_binary_file
      ierr = bin_fcflags%ierr_IO
!
      end subroutine read_filter_geometry_file_b
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_file_b                           &
     &         (file_name, id_rank, filter_IO)
!
      use mesh_data_IO_b
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter file: ', trim(file_name)
      end if
!
      call open_write_binary_file(file_name)
      call write_filter_geometry_b                                      &
     &   (id_rank, filter_IO%nod_comm, filter_IO%node)
      call close_binary_file
!
      call dealloc_filter_geometry_data(filter_IO)
!
      end subroutine write_filter_geometry_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_3d_filter_stack_b(bin_flags, IO_filters)
!
      use cal_minmax_and_stacks
      use binary_IO
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
      integer(kind = kint_gl) :: num64
!
!
      call read_one_integer_b(bin_flags%iflag_bin_swap,                 &
     &    IO_filters%ngrp_node, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call alloc_num_filtering_comb(ione, IO_filters)
!
      num64 = IO_filters%ngrp_node
      call read_integer_stack_b(bin_flags%iflag_bin_swap,               &
     &    num64, IO_filters%istack_node,                                &
     &    IO_filters%ntot_nod, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_mul_character_b                                         &
     &   (IO_filters%ngrp_node, IO_filters%group_name,                  &
     &    bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call s_cal_numbers_from_stack(IO_filters%ngrp_node,               &
     &    IO_filters%num_node, IO_filters%istack_node)
!
      call alloc_inod_filter_comb(IO_filters)
!
      num64 = IO_filters%ntot_nod
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    num64, IO_filters%inod_filter,                                &
     &    bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_integer_stack_b(bin_flags%iflag_bin_swap,               &
     &    num64, IO_filters%istack_near_nod,                            &
     &    IO_filters%ntot_near_nod, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call s_cal_numbers_from_stack(IO_filters%ntot_nod,                &
     &    IO_filters%nnod_near, IO_filters%istack_near_nod)
!
      end subroutine read_3d_filter_stack_b
!
!  ---------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_3d_filter_weights_coef_b(bin_flags, IO_filters)
!
      use binary_IO
!
      type(file_IO_flags), intent(inout) :: bin_flags
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
      integer(kind = kint_gl) :: num64
!
!
      call alloc_3d_filter_comb(IO_filters)
      call alloc_3d_filter_func(IO_filters)
!
      num64 = IO_filters%ntot_near_nod
      call read_mul_integer_b(bin_flags%iflag_bin_swap,                 &
     &    num64, IO_filters%inod_near,                                  &
     &    bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      num64 = IO_filters%ntot_near_nod
      call read_1d_vector_b(bin_flags%iflag_bin_swap,                   &
     &    num64, IO_filters%func, bin_flags%ierr_IO)
      if(bin_flags%ierr_IO .gt. 0) return
!
      call read_1d_vector_b(bin_flags%iflag_bin_swap,                   &
     &    num64, IO_filters%weight, bin_flags%ierr_IO)
!
      end subroutine read_3d_filter_weights_coef_b
!
!  ---------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_3d_filter_stack_b(IO_filters)
!
      use binary_IO
!
      type(filter_coefficients_type), intent(in) :: IO_filters
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = IO_filters%ngrp_node
      call write_one_integer_b(IO_filters%ngrp_node)
      call write_integer_stack_b                                        &
     &   (num64, IO_filters%istack_node)
      call write_mul_character_b                                        &
     &   (IO_filters%ngrp_node, IO_filters%group_name)
!
      num64 = IO_filters%ntot_nod
      call write_mul_integer_b                                          &
     &   (num64, IO_filters%inod_filter)
      call write_integer_stack_b                                        &
     &   (num64, IO_filters%istack_near_nod)
!
      end subroutine write_3d_filter_stack_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_weights_coef_b(IO_filters)
!
      use binary_IO
!
      type(filter_coefficients_type), intent(in) :: IO_filters
!
      integer(kind = kint_gl) :: num64
!
!
      num64 = IO_filters%ntot_near_nod
      call write_mul_integer_b(num64, IO_filters%inod_near)
      call write_1d_vector_b(num64, IO_filters%func)
      call write_1d_vector_b(num64, IO_filters%weight)
!
      end subroutine write_3d_filter_weights_coef_b
!
!  ---------------------------------------------------------------------
!
      end module filter_coefs_file_IO_b
