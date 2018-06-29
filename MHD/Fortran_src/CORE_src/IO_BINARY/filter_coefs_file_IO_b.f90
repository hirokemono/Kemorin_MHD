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
!!     &         (file_name, my_rank_IO, filter_IO, ierr)
!!      subroutine write_sorted_filter_coef_file_b                      &
!!     &         (file_name, my_rank_IO, filter_IO)
!!        type(filter_file_data), intent(inout) :: filter_IO
!!
!!      subroutine read_filter_geometry_file_b                          &
!!     &         (file_name, my_rank_IO, filter_IO, ierr)
!!      subroutine write_filter_geometry_file_b                         &
!!     &         (file_name, my_rank_IO, filter_IO)
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
     &         (file_name, my_rank_IO, filter_IO, ierr)
!
      use mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter file: ', trim(file_name)
      end if
!
      call open_read_binary_file(file_name, my_rank_IO)
      call read_filter_geometry_b                                       &
     &   (my_rank_IO, filter_IO%nod_comm, filter_IO%node, ierr)
      call read_3d_filter_stack_b(filter_IO%filters)
      call read_3d_filter_weights_coef_b(filter_IO%filters)
      call close_binary_file
!
      end subroutine read_sorted_filter_coef_file_b
!
!------------------------------------------------------------------
!
      subroutine write_sorted_filter_coef_file_b                        &
     &         (file_name, my_rank_IO, filter_IO)
!
      use mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
      type(filter_file_data), intent(inout) :: filter_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter file: ', trim(file_name)
      end if
!
      call open_write_binary_file(file_name)
      call write_filter_geometry_b                                      &
     &   (my_rank_IO, filter_IO%nod_comm, filter_IO%node)
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
     &         (file_name, my_rank_IO, filter_IO, ierr)
!
      use mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter file: ', trim(file_name)
      end if
!
      call open_read_binary_file(file_name, my_rank_IO)
      call read_filter_geometry_b                                       &
     &   (my_rank_IO, filter_IO%nod_comm, filter_IO%node, ierr)
      call close_binary_file
!
      end subroutine read_filter_geometry_file_b
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_file_b                           &
     &         (file_name, my_rank_IO, filter_IO)
!
      use mesh_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter file: ', trim(file_name)
      end if
!
      call open_write_binary_file(file_name)
      call write_filter_geometry_b                                      &
     &   (my_rank_IO, filter_IO%nod_comm, filter_IO%node)
      call close_binary_file
!
      call dealloc_filter_geometry_data(filter_IO)
!
      end subroutine write_filter_geometry_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_3d_filter_stack_b(IO_filters)
!
      use cal_minmax_and_stacks
      use binary_IO
!
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
!
      call read_one_integer_b(IO_filters%ngrp_node)
      call alloc_num_filtering_comb(ione, IO_filters)
!
      call read_integer_stack_b(IO_filters%ngrp_node,                   &
     &    IO_filters%istack_node, IO_filters%ntot_nod)
      call read_mul_character_b                                         &
     &   (IO_filters%ngrp_node, IO_filters%group_name)
!
      call s_cal_numbers_from_stack(IO_filters%ngrp_node,               &
     &    IO_filters%num_node, IO_filters%istack_node)
!
      call alloc_inod_filter_comb(IO_filters)
!
      call read_mul_integer_b                                           &
     &   (IO_filters%ntot_nod, IO_filters%inod_filter)
      call read_integer_stack_b(IO_filters%ntot_nod,                    &
     &   IO_filters%istack_near_nod, IO_filters%ntot_near_nod)
!
      call s_cal_numbers_from_stack(IO_filters%ntot_nod,                &
     &    IO_filters%nnod_near, IO_filters%istack_near_nod)
!
      end subroutine read_3d_filter_stack_b
!
!  ---------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_3d_filter_weights_coef_b(IO_filters)
!
      use binary_IO
!
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
!
      call alloc_3d_filter_comb(IO_filters)
      call alloc_3d_filter_func(IO_filters)
!
      call read_mul_integer_b                                           &
     &  (IO_filters%ntot_near_nod, IO_filters%inod_near)
      call read_1d_vector_b(IO_filters%ntot_near_nod, IO_filters%func)
      call read_1d_vector_b                                             &
     &  (IO_filters%ntot_near_nod, IO_filters%weight)
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
!
      call write_one_integer_b(IO_filters%ngrp_node)
      call write_integer_stack_b                                        &
     &   (IO_filters%ngrp_node, IO_filters%istack_node)
      call write_mul_character_b                                        &
     &   (IO_filters%ngrp_node, IO_filters%group_name)
!
      call write_mul_integer_b                                          &
     &   (IO_filters%ntot_nod, IO_filters%inod_filter)
      call write_integer_stack_b                                        &
     &   (IO_filters%ntot_nod, IO_filters%istack_near_nod)
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
!
      call write_mul_integer_b                                          &
     &  (IO_filters%ntot_near_nod, IO_filters%inod_near)
      call write_1d_vector_b(IO_filters%ntot_near_nod, IO_filters%func)
      call write_1d_vector_b                                            &
     &  (IO_filters%ntot_near_nod, IO_filters%weight)
!
      end subroutine write_3d_filter_weights_coef_b
!
!  ---------------------------------------------------------------------
!
      end module filter_coefs_file_IO_b
