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
!!     &         (file_name, my_rank, IO_filters)
!!      subroutine write_sorted_filter_coef_file_b                      &
!!     &         (file_name, my_rank, IO_filters)
!!        type(filter_coefficients_type), intent(in) :: IO_filters
!!
!!      subroutine read_filter_geometry_file_b(file_name, my_rank)
!!      subroutine write_filter_geometry_file_b(file_name, my_rank)
!!
!!      subroutine read_filter_geometry_b
!!      subroutine write_filter_geometry_b
!!@endverbatim
!
      module filter_coefs_file_IO_b
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
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
     &         (file_name, my_rank, IO_filters)
!
      use m_filter_file_names
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter file: ', trim(file_name)
      end if
!
      call open_read_binary_file(file_name, my_rank)
      call read_filter_geometry_b
      call read_3d_filter_stack_b(IO_filters)
      call read_3d_filter_weights_coef_b(IO_filters)
      call close_binary_file
!
      end subroutine read_sorted_filter_coef_file_b
!
!------------------------------------------------------------------
!
      subroutine write_sorted_filter_coef_file_b                        &
     &         (file_name, my_rank, IO_filters)
!
      use m_filter_file_names
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      type(filter_coefficients_type), intent(in) :: IO_filters
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter file: ', trim(file_name)
      end if
!
      call open_write_binary_file(file_name)
      call write_filter_geometry_b
      call write_3d_filter_stack_b(IO_filters)
      call write_3d_filter_weights_coef_b(IO_filters)
!
      call close_binary_file
!
      end subroutine write_sorted_filter_coef_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_filter_geometry_file_b(file_name, my_rank)
!
      use m_filter_file_names
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter file: ', trim(file_name)
      end if
!
      call open_read_binary_file(file_name, my_rank)
      call read_filter_geometry_b
      call close_binary_file
!
      end subroutine read_filter_geometry_file_b
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_file_b(file_name, my_rank)
!
      use m_filter_file_names
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter file: ', trim(file_name)
      end if
!
      call open_write_binary_file(file_name)
      call write_filter_geometry_b
      call close_binary_file
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
      call read_fld_inthead_b(IO_filters%ngrp_node)
      call alloc_num_filtering_comb(ione, IO_filters)
!
      call read_fld_intstack_b(IO_filters%ngrp_node,                    &
     &    IO_filters%istack_node, IO_filters%ntot_nod)
      call read_fld_mul_charhead_b                                      &
     &   (IO_filters%ngrp_node, IO_filters%group_name)
!
      call s_cal_numbers_from_stack(IO_filters%ngrp_node,               &
     &    IO_filters%num_node, IO_filters%istack_node)
!
      call alloc_inod_filter_comb(IO_filters)
!
      call read_fld_mul_inthead_b                                       &
     &   (IO_filters%ntot_nod, IO_filters%inod_filter)
      call read_fld_intstack_b(IO_filters%ntot_nod,                     &
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
      subroutine read_filter_geometry_b
!
      use domain_data_IO_b
      use mesh_data_IO_b
!
!
      call read_domain_info_b
      call read_number_of_node_b
      call read_geometry_info_b
!
! ----  import & export 
!
      call read_import_data_b
      call read_export_data_b
!
      end subroutine read_filter_geometry_b
!
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
      call read_fld_mul_inthead_b                                      &
     &  (IO_filters%ntot_near_nod, IO_filters%inod_near)
      call read_fld_realarray_b                                        &
     &  (IO_filters%ntot_near_nod, IO_filters%func)
      call read_fld_realarray_b                                        &
     &  (IO_filters%ntot_near_nod, IO_filters%weight)
!
      end subroutine read_3d_filter_weights_coef_b
!
!  ---------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_b
!
      use mesh_data_IO_b
      use domain_data_IO_b
!
!
      call write_domain_info_b
!
      call write_geometry_info_b
!
      call write_import_data_b
      call write_export_data_b
!
      end subroutine write_filter_geometry_b
!
!------------------------------------------------------------------
!
      subroutine write_3d_filter_stack_b(IO_filters)
!
      use binary_IO
!
      type(filter_coefficients_type), intent(in) :: IO_filters
!
!
      call write_fld_inthead_b(IO_filters%ngrp_node)
      call write_fld_intstack_b                                         &
     &   (IO_filters%ngrp_node, IO_filters%istack_node)
      call write_fld_mul_charhead_b                                     &
     &   (IO_filters%ngrp_node, IO_filters%group_name)
!
      call write_fld_mul_inthead_b                                      &
     &   (IO_filters%ntot_nod, IO_filters%inod_filter)
      call write_fld_intstack_b                                         &
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
      call write_fld_mul_inthead_b                                      &
     &  (IO_filters%ntot_near_nod, IO_filters%inod_near)
      call write_fld_realarray_b                                        &
     &  (IO_filters%ntot_near_nod, IO_filters%func)
      call write_fld_realarray_b                                        &
     &  (IO_filters%ntot_near_nod, IO_filters%weight)
!
      end subroutine write_3d_filter_weights_coef_b
!
!  ---------------------------------------------------------------------
!
      end module filter_coefs_file_IO_b
