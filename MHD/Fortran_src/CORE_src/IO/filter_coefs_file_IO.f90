!>@file   filter_coefs_file_IO.f90
!!@brief  module filter_coefs_file_IO
!!
!!@author H. Matsui
!!@date Programmed in 2004
!
!> @brief ASCII filter data file IO
!!
!!@verbatim
!!      subroutine read_sorted_filter_coef_file(file_name, my_rank)
!!      subroutine write_sorted_filter_coef_file(file_name, my_rank)
!!      subroutine read_filter_geometry_file(file_name, my_rank)
!!      subroutine write_filter_geometry_file(file_name, my_rank)
!!
!!      subroutine read_sorted_filter_coef_file_b(file_name, my_rank)
!!      subroutine write_sorted_filter_coef_file_b(file_name, my_rank)
!!      subroutine read_filter_geometry_file_b(file_name, my_rank)
!!      subroutine write_filter_geometry_file_b(file_name, my_rank)
!!@endverbatim
!
      module filter_coefs_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
!
        implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_sorted_filter_coef_file(file_name, my_rank)
!
      use m_filter_file_names
      use filter_geometry_IO
      use filter_coef_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read ascii filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name,                            &
     &      form='formatted', status= 'old')
      call read_filter_geometry(filter_coef_code)
      call read_3d_filter_stack(filter_coef_code)
      call read_3d_filter_weights_coef(filter_coef_code)
      close (filter_coef_code)
!
      end subroutine read_sorted_filter_coef_file
!
!------------------------------------------------------------------
!
      subroutine write_sorted_filter_coef_file(file_name, my_rank)
!
      use m_filter_file_names
      use filter_geometry_IO
      use filter_coef_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write ascii filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name, form='formatted')
      call write_filter_geometry(filter_coef_code)
      call write_3d_filter_stack(filter_coef_code)
      call write_3d_filter_weights_coef(filter_coef_code)
      close(filter_coef_code)
!
      end subroutine write_sorted_filter_coef_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_filter_geometry_file(file_name, my_rank)
!
      use m_filter_file_names
      use filter_geometry_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read ascii filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name,                            &
     &      form='formatted', status= 'old')
      call read_filter_geometry(filter_coef_code)
      close (filter_coef_code)
!
      end subroutine read_filter_geometry_file
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_file(file_name, my_rank)
!
      use m_filter_file_names
      use filter_geometry_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write ascii filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name, form='formatted')
      call write_filter_geometry(filter_coef_code)
!
      close(filter_coef_code)
!
      end subroutine write_filter_geometry_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_sorted_filter_coef_file_b(file_name, my_rank)
!
      use m_filter_file_names
      use filter_geometry_IO
      use filter_coef_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name,                            &
     &      form='unformatted', status= 'old')
      call read_filter_geometry_b(filter_coef_code)
      call read_3d_filter_stack_b(filter_coef_code)
      call read_3d_filter_weights_coef_b(filter_coef_code)
      close (filter_coef_code)
!
      end subroutine read_sorted_filter_coef_file_b
!
!------------------------------------------------------------------
!
      subroutine write_sorted_filter_coef_file_b(file_name, my_rank)
!
      use m_filter_file_names
      use filter_geometry_IO
      use filter_coef_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name,                            &
     &      form='unformatted')
      call write_filter_geometry_b(filter_coef_code)
      call write_3d_filter_stack_b(filter_coef_code)
      call write_3d_filter_weights_coef_b(filter_coef_code)
!
      close(filter_coef_code)
!
      end subroutine write_sorted_filter_coef_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_filter_geometry_file_b(file_name, my_rank)
!
      use m_filter_file_names
      use filter_geometry_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read binary filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name,                            &
     &      form='unformatted', status= 'old')
      call read_filter_geometry_b(filter_coef_code)
      close (filter_coef_code)
!
      end subroutine read_filter_geometry_file_b
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_file_b(file_name, my_rank)
!
      use m_filter_file_names
      use filter_geometry_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write binary filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name,                            &
     &      form='unformatted')
      call write_filter_geometry_b(filter_coef_code)
      close(filter_coef_code)
!
      end subroutine write_filter_geometry_file_b
!
!------------------------------------------------------------------
!
      end module filter_coefs_file_IO
