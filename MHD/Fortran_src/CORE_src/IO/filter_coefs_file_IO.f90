!>@file   filter_coefs_file_IO.f90
!!@brief  module filter_coefs_file_IO
!!
!!@author H. Matsui
!!@date Programmed in 2004
!
!> @brief ASCII filter data file IO
!!
!!@verbatim
!!      subroutine read_sorted_filter_coef_file                         &
!!     &          (file_name, my_rank_IO, filter_IO, ierr)
!!      subroutine write_sorted_filter_coef_file                        &
!!     &         (file_name, my_rank_IO, filter_IO)
!!        type(filter_file_data), intent(inout) :: filter_IO
!!
!!      subroutine read_filter_geometry_file                            &
!!     &         (file_name, my_rank_IO, filter_IO, ierr)
!!      subroutine write_filter_geometry_file                           &
!!     &         (file_name, my_rank_IO, filter_IO)
!!        type(filter_file_data), intent(inout) :: filter_IO
!!@endverbatim
!
      module filter_coefs_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_file_format_switch
      use t_filter_file_data
      use t_filter_coefficients
!
      implicit none
!
      private :: read_3d_filter_stack,  read_3d_filter_weights_coef
      private :: write_3d_filter_stack, write_3d_filter_weights_coef
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_sorted_filter_coef_file                           &
     &          (file_name, my_rank_IO, filter_IO, ierr)
!
      use mesh_data_IO
      use m_filter_file_names
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read ascii filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name,                            &
     &      form='formatted', status= 'old')
      call read_filter_geometry(filter_coef_code, my_rank_IO,           &
     &    filter_IO%nod_comm, filter_IO%node, ierr)
      call read_3d_filter_stack(filter_coef_code, filter_IO%filters)
      call read_3d_filter_weights_coef                                  &
     &   (filter_coef_code, filter_IO%filters)
      close (filter_coef_code)
!
      end subroutine read_sorted_filter_coef_file
!
!------------------------------------------------------------------
!
      subroutine write_sorted_filter_coef_file                          &
     &         (file_name, my_rank_IO, filter_IO)
!
      use mesh_data_IO
      use m_filter_file_names
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write ascii filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name, form='formatted')
      call write_filter_geometry(filter_coef_code, my_rank_IO,          &
     &    filter_IO%nod_comm, filter_IO%node)
      call write_3d_filter_stack(filter_coef_code, filter_IO%filters)
      call write_3d_filter_weights_coef                                 &
     &   (filter_coef_code, filter_IO%filters)
      close(filter_coef_code)
!
      call deallocate_type_neib_id(filter_IO%nod_comm)
      call deallocate_type_import(filter_IO%nod_comm)
      call deallocate_type_export(filter_IO%nod_comm)
      call dealloc_node_geometry_base(filter_IO%node)
!
      end subroutine write_sorted_filter_coef_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_filter_geometry_file                              &
     &         (file_name, my_rank_IO, filter_IO, ierr)
!
      use mesh_data_IO
      use m_filter_file_names
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read ascii filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name,                            &
     &      form='formatted', status= 'old')
      call read_filter_geometry(filter_coef_code, my_rank_IO,           &
     &    filter_IO%nod_comm, filter_IO%node, ierr)
      close (filter_coef_code)
!
      end subroutine read_filter_geometry_file
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_file                             &
     &         (file_name, my_rank_IO, filter_IO)
!
      use mesh_data_IO
      use m_filter_file_names
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
!
!
      if(my_rank_IO.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write ascii filter file: ', trim(file_name)
      end if
!
      open(filter_coef_code, file=file_name, form='formatted')
      call write_filter_geometry(filter_coef_code, my_rank_IO,          &
     &    filter_IO%nod_comm, filter_IO%node)
!
      close(filter_coef_code)
!
      call deallocate_type_neib_id(filter_IO%nod_comm)
      call deallocate_type_import(filter_IO%nod_comm)
      call deallocate_type_export(filter_IO%nod_comm)
      call dealloc_node_geometry_base(filter_IO%node)
!
      end subroutine write_filter_geometry_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_3d_filter_stack(id_file, IO_filters)
!
      use skip_comment_f
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
      integer(kind = kint) :: i, j, ist, ied
      character(len=255) :: character_4_read
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) IO_filters%ngrp_node
!
      call alloc_num_filtering_comb(ione, IO_filters)
!
      read(id_file,*) IO_filters%istack_node(1:IO_filters%ngrp_node)
!
      call s_cal_numbers_from_stack(IO_filters%ngrp_node,               &
     &    IO_filters%num_node, IO_filters%istack_node)
      IO_filters%ntot_nod                                               &
     &     = IO_filters%istack_node(IO_filters%ngrp_node)
!
      call alloc_inod_filter_comb(IO_filters)
!
      do i = 1, IO_filters%ngrp_node
        ist = IO_filters%istack_node(i-1)+1
        ied = IO_filters%istack_node(i)
        read(id_file,*) IO_filters%group_name(i)
        do j = ist, ied
          read(id_file,*) IO_filters%inod_filter(j),                    &
     &                    IO_filters%istack_near_nod(j)
        end do
      end do
!
      call s_cal_numbers_from_stack(IO_filters%ntot_nod,                &
     &    IO_filters%nnod_near, IO_filters%istack_near_nod)
      IO_filters%ntot_near_nod                                          &
     &       = IO_filters%istack_near_nod(IO_filters%ntot_nod)
!
      end subroutine read_3d_filter_stack
!
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_weights_coef(id_file, IO_filters)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
      integer(kind = kint) :: j, itmp
      character(len=255) :: character_4_read
!
!
      call alloc_3d_filter_comb(IO_filters)
      call alloc_3d_filter_func(IO_filters)
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) itmp
!
      do j = 1, IO_filters%ntot_near_nod
        read(id_file,*) itmp, IO_filters%inod_near(j),                  &
     &                  IO_filters%func(j), IO_filters%weight(j)
      end do
!
      end subroutine read_3d_filter_weights_coef
!
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_stack(id_file, IO_filters)
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(in) :: IO_filters
!
      integer(kind = kint) :: i, j, ist, ied
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! nodes for filtering'
      write(id_file,'(a)') '!'
!
      write(id_file,'(i12)') IO_filters%ngrp_node
      write(id_file,'(10i12)')                                          &
     &              IO_filters%istack_node(1:IO_filters%ngrp_node)
!
      do i = 1, IO_filters%ngrp_node
        ist = IO_filters%istack_node(i-1)+1
        ied = IO_filters%istack_node(i)
        write(id_file,'(a)') trim(IO_filters%group_name(i))
        do j = ist, ied
          write(id_file,'(3i12)') IO_filters%inod_filter(j),            &
     &                            IO_filters%istack_near_nod(j)
        end do
      end do
!
      end subroutine write_3d_filter_stack
!
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_weights_coef(id_file, IO_filters)
!
      integer(kind = kint), intent(in) :: id_file
      type(filter_coefficients_type), intent(in) :: IO_filters
!
      integer(kind = kint) :: j
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!   filter coefficients'
      write(id_file,'(a)') '!'
      write(id_file,'(i12)') IO_filters%ntot_near_nod
      do j = 1, IO_filters%ntot_near_nod
        write(id_file,'(2i12,1p2E25.15e3)') j, IO_filters%inod_near(j), &
     &     IO_filters%func(j), IO_filters%weight(j)
      end do
!
      end subroutine write_3d_filter_weights_coef
!
!  ---------------------------------------------------------------------
!
      end module filter_coefs_file_IO
