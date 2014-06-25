!> @file  rst_data_IO_by_fld.f90
!!      module rst_data_IO_by_fld
!!
!! @author  H. Matsui
!! @date Programmed in Nov., 2008
!
!> @brief read restart file
!!
!!@verbatim
!!      subroutine read_rst_file(my_rank, file_name)
!!      subroutine read_rst_file_b(my_rank, file_name)
!!
!!      subroutine read_rst_data_comps(my_rank, file_name)
!!      subroutine read_rst_data_comps_b(my_rank, file_name)
!!@endverbatim
!
      module rst_data_IO_by_fld
!
      use m_precision
      use m_machine_parameter
!
      use m_time_data_IO
      use m_field_data_IO
!
      implicit none
!
      private :: read_rst_field_comps, read_rst_field_comps_b
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine read_rst_file(my_rank, file_name)
!
      use set_parallel_file_name
      use field_data_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      character(len=kchara) :: character_4_read
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read ascii restart file: ', trim(file_name)
      open (id_phys_file, file = file_name, form='formatted')
!
      call read_step_data(id_phys_file)
!
      call skip_comment(character_4_read,id_phys_file)
      read(character_4_read,*) num_phys_data_IO
!
      call read_field_data(id_phys_file,                                &
     &          numgrid_phys_IO, num_phys_data_IO, ntot_phys_data_IO,   &
     &          num_phys_comp_IO, phys_data_name_IO, phys_data_IO)
      close (id_phys_file)
!
      end subroutine read_rst_file
!
!------------------------------------------------------------------
!
      subroutine read_rst_file_b(my_rank, file_name)
!
      use set_parallel_file_name
      use field_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read binary restart file: ', trim(file_name)
      open (id_phys_file, file = file_name, form='unformatted')
      call read_step_data_b(id_phys_file)
!
      read(id_phys_file) num_phys_data_IO
      call read_field_data_b(id_phys_file,                              &
     &          numgrid_phys_IO, num_phys_data_IO, ntot_phys_data_IO,   &
     &          phys_data_name_IO, phys_data_IO)
      close (id_phys_file)
!
      end subroutine read_rst_file_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_rst_data_comps(my_rank, file_name)
!
      use set_parallel_file_name
      use field_data_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
      character(len=kchara) :: character_4_read
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read ascii restart file: ', trim(file_name)
      open (id_phys_file, file = file_name, form='formatted')
!
      call read_step_data(id_phys_file)
!
      call skip_comment(character_4_read,id_phys_file)
      read(character_4_read,*) num_phys_data_IO
!
      call allocate_phys_data_name_IO
!
      call read_rst_field_comps
      close (id_phys_file)
!
      call cal_istack_phys_comp_IO
!
      end subroutine read_rst_data_comps
!
!------------------------------------------------------------------
!
      subroutine read_rst_data_comps_b(my_rank, file_name)
!
      use set_parallel_file_name
      use field_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &    'Read binary restart file: ', trim(file_name)
      open (id_phys_file, file = file_name, form='unformatted')
      call read_step_data_b(id_phys_file)
!
      read(id_phys_file) num_phys_data_IO
      call allocate_phys_data_name_IO
!
      call read_rst_field_comps_b
      close (id_phys_file)
!
      call cal_istack_phys_comp_IO
!
      end subroutine read_rst_data_comps_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_rst_field_comps
!
      use skip_comment_f
      use set_restart_data
      use skip_comment_f
!
      character(len=kchara) :: character_4_read
      integer(kind = kint) :: i, inod
      real(kind = kreal) :: rtmp
!
!
      do i = 1, num_phys_data_IO
        call skip_comment(character_4_read,id_phys_file)
        read(character_4_read,*) phys_data_name_IO(i)
!
        call set_num_comps_4_rst(phys_data_name_IO(i),                  &
     &      num_phys_comp_IO(i) )
!
        do inod = 1, numgrid_phys_IO
          read(id_phys_file,*)  rtmp
        end do
      end do
!
      end subroutine read_rst_field_comps
!
! -------------------------------------------------------------------
!
      subroutine read_rst_field_comps_b
!
      use set_restart_data
!
      integer(kind = kint) :: i
!
!
      read(id_phys_file) phys_data_name_IO(1:num_phys_data_IO)
!
      do i = 1, num_phys_data_IO
        call set_num_comps_4_rst(phys_data_name_IO(i),                  &
     &      num_phys_comp_IO(i) )
      end do
!
      end subroutine read_rst_field_comps_b
!
! -------------------------------------------------------------------
!
      end module rst_data_IO_by_fld
