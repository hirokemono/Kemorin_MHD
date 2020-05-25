!>@file   t_control_data_LIC_noise.f90
!!@brief  module t_control_data_LIC_noise
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for noise configration for LIC
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_cube_noise_control_file(id_control, file_name,  &
!!     &          header, noise_ctl)
!!
!!      subroutine read_cube_noise_control_data                         &
!!     &         (id_control, hd_lic_ctl, noise_ctl, c_buf)
!!        type(cube_noise_ctl), intent(inout) :: noise_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine reset_cube_noise_control_data(noise_ctl)
!!      subroutine copy_cube_noise_control_data(org_lic_c, new_lic_c)
!!        type(cube_noise_ctl), intent(in) :: org_lic_c
!!        type(cube_noise_ctl), intent(inout) :: new_lic_c
!!      subroutine bcast_cube_noise_control_data(noise_ctl)
!!        type(cube_noise_ctl), intent(inout) :: noise_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      List of flags  (Not used currently)
!!    noise_type:             'external_file' or 'randum'
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin cube_noise_ctl
!!    noise_type             'external_file'
!!    noise_file_prefix      'noise/noise_64'
!!
!!    noise_resolution          256
!!    noise_step_size            20
!!
!!    noise_cube_size          0.4
!!  end cube_noise_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_LIC_noise
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use skip_comment_f
!
      implicit  none
!
!
      type cube_noise_ctl
!>         Noise type name
        type(read_character_item) :: noise_type_ctl
!>         prefix of noise file
        type(read_character_item) :: noise_file_name_ctl
!>         number of grid of noise cube (each direction)
        type(read_integer_item) ::   noise_resolution_ctl
!>         number of stepping for noide density
        type(read_integer_item) ::   noise_stepping_ctl
!
!>         size of noise cube (each direction)
        type(read_real_item) :: noise_cube_size_ctl
!
!          loaded flag for cube_noise_ctl
        integer (kind=kint) :: i_cube_noise_control = 0
      end type cube_noise_ctl
!
!
!      character(len=kchara) :: hd_cube_noise =      'cube_noise_ctl'
!
!     3rd level for noise control
      character(len=kchara) :: hd_noise_type =      'noise_type'
      character(len=kchara) :: hd_noise_file_head = 'noise_file_prefix'
      character(len=kchara) :: hd_noise_grid_size = 'noise_resolution'
      character(len=kchara) :: hd_noise_stepping =  'noise_step_size'
!
      character(len=kchara) :: hd_noise_cube_size = 'noise_cube_size'
!
      private :: hd_noise_type, hd_noise_file_head, hd_noise_grid_size
      private :: hd_noise_stepping, hd_noise_cube_size
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_cube_noise_control_file(id_control, file_name,    &
     &          header, noise_ctl)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len = kchara), intent(in) :: header
      type(cube_noise_ctl), intent(inout) :: noise_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(file_name .eq. 'NO_FILE') return
!
      write(*,*) 'LIC noise control file: ', trim(file_name)
!
      open(id_control, file=file_name, status='old')
      do
        call load_one_line_from_control(id_control, c_buf1)
        call read_cube_noise_control_data                               &
     &     (id_control, header, noise_ctl, c_buf1)
        if(noise_ctl%i_cube_noise_control .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_cube_noise_control_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_cube_noise_control_data                           &
     &         (id_control, header, noise_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: header
!
      type(cube_noise_ctl), intent(inout) :: noise_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, header) .eqv. .FALSE.) return
      if(noise_ctl%i_cube_noise_control .gt. 0) return
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, header)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_noise_type, noise_ctl%noise_type_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_noise_file_head, noise_ctl%noise_file_name_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_noise_grid_size, noise_ctl%noise_resolution_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_noise_stepping, noise_ctl%noise_stepping_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_noise_cube_size, noise_ctl%noise_cube_size_ctl)
      end do
      noise_ctl%i_cube_noise_control = 1
!
      end subroutine read_cube_noise_control_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_cube_noise_control_data(noise_ctl)
!
      type(cube_noise_ctl), intent(inout) :: noise_ctl
!
!
      noise_ctl%noise_type_ctl%iflag =        0
      noise_ctl%noise_file_name_ctl%iflag =   0
      noise_ctl%noise_resolution_ctl%iflag =  0
      noise_ctl%noise_stepping_ctl%iflag =    0
      noise_ctl%noise_cube_size_ctl%iflag =   0
!
      noise_ctl%i_cube_noise_control = 0
!
      end subroutine reset_cube_noise_control_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_cube_noise_control_data(noise_ctl)
!
      use bcast_control_arrays
!
      type(cube_noise_ctl), intent(inout) :: noise_ctl
!
!
      call MPI_BCAST(noise_ctl%i_cube_noise_control,  1,                &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_c1(noise_ctl%noise_type_ctl)
      call bcast_ctl_type_c1(noise_ctl%noise_file_name_ctl)
      call bcast_ctl_type_i1(noise_ctl%noise_resolution_ctl)
      call bcast_ctl_type_i1(noise_ctl%noise_stepping_ctl)
!
      call bcast_ctl_type_r1(noise_ctl%noise_cube_size_ctl)
!
      end subroutine bcast_cube_noise_control_data
!
!  ---------------------------------------------------------------------
!
      subroutine copy_cube_noise_control_data(org_lic_c, new_lic_c)
!
      type(cube_noise_ctl), intent(in) :: org_lic_c
      type(cube_noise_ctl), intent(inout) :: new_lic_c
!
!
      new_lic_c%i_cube_noise_control = org_lic_c%i_cube_noise_control
!
      call copy_chara_ctl(org_lic_c%noise_type_ctl,                     &
     &                    new_lic_c%noise_type_ctl)
      call copy_chara_ctl(org_lic_c%noise_file_name_ctl,                &
     &                    new_lic_c%noise_file_name_ctl)
      call copy_integer_ctl(org_lic_c%noise_resolution_ctl,             &
     &                      new_lic_c%noise_resolution_ctl)
      call copy_integer_ctl(org_lic_c%noise_stepping_ctl,               &
     &                      new_lic_c%noise_stepping_ctl)
      call copy_real_ctl(org_lic_c%noise_cube_size_ctl,                 &
     &                   new_lic_c%noise_cube_size_ctl)
!
      end subroutine copy_cube_noise_control_data
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_LIC_noise
