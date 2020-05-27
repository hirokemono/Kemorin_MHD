!>@file   t_control_data_LIC_kernel.f90
!!@brief  module t_control_data_LIC_kernel
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for noise configration for LIC
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_kernel_control_file(id_control, file_name,      &
!!     &          header, kernel_ctl)
!!
!!      subroutine read_kernel_control_data                             &
!!     &         (id_control, hd_lic_ctl, kernel_ctl, c_buf)
!!        type(lic_kernel_ctl), intent(inout) :: kernel_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine reset_kernel_control_data(kernel_ctl)
!!      subroutine copy_kernel_control_data(org_kernel_c, new_kernel_c)
!!        type(lic_kernel_ctl), intent(in) :: org_kernel_c
!!        type(lic_kernel_ctl), intent(inout) :: new_kernel_c
!!      subroutine bcast_kernel_control_data(kernel_ctl)
!!        type(lic_kernel_ctl), intent(inout) :: kernel_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      List of flags  (Not used currently)
!!    kernel_type:             'gaussian' or 'triangle'
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin kernel_ctl
!!    kernel_type            'external_file'
!!
!!    half_kernel_resolution          256
!!    peak_position_ctl          0.4
!!    gaussian_width_ctl         0.25
!!  end kernel_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_LIC_kernel
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
      type lic_kernel_ctl
!>         Kernel type name
        type(read_character_item) :: kernel_type_ctl
!
!>         number of grid of half kernel
        type(read_integer_item) :: half_kernel_resolution_ctl
!
!>         peak position of kernel
        type(read_real_item) :: kernel_peak_ctl
!>         width of Gaussian
        type(read_real_item) :: kernel_sigma_ctl
!
!          loaded flag for kernel_ctl
        integer (kind=kint) :: i_kernel_control = 0
      end type lic_kernel_ctl
!
!
!      character(len=kchara) :: hd_kernel =      'kernel_ctl'
!
!     3rd level for noise control
      character(len=kchara) :: hd_kernel_type =      'kernel_type'
!
      character(len=kchara) :: hd_half_kernel_grid_size                 &
     &                        = 'half_kernel_resolution'
!
      character(len=kchara) :: hd_kernel_sigma = 'gaussian_width_ctl'
      character(len=kchara) :: hd_kernel_peak = 'peak_position_ctl'
!
      private :: hd_kernel_type
      private :: hd_half_kernel_grid_size
      private :: hd_kernel_sigma, hd_kernel_peak
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_kernel_control_file(id_control, file_name,        &
     &          header, kernel_ctl)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len = kchara), intent(in) :: header
      type(lic_kernel_ctl), intent(inout) :: kernel_ctl
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
        call read_kernel_control_data                                   &
     &     (id_control, header, kernel_ctl, c_buf1)
        if(kernel_ctl%i_kernel_control .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_kernel_control_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_kernel_control_data                               &
     &         (id_control, header, kernel_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: header
!
      type(lic_kernel_ctl), intent(inout) :: kernel_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, header) .eqv. .FALSE.) return
      if(kernel_ctl%i_kernel_control .gt. 0) return
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, header)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_kernel_type, kernel_ctl%kernel_type_ctl)
!
        call read_integer_ctl_type(c_buf, hd_half_kernel_grid_size,     &
     &      kernel_ctl%half_kernel_resolution_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_kernel_sigma, kernel_ctl%kernel_sigma_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_kernel_peak, kernel_ctl%kernel_peak_ctl)
      end do
      kernel_ctl%i_kernel_control = 1
!
      end subroutine read_kernel_control_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_kernel_control_data(kernel_ctl)
!
      type(lic_kernel_ctl), intent(inout) :: kernel_ctl
!
!
      kernel_ctl%kernel_type_ctl%iflag =       0
      kernel_ctl%half_kernel_resolution_ctl%iflag =  0
      kernel_ctl%kernel_sigma_ctl%iflag =  0
      kernel_ctl%kernel_peak_ctl%iflag =   0
!
      kernel_ctl%i_kernel_control = 0
!
      end subroutine reset_kernel_control_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_kernel_control_data(kernel_ctl)
!
      use bcast_control_arrays
!
      type(lic_kernel_ctl), intent(inout) :: kernel_ctl
!
!
      call MPI_BCAST(kernel_ctl%i_kernel_control,  1,                   &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_c1(kernel_ctl%kernel_type_ctl)
!
      call bcast_ctl_type_i1(kernel_ctl%half_kernel_resolution_ctl)
!
      call bcast_ctl_type_r1(kernel_ctl%kernel_sigma_ctl)
      call bcast_ctl_type_r1(kernel_ctl%kernel_peak_ctl)
!
      end subroutine bcast_kernel_control_data
!
!  ---------------------------------------------------------------------
!
      subroutine copy_kernel_control_data(org_kernel_c, new_kernel_c)
!
      type(lic_kernel_ctl), intent(in) :: org_kernel_c
      type(lic_kernel_ctl), intent(inout) :: new_kernel_c
!
!
      new_kernel_c%i_kernel_control = org_kernel_c%i_kernel_control
!
      call copy_chara_ctl(org_kernel_c%kernel_type_ctl,                 &
     &                    new_kernel_c%kernel_type_ctl)
      call copy_integer_ctl(org_kernel_c%half_kernel_resolution_ctl,    &
     &                      new_kernel_c%half_kernel_resolution_ctl)
      call copy_real_ctl(org_kernel_c%kernel_sigma_ctl,                 &
     &                   new_kernel_c%kernel_sigma_ctl)
      call copy_real_ctl(org_kernel_c%kernel_peak_ctl,                  &
     &                   new_kernel_c%kernel_peak_ctl)
!
      end subroutine copy_kernel_control_data
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_LIC_kernel
