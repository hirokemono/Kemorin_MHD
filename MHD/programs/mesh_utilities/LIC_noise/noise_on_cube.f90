!
      program noise_generation
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_3d_noise
      use t_control_data_LIC_noise
      use t_read_control_elements
      use ctl_file_LIC_noise_IO
      use cal_3d_noise
!
      implicit none
!
      integer(kind = kint), parameter :: id_control = 11
      character(len = kchara) :: ctl_file_name = 'ctl_noise'
      character(len = kchara) :: hd_cube_noise = 'cube_noise_ctl'
!
      type(noise_cube) :: noise_t1
      type(cube_noise_ctl) :: noise_c1
      integer(kind = kint) :: ierr
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      call read_cube_noise_control_file(id_control, ctl_file_name,      &
     &    hd_cube_noise, noise_c1, c_buf1)
      if(c_buf1%iend .gt. 0) stop 'control file is broken'
!
      call set_control_3d_cube_noise(noise_c1, noise_t1)
      call sel_const_3d_cube_noise(noise_t1)
      call finalize_kemo_mt_stream
      call sel_input_3d_cube_noise(0, noise_t1, ierr)
      call sel_output_3d_cube_noise(noise_t1)
      call dealloc_3d_cube_noise(noise_t1)
      if(ierr .gt. 0) write(*,*) e_message
!
      end program noise_generation
