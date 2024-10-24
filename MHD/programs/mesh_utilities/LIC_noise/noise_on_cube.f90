!
      program noise_generation
!
      use m_precision
      use m_constants
      use t_3d_noise
      use t_control_data_LIC_noise
!
      implicit none
!
      integer(kind = kint), parameter :: id_control = 11
      character(len = kchara) :: ctl_file_name = 'ctl_noise'
      character(len = kchara) :: hd_cube_noise = 'cube_noise_ctl'
!
      type(noise_cube) :: noise_t1
      type(cube_noise_ctl) :: noise_c1
!
!
      call  read_cube_noise_control_file(id_control, ctl_file_name,     &
     &    hd_cube_noise, noise_c1)
      call set_control_3d_cube_noise(noise_c1, noise_t1)
      call sel_const_3d_cube_noise(0, noise_t1)
      call dealloc_3d_cube_noise(noise_t1)
!
      end program noise_generation
