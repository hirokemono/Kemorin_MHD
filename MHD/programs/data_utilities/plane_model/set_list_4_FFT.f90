!
!      module set_list_4_FFT
!
!      Written by H. Matsui
!
!!      subroutine set_parameters_plane_ene                             &
!!     &         (t_zfft_ctl, cube_c, c_size, num_pe, ist, ied, iint)
!!      subroutine set_parameters_4_FFT                                 &
!!     &         (t_zfft_ctl, cube_c, c_size, plane_fft_wk,             &
!!     &          num_pe, ist, ied, iint)
!!      subroutine set_parameters_rst_by_spec                           &
!!     &         (new_p_plt, t_zfft_ctl, cube_c, cube2nd_c, c_size,     &
!!     &          num_pe, ist, ied, ifactor_step, ifactor_rst,          &
!!     &          dt, t_init, kx_org, ky_org, iz_org, nnod_new_k_org_z, &
!!     &          mesh_file, plane_fft_wk)
!!      subroutine set_parameters_data_by_spec                          &
!!     &         (new_p_plt, cube_c, cube2nd_c, c_size, num_pe,         &
!!     &          kx_org, ky_org, iz_org, nnod_new_k_org_z,             &
!!     &          mesh_file, plane_fft_wk, ucd_param)
!!        type(ctl_data_4_plane_model), intent(in) :: cube_c, cube2nd_c
!!        type(size_of_cube), intent(inout) :: c_size
!!        type(field_IO_params), intent(inout) :: ucd_param
!
      module set_list_4_FFT
!
      use m_precision
      use t_file_IO_parameter
      use t_size_of_cube
      use t_ctl_data_4_plane_model
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
!
      implicit none
!
      integer(kind=kint )               ::  plane_data_code = 16
!
      character(len=kchara), parameter                                  &
     &           :: def_new_mesh_head = 'mesh_target/in'
      character(len=kchara), parameter                                  &
     &           :: def_newrst_head =   "rst_new/rst"
      character(len=kchara), parameter                                  &
     &           :: def_newadams_head = "rst_new/adams"
!
      character(len=kchara) :: rst_head_plane
!
      character(len=kchara) :: dx_node_plane_fname
      character(len=kchara) :: dx_connect_plane_fname
!
      private :: def_new_mesh_head
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_parameters_plane_ene                               &
     &         (t_zfft_ctl, cube_c, c_size, num_pe, ist, ied, iint)
!
      use t_spectr_4_ispack
!
      type(time_data_control), intent(in) :: t_zfft_ctl
      type(ctl_data_4_plane_model), intent(in) :: cube_c
!
      type(size_of_cube), intent(inout) :: c_size
      integer, intent(inout) :: num_pe
      integer(kind = kint), intent(inout) :: ist, ied, iint
!
!
      c_size%nx_all = cube_c%nnod_plane_ctl%intvalue(1)
      c_size%ny_all = cube_c%nnod_plane_ctl%intvalue(2)
      c_size%nz_all = cube_c%nnod_plane_ctl%intvalue(3)
      num_pe =  c_size%nx_all * c_size%ny_all * c_size%nz_all
!
      ist = 0
      if(t_zfft_ctl%i_step_init_ctl%iflag .gt. 0) then
        ist = t_zfft_ctl%i_step_init_ctl%intvalue
      end if
!
      ied = 0
      if(t_zfft_ctl%i_step_number_ctl%iflag .gt. 0) then
        ied = t_zfft_ctl%i_step_number_ctl%intvalue
      end if
!
      iint = 1
      if(t_zfft_ctl%i_step_ucd_ctl%iflag .gt. 0) then
        iint = t_zfft_ctl%i_step_ucd_ctl%intvalue
      end if
!
      end subroutine set_parameters_plane_ene
!
! -----------------------------------------------------------------------
!
      subroutine set_parameters_4_FFT                                   &
     &         (t_zfft_ctl, cube_c, c_size, plane_fft_wk,               &
     &          num_pe, ist, ied, iint)
!
      use t_spectr_4_ispack
!
      type(time_data_control), intent(in) :: t_zfft_ctl
      type(ctl_data_4_plane_model), intent(in) :: cube_c
!
      type(size_of_cube), intent(inout) :: c_size
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
      integer, intent(inout) :: num_pe
      integer(kind = kint), intent(inout) :: ist, ied, iint
!
!
      call set_parameters_plane_ene                                     &
     &   (t_zfft_ctl, cube_c, c_size, num_pe, ist, ied, iint)
!
      plane_fft_wk%kx_max = c_size%nx_all
      plane_fft_wk%ky_max = c_size%ny_all
      plane_fft_wk%iz_max = c_size%nz_all
      plane_fft_wk%num_spectr = plane_fft_wk%kx_max                     &
     &         * plane_fft_wk%ky_max * plane_fft_wk%iz_max
!
      end subroutine set_parameters_4_FFT
!
! -----------------------------------------------------------------------
!
      subroutine set_parameters_rst_by_spec                             &
     &          (new_p_plt, t_zfft_ctl, cube_c, cube2nd_c, c_size,      &
     &          num_pe, ist, ied, ifactor_step, ifactor_rst,            &
     &          dt, t_init, kx_org, ky_org, iz_org, nnod_new_k_org_z,   &
     &          mesh_file, plane_fft_wk)
!
      use m_default_file_prefix
      use t_spectr_4_ispack
      use set_control_platform_data
!
      type(platform_data_control), intent(in) :: new_p_plt
      type(time_data_control), intent(in) :: t_zfft_ctl
      type(ctl_data_4_plane_model), intent(in) :: cube_c, cube2nd_c
!
      type(size_of_cube), intent(inout) :: c_size
      type(field_IO_params), intent(inout) ::  mesh_file
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
      integer, intent(inout) :: num_pe
      integer(kind = kint), intent(inout) :: ist, ied
      integer(kind = kint), intent(inout) :: ifactor_step, ifactor_rst
      integer(kind = kint), intent(inout) :: kx_org, ky_org, iz_org
      real(kind = kreal), intent(inout) :: dt, t_init
!
      integer(kind = kint), intent(inout) :: nnod_new_k_org_z
!
!
      write(*,*) 'new_mesh_prefix    ', new_p_plt%mesh_file_prefix
!      write(*,*) 'new_restart_prefix', new_p_plt%restart_file_prefix
!      write(*,*) 'new_udt_type_ctl  ', new_udt_type_ctl
      write(*,*) 'nnod_plane_ctl     ', cube_c%nnod_plane_ctl%intvalue
      write(*,*) 'ndomain_plane_ctl  ',                                 &
     &          cube_c%ndomain_plane_ctl%intvalue
      write(*,*) 'nnod_plane2_ctl    ',                                 &
     &          cube2nd_c%nnod_plane_ctl%intvalue
      write(*,*) 'ndomain_plane2_ctl ',                                 &
     &          cube2nd_c%ndomain_plane_ctl%intvalue
!
      call set_control_mesh_file_def                                    &
     &   (def_new_mesh_head, new_p_plt, mesh_file)
!
      if (new_p_plt%restart_file_prefix%iflag .gt. 0) then
        rst_head_plane = new_p_plt%restart_file_prefix%charavalue
      else
        rst_head_plane = def_newrst_head
      end if
!
      plane_fft_wk%kx_max = c_size%nx_all
      plane_fft_wk%ky_max = c_size%ny_all
      plane_fft_wk%iz_max = c_size%nz_all
      plane_fft_wk%num_spectr = plane_fft_wk%kx_max                     &
     &         * plane_fft_wk%ky_max * plane_fft_wk%iz_max
      kx_org = plane_fft_wk%kx_max
      ky_org = plane_fft_wk%ky_max
      iz_org = plane_fft_wk%ky_max
!
      c_size%nx_all = cube2nd_c%nnod_plane_ctl%intvalue(1)
      c_size%ny_all = cube2nd_c%nnod_plane_ctl%intvalue(2)
      c_size%nz_all = cube2nd_c%nnod_plane_ctl%intvalue(3)
!
      nnod_new_k_org_z = plane_fft_wk%kx_max * plane_fft_wk%ky_max      &
     &                  * c_size%nz_all
!
      num_pe =  cube2nd_c%ndomain_plane_ctl%intvalue(1)                 &
     &        * cube2nd_c%ndomain_plane_ctl%intvalue(2)                 &
     &        * cube2nd_c%ndomain_plane_ctl%intvalue(3)
!
      ist = 0
      if(t_zfft_ctl%i_step_init_ctl%iflag .gt. 0) then
        ist = t_zfft_ctl%i_step_init_ctl%intvalue
      end if
!
      ied = 0
      if(t_zfft_ctl%i_step_number_ctl%iflag .gt. 0) then
        ied = t_zfft_ctl%i_step_number_ctl%intvalue
      end if
!
      ifactor_step = 1
      if(t_zfft_ctl%i_step_ucd_ctl%iflag .gt. 0) then
        ifactor_step = t_zfft_ctl%i_step_ucd_ctl%intvalue
      end if
!
      ifactor_rst = 1
      if(t_zfft_ctl%i_step_rst_ctl%iflag .gt. 0) then
        ifactor_rst = t_zfft_ctl%i_step_rst_ctl%intvalue
      end if
!
      dt = 0.0d0
      if(t_zfft_ctl%dt_ctl%iflag .gt. 0) then
        t_init = t_zfft_ctl%dt_ctl%realvalue
      end if
!
      t_init = 0.0d0
      if(t_zfft_ctl%time_init_ctl%iflag .gt. 0) then
        t_init = t_zfft_ctl%time_init_ctl%realvalue
      end if
!
      end subroutine set_parameters_rst_by_spec
!
! -----------------------------------------------------------------------
!
      subroutine set_parameters_data_by_spec                            &
     &         (new_p_plt, cube_c, cube2nd_c, c_size, num_pe,           &
     &          kx_org, ky_org, iz_org, nnod_new_k_org_z,               &
     &          mesh_file, plane_fft_wk, ucd_param)
!
      use m_default_file_prefix
      use t_spectr_4_ispack
      use m_field_file_format
      use t_ucd_data
      use set_parallel_file_name
      use set_control_platform_data
!
      type(platform_data_control), intent(in) :: new_p_plt
      type(ctl_data_4_plane_model), intent(in) :: cube_c, cube2nd_c
!
      type(size_of_cube), intent(inout) :: c_size
      integer, intent(inout) :: num_pe
      integer(kind = kint), intent(inout) :: kx_org, ky_org, iz_org
      type(field_IO_params),  intent(inout) ::  mesh_file
      type(field_IO_params), intent(inout) :: ucd_param
      type(plane_spectr_by_ispack), intent(inout) :: plane_fft_wk
!
      integer(kind = kint), intent(inout) :: nnod_new_k_org_z
!
!
      write(*,*) 'nnod_plane_ctl:     ', cube_c%nnod_plane_ctl%intvalue
      write(*,*) 'ndomain_plane_ctl:  ',                                &
     &          cube_c%ndomain_plane_ctl%intvalue
      write(*,*) 'nnod_plane2_ctl:    ',                                &
     &           cube2nd_c%nnod_plane_ctl%intvalue
      write(*,*) 'ndomain_plane2_ctl: ',                                &
     &          cube2nd_c%ndomain_plane_ctl%intvalue
!
      call set_control_mesh_file_def                                    &
     &   (def_new_mesh_head, new_p_plt, mesh_file)
!
      if (new_p_plt%field_file_prefix%iflag .gt. 0) then
        ucd_param%iflag_format                                          &
     &     = choose_file_format(new_p_plt%field_file_fmt_ctl)
        ucd_param%file_prefix = new_p_plt%field_file_prefix%charavalue
      end if
!
!
      kx_org = cube_c%nnod_plane_ctl%intvalue(1)
      ky_org = cube_c%nnod_plane_ctl%intvalue(2)
      iz_org = cube_c%nnod_plane_ctl%intvalue(3)
!
      c_size%nx_all = cube2nd_c%nnod_plane_ctl%intvalue(1)
      c_size%ny_all = cube2nd_c%nnod_plane_ctl%intvalue(2)
      c_size%nz_all = cube2nd_c%nnod_plane_ctl%intvalue(3)

      num_pe =  cube2nd_c%ndomain_plane_ctl%intvalue(1)                 &
     &        * cube2nd_c%ndomain_plane_ctl%intvalue(2)                 &
     &        * cube2nd_c%ndomain_plane_ctl%intvalue(3)
      nnod_new_k_org_z = plane_fft_wk%kx_max * plane_fft_wk%ky_max      &
     &        * c_size%nz_all
!
      end subroutine set_parameters_data_by_spec
!
! -----------------------------------------------------------------------
!
      end module set_list_4_FFT
