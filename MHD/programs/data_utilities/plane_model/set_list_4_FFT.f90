!
!      module set_list_4_FFT
!
!      Written by H. Matsui
!
!!      subroutine set_parameters_4_FFT                                 &
!!     &         (cube_c, c_size, num_pe, ist, ied, iint)
!!      subroutine set_parameters_rst_by_spec(cube_c, c_size, num_pe,   &
!!     &          ist, ied, ifactor_step, ifactor_rst, dt, t_init,      &
!!     &          kx_org, ky_org, iz_org, mesh_file)
!!      subroutine set_parameters_data_by_spec(cube_c, c_size, num_pe,  &
!!     &          kx_org, ky_org, iz_org, mesh_file, ucd_param)
!!        type(ctl_data_4_plane_model), intent(in) :: cube_c
!!        type(size_of_cube), intent(inout) :: c_size
!!        type(field_IO_params), intent(inout) :: ucd_param
!
      module set_list_4_FFT
!
      use m_precision
      use t_file_IO_parameter
      use t_size_of_cube
      use t_ctl_data_4_plane_model
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
      subroutine set_parameters_4_FFT                                   &
     &         (cube_c, c_size, num_pe, ist, ied, iint)
!
      use m_control_plane_fft
      use m_spectr_4_ispack
!
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
      kx_max = c_size%nx_all
      ky_max = c_size%ny_all
      iz_max = c_size%nz_all
      num_spectr = kx_max*ky_max*iz_max
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
      end subroutine set_parameters_4_FFT
!
! -----------------------------------------------------------------------
!
      subroutine set_parameters_rst_by_spec(cube_c, c_size, num_pe,     &
     &          ist, ied, ifactor_step, ifactor_rst, dt, t_init,        &
     &          kx_org, ky_org, iz_org, mesh_file)
!
      use m_control_plane_fft
      use m_ctl_data_2nd_plane
      use m_default_file_prefix
      use m_spectr_4_ispack
      use m_set_new_spectr
      use set_control_platform_data
!
      type(ctl_data_4_plane_model), intent(in) :: cube_c
!
      type(size_of_cube), intent(inout) :: c_size
      type(field_IO_params), intent(inout) ::  mesh_file
      integer, intent(inout) :: num_pe
      integer(kind = kint), intent(inout) :: ist, ied
      integer(kind = kint), intent(inout) :: ifactor_step, ifactor_rst
      integer(kind = kint), intent(inout) :: kx_org, ky_org, iz_org
      real(kind = kreal), intent(inout) :: dt, t_init
!
!
      write(*,*) 'new_mesh_prefix    ', new_p_plt%mesh_file_prefix
!      write(*,*) 'new_restart_prefix', new_p_plt%restart_file_prefix
!      write(*,*) 'new_udt_type_ctl  ', new_udt_type_ctl
      write(*,*) 'nnod_plane_ctl     ', cube_c%nnod_plane_ctl%intvalue
      write(*,*) 'ndomain_plane_ctl  ',                                 &
     &          cube_c%ndomain_plane_ctl%intvalue
      write(*,*) 'nnod_plane2_ctl    ', nnod_plane2_ctl%intvalue
      write(*,*) 'ndomain_plane2_ctl ', ndomain_plane2_ctl%intvalue
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
      kx_max = cube_c%nnod_plane_ctl%intvalue(1)
      ky_max = cube_c%nnod_plane_ctl%intvalue(2)
      iz_max = cube_c%nnod_plane_ctl%intvalue(3)
      num_spectr =  kx_max * ky_max * iz_max
      kx_org = kx_max
      ky_org = ky_max
      iz_org = ky_max
!
      c_size%nx_all = nnod_plane2_ctl%intvalue(1)
      c_size%ny_all = nnod_plane2_ctl%intvalue(2)
      c_size%nz_all = nnod_plane2_ctl%intvalue(3)
!
      nnod_new_k_org_z = kx_max * ky_max * c_size%nz_all
!
      num_pe =  ndomain_plane2_ctl%intvalue(1)                          &
     &        * ndomain_plane2_ctl%intvalue(2)                          &
     &        * ndomain_plane2_ctl%intvalue(3)
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
      subroutine set_parameters_data_by_spec(cube_c, c_size, num_pe,    &
     &          kx_org, ky_org, iz_org, mesh_file, ucd_param)
!
      use m_control_plane_fft
      use m_ctl_data_2nd_plane
      use m_default_file_prefix
      use m_spectr_4_ispack
      use m_set_new_spectr
      use m_field_file_format
      use t_ucd_data
      use set_parallel_file_name
      use set_control_platform_data
!
      type(ctl_data_4_plane_model), intent(in) :: cube_c
!
      type(size_of_cube), intent(inout) :: c_size
      integer, intent(inout) :: num_pe
      integer(kind = kint), intent(inout) :: kx_org, ky_org, iz_org
      type(field_IO_params),  intent(inout) ::  mesh_file
      type(field_IO_params), intent(inout) :: ucd_param
!
!
      write(*,*) 'nnod_plane_ctl:     ', cube_c%nnod_plane_ctl%intvalue
      write(*,*) 'ndomain_plane_ctl:  ',                                &
     &          cube_c%ndomain_plane_ctl%intvalue
      write(*,*) 'nnod_plane2_ctl:    ', nnod_plane2_ctl%intvalue
      write(*,*) 'ndomain_plane2_ctl: ', ndomain_plane2_ctl%intvalue
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
      c_size%nx_all = nnod_plane2_ctl%intvalue(1)
      c_size%ny_all = nnod_plane2_ctl%intvalue(2)
      c_size%nz_all = nnod_plane2_ctl%intvalue(3)

      num_pe =  ndomain_plane2_ctl%intvalue(1)                          &
     &        * ndomain_plane2_ctl%intvalue(2)                          &
     &        * ndomain_plane2_ctl%intvalue(3)
      nnod_new_k_org_z = kx_max * ky_max * c_size%nz_all
!
      end subroutine set_parameters_data_by_spec
!
! -----------------------------------------------------------------------
!
      end module set_list_4_FFT
