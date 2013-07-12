!
!      module set_list_4_FFT
!
!      Written by H. Matsui
!
!      subroutine set_parameters_4_FFT(num_pe, ist, ied, iint)
!      subroutine set_fields_4_FFT
!
      module set_list_4_FFT
!
      use m_precision
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
      subroutine set_parameters_4_FFT(num_pe, ist, ied, iint)
!
      use m_control_plane_fft
      use m_ctl_data_4_plane_model
      use m_ctl_data_4_time_steps
      use m_size_4_plane
      use m_spectr_4_ispack
      use m_ucd_data
!
      integer(kind=kint ), intent(inout) :: num_pe, ist, ied, iint
!
!
      nx_all = nnod_plane_ctl(1)
      ny_all = nnod_plane_ctl(2)
      nz_all = nnod_plane_ctl(3)
      num_pe =  ndomain_plane_ctl(1)                                    &
     &        * ndomain_plane_ctl(2)                                    &
     &        * ndomain_plane_ctl(3)
!
      kx_max = nx_all
      ky_max = ny_all
      iz_max = nz_all
      num_spectr = kx_max*ky_max*iz_max
!
      ist = i_step_init_ctl
      ied = i_step_number_ctl
      iint = i_step_ucd_ctl
!
      end subroutine set_parameters_4_FFT
!
! -----------------------------------------------------------------------
!
      subroutine set_parameters_rst_by_spec(num_pe, ist, ied,           &
     &          ifactor_step, ifactor_rst, dt, t_init,                  &
     &          kx_org, ky_org, iz_org)
!
      use m_control_plane_fft
      use m_ctl_data_4_plane_model
      use m_ctl_data_2nd_plane
      use m_ctl_data_4_time_steps
      use m_ctl_data_4_2nd_data
      use m_read_mesh_data
      use m_size_4_plane
      use m_spectr_4_ispack
      use m_set_new_spectr
!
      integer(kind=kint ), intent(inout) :: num_pe, ist, ied
      integer(kind=kint ), intent(inout) :: ifactor_step, ifactor_rst
      integer(kind=kint ), intent(inout) :: kx_org, ky_org, iz_org
      real(kind = kreal), intent(inout) :: dt, t_init
!
!
      write(*,*) 'new_mesh_head_ctl    ', new_mesh_head_ctl
!      write(*,*) 'new_field_head_ctl     ', new_field_head_ctl
!      write(*,*) 'new_rst_head_ctl     ', new_rst_head_ctl
!      write(*,*) 'new_udt_type_ctl     ', new_udt_type_ctl
      write(*,*) 'nnod_plane_ctl       ', nnod_plane_ctl
      write(*,*) 'ndomain_plane_ctl    ', ndomain_plane_ctl
!      write(*,*) 'num_of_sleeve_ctl     ', num_of_sleeve_ctl
!      write(*,*) 'plane_size_ctl        ', plane_size_ctl
!      write(*,*) 'unit_len_plane_ctl    ', unit_len_plane_ctl
!      write(*,*) 'horizontal_grid_ctl   ', horizontal_grid_ctl
      write(*,*) 'nnod_plane2_ctl       ', nnod_plane2_ctl
      write(*,*) 'ndomain_plane2_ctl    ', ndomain_plane2_ctl
!      write(*,*) 'num_of_sleeve2_ctl    ', num_of_sleeve2_ctl
!      write(*,*) 'plane_size2_ctl       ', plane_size2_ctl
!      write(*,*) 'unit_len_plane2_ctl   ', unit_len_plane2_ctl
!      write(*,*) 'horizontal_grid2_ctl  ', horizontal_grid2_ctl
!
      if (i_new_mesh_head .gt. 0) then
        mesh_file_head = new_mesh_head_ctl
      else
        mesh_file_head = def_new_mesh_head
      end if
!
      if (i_new_rst_head .gt. 0) then
        rst_head_plane = new_rst_head_ctl
      else
        rst_head_plane = def_newrst_head
      end if
!
      kx_max = nnod_plane_ctl(1)
      ky_max = nnod_plane_ctl(2)
      iz_max = nnod_plane_ctl(3)
      num_spectr =  nnod_plane_ctl(1)                                   &
     &            * nnod_plane_ctl(2)                                   &
     &            * nnod_plane_ctl(3)
      kx_org = nnod_plane_ctl(1)
      ky_org = nnod_plane_ctl(2)
      iz_org = nnod_plane_ctl(3)
!
      nx_all = nnod_plane2_ctl(1)
      ny_all = nnod_plane2_ctl(2)
      nz_all = nnod_plane2_ctl(3)
!
      nnod_new_k_org_z = kx_max*ky_max*nz_all
!
      num_pe =  ndomain_plane2_ctl(1)                                   &
     &        * ndomain_plane2_ctl(2)                                   &
     &        * ndomain_plane2_ctl(3)
!
      ist = i_step_init_ctl
      ied = i_step_number_ctl
      ifactor_step = i_step_ucd_ctl
      ifactor_rst = i_step_rst_ctl
      dt = dt_ctl
      t_init = time_init_ctl
!
      end subroutine set_parameters_rst_by_spec
!
! -----------------------------------------------------------------------
!
      subroutine set_parameters_data_by_spec(num_pe,                    &
     &          kx_org, ky_org, iz_org)
!
      use m_control_plane_fft
      use m_ctl_data_4_plane_model
      use m_ctl_data_2nd_plane
      use m_ctl_data_4_time_steps
      use m_ctl_data_4_2nd_data
      use m_read_mesh_data
      use m_size_4_plane
      use m_spectr_4_ispack
      use m_set_new_spectr
      use m_ucd_data
      use m_field_file_format
      use set_parallel_file_name
!
      integer(kind=kint ), intent(inout) :: num_pe
      integer(kind=kint ), intent(inout) :: kx_org, ky_org, iz_org
!
!
      write(*,*) 'nnod_plane_ctl       ', nnod_plane_ctl
      write(*,*) 'ndomain_plane_ctl    ', ndomain_plane_ctl
!      write(*,*) 'num_of_sleeve_ctl     ', num_of_sleeve_ctl
!      write(*,*) 'plane_size_ctl        ', plane_size_ctl
!      write(*,*) 'unit_len_plane_ctl    ', unit_len_plane_ctl
!      write(*,*) 'horizontal_grid_ctl   ', horizontal_grid_ctl
      write(*,*) 'nnod_plane2_ctl       ', nnod_plane2_ctl
      write(*,*) 'ndomain_plane2_ctl    ', ndomain_plane2_ctl
!      write(*,*) 'num_of_sleeve2_ctl    ', num_of_sleeve2_ctl
!      write(*,*) 'plane_size2_ctl       ', plane_size2_ctl
!      write(*,*) 'unit_len_plane2_ctl   ', unit_len_plane2_ctl
!      write(*,*) 'horizontal_grid2_ctl  ', horizontal_grid2_ctl
!
      if (i_new_mesh_head .gt. 0) then
        mesh_file_head = new_mesh_head_ctl
      else
        mesh_file_head = def_new_mesh_head
      end if
!
      if (i_new_udt_head .gt. 0) then
        fem_ucd%ifmt_file = iflag_udt
        fem_ucd%file_prefix = new_field_head_ctl
      else if (i_new_vtk_head .gt. 0) then
        fem_ucd%ifmt_file = iflag_vtk
        fem_ucd%file_prefix = new_vtk_head_ctl
      else
        fem_ucd%ifmt_file = iflag_fld
      end if
!
!
      kx_org = nnod_plane_ctl(1)
      ky_org = nnod_plane_ctl(2)
      iz_org = nnod_plane_ctl(3)
!
      nx_all = nnod_plane2_ctl(1)
      ny_all = nnod_plane2_ctl(2)
      nz_all = nnod_plane2_ctl(3)
      num_pe =  ndomain_plane2_ctl(1)                                   &
     &        * ndomain_plane2_ctl(2)                                   &
     &        * ndomain_plane2_ctl(3)
      nnod_new_k_org_z = kx_max*ky_max*nz_all
!
      end subroutine set_parameters_data_by_spec
!
! -----------------------------------------------------------------------
!
      end module set_list_4_FFT
