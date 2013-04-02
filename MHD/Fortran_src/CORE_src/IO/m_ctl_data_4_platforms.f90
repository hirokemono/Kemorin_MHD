!m_ctl_data_4_platforms.f90
!      module m_ctl_data_4_platforms
!
!> @brief Control input routine for data file headers
!
!
!        programmed by H.Matsui on July, 2007
!
!       Subroutine to read file name parameter block
!
!      subroutine read_ctl_data_4_platform
!
!>
! ------------------------------------------------------------------
!>@n      Example of control parameters
!
!>@n@code
!!    begin data_files_def
!!      debug_flag_ctl            'ON'
!!
!!      num_subdomain_ctl           2
!!      num_smp_ctl                 4
!!
!!      mesh_file_head_ctl          'mesh/in'
!!      elem_file_head_ctl          'mesh/in_element'
!!      surf_file_head_ctl          'mesh/in_surface'
!!      edge_file_head_ctl          'mesh/in_edge'
!!
!!      sph_files_head_ctl          'sph_shell/in'
!!
!!      mesh_sph_grid_ctl           'sph_shell/in_rtp'
!!      mesh_sph_lag_ctl            'sph_shell/in_rtm'
!!      mesh_sph_lag_spec_ctl       'sph_shell/in_rlm'
!!      mesh_sph_mode_ctl           'sph_shell/in_rj'
!!
!!      coriolis_tri_int_name_ctl   'sph_shell/rot_int.dat'
!!      interpolate_sph_to_fem_ctl  'sph_shell/sph_to_fem'
!!      interpolate_fem_to_sph_ctl  'sph_shell/fem_to_sph'
!!
!!      field_file_head_ctl         'field/out'
!!      rst_file_head_ctl           'restart/rst'
!!
!!      spectr_file_head_ctl        'sph_spectr/spectr'
!!
!!
!!      mesh_file_fmt_ctl           'ascii'
!!      rst_files_fmt_ctl           'ascii'
!!      field_files_fmt_ctl         'ucd_ascii'
!!      sph_files_fmt_ctl           'ascii'
!!      spectr_files_fmt_ctl        'ascii'
!!      itp_files_fmt_ctl           'ascii'
!!      coriolis_file_fmt_ctl       'ascii'
!!
!!      mesh_extension_flags_ctl    'ON'
!!      memory_conservation_ctl     'YES'
!!    end data_files_def
!>@endcode
!
! ------------------------------------------------------------------
!>@n
!>@n@param      debug_flag_ctl             Debug flag ('On' or 'Off')
!>@n
!>@n@param      num_subdomain_ctl
!>                Number of subdomain (MPI processes)
!>@n@param      num_smp_ctl                Number of SMP threads
!>@n
!>@n@param      mesh_file_head_ctl         File header for FEM mesh
!>@n@param      elem_file_head_ctl
!>                File header for FEM element comm. table
!>@n@param      surf_file_head_ctl         File header for surface data
!>@n@param      edge_file_head_ctl         File header for edge data
!>@n
!>@n@param      sph_files_head_ctl
!>               File header for spherical hermonics mode files
!>@n
!>@n@param      mesh_sph_grid_ctl
!>               File header for spherical grid data
!>@n@param      mesh_sph_lag_ctl
!>               File header for spherical grid data after FFT
!>@n@param      mesh_sph_lag_spec_ctl
!>               File header for spectr before Lagendre transform
!>@n@param      mesh_sph_mode_ctl
!>               File header for spherical mode data
!>@n
!>@n@param      field_file_head_ctl        File header for field data
!>@n@param      rst_file_head_ctl          File header for restart data
!>@n
!>@n@param      coriolis_tri_int_name_ctl
!>               File name for hermonic integration for Coriolis term
!>@n@param      interpolate_sph_to_fem_ctl 
!>               File header for interpolation table
!>               from spherical grid to FEM grid
!>@n@param      interpolate_fem_to_sph_ctl  
!>               File header for interpolation table
!>               from FEM grid to spherical grid
!>@n
!>@n@param      mesh_file_fmt_ctl        mesh data  file format
!>@n@param      rst_files_fmt_ctl        restart data  file format
!>@n@param      field_files_fmt_ctl      field data  file format
!>@n@param      sph_files_fmt_ctl        spectr data  file format
!>@n@param      itp_files_fmt_ctl        interpolation data file format
!>@n@param      spectr_files_fmt_ctl     Spectr data file format
!>@n@param      coriolis_file_fmt_ctl    integration data  file format
!
!
!>@n@param      memory_conservation_ctl    memory conservation flag
!>              File format for spherical shell grid data
!
      module m_ctl_data_4_platforms
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint) :: num_subdomain_ctl
      integer(kind = kint) :: num_smp_ctl
!
      character(len=kchara) :: mesh_file_head_ctl
!
      character(len=kchara) :: elem_file_head_ctl
      character(len=kchara) :: surf_file_head_ctl
      character(len=kchara) :: edge_file_head_ctl
!
!
      character(len=kchara) :: udt_file_head_ctl
      character(len=kchara) :: rst_file_head_ctl
      character(len=kchara) :: spectr_file_head_ctl
!
      character(len=kchara) :: sph_files_head_ctl
!
      character(len=kchara) :: sph_grid_head_ctl
      character(len=kchara) :: sph_lag_head_ctl
      character(len=kchara) :: sph_lag_spec_head_ctl
      character(len=kchara) :: sph_mode_head_ctl
!
      character(len=kchara) :: coriolis_tri_int_name_ctl
      character(len=kchara) :: interpolate_sph_to_fem_ctl
      character(len=kchara) :: interpolate_fem_to_sph_ctl
!
      character(len=kchara) :: mesh_file_fmt_ctl =      'ascii'
      character(len=kchara) :: sph_file_fmt_ctl =       'ascii'
      character(len=kchara) :: rst_files_fmt_ctl =      'ascii'
      character(len=kchara) :: udt_file_fmt_ctl =       'ascii'
      character(len=kchara) :: itp_files_fmt_ctl =      'ascii'
      character(len=kchara) :: spectr_files_fmt_ctl =   'ascii'
      character(len=kchara) :: coriolis_file_fmt_ctl =  'ascii'
!
      character(len=kchara) :: debug_flag_ctl = 'OFF'
!
      character(len=kchara) :: memory_conservation_ctl =  'OFF'
      character(len=kchara) :: mesh_extension_flags_ctl = 'ON'
!
!     Label for the entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      integer (kind=kint) :: i_platform =   0
!
!   file and domain controls
!
      character(len=kchara), parameter                                  &
     &       :: hd_num_subdomain = 'num_subdomain_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_num_smp =   'num_smp_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_mesh_header = 'mesh_file_head_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_elem_header = 'elem_file_head_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_surf_header = 'surf_file_head_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_edge_header = 'edge_file_head_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_udt_header =   'field_file_head_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_rst_header =   'rst_file_head_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_spectr_header =   'spectr_file_head_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_sph_files_header = 'sph_files_head_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_sph_grid_header = 'mesh_sph_grid_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_sph_lag_header =  'mesh_sph_lag_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_sph_rlm_header =  'mesh_sph_lag_spec_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_sph_mode_header = 'mesh_sph_mode_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_coriolis_tri_int_name = 'coriolis_tri_int_name_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_itp_sph_to_fem =  'interpolate_sph_to_fem_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_itp_fem_to_sph =  'interpolate_fem_to_sph_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_mesh_file_fmt =  'mesh_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_rst_files_fmt =  'rst_files_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_udt_files_fmt =  'field_files_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_sph_files_fmt =  'sph_files_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_itp_files_fmt =  'itp_files_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_spect_files_fmt =  'spectr_files_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_coriolis_file_fmt =  'coriolis_file_fmt_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_debug_flag_ctl =  'debug_flag_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_mem_conserve =   'memory_conservation_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_mesh_extension = 'mesh_extension_flags_ctl'
!
!
      integer(kind = kint) :: i_num_subdomain = 0
      integer(kind = kint) :: i_num_smp =       0
!
      integer(kind = kint) :: i_mesh_header =   0
      integer(kind = kint) :: i_elem_header =   0
      integer(kind = kint) :: i_surf_header =   0
      integer(kind = kint) :: i_edge_header =   0
!
      integer(kind = kint) :: i_udt_header =    0
      integer(kind = kint) :: i_rst_header =    0
      integer(kind = kint) :: i_spectr_header = 0
!
      integer(kind = kint) :: i_sph_files_header = 0
!
      integer(kind = kint) :: i_sph_grid_header = 0
      integer(kind = kint) :: i_sph_lag_header =  0
      integer(kind = kint) :: i_sph_rlm_header =  0
      integer(kind = kint) :: i_sph_mode_header = 0
!
      integer(kind = kint) :: i_coriolis_tri_int_name = 0
      integer(kind = kint) :: i_itp_sph_to_fem = 0
      integer(kind = kint) :: i_itp_fem_to_sph = 0
!
      integer(kind = kint) :: i_mesh_file_fmt =   0
      integer(kind = kint) :: i_rst_files_fmt =   0
      integer(kind = kint) :: i_udt_files_fmt =   0
      integer(kind = kint) :: i_sph_files_fmt =   0
      integer(kind = kint) :: i_itp_files_fmt =   0
      integer(kind = kint) :: i_spect_files_fmt =    0
      integer(kind = kint) :: i_coriolis_file_fmt =  0
!
      integer(kind = kint) :: i_debug_flag_ctl =   0
      integer(kind = kint) :: i_mem_conserve =     0
      integer(kind = kint) :: i_mesh_extension =   0
!
      private :: hd_platform, i_platform
      private :: hd_num_subdomain, hd_num_smp, hd_sph_files_header
      private :: hd_mesh_header, hd_elem_header, hd_surf_header
      private :: hd_udt_header, hd_rst_header
      private :: hd_spectr_header
      private :: hd_edge_header
      private :: hd_sph_grid_header, hd_sph_lag_header
      private :: hd_sph_rlm_header, hd_sph_mode_header
      private :: hd_mesh_file_fmt, hd_rst_files_fmt
      private :: hd_udt_files_fmt, hd_sph_files_fmt
      private :: hd_debug_flag_ctl, hd_mem_conserve
      private :: hd_coriolis_tri_int_name, hd_mesh_extension
      private :: hd_itp_sph_to_fem, hd_itp_fem_to_sph
      private :: hd_itp_files_fmt, hd_coriolis_file_fmt
      private :: hd_spect_files_fmt
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_platform
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_platform) .eq. 0) return
      if (i_platform .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_platform, i_platform)
        if(i_platform .gt. 0) exit
!
!
        call read_integer_ctl_item(hd_num_subdomain,                    &
     &        i_num_subdomain, num_subdomain_ctl)
        call read_integer_ctl_item(hd_num_smp, i_num_smp, num_smp_ctl)
!
!
        call read_character_ctl_item(hd_mesh_header,                    &
     &        i_mesh_header, mesh_file_head_ctl)
!
        call read_character_ctl_item(hd_elem_header,                    &
     &        i_elem_header, elem_file_head_ctl)
        call read_character_ctl_item(hd_surf_header,                    &
     &        i_surf_header, surf_file_head_ctl)
        call read_character_ctl_item(hd_edge_header,                    &
     &        i_edge_header, edge_file_head_ctl)
!
        call read_character_ctl_item(hd_udt_header,                     &
     &        i_udt_header, udt_file_head_ctl)
        call read_character_ctl_item(hd_rst_header,                     &
     &        i_rst_header, rst_file_head_ctl)
        call read_character_ctl_item(hd_spectr_header,                  &
     &        i_spectr_header, spectr_file_head_ctl)
!
        call read_character_ctl_item(hd_sph_files_header,               &
     &        i_sph_files_header, sph_files_head_ctl)
!
        call read_character_ctl_item(hd_sph_grid_header,                &
     &        i_sph_grid_header, sph_grid_head_ctl)
        call read_character_ctl_item(hd_sph_lag_header,                 &
     &        i_sph_lag_header, sph_lag_head_ctl)
        call read_character_ctl_item(hd_sph_rlm_header,                 &
     &        i_sph_rlm_header, sph_lag_spec_head_ctl)
        call read_character_ctl_item(hd_sph_mode_header,                &
     &        i_sph_mode_header, sph_mode_head_ctl)
!
        call read_character_ctl_item(hd_coriolis_tri_int_name,          &
     &        i_coriolis_tri_int_name, coriolis_tri_int_name_ctl)
        call read_character_ctl_item(hd_itp_sph_to_fem,                 &
     &        i_itp_sph_to_fem, interpolate_sph_to_fem_ctl)
        call read_character_ctl_item(hd_itp_fem_to_sph,                 &
     &        i_itp_fem_to_sph, interpolate_fem_to_sph_ctl)
!
        call read_character_ctl_item(hd_mesh_file_fmt,                  &
     &        i_mesh_file_fmt, mesh_file_fmt_ctl)
        call read_character_ctl_item(hd_rst_files_fmt,                  &
     &        i_rst_files_fmt, rst_files_fmt_ctl)
        call read_character_ctl_item(hd_udt_files_fmt,                  &
     &        i_udt_files_fmt, udt_file_fmt_ctl)
        call read_character_ctl_item(hd_sph_files_fmt,                  &
     &        i_sph_files_fmt, sph_file_fmt_ctl)
        call read_character_ctl_item(hd_itp_files_fmt,                  &
     &        i_itp_files_fmt, itp_files_fmt_ctl)
        call read_character_ctl_item(hd_spect_files_fmt,                &
     &        i_spect_files_fmt, spectr_files_fmt_ctl)
        call read_character_ctl_item(hd_coriolis_file_fmt,              &
     &        i_coriolis_file_fmt, coriolis_file_fmt_ctl)
!
        call read_character_ctl_item(hd_debug_flag_ctl,                 &
     &          i_debug_flag_ctl, debug_flag_ctl)
        call read_character_ctl_item(hd_mem_conserve,                   &
     &          i_mem_conserve, memory_conservation_ctl)
        call read_character_ctl_item(hd_mesh_extension,                 &
     &          i_mesh_extension, mesh_extension_flags_ctl)
       end do
!
      end subroutine read_ctl_data_4_platform
!
!  ---------------------------------------------------------------------
!
      end module  m_ctl_data_4_platforms
