!>@file   t_ctl_data_gen_sph_w_repart.f90
!!@brief  module t_ctl_data_gen_sph_w_repart
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_ctl_file_gen_sph_w_repart(file_name,            &
!!     &                                          gen_SPH_wP_c, c_buf)
!!      subroutine write_ctl_file_gen_sph_w_repart(file_name,           &
!!     &                                           gen_SPH_wP_c)
!!        character(len=kchara), intent(in) :: file_name
!!        type(ctl_data_gen_sph_w_repart), intent(inout) :: gen_SPH_wP_c
!!@endverbatim
!
      module t_ctl_data_gen_sph_w_repart
!
      use m_precision
!
      use m_machine_parameter
      use skip_comment_f
!
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_gen_sph_shell
      use t_ctl_data_volume_repart
!
      implicit none
!
!
      integer(kind=kint), parameter, private :: control_file_code = 11
!
      type ctl_data_gen_sph_w_repart
!>        Block name
        character(len=kchara) :: hd_gen_sph_w_repart = 'MHD_control'
!
!>        Structure for file settings
        type(platform_data_control) :: plt
!
!>        File name to read spherical shell control file
        character (len = kchara) :: fname_psph = 'NO_FILE'
!>        Control structure for parallel spherical shell
        type(parallel_sph_shell_control) :: psph_ctl
!
!>         File name for repartition control block
        character(len = kchara) :: fname_vol_repart_ctl = 'NO_FILE'
!>        Structure for new partitioning controls
        type(viz_repartition_ctl) :: repart_ctl
!
        integer(kind=kint) :: i_sph_mesh_ctl = 0
        integer(kind=kint) :: i_viz_control =  0
      end type ctl_data_gen_sph_w_repart
!
!   2nd level for MHD
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_shell = 'spherical_shell_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_control = 'visual_control'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_partition = 'viz_repartition_ctl'
!
!       Deplecated label
      character(len=kchara), parameter, private                         &
     &                    :: hd_lic_partition = 'LIC_repartition_ctl'
!
      private :: read_ctl_data_gen_sph_w_repart
      private :: read_viz_repart_ctl_only, write_viz_repart_ctl_only
      private :: write_ctl_data_gen_sph_w_repart
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_ctl_file_gen_sph_w_repart(file_name,              &
     &                                          gen_SPH_wP_c, c_buf)
!
      character(len=kchara), intent(in) :: file_name
      type(ctl_data_gen_sph_w_repart), intent(inout) :: gen_SPH_wP_c
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open(control_file_code, file = file_name, status='old' )
!
      do
        call load_one_line_from_control(control_file_code,              &
     &      gen_SPH_wP_c%hd_gen_sph_w_repart, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_ctl_data_gen_sph_w_repart                             &
     &     (control_file_code, gen_SPH_wP_c%hd_gen_sph_w_repart,        &
     &      gen_SPH_wP_c, c_buf)
        if(gen_SPH_wP_c%i_sph_mesh_ctl .gt. 0) exit
      end do
      close(control_file_code)
!
      c_buf%level = c_buf%level - 1
      if(c_buf%iend .gt. 0) return
!
      end subroutine read_ctl_file_gen_sph_w_repart
!
! ----------------------------------------------------------------------
!
      subroutine write_ctl_file_gen_sph_w_repart(file_name,             &
     &                                           gen_SPH_wP_c)
!
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(ctl_data_gen_sph_w_repart), intent(in) :: gen_SPH_wP_c
!
      integer(kind = kint) :: level1
!
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      write(*,*) 'Write control file: ', trim(file_name)
      level1 = 0
      open(control_file_code, file = file_name, status='old' )
      call write_ctl_data_gen_sph_w_repart                              &
     &   (control_file_code, gen_SPH_wP_c%hd_gen_sph_w_repart,          &
     &    gen_SPH_wP_c, level1)
      close(control_file_code)
!
      end subroutine write_ctl_file_gen_sph_w_repart
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_ctl_data_gen_sph_w_repart                         &
     &         (id_control, hd_block, gen_SPH_wP_c, c_buf)
!
      use ctl_data_platforms_IO
      use ctl_file_gen_sph_shell_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_gen_sph_w_repart), intent(inout) :: gen_SPH_wP_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(gen_SPH_wP_c%i_sph_mesh_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, gen_SPH_wP_c%plt, c_buf)
        call sel_read_ctl_gen_shell_grids(id_control, hd_sph_shell,     &
     &      gen_SPH_wP_c%fname_psph, gen_SPH_wP_c%psph_ctl, c_buf)
        call read_viz_repart_ctl_only(id_control, hd_viz_control,       &
     &                                gen_SPH_wP_c, c_buf)
      end do
      gen_SPH_wP_c%i_sph_mesh_ctl = 1
!
      end subroutine read_ctl_data_gen_sph_w_repart
!
!  ---------------------------------------------------------------------
!
      subroutine write_ctl_data_gen_sph_w_repart                        &
     &         (id_control, hd_block, gen_SPH_wP_c, level)
!
      use ctl_data_platforms_IO
      use ctl_file_gen_sph_shell_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(ctl_data_gen_sph_w_repart), intent(in) :: gen_SPH_wP_c
      integer(kind = kint), intent(inout) :: level
!
!
      if(gen_SPH_wP_c%i_sph_mesh_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms                                      &
     &   (id_control, hd_platform, gen_SPH_wP_c%plt, level)
      call sel_write_ctl_gen_shell_grids(id_control, hd_sph_shell,      &
     &    gen_SPH_wP_c%fname_psph, gen_SPH_wP_c%psph_ctl, level)
      call write_viz_repart_ctl_only(id_control, hd_viz_control,        &
     &                               gen_SPH_wP_c, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_ctl_data_gen_sph_w_repart
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_viz_repart_ctl_only                               &
     &         (id_control, hd_block, gen_SPH_wP_c, c_buf)
!
      use t_read_control_elements
      use ctl_file_volume_repart_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      character(len=kchara), intent(in) :: hd_block
      type(ctl_data_gen_sph_w_repart), intent(inout) :: gen_SPH_wP_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(gen_SPH_wP_c%i_viz_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call sel_read_ctl_file_vol_repart(id_control, hd_viz_partition, &
     &      gen_SPH_wP_c%fname_vol_repart_ctl,                          &
     &      gen_SPH_wP_c%repart_ctl, c_buf)
        call sel_read_ctl_file_vol_repart(id_control, hd_lic_partition, &
     &      gen_SPH_wP_c%fname_vol_repart_ctl,                          &
     &      gen_SPH_wP_c%repart_ctl, c_buf)
      end do
      gen_SPH_wP_c%i_viz_control = 1
!
      end subroutine read_viz_repart_ctl_only
!
!  ---------------------------------------------------------------------
!
      subroutine write_viz_repart_ctl_only                              &
     &         (id_control, hd_block, gen_SPH_wP_c, level)
!
      use ctl_file_volume_repart_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control 
      character(len=kchara), intent(in) :: hd_block
      type(ctl_data_gen_sph_w_repart), intent(in) :: gen_SPH_wP_c
      integer(kind = kint), intent(inout) :: level
!
!
      if(gen_SPH_wP_c%i_viz_control .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call sel_write_ctl_file_vol_repart(id_control, hd_viz_partition,  &
     &    gen_SPH_wP_c%fname_vol_repart_ctl, gen_SPH_wP_c%repart_ctl,   &
     &    level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_viz_repart_ctl_only
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_sph_shell_define_ctl(gen_SPH_wP_c)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
!
      type(ctl_data_gen_sph_w_repart), intent(inout) :: gen_SPH_wP_c
!
!
      call reset_control_platforms(gen_SPH_wP_c%plt)
      call dealloc_parallel_shell_ctl(gen_SPH_wP_c%psph_ctl)
      call dealloc_control_vol_repart(gen_SPH_wP_c%repart_ctl)
!
      gen_SPH_wP_c%i_viz_control =  0
      gen_SPH_wP_c%i_sph_mesh_ctl = 0
!
      end subroutine dealloc_sph_shell_define_ctl
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_gen_sph_w_repart
