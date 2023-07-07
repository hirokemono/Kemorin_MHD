!>@file   t_ctl_data_volume_repart.f90
!!@brief  module t_ctl_data_volume_repart
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Control data for repartitioning by volume
!!
!!@verbatim
!!      subroutine init_control_vol_repart_label(hd_block, viz_repart_c)
!!      subroutine read_control_vol_repart                              &
!!     &         (id_control, hd_block, viz_repart_c, c_buf)
!!        character(len=kchara), intent(in) :: file_name
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(viz_repartition_ctl), intent(inout) :: viz_repart_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_vol_repart                             &
!!     &         (id_control, hd_block, viz_repart_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(viz_repartition_ctl), intent(in) :: viz_repart_c
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine dealloc_control_vol_repart(viz_repart_c)
!!        type(viz_repartition_ctl), intent(inout) :: viz_repart_c
!!      subroutine dup_control_vol_repart(org_viz_repart_c,             &
!!     &                                  new_viz_repart_c)
!!        type(viz_repartition_ctl), intent(in) :: org_viz_repart_c
!!        type(viz_repartition_ctl), intent(inout) :: new_viz_repart_c
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin viz_repartition_ctl
!!    begin viz_data_files_def
!!      num_subdomain_ctl    96
!!      mesh_file_prefix         'mesh_new/in'
!!      mesh_file_fmt_ctl    'merged_bin_gz'
!!    end viz_data_files_def
!!
!!    begin FEM_mesh_ctl
!!      ...
!!    end FEM_mesh_ctl
!!
!!    begin new_partitioning_ctl
!!      ...
!!    end new_partitioning_ctl
!!
!!    begin FEM_sleeve_ctl
!!      ...
!!    end FEM_sleeve_ctl
!!  end viz_repartition_ctl
!!
!! -------------------------------------------------------------------
!!@endverbatim
      module t_ctl_data_volume_repart
!
      use m_precision
      use m_machine_parameter
!
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_FEM_sleeve_size
      use t_ctl_data_volume_grouping
!
      implicit  none
!
!
!>      Structure for new partitioning controls
      type viz_repartition_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'viz_repartition_ctl'
!
!>        Structure for new file controls
        type(platform_data_control) :: viz_plt
!
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
!>        Structure for new partitioning controls
        type(new_patition_control) :: new_part_ctl
!>        Structure of Sleeve size controls
        type(FEM_sleeve_control) :: Fsleeve_ctl
!
        integer(kind = kint) :: i_viz_repartition_ctl = 0
      end type viz_repartition_ctl
!
!     Top level
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_platform =  'viz_data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_FEM_mesh =      'FEM_mesh_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_new_partition = 'new_partitioning_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_FEM_sleeve =    'FEM_sleeve_ctl'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_vol_repart                                &
     &         (id_control, hd_block, viz_repart_c, c_buf)
!
      use t_read_control_elements
      use ctl_data_platforms_IO
      use ctl_data_volume_grouping_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(viz_repartition_ctl), intent(inout) :: viz_repart_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(viz_repart_c%i_viz_repartition_ctl .gt. 0) return
      call init_platforms_labels(hd_viz_platform, viz_repart_c%viz_plt)
      call init_ctl_label_new_partition(hd_new_partition,               &
     &                                  viz_repart_c%new_part_ctl)
      call init_FEM_mesh_ctl_label(hd_FEM_mesh, viz_repart_c%Fmesh_ctl)
      call init_FEM_sleeve_ctl_label(hd_FEM_sleeve,                     &
     &                               viz_repart_c%Fsleeve_ctl)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_viz_platform, viz_repart_c%viz_plt, c_buf)
!
        call read_FEM_mesh_control                                      &
     &     (id_control, hd_FEM_mesh, viz_repart_c%Fmesh_ctl, c_buf)
        call read_ctl_data_new_partition(id_control, hd_new_partition,  &
     &      viz_repart_c%new_part_ctl, c_buf)
        call read_FEM_sleeve_control                                    &
     &     (id_control, hd_FEM_sleeve, viz_repart_c%Fsleeve_ctl, c_buf)
      end do
      viz_repart_c%i_viz_repartition_ctl = 1
!
      end subroutine read_control_vol_repart
!
!   --------------------------------------------------------------------
!
      subroutine write_control_vol_repart                               &
     &         (id_control, hd_block, viz_repart_c, level)
!
      use t_read_control_elements
      use ctl_data_platforms_IO
      use ctl_data_volume_grouping_IO
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(viz_repartition_ctl), intent(in) :: viz_repart_c
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(viz_repart_c%i_viz_repartition_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms                                      &
     &   (id_control, hd_viz_platform, viz_repart_c%viz_plt, level)
!
      call write_FEM_mesh_control                                       &
     &   (id_control, viz_repart_c%Fmesh_ctl, level)
      call write_ctl_data_new_partition(id_control, hd_new_partition,   &
     &    viz_repart_c%new_part_ctl, level)
      call write_FEM_sleeve_control                                     &
     &   (id_control, hd_FEM_sleeve, viz_repart_c%Fsleeve_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_control_vol_repart
!
!   --------------------------------------------------------------------
!
      subroutine init_control_vol_repart_label(hd_block, viz_repart_c)
!
      use ctl_data_platforms_IO
      use ctl_data_volume_grouping_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(viz_repartition_ctl), intent(inout) :: viz_repart_c
!
!
      viz_repart_c%block_name = hd_block
      call init_platforms_labels(hd_viz_platform, viz_repart_c%viz_plt)
      call init_ctl_label_new_partition(hd_new_partition,               &
     &                                  viz_repart_c%new_part_ctl)
      call init_FEM_mesh_ctl_label(hd_FEM_mesh, viz_repart_c%Fmesh_ctl)
      call init_FEM_sleeve_ctl_label(hd_FEM_sleeve,                     &
     &                               viz_repart_c%Fsleeve_ctl)
!
      end subroutine init_control_vol_repart_label
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_vol_repart(viz_repart_c)
!
      type(viz_repartition_ctl), intent(inout) :: viz_repart_c
!
!
      call reset_control_platforms(viz_repart_c%viz_plt)
      call reset_FEM_mesh_control(viz_repart_c%Fmesh_ctl)
      call dealloc_ctl_data_new_decomp(viz_repart_c%new_part_ctl)
      call dealloc_ctl_data_FEM_sleeve(viz_repart_c%Fsleeve_ctl)
!
      viz_repart_c%i_viz_repartition_ctl =   0
!
      end subroutine dealloc_control_vol_repart
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dup_control_vol_repart(org_viz_repart_c,               &
      &                                  new_viz_repart_c)
!
      type(viz_repartition_ctl), intent(in) :: org_viz_repart_c
      type(viz_repartition_ctl), intent(inout) :: new_viz_repart_c
!
!
      call copy_ctl_data_4_platform(org_viz_repart_c%viz_plt,           &
     &                              new_viz_repart_c%viz_plt)
!
      call copy_FEM_mesh_control(org_viz_repart_c%Fmesh_ctl,            &
     &                           new_viz_repart_c%Fmesh_ctl)
      call dup_ctl_data_new_decomp(org_viz_repart_c%new_part_ctl,       &
     &                             new_viz_repart_c%new_part_ctl)
      call copy_FEM_sleeve_control(org_viz_repart_c%Fsleeve_ctl,        &
     &                             new_viz_repart_c%Fsleeve_ctl)
!
      new_viz_repart_c%block_name = org_viz_repart_c%block_name
      new_viz_repart_c%i_viz_repartition_ctl                            &
     &      = org_viz_repart_c%i_viz_repartition_ctl
!
      end subroutine dup_control_vol_repart
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_volume_repart
