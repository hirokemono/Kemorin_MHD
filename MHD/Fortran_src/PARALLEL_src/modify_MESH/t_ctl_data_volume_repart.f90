!>@file   t_ctl_data_volume_repart.f90
!!@brief  module t_ctl_data_volume_repart
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Control data for repartitioning by volume
!!
!!@verbatim
!!      subroutine sel_read_ctl_file_vol_repart(id_control, hd_block,   &
!!     &                                        viz_repart_c, c_buf)
!!      subroutine read_ctl_file_vol_repart(id_control, file_name,      &
!!     &                                    hd_block, viz_repart_c)
!!      subroutine read_control_vol_repart                              &
!!     &         (id_control, hd_block, viz_repart_c, c_buf)
!!        character(len=kchara), intent(in) :: file_name
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(viz_repartition_ctl), intent(inout) :: viz_repart_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dealloc_control_vol_repart(viz_repart_c)
!!      subroutine bcast_control_vol_repart(viz_repart_c)
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
      subroutine sel_read_ctl_file_vol_repart(id_control, hd_block,     &
     &                                        viz_repart_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(viz_repartition_ctl), intent(inout) :: viz_repart_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
      character(len=kchara) :: file_name
!
!
      if(check_file_flag(c_buf, hd_block)) then
        file_name = third_word(c_buf)
        write(*,'(4a)') 'Read file for ', trim(hd_block),               &
     &                 ' from ', trim(file_name)
        call read_ctl_file_vol_repart(id_control+1, file_name,          &
     &                                hd_block, viz_repart_c)
      else if(check_begin_flag(c_buf, hd_block)) then
        write(*,*)  'Repartioning control is included'
        call read_control_vol_repart(id_control, hd_block,              &
     &                               viz_repart_c, c_buf)
      end if
!
      end subroutine sel_read_ctl_file_vol_repart
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_file_vol_repart(id_control, file_name,        &
     &                                    hd_block, viz_repart_c)
!
      use calypso_mpi
      use skip_comment_f
      use bcast_4_platform_ctl
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(viz_repartition_ctl), intent(inout) :: viz_repart_c
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(id_control, file=file_name,status='old')
!
        do
          call load_one_line_from_control(id_control, c_buf1)
          call read_control_vol_repart                                  &
     &       (id_control, hd_block, viz_repart_c, c_buf1)
          if(viz_repart_c%i_viz_repartition_ctl .gt. 0) exit
        end do
        close(id_control)
      end if
!
      end subroutine read_ctl_file_vol_repart
!
!   --------------------------------------------------------------------
!
      subroutine read_control_vol_repart                                &
     &         (id_control, hd_block, viz_repart_c, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(viz_repartition_ctl), intent(inout) :: viz_repart_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(viz_repart_c%i_viz_repartition_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
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
      subroutine bcast_control_vol_repart(viz_repart_c)
!
      use calypso_mpi
      use calypso_mpi_int
      use bcast_4_platform_ctl
!
      type(viz_repartition_ctl), intent(inout) :: viz_repart_c
!
!
      call bcast_ctl_data_4_platform(viz_repart_c%viz_plt)
!
      call bcast_FEM_mesh_control(viz_repart_c%Fmesh_ctl)
      call bcast_ctl_data_new_decomp(viz_repart_c%new_part_ctl)
      call bcast_FEM_sleeve_control(viz_repart_c%Fsleeve_ctl)
!
      call calypso_mpi_bcast_one_int                                    &
     &   (viz_repart_c%i_viz_repartition_ctl, 0)
!
      end subroutine bcast_control_vol_repart
!
!   --------------------------------------------------------------------
!
      subroutine dup_control_vol_repart(org_viz_repart_c,              &
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
      new_viz_repart_c%i_viz_repartition_ctl                            &
     &      = org_viz_repart_c%i_viz_repartition_ctl
!
      end subroutine dup_control_vol_repart
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_volume_repart
