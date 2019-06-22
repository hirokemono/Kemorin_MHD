!!bcast_ctl_data_4_cub_kemo.f90
!!      module bcast_ctl_data_4_cub_kemo
!!
!!        programmed by H.Matsui on Aug., 2007
!!
!!      subroutine bcsst_control_data_plane_mesh(cubmesh_c)
!!        type(ctl_data_4_cub_kemo), intent(inout) :: cubmesh_c
!!      subroutine bcast_plane_model_param_ctl(cube_c)
!!        type(ctl_data_4_plane_model), intent(inout) :: cube_c
!
      module bcast_ctl_data_4_cub_kemo
!
      use calypso_mpi
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine bcsst_control_data_plane_mesh(cubmesh_c)
!
      use calypso_mpi
      use t_ctl_data_4_cub_kemo
      use bcast_4_platform_ctl
      use bcast_4_filter_files_ctl
!
      type(ctl_data_4_cub_kemo), intent(inout) :: cubmesh_c
!
!
      call bcast_ctl_data_4_platform(cubmesh_c%cubmesh_plt)
      call bcast_ctl_data_4_filter_files(cubmesh_c%ffile_cub_ctl)
!
      call bcast_plane_model_param_ctl(cubmesh_c%cube_c)
!
      call bcast_ctl_type_i1(cubmesh_c%num_z_filter_ctl)
      call bcast_ctl_type_r1(cubmesh_c%omitting_value_ctl)
!
      call bcast_ctl_type_c1(cubmesh_c%z_filter_head_ctl)
      call bcast_ctl_type_c1(cubmesh_c%vert_filter_type_ctl)
!
      call MPI_BCAST(cubmesh_c%i_plane_mesh, 1,                         &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call MPI_BCAST(cubmesh_c%i_l_filter_ctl, 1,                       &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcsst_control_data_plane_mesh
!
! -----------------------------------------------------------------------
!
      subroutine bcast_plane_model_param_ctl(cube_c)
!
      use t_ctl_data_4_plane_model
!
      type(ctl_data_4_plane_model), intent(inout) :: cube_c
!
!
      call bcast_ctl_type_i3(cube_c%nnod_plane_ctl)
      call bcast_ctl_type_i3(cube_c%ndomain_plane_ctl)
      call bcast_ctl_type_c1(cube_c%num_of_sleeve_ctl)
      call bcast_ctl_type_r3(cube_c%plane_size_ctl)
!
      call bcast_ctl_array_c3(cube_c%unit_len_plane_ctl)
      call bcast_ctl_array_c1(cube_c%horizontal_grid_ctl)
!
      call MPI_BCAST(cube_c%i_plane_def, 1,                             &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_plane_model_param_ctl
!
! -----------------------------------------------------------------------
!
      end module bcast_ctl_data_4_cub_kemo
