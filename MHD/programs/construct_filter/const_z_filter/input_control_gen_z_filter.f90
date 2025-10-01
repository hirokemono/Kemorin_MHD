!
!      module input_control_gen_z_filter
!
!     Written by H. Matsui on June, 2007
!
!!      subroutine s_input_control_4_z_commute                          &
!!     &         (nod_comm, node, ele, surf, edge,                      &
!!     &          mat_crs, CG_param, DJDS_param)
!!        type(communication_table), intent(inout) :: nod_comm
!!        type(node_data), intent(inout) :: node
!!        type(element_data), intent(inout) :: ele
!!        type(surface_data), intent(inout) :: surf
!!        type(edge_data), intent(inout) :: edge
!!        type(CRS_matrix), intent(inout) :: mat_crs
!!        type(CG_poarameter), intent(inout) :: CG_param
!!        type(DJDS_poarameter), intent(inout) :: DJDS_param
!
      module input_control_gen_z_filter
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_iccg_parameter
      use t_crs_matrix
      use t_ctl_data_gen_z_filter
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: fname_zfilter_ctl = "ctl_z_filter"
      type(ctl_data_gen_z_filter), save, private :: z_filter_ctl1
!
      private :: bcast_ctl_data_gen_z_filter
      private :: bcast_plane_model_param_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_4_z_commute                            &
     &         (nod_comm, node, ele, surf, edge,                        &
     &          mat_crs, CG_param, DJDS_param)
!
      use m_machine_parameter
      use calypso_mpi
!
      use set_ctl_gen_z_filter
      use const_geometry_z_commute
!
      type(communication_table), intent(inout) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      type(CRS_matrix), intent(inout) :: mat_crs
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_poarameter), intent(inout) :: DJDS_param
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_z_filter'
      if(my_rank .eq. 0) then
        call read_control_4_z_filter(fname_zfilter_ctl, z_filter_ctl1)
      end if
      call bcast_ctl_data_gen_z_filter(z_filter_ctl1)
!
      if(z_filter_ctl1%i_filter_control .ne. 1) then
        call calypso_MPI_abort(z_filter_ctl1%i_filter_control,          &
     &                             'control file is broken')
      end if
!
      call set_ctl_params_4_gen_z_filter                                &
     &   (z_filter_ctl1, mat_crs, CG_param, DJDS_param)
      call dealloc_ctl_data_gen_z_filter(z_filter_ctl1)
!
!  --  set geometry
!
      if (iflag_debug.eq.1) write(*,*) 'set_geometry_z_commute'
      call set_geometry_z_commute(nod_comm, node, ele, surf, edge)
!
      end subroutine s_input_control_4_z_commute
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_gen_z_filter(z_filter_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
      use bcast_ctl_data_gen_filter
!
      type(ctl_data_gen_z_filter), intent(inout) :: z_filter_ctl
!
!
      call bcast_plane_model_param_ctl(z_filter_ctl%cube_c)
      call bcast_filter_param_ctl(z_filter_ctl%gen_f_ctl)
!
      call bcast_ctl_type_c1(z_filter_ctl%z_filter_head_ctl)
      call bcast_ctl_type_i1(z_filter_ctl%ip_smp_z_ctl)
!
      call calypso_mpi_bcast_one_int(z_filter_ctl%i_filter_control, 0)
!
      end subroutine bcast_ctl_data_gen_z_filter
!
!   --------------------------------------------------------------------
!
      subroutine bcast_plane_model_param_ctl(cube_c)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(ctl_data_4_plane_model), intent(inout) :: cube_c
!
!
      call bcast_ctl_type_i1(cube_c%num_of_sleeve_ctl)
      call bcast_ctl_type_c1(cube_c%horizontal_grid_ctl)
      call bcast_ctl_type_r3(cube_c%plane_size_ctl)
      call bcast_ctl_type_i3(cube_c%nnod_plane_ctl)
      call bcast_ctl_type_i3(cube_c%ndomain_plane_ctl)
      call bcast_ctl_type_c3(cube_c%unit_len_plane_ctl)
!
      call calypso_mpi_bcast_one_int(cube_c%i_plane_def, 0)
!
      end subroutine bcast_plane_model_param_ctl
!
! -----------------------------------------------------------------------
!
      end module input_control_gen_z_filter
