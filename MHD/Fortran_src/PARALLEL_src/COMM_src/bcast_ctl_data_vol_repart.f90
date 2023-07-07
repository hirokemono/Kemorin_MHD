!>@file   bcast_ctl_data_vol_repart.f90
!!@brief  module bcast_ctl_data_vol_repart
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine bcast_control_vol_repart(viz_repart_c)
!!        type(viz_repartition_ctl), intent(inout) :: viz_repart_c
!!      subroutine bcast_ctl_data_new_decomp(new_part_ctl)
!!        type(new_patition_control), intent(inout) :: new_part_ctl
!!      subroutine bcast_FEM_sleeve_control(sleeve_ctl)
!!        type(FEM_sleeve_control), intent(inout) :: sleeve_ctl
!!      subroutine bcast_masking_ctl_data(mask_ctl)
!!        type(masking_by_field_ctl), intent(inout) :: mask_ctl
!!@endverbatim
      module bcast_ctl_data_vol_repart
!
      use m_precision
      use m_machine_parameter
!
      use calypso_mpi
!
      implicit  none
!
      private :: bcast_ctl_data_new_decomp
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine bcast_control_vol_repart(viz_repart_c)
!
      use t_ctl_data_volume_repart
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
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
      call calypso_mpi_bcast_character(viz_repart_c%block_name,         &
     &                                 cast_long(kchara), 0)
!
      end subroutine bcast_control_vol_repart
!
!   --------------------------------------------------------------------
!
      subroutine bcast_ctl_data_new_decomp(new_part_ctl)
!
      use t_ctl_data_volume_grouping
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_control_arrays
!
      type(new_patition_control), intent(inout) :: new_part_ctl
!
      integer(kind = kint) :: i
!
      call bcast_ctl_type_c1(new_part_ctl%repart_table_head_ctl)
      call bcast_ctl_type_c1(new_part_ctl%repart_table_fmt_ctl)
!
      call bcast_ctl_array_ci(new_part_ctl%ndomain_section_ctl)
      call bcast_ctl_type_c1(new_part_ctl%partition_reference_ctl)
      call bcast_ctl_type_c1(new_part_ctl%trace_count_head_ctl)
      call bcast_ctl_type_c1(new_part_ctl%trace_count_fmt_ctl)
      call bcast_ctl_type_c1(new_part_ctl%masking_switch_ctl)
      call bcast_ctl_type_r1(new_part_ctl%power_of_volume_ctl)
      call bcast_ctl_type_r1(new_part_ctl%masking_weight_ctl)
      call bcast_ctl_type_r1(new_part_ctl%weight_to_previous_ctl)
      call bcast_ctl_type_i1(new_part_ctl%sleeve_level_ctl)
      call bcast_ctl_type_i1(new_part_ctl%ratio_of_grouping_ctl)
!
      call calypso_mpi_bcast_one_int(new_part_ctl%num_masking_ctl, 0)
      if(my_rank .ne. 0) call alloc_repart_masking_ctl(new_part_ctl)
      do i = 1, new_part_ctl%num_masking_ctl
        call bcast_masking_ctl_data(new_part_ctl%mask_ctl(i))
      end do
!
      call calypso_mpi_bcast_character(new_part_ctl%block_name,         &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int                                    &
     &   (new_part_ctl%i_new_patition_ctl, 0)
!
      end subroutine bcast_ctl_data_new_decomp
!
! -----------------------------------------------------------------------
!
      subroutine bcast_FEM_sleeve_control(sleeve_ctl)
!
      use t_ctl_data_FEM_sleeve_size
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(FEM_sleeve_control), intent(inout) :: sleeve_ctl
!
!
      call bcast_ctl_type_c1(sleeve_ctl%ref_vector_ctl)
      call bcast_ctl_type_c1(sleeve_ctl%sleeve_extension_mode_ctl)
      call bcast_ctl_type_i1(sleeve_ctl%sleeve_level_ctl)
      call bcast_ctl_type_r1(sleeve_ctl%sleeve_size_ctl)
!
      call calypso_mpi_bcast_one_int(sleeve_ctl%i_FEM_sleeve_ctl, 0)
      call calypso_mpi_bcast_character(sleeve_ctl%block_name,           &
     &                                 cast_long(kchara), 0)
!
      end subroutine bcast_FEM_sleeve_control
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_masking_ctl_data(mask_ctl)
!
      use t_control_data_masking
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(masking_by_field_ctl), intent(inout) :: mask_ctl
!
!
      call bcast_ctl_type_c1(mask_ctl%mask_type_ctl)
      call bcast_ctl_type_c1(mask_ctl%field_name_ctl)
      call bcast_ctl_type_c1(mask_ctl%component_ctl)
      call bcast_ctl_array_r2(mask_ctl%mask_range_ctl)
!
      call calypso_mpi_bcast_character(mask_ctl%block_name,              &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(mask_ctl%i_mask_control, 0)
!
      end subroutine bcast_masking_ctl_data
!
!  ---------------------------------------------------------------------
!
      end module bcast_ctl_data_vol_repart
