!
!      module cal_filtering_scalars
!
!      Written by H. Matsui
!
!      subroutine cal_filtered_scalar(n_filter, i_filter, i_scalar)
!      subroutine cal_filtered_scalar_in_fluid(i_filter, i_scalar)
!          i_filter: field ID foe filtered field
!          i_scalar: original field ID
!
      module cal_filtering_scalars
!
      use m_precision
!
      implicit none
!
      private :: cal_filtered_scalar_in_fluid
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_scalar(i_filter, i_scalar)
!
       use m_geometry_data
       use m_control_parameter
!
       use cal_3d_filter_phys
       use cal_3d_filter_phys_smp
       use cal_line_filtering_scalar
       use copy_nodal_fields
       use nod_phys_send_recv
!
       integer (kind=kint), intent(in) :: i_filter, i_scalar
!
!
      if (iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING) then
!
        call cal_3d_ez_filter_scalar_phys(num_whole_filter_grp,         &
     &      id_whole_filter_grp, i_filter, i_scalar)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_3d_filter_scalar_phys_smp(num_whole_filter_grp,        &
     &      id_whole_filter_grp, i_filter, i_scalar)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_3d_ez_filter_scalar_smp(num_whole_filter_grp,          &
     &      id_whole_filter_grp, i_filter, i_scalar)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_3d_filter_scalar_phys(num_whole_filter_grp,            &
     &      id_whole_filter_grp, i_filter, i_scalar)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
!
        if (i_filter .ne. i_scalar) then
           call copy_scalar_component(i_filter, i_scalar)
        end if
        call cal_l_filtering_scalar(i_filter, node1%istack_nod_smp)
        call scalar_send_recv(num_tot_nod_phys, i_filter, d_nod)
      end if
!
      end subroutine cal_filtered_scalar
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_scalar_in_fluid(i_filter, i_scalar)
!
       use m_geometry_data
       use m_control_parameter
!
       use cal_3d_filter_phys
       use cal_3d_filter_phys_smp
       use cal_line_filtering_scalar
       use copy_nodal_fields
       use nod_phys_send_recv
!
       integer (kind=kint), intent(in) :: i_filter, i_scalar
!
!
      if (iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING) then
!
        call cal_3d_ez_filter_scalar_phys(num_fluid_filter_grp,         &
     &         id_fluid_filter_grp, i_filter, i_scalar)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_3d_filter_scalar_phys_smp(num_fluid_filter_grp,        &
     &         id_fluid_filter_grp, i_filter, i_scalar)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_3d_ez_filter_scalar_smp(num_fluid_filter_grp,          &
     &         id_fluid_filter_grp, i_filter, i_scalar)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_3d_filter_scalar_phys(num_fluid_filter_grp,            &
     &         id_fluid_filter_grp, i_filter, i_scalar)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
!
        if (i_filter .ne. i_scalar) then
           call copy_scalar_component(i_filter, i_scalar)
        end if
        call cal_l_filtering_scalar(i_filter, node1%istack_nod_smp)
        call scalar_send_recv(num_tot_nod_phys, i_filter, d_nod)
      end if
!
      end subroutine cal_filtered_scalar_in_fluid
!
! ----------------------------------------------------------------------
!
      end module cal_filtering_scalars
