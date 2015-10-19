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
      use m_nod_comm_table
      use m_geometry_data
      use m_node_phys_data
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
     &      id_whole_filter_grp, i_scalar,                              &
     &      nod_fld1%ntot_phys, i_filter, nod_fld1%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_3d_filter_scalar_phys_smp(num_whole_filter_grp,        &
     &      id_whole_filter_grp, i_scalar,                              &
     &      nod_fld1%ntot_phys, i_filter, nod_fld1%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_3d_ez_filter_scalar_smp(num_whole_filter_grp,          &
     &      id_whole_filter_grp, i_scalar,                              &
     &      nod_fld1%ntot_phys, i_filter, nod_fld1%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_3d_filter_scalar_phys(num_whole_filter_grp,            &
     &      id_whole_filter_grp, i_scalar,                              &
     &      nod_fld1%ntot_phys, i_filter, nod_fld1%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
!
        if (i_filter .ne. i_scalar) then
           call copy_scalar_component                                   &
     &        (node1, nod_fld1, i_scalar, i_filter)
        end if
        call cal_l_filtering_scalar(node1%numnod, node1%istack_nod_smp, &
     &      nod_fld1%ntot_phys, i_filter, nod_fld1%d_fld)
        call scalar_send_recv(i_filter, node1, nod_comm, nod_fld1)
      end if
!
      end subroutine cal_filtered_scalar
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_scalar_in_fluid(i_filter, i_scalar)
!
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
     &         id_fluid_filter_grp, i_scalar,                           &
     &         nod_fld1%ntot_phys, i_filter, nod_fld1%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_3d_filter_scalar_phys_smp(num_fluid_filter_grp,        &
     &      id_fluid_filter_grp, i_scalar,                              &
     &      nod_fld1%ntot_phys, i_filter, nod_fld1%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_3d_ez_filter_scalar_smp(num_fluid_filter_grp,          &
     &      id_fluid_filter_grp, i_scalar,                              &
     &      nod_fld1%ntot_phys, i_filter, nod_fld1%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_3d_filter_scalar_phys(num_fluid_filter_grp,            &
     &      id_fluid_filter_grp, i_scalar,                              &
     &      nod_fld1%ntot_phys, i_filter, nod_fld1%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
!
        if (i_filter .ne. i_scalar) then
           call copy_scalar_component                                   &
     &        (node1, nod_fld1, i_scalar, i_filter)
        end if
        call cal_l_filtering_scalar(node1%numnod, node1%istack_nod_smp, &
     &      nod_fld1%ntot_phys, i_filter, nod_fld1%d_fld)
        call scalar_send_recv(i_filter, node1, nod_comm, nod_fld1)
      end if
!
      end subroutine cal_filtered_scalar_in_fluid
!
! ----------------------------------------------------------------------
!
      end module cal_filtering_scalars
