!
!      module cal_filtering_tensors
!
!      Written by H. Matsui
!
!!      subroutine cal_filtered_sym_tensor                              &
!!     &         (nod_comm, node, i_filter, i_vect, nod_fld)
!!      subroutine cal_filtered_tensor_in_fluid                         &
!!     &         (nod_comm, node, i_filter, i_vect, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!          i_filter: field UD foe filtered field
!!          i_vect:   original field ID
!
      module cal_filtering_tensors
!
      use m_precision
!
      use m_control_parameter
      use t_comm_table
      use t_geometry_data
      use t_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_sym_tensor                                &
     &         (nod_comm, node, i_filter, i_vect, nod_fld)
!
      use cal_3d_filter_phys
      use cal_3d_filter_phys_smp
      use cal_line_filtering_tensor
      use copy_nodal_fields
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      integer (kind = kint), intent(in) :: i_filter, i_vect
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
!
        call cal_3d_ez_filter_tensor_phys                               &
     &     (nod_comm, num_whole_filter_grp, id_whole_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_3d_filter_tensor_phys_smp                              &
     &     (nod_comm, num_whole_filter_grp, id_whole_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_3d_ez_filter_tensor_smp                                &
     &     (nod_comm, num_whole_filter_grp, id_whole_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_3d_filter_tensor_phys                                  &
     &     (nod_comm, num_whole_filter_grp, id_whole_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
        if (i_filter .ne. i_vect) then
          call copy_tensor_component(node, nod_fld, i_vect, i_filter)
        end if
        call cal_l_filtering_tensor(node%numnod, node%istack_nod_smp,   &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
        call sym_tensor_send_recv(i_filter, node, nod_comm, nod_fld)
      end if
!
      end subroutine cal_filtered_sym_tensor
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_tensor_in_fluid                           &
     &         (nod_comm, node, i_filter, i_vect, nod_fld)
!
      use cal_3d_filter_phys
      use cal_3d_filter_phys_smp
      use cal_line_filtering_tensor
      use copy_nodal_fields
      use nod_phys_send_recv
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      integer (kind=kint), intent(in) :: i_filter, i_vect
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
!
        call cal_3d_ez_filter_tensor_phys                               &
     &     (nod_comm, num_fluid_filter_grp, id_fluid_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_3d_filter_tensor_phys_smp                              &
     &     (nod_comm, num_fluid_filter_grp, id_fluid_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_3d_ez_filter_tensor_smp                                &
     &     (nod_comm, num_fluid_filter_grp, id_fluid_filter_grp,        &
     &      node%numnod, node%internal_node,                            &
     &      i_vect, nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_3d_filter_tensor_phys                                  &
     &     (nod_comm, num_fluid_filter_grp, id_fluid_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
        if (i_filter .ne. i_vect) then
          call copy_tensor_component(node, nod_fld, i_vect, i_filter)
        end if
        call cal_l_filtering_tensor(node%numnod, node%istack_nod_smp,   &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
        call sym_tensor_send_recv(i_filter, node, nod_comm, nod_fld)
      end if
!
      end subroutine cal_filtered_tensor_in_fluid
!
! ----------------------------------------------------------------------
!
      end module cal_filtering_tensors
