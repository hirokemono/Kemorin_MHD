!
!      module cal_filtering_vectors
!
!      Written by H. Matsui
!
!!      subroutine cal_filtered_vector                                  &
!!     &         (nod_comm, node, i_filter, i_vect, nod_fld)
!!      subroutine cal_filtered_vector_in_fluid                         &
!!     &         (nod_comm, node, i_filter, i_vect, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        integer (kind=kint), intent(in) :: i_filter, i_vect
!!        type(phys_data), intent(inout) :: nod_fld
!!          i_filter: field UD foe filtered field
!!          i_vect: original field ID
!
      module cal_filtering_vectors
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
      subroutine cal_filtered_vector                                    &
     &         (nod_comm, node, i_filter, i_vect, nod_fld)
!
      use cal_3d_filter_phys
      use cal_3d_filter_phys_smp
      use cal_line_filtering_vector
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
        call cal_3d_ez_filter_vector_phys                               &
     &     (nod_comm, num_whole_filter_grp, id_whole_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_3d_filter_vector_phys_smp                              &
     &     (nod_comm, num_whole_filter_grp, id_whole_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_3d_ez_filter_vector_smp                                &
     &     (nod_comm, num_whole_filter_grp, id_whole_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_3d_filter_vector_phys                                  &
     &     (nod_comm, num_whole_filter_grp, id_whole_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
!
        if (i_filter .ne. i_vect) then
          call copy_vector_component(node, nod_fld, i_vect, i_filter)
        end if
        call cal_l_filtering_vector(node%numnod, node%istack_nod_smp,   &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
        call vector_send_recv(i_filter, node, nod_comm, nod_fld)
      end if
!
      end subroutine cal_filtered_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_vector_in_fluid                           &
     &         (nod_comm, node, i_filter, i_vect, nod_fld)
!
!
       use cal_3d_filter_phys
       use cal_3d_filter_phys_smp
       use cal_line_filtering_vector
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
        call cal_3d_ez_filter_vector_phys                               &
     &     (nod_comm, num_fluid_filter_grp, id_fluid_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_3d_filter_vector_phys_smp                              &
     &     (nod_comm, num_fluid_filter_grp, id_fluid_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_3d_ez_filter_vector_smp                                &
     &     (nod_comm, num_fluid_filter_grp, id_fluid_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_3d_filter_vector_phys                                  &
     &     (nod_comm, num_fluid_filter_grp, id_fluid_filter_grp,        &
     &      i_vect, node%numnod, node%internal_node,                    &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
!
        if (i_filter .ne. i_vect) then
          call copy_vector_component(node, nod_fld, i_vect, i_filter)
        end if
        call cal_l_filtering_vector(node%numnod, node%istack_nod_smp,   &
     &      nod_fld%ntot_phys, i_filter, nod_fld%d_fld)
        call vector_send_recv(i_filter, node, nod_comm, nod_fld)
      end if
!
      end subroutine cal_filtered_vector_in_fluid
!
! ----------------------------------------------------------------------
!
      end module cal_filtering_vectors
