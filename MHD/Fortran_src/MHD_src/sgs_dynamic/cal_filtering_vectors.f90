!
!      module cal_filtering_vectors
!
!      Written by H. Matsui
!
!      subroutine cal_filtered_vector(i_filter, i_vect)
!      subroutine cal_filtered_vector_in_fluid(i_filter, i_vect)
!          i_filter: field UD foe filtered field
!          i_vect: original field ID
!
      module cal_filtering_vectors
!
      use m_precision
!
      implicit none
!
      private :: cal_filtered_vector_in_fluid
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_vector(i_filter, i_vect)
!
      use m_control_parameter
      use m_geometry_data
!
      use cal_3d_filter_phys
      use cal_3d_filter_phys_smp
      use cal_line_filtering_vector
      use copy_nodal_fields
      use nod_phys_send_recv
!
      integer (kind=kint), intent(in) :: i_filter, i_vect
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
!
        call cal_3d_ez_filter_vector_phys(num_whole_filter_grp,         &
     &      id_whole_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_3d_filter_vector_phys_smp(num_whole_filter_grp,        &
     &      id_whole_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_3d_ez_filter_vector_smp(num_whole_filter_grp,          &
     &      id_whole_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_3d_filter_vector_phys(num_whole_filter_grp,            &
     &      id_whole_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
!
        if (i_filter .ne. i_vect) then
          call copy_vector_component(i_filter, i_vect)
        end if
        call cal_l_filtering_vector(i_filter, inod_smp_stack)
        call vector_send_recv(i_filter)
!
      end if
!
      end subroutine cal_filtered_vector
!
! ----------------------------------------------------------------------
!
      subroutine cal_filtered_vector_in_fluid(i_filter, i_vect)
!
       use m_control_parameter
       use m_geometry_data
!
       use cal_3d_filter_phys
       use cal_3d_filter_phys_smp
       use cal_line_filtering_vector
       use copy_nodal_fields
       use nod_phys_send_recv
!
       integer (kind=kint), intent(in) :: i_filter, i_vect
!
!
      if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING ) then
!
        call cal_3d_ez_filter_vector_phys(num_fluid_filter_grp,         &
     &      id_fluid_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING ) then
!
        call cal_3d_filter_vector_phys_smp(num_fluid_filter_grp,        &
     &      id_fluid_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING) then
!
        call cal_3d_ez_filter_vector_smp(num_fluid_filter_grp,          &
     &      id_fluid_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_3D_FILTERING) then
!
        call cal_3d_filter_vector_phys(num_fluid_filter_grp,            &
     &      id_fluid_filter_grp, i_filter, i_vect)
!
      else if ( iflag_SGS_filter .eq. id_SGS_LINE_FILTERING) then
!
        if (i_filter .ne. i_vect) then
          call copy_vector_component(i_filter, i_vect)
        end if
        call cal_l_filtering_vector(i_filter, inod_smp_stack)
        call vector_send_recv(i_filter)
!
      end if
!
      end subroutine cal_filtered_vector_in_fluid
!
! ----------------------------------------------------------------------
!
      end module cal_filtering_vectors
